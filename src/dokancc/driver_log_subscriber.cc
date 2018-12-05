#include "driver_log_subscriber.h"

#include <windows.h>
#include <winevt.h>

#include "util.h"

namespace dokan {
namespace {

const int kDokanBuildLabel = DOKAN_BUILD_LABEL;
const wchar_t kEventLogChannelPath[] = L"System";
const wchar_t kEventLogQueryFormat[] =
    L"*[System/Provider[@Name = \"googledrivefs%d\"]]";

// The name "LogDriverString" is used despite the obviousness of "Driver",
// because the function name appears in the log file.

void LogDriverString(Logger* logger, const std::string& str) {
  if (!str.empty()) {
    DOKAN_LOG_INFO(logger, str.c_str());
  }
}

void LogDriverString(Logger* logger, const std::wstring& str) {
  std::string narrow_str;
  if (!util::Narrow(str, &narrow_str)) {
    DOKAN_LOG_ERROR(logger, "Failed to narrow event log message.");
    return;
  }
  LogDriverString(logger, narrow_str);
}

DWORD WINAPI SubscriberCallback(EVT_SUBSCRIBE_NOTIFY_ACTION action,
                                PVOID context,
                                EVT_HANDLE event_handle) {
  auto subscriber = static_cast<DriverLogSubscriber*>(context);
  subscriber->Log(event_handle);
  return ERROR_SUCCESS;
}

}  // namespace

bool DriverLogSubscriber::Start() {
  render_context_ = EvtCreateRenderContext(0, 0, EvtRenderContextUser);
  if (!render_context_) {
    DOKAN_LOG_ERROR(logger_, "Could not create render context: 0x%x",
                    GetLastError());
    return false;
  }

  wchar_t query[128];
  swprintf_s(query, kEventLogQueryFormat, kDokanBuildLabel);
  subscription_ = EvtSubscribe(
      0,  // session
      0,  // signal event
      kEventLogChannelPath,
      query,
      0,  // bookmark event to start from
      this,  // context parameter to callback
      SubscriberCallback,
      EvtSubscribeToFutureEvents);
  if (!subscription_) {
    DOKAN_LOG_ERROR(logger_, "Could not create event log subscription: 0x%x",
                    GetLastError());
    return false;
  }

  return true;
}

DriverLogSubscriber::~DriverLogSubscriber() {
  if (subscription_) {
    EvtClose(subscription_);
  }
  if (render_context_) {
    EvtClose(render_context_);
  }
}

void DriverLogSubscriber::Log(EVT_HANDLE event_handle) {
  DWORD bytes_used = 0;
  DWORD property_count = 0;
  char buffer[2048];
  BOOL result = EvtRender(render_context_, event_handle, EvtRenderEventValues,
                          sizeof(buffer), buffer, &bytes_used,
                          &property_count);
  if (!result) {
    DOKAN_LOG_INFO(logger_, "Failed to render event: 0x%x", GetLastError());
    return;
  }

  EVT_VARIANT* variant = (EVT_VARIANT*)buffer;
  for (DWORD i = 0; i < property_count; i++, variant++) {
    switch (variant->Type) {
      case EvtVarTypeString:
        LogDriverString(logger_, variant->StringVal);
        break;
      case EvtVarTypeAnsiString:
        LogDriverString(logger_, variant->AnsiStringVal);
        break;
      default:
        // We encounter stuff of various other data types, which seems to be
        // noise.
        break;
    }
  }
}

}  // namespace dokan
