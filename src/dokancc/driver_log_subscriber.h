#ifndef DOKAN_DRIVER_LOG_SUBSCRIBER_H_
#define DOKAN_DRIVER_LOG_SUBSCRIBER_H_

#include <windows.h>
#include <winevt.h>

#include <memory>

#include "logger.h"

namespace dokan {

// Subscribes to Event Log messages from the driver, and logs them in user mode
// via DOKAN_LOG_INFO/DOKAN_LOG_ERROR. A dokan::FileSystem object uses one of
// these internally and has it active for its own whole lifetime (i.e. before
// mounting and after unmounting). Clients using the C-based dokan DLL can
// instantiate and use one of these separately.
class __declspec(dllexport) DriverLogSubscriber {
 public:
  DriverLogSubscriber(Logger* logger) : logger_(logger) {}
  virtual ~DriverLogSubscriber();

  bool Start();
  void Log(EVT_HANDLE event_handle);

 private:
  Logger* const logger_;
  EVT_HANDLE render_context_ = 0;
  EVT_HANDLE subscription_ = 0;
};

}  // namespace dokan

#endif  // DOKAN_DRIVER_LOG_SUBSCRIBER_H_
