// clang-format off
#include "file_system.h"
// clang-format on

#include "notification_handler.h"

#include <cassert>

#include "public.h"
#include "util.h"
#include "hex_util.h"

namespace dokan {
namespace {

class NotificationHandlerImpl : public NotificationHandler {
 public:
  NotificationHandlerImpl(const FileSystem* fs, Logger* logger)
      : fs_(fs),
        logger_(logger),
        notification_handle_(logger) {
  }

  bool NotifyCreate(const std::wstring& path, bool directory) override {
    return SendNotification(path, directory ? FILE_NOTIFY_CHANGE_DIR_NAME :
                                              FILE_NOTIFY_CHANGE_FILE_NAME,
                            FILE_ACTION_ADDED);
  }

  bool NotifyUpdate(const std::wstring& path) override {
    return SendNotification(path, FILE_NOTIFY_CHANGE_ATTRIBUTES,
                            FILE_ACTION_MODIFIED);
  }

  bool NotifyDelete(const std::wstring& path, bool directory) override {
    return SendNotification(path, directory ? FILE_NOTIFY_CHANGE_DIR_NAME :
                                              FILE_NOTIFY_CHANGE_FILE_NAME,
                            FILE_ACTION_REMOVED);
  }

  bool NotifyRename(const std::wstring& old_path,
                    const std::wstring& new_path,
                    bool directory,
                    bool same_parent) override {
    bool success = SendNotification(
        old_path,
        directory ? FILE_NOTIFY_CHANGE_DIR_NAME : FILE_NOTIFY_CHANGE_FILE_NAME,
        same_parent ? FILE_ACTION_RENAMED_OLD_NAME : FILE_ACTION_REMOVED);
    success &= SendNotification(
        new_path,
        directory ? FILE_NOTIFY_CHANGE_DIR_NAME : FILE_NOTIFY_CHANGE_FILE_NAME,
        same_parent ? FILE_ACTION_RENAMED_NEW_NAME : FILE_ACTION_ADDED);
    return success;
  }

 private:
  // Sends a notification received via a public NotifyXXX function to the
  // driver. The filter value is one of the options for the dwNotifyFilter
  // parameter of ::FindFirstChangeNotification. The action value is one of
  // the options for the Action field of FILE_NOTIFY_INFORMATION.
  bool SendNotification(const std::wstring& path,
                        DWORD filter,
                        DWORD action) {
    assert(fs_->safe_to_access());
    fs_->AssertNotCalledOnIoThread();
    static const std::wstring notification_handle_path =
        fs_->mount_point() + DOKAN_NOTIFICATION_FILE_NAME;
    if (!notification_handle_.is_open() &&
        !notification_handle_.Open(notification_handle_path)) {
      DOKAN_LOG_(ERROR) << "Failed to open notification handle path: "
                        << notification_handle_path;
      return false;
    }
    if (path.empty() || path[0] != L'\\') {
      DOKAN_LOG_(ERROR) << "Notification sent for invalid path: '" << path
                        << "'";
      return false;
    }
    SHORT string_size = (USHORT)(path.size() * sizeof(WCHAR));
    ULONG size = sizeof(DOKAN_NOTIFY_PATH_INTERMEDIATE) + string_size;
    auto input =
        util::MakeUniqueVarStruct<DOKAN_NOTIFY_PATH_INTERMEDIATE>(size);
    input->CompletionFilter = filter;
    input->Action = action;
    input->Length = string_size;
    memcpy(input->Buffer, path.c_str(), string_size);
    if (!notification_handle_.Control(FSCTL_NOTIFY_PATH, input.get(), size)) {
      DOKAN_LOG_(ERROR) << "Failed to send notification for path " << path
                        << ", filter " << Hex(filter) << ", action "
                        << Hex(action);
      return false;
    }
    return true;
  }

  const FileSystem* const fs_;
  Logger* const logger_;

  // A handle to the special dokan file used for change notification
  // DeviceIoControl requests.
  Device notification_handle_;
};

}  // namespace

std::unique_ptr<NotificationHandler> CreateNotificationHandler(
    const FileSystem* fs, Logger* logger) {
  return std::make_unique<NotificationHandlerImpl>(fs, logger);
}

}  // namespace dokan
