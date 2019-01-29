/*
  Dokan : user-mode file system library for Windows

  Copyright (C) 2018 Google, Inc.
  http://dokan-dev.github.io

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with this program. If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef DOKAN_FILE_SYSTEM_H_
#define DOKAN_FILE_SYSTEM_H_

#include <ntstatus.h>
#include <windows.h>

#include <atomic>
#include <condition_variable>
#include <functional>
#include <memory>
#include <mutex>
#include <string>
#include <unordered_set>

#include "change_handler.h"
#include "device.h"
#include "driver_log_subscriber.h"
#include "file_callbacks.h"
#include "file_info.h"
#include "file_info_handler.h"
#include "find_handler.h"
#include "kernel_defs.h"
#include "logger.h"
#include "notification_handler.h"
#include "public.h"
#include "reply_handler.h"
#include "startup_options.h"
#include "volume_callbacks.h"
#include "volume_info_handler.h"

namespace dokan {

enum MountResult {
  SUCCESS,
  FAILED_TO_OPEN_GLOBAL_DEVICE = 1,
  FAILED_TO_PREPARE_MOUNT_REQUEST = 2,
  FAILED_TO_ISSUE_MOUNT_REQUEST = 3,
  FAILED_TO_OPEN_MOUNTED_DEVICE = 4,
  DRIVER_VERSION_MISMATCH = 5,
  GENERIC_FAILURE = 6,
  UNRECOGNIZED_DRIVER_MOUNT_RESPONSE = 7
};

// A FileSystem is the user-mode representation of a running file system. It
// uses a FileCallbacks object and a VolumeCallbacks object to implement the
// file system operations, and it provides an API to control the kernel-mode
// driver.
class __declspec(dllexport) FileSystem {
 public:
  // The maximum amount of data a read can actually transfer at once. We are
  // limited to less than uint32 max because the buffer gets wrapped in a
  // DeviceIoControl payload that itself is limited to uint32 max. Currently we
  // fail reads bigger than this size. Writes over this size are problematic at
  // the driver level and don't make it to the DLL.
  static const ULONG kMaxReadSize;

  // Creates a FileSystem that is completely inactive and not mounted until its
  // Mount method is invoked. The caller retains ownership of the callbacks and
  // logger, which must outlive this FileSystem.
  FileSystem(FileCallbacks* file_callbacks, VolumeCallbacks* volume_callbacks,
             Logger* logger);

  FileSystem(const FileSystem&) = delete;
  FileSystem& operator=(const FileSystem&) = delete;

  // Asserts that the file system has been unmounted. It should not be destroyed
  // until the Unmounted callback has returned. It is then safe to destroy it on
  // the I/O thread.
  ~FileSystem();

  // Mounts the file system, leaving it in a suspended state where virtually any
  // I/O sent to it will block (except for requests that can be answered
  // directly by the driver). Its owner must then execute a ReceiveIo/DispatchIo
  // loop to process I/O. A FileSystem object can only be mounted once.
  MountResult Mount(const std::wstring& requested_mount_point,
                    const StartupOptions& startup_options);

  const StartupOptions& startup_options() const {
    return startup_options_;
  }

  // Returns the device name assigned to this particular file system by the
  // driver, in the format Volume{guid}. This can be used with functions like
  // CreateFile in place of the associated drive letter.
  const std::wstring& device_name() const {
    return device_name_;
  }

  // Returns the actual mount point assigned by the mount manager, like "G:".
  // This is valid starting when the Mounted callback is invoked.
  const std::wstring& mount_point() const {
    return mount_point_;
  }

  // Returns whether this FileSystem is mounted and ready to receive I/O.
  bool safe_to_access() const {
    return safe_to_access_;
  }

  // To operate the file system, the owner should run a loop like this:
  // while (fs->ReceiveIo()) {
  //   WaitForSingleObject(fs->wait_handle());
  //   fs->DispatchIo();
  // }
  // This logic is broken into three steps so that if the caller shares the
  // loop with other sources of work, it can WaitForMultipleObjects. If the file
  // system is the only source of work for the loop, the wait shown above is
  // still required, since neither ReceiveIo nor DispatchIo will block waiting
  // for the I/O request to come in.
  bool ReceiveIo();
  void DispatchIo();
  HANDLE wait_handle() const {
    return overlapped_.hEvent;
  }

  // TODO(drivefs-team): Change notification functions will go here.

  // Unmounts the file system. This kicks off an asynchronous sequence whereby
  // at some point in the future ReceiveIo will return false, terminating the
  // I/O loop. The Unmounted callback is invoked either within the ReceiveIo
  // call that returns false (if there are no pending callbacks), or when the
  // last pending callback completes. After the Unmounted callback returns, it
  // is safe to destroy this FileSystem object on the I/O thread.
  void Unmount();

  // Waits until the file system's I/O loop gets out of the primordial stage
  // where the driver fakes success for CreateFile calls. See
  // DokanDispatchCreate:563 in the driver. For now there's an assumption
  // that we can't remove that state, since it's hard to prove otherwise. If
  // the file system is unmounted before becoming safe to access, this function
  // unblocks and returns false.
  bool WaitUntilSafeToAccess();

  // Waits until automatic post-start work is done. This is generally only
  // useful for establishing noise-free conditions in tests. A normal consumer
  // of FileSystem does not need to care when this is done.
  void WaitUntilPostStartDone();

  // Returns the handler that the FileSystem's owner can use to send
  // notifications when files change via a side channel (i.e. not via the
  // FileSystem).
  NotificationHandler* notification_handler() const {
    return notification_handler_.get();
  }

  void AssertNotCalledOnIoThread() const;

 private:
  // Sends a broadcast message that causes, for example, existing Explorer
  // windows to react to the drive being mounted or unmounted. Otherwise,
  // existing Explorer windows would have to be refreshed or reopened in order
  // to show the drive immediately after mounting.
  void BroadcastExistenceToApps(bool exists);

  // Invoked on an ephemeral thread to carry out post-mount work that needs to
  // be done while the driver event loop is active, such as activating the
  // keepalive handle.
  void PostStart();

  void MaybeFinishUnmount();

  typedef void (FileSystem::*DispatchFn)(EVENT_CONTEXT*);
  // Converts the given IRP function code from the Windows Driver Kit to the
  // pointer to the corresponding dispatch function in this class.
  static DispatchFn GetDispatchFn(ULONG major_irp_function);

  void DispatchCreate(EVENT_CONTEXT* request);
  void CompleteCreate(EVENT_CONTEXT* request, FileHandle* handle,
                      ULONG disposition, ULONG options, NTSTATUS status);

  // Helper for dispatching a Create request that is really aimed at the parent
  // directory of the specified file. This invokes the Create callback for the
  // parent and handles the result.
  void DispatchParentCreate(EVENT_CONTEXT* request,
                            const std::wstring& file_name,
                            ULONG disposition, DWORD options,
                            DWORD desired_access, DWORD file_attributes,
                            DWORD share_access);

  void DispatchGetInfo(EVENT_CONTEXT* request);
  void CompleteGetInfo(EVENT_CONTEXT* request, FileHandle* handle,
                       ULONG info_class, NTSTATUS status,
                       ULONG used_buffer_size,
                       util::UniqueVarStructPtr<EVENT_INFORMATION> reply);

  // This always completes synchronously.
  void DispatchGetSecurity(EVENT_CONTEXT* context);

  void DispatchGetVolumeInfo(EVENT_CONTEXT* request);
  void CompleteGetVolumeInfo(
       EVENT_CONTEXT* request, ULONG info_class, NTSTATUS status,
       ULONG used_buffer_size,
       util::UniqueVarStructPtr<EVENT_INFORMATION> reply);

  void DispatchChange(EVENT_CONTEXT* request);
  void CompleteChange(EVENT_CONTEXT* request, FileHandle* handle,
                      ULONG info_class, NTSTATUS status,
                      ULONG buffer_size,
                      util::UniqueVarStructPtr<EVENT_INFORMATION> reply);

  void DispatchFindFiles(EVENT_CONTEXT* request);
  void CompleteFindFiles(EVENT_CONTEXT* request, FileHandle* handle,
                         const std::wstring& pattern, ULONG info_class,
                         NTSTATUS status, ULONG used_buffer_size,
                         ULONG next_request_start_index,
                         util::UniqueVarStructPtr<EVENT_INFORMATION> reply);

  void DispatchRead(EVENT_CONTEXT* request);
  void CompleteRead(EVENT_CONTEXT* request, FileHandle* handle,
                    int64_t offset, uint32_t buffer_length, NTSTATUS status,
                    util::UniqueVarStructPtr<EVENT_INFORMATION> reply);

  void DispatchWrite(EVENT_CONTEXT* request);
  void CompleteWrite(util::UniqueVarStructPtr<EVENT_CONTEXT> request,
                     FileHandle* handle, int64_t offset, NTSTATUS status);

  void DispatchFlush(EVENT_CONTEXT* context);
  void CompleteFlush(EVENT_CONTEXT* request, FileHandle* handle,
                     NTSTATUS status);

  void DispatchCleanup(EVENT_CONTEXT* request);
  void CompleteCleanup(EVENT_CONTEXT* request, FileHandle* handle,
                       NTSTATUS status);

  void DispatchClose(EVENT_CONTEXT* request);

  // Helper for CompleteXXX functions that have a variable-sized reply. This
  // replies to the driver and cleans up the request. provided_var_buffer_size
  // is the number of bytes the request said we could use for reply->Buffer.
  // used_var_buffer_size is how much of that we actually filled with data.
  void CompleteRequestWithVarReply(
       EVENT_CONTEXT* request,
       FileHandle* handle,
       NTSTATUS status,
       ULONG provided_var_buffer_size,
       ULONG used_var_buffer_size,
       util::UniqueVarStructPtr<EVENT_INFORMATION> reply);

  // Called when the Complete part of a request has done its work.
  // ReplyToDriver calls this, but in cases where there is no reply, it must be
  // called directly.
  void RequestCompleted(EVENT_CONTEXT* request);

  // Sends the given reply to the driver, and calls RequestCompleted. The size
  // is sizeof(EVENT_INFORMATION) for a fixed-size reply, or something larger
  // for a variable-sized reply.
  void ReplyToDriver(EVENT_CONTEXT* request,
                     util::UniqueVarStructPtr<EVENT_INFORMATION> reply,
                     ULONG size);

  // Increments the reference count for the file handle in the given request
  // and returns the handle. If the request does not refer to a file handle,
  // then this returns nullptr. One way this would occur is if something
  // outside the file system tries to perform an operation on the keepalive
  // handle.
  FileHandle* ReferenceHandle(EVENT_CONTEXT* request);

  // Decrements the reference count for the given file handle and returns the
  // new count.
  // - handle must not be nullptr.
  // - If *handle is nullptr then the result is 0.
  // - If *handle is non-null and this call drops its reference count to 0,
  //   then this function calls the Close file system callback, deletes
  //   *handle, and sets handle = nullptr.
  int64_t RemoveReference(FileHandle** handle);

  void AssertCalledOnIoThread() const;

  // These are usable on any thread. The non-const members in this group are
  // effectively const after Mount returns.
  FileCallbacks* const file_callbacks_;
  VolumeCallbacks* const volume_callbacks_;
  Logger* const logger_;
  StartupOptions startup_options_;
  DriverLogSubscriber driver_log_subscriber_;
  std::wstring device_name_;
  std::wstring mount_point_;
  ULONG driver_mount_id_ = 0;
  DWORD io_thread_id_ = 0;
  std::atomic<bool> mounted_ = false;
  std::unique_ptr<ChangeHandler> change_handler_;
  std::unique_ptr<FileInfoHandler> file_info_handler_;
  std::unique_ptr<VolumeInfoHandler> volume_info_handler_;
  std::unique_ptr<FindHandler> find_handler_;

  // These are usable on any thread.
  std::atomic<bool> unmount_requested_ = false;
  std::atomic<bool> safe_to_access_ = false;
  std::atomic<bool> post_start_done_ = false;
  HANDLE keepalive_handle_ = INVALID_HANDLE_VALUE;
  std::unique_ptr<NotificationHandler> notification_handler_;

  // These must only be used on the I/O dispatching thread on which Mount,
  // ReceiveIo, and DispatchIo are called.
  std::unique_ptr<std::thread> post_start_thread_;
  std::unique_ptr<ReplyHandler> reply_handler_;
  bool io_stopped_ = false;
  Device global_device_;
  Device device_;
  OVERLAPPED overlapped_;
  std::vector<char> io_buffer_;
  std::unordered_set<EVENT_CONTEXT*> pending_requests_;

  // state_ must be guarded by mutex_.
  std::mutex mutex_;
  std::condition_variable state_;
};

}  // namespace dokan

#endif // DOKAN_FILE_SYSTEM_H_