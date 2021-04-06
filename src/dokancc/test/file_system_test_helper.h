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

// A test helper that facilitates mounting and using a dokan drive based on
// TestFileCallbacks, TestLogger, etc.

#ifndef DOKAN_TEST_FILE_SYSTEM_TEST_HELPER_H_
#define DOKAN_TEST_FILE_SYSTEM_TEST_HELPER_H_

// clang-format off
#include <stdio.h>
#include <Windows.h>
#include <sddl.h>  // Must be after Windows.h.
#include <WinUser.h>
// clang-format on

#include <functional>
#include <mutex>  // NOLINT
#include <string>
#include <thread>  // NOLINT
#include <vector>

#include "file_system.h"
#include "gtest/gtest.h"
#include "kernel_defs.h"
#include "logger.h"
#include "test_file_callbacks.h"
#include "test_logger.h"
#include "util.h"
#include "volume_callbacks.h"

using std::placeholders::_1;

namespace dokan {
namespace test {

// Test param values. There must be exactly one callback threading option
// specified, and 0 or more other options. The callback threading options
// determine whether TestFileCallbacks functions reply synchronously or
// asynchronously. This distinction only matters for testing DLL code and not
// driver code. All replies are asynchronously received by the driver code.
const uint64_t kCallbackSync = 1;
const uint64_t kCallbackAsyncEphemeralThread = 2;
const uint64_t kSuppressFileNameInEventContext = 4;
const uint64_t kAssumePagingIoIsLocked = 8;
const uint64_t kAllowRequestBatching = 16;
const uint64_t kAllowFullBatching = 32;
const uint64_t kFcbGarbageCollection = 64;
const uint64_t kDispatchDriverLogs = 128;
const uint64_t kUseFsctlEvents = 256;

class FileSystemTestHelper : public VolumeCallbacks {
 public:
  FileSystemTestHelper(const std::wstring& mount_point, uint64_t param, std::unique_ptr<Logger>&& logger)
      : mount_point_(mount_point),
        drive_letter_(mount_point[0]),
        param_(param),
        logger_(std::move(logger)) {
    callbacks_.reset(new TestFileCallbacks(
        std::bind(&FileSystemTestHelper::InvokeCallback, this, _1)));
    fs_.reset(new FileSystem(callbacks_.get(), this, logger_.get()));
    main_loop_reply_waiting_ =
        CreateEvent(nullptr, true, false, L"main loop reply");
    free_space_.total_bytes = 1024 * 1024 * 1024;
    free_space_.free_bytes_for_all_users = 1024 * 1024 * 512;
    free_space_.free_bytes_for_process_user = 1024 * 1024 * 256;
  }

  FileSystemTestHelper(const std::wstring& mount_point, uint64_t param)
      : FileSystemTestHelper(mount_point, param,
                             std::make_unique<TestLogger>()) {}

  virtual ~FileSystemTestHelper() {
    CloseHandle(main_loop_reply_waiting_);
  }

  bool unmounted() const {
    return unmounted_;
  }

  void SetMountedCallback(std::function<void()> callback) {
    mounted_callback_ = std::move(callback);
  }

  void DisableExistenceCheckAfterUnmount() {
    existence_check_after_unmount_ = false;
  }

  void set_free_space_result(NTSTATUS status) {
    free_space_result_ = status;
  }

  void GetVolumeFreeSpace(uint32_t process_id,
                          FreeSpace* free_space,
                          const util::StatusCallback& callback) override {
    *free_space = free_space_;
    callback(free_space_result_);
  }

  void Mounted(FileSystem*) override {
    mounted_callback_();
    if (util::IsMountPointDriveLetter(mount_point_)) {
      CheckDriveExistence(drive_letter_, true);
    } else {
      EXPECT_TRUE(MountPointIsAReparsePoint());
      EXPECT_TRUE(MountPointHasVolumeAttached());
    }
  }

  void Unmounted() override {
    if (existence_check_after_unmount_) {
      if (util::IsMountPointDriveLetter(mount_point_)) {
        CheckDriveExistence(drive_letter_, false);
      } else {
        EXPECT_FALSE(MountPointIsAReparsePoint());
        EXPECT_FALSE(MountPointHasVolumeAttached());
      }
    }
    std::unique_lock<std::mutex> lock(main_loop_reply_mutex_);
    unmounted_ = true;
    SetEvent(main_loop_reply_waiting_);
  }

  void SetSecurityDescriptor(const std::wstring& sddl,
                             PSECURITY_DESCRIPTOR* descriptor,
                             uint32_t* descriptor_length) {
    *descriptor = (PSECURITY_DESCRIPTOR)new char[2048];
    bool result = ConvertStringSecurityDescriptorToSecurityDescriptor(
        sddl.c_str(), 1, descriptor, (ULONG*)descriptor_length);
    if (!result) {
      DOKAN_LOG(ERROR(logger_.get()))
          << "SDDL \"" << sddl
          << "\" could not be converted to a security descriptor; error "
          << GetLastError();
    }
    ASSERT_TRUE(result);
  }

  std::wstring ReadSecurityDescriptorAsSddl(const std::wstring& path,
                                            uint32_t allowed_size,
                                            bool size_must_equal_allowed_size) {
    HANDLE handle = Open(path, GENERIC_READ);
    SECURITY_INFORMATION info = DACL_SECURITY_INFORMATION |
        OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION;
    auto sd = util::MakeUniqueVarStruct<SECURITY_DESCRIPTOR>(allowed_size);
    DWORD length_needed = 0;
    bool result = GetUserObjectSecurity(handle, &info, sd.get(), allowed_size,
        &length_needed);
    EXPECT_TRUE(result);
    if (size_must_equal_allowed_size) {
      EXPECT_EQ(allowed_size, length_needed);
    }
    LPWSTR sd_string = nullptr;
    DWORD sd_string_length = 0;
    result = ConvertSecurityDescriptorToStringSecurityDescriptor(
        sd.get(), SECURITY_DESCRIPTOR_REVISION, info, &sd_string,
        &sd_string_length);
    EXPECT_TRUE(result);
    return sd_string;
  }

  void CheckDriveExistence(char letter, bool exists) {
    DWORD drives = GetLogicalDrives();
    EXPECT_EQ(exists, (drives & (1 << (std::toupper(letter) - 'A'))) != 0);
  }

  void RunFS(StartupOptions options,
             const std::function<void (FileSystem*)>& test_logic) {
    if (param_ & kSuppressFileNameInEventContext) {
      options.flags |= DOKAN_OPTION_SUPPRESS_FILE_NAME_IN_EVENT_CONTEXT;
    }
    if (param_ & kAssumePagingIoIsLocked) {
      options.flags |= DOKAN_OPTION_ASSUME_PAGING_IO_IS_LOCKED;
    }
    if (param_ & kAllowRequestBatching) {
      options.flags |= DOKAN_OPTION_ALLOW_REQUEST_BATCHING;
    }
    if (param_ & kAllowFullBatching) {
      options.flags |= DOKAN_OPTION_ALLOW_FULL_BATCHING;
    }
    if (param_ & kFcbGarbageCollection) {
      options.fcb_garbage_collection_interval_ms = 2000;
    }
    if (param_ & kDispatchDriverLogs) {
        options.flags |= DOKAN_OPTION_DISPATCH_DRIVER_LOGS;
    }
    if (param_ & kUseFsctlEvents) {
      options.flags |= DOKAN_OPTION_USE_FSCTL_EVENTS;
    }
    MountResult result = fs_->Mount(mount_point_, options);
    ASSERT_EQ(MountResult::SUCCESS, result);
    ASSERT_EQ(mount_point_, fs_->mount_point());
    std::thread test_thread([&]() {
      fs_->WaitUntilSafeToAccess();
      test_logic(fs_.get());
    });
    if (fs_->ReceiveIo()) {
      for (;;) {
        HANDLE handles[] = {fs_->wait_handle(), main_loop_reply_waiting_};
        DWORD result = WaitForMultipleObjects(2, handles, FALSE, 45000);
        ASSERT_NE(WAIT_TIMEOUT, result);
        if (result == WAIT_OBJECT_0) {
          fs_->DispatchIo();
          if (!fs_->ReceiveIo()) {
            break;
          }
        } else {
          DispatchMainLoopReplies();
        }
      }
    }
    while (!unmounted_) {
      ASSERT_EQ(WAIT_OBJECT_0,
                WaitForSingleObject(main_loop_reply_waiting_, 45000));
      DispatchMainLoopReplies();
    }
    for (auto& thread : callback_threads_) {
      thread.join();
    }
    test_thread.join();
  }

  void InvokeCallback(const std::function<void()>& callback) {
    if (param_ & kCallbackSync) {
      callback();
    } else if (param_ & kCallbackAsyncEphemeralThread) {
      callback_threads_.emplace_back([this, callback]() {
        PostToMainLoop(callback);
      });
    } else {
      ASSERT_FALSE(true);
    }
  }

  void PostToMainLoop(const std::function<void ()>& callback) {
    std::unique_lock<std::mutex> lock(main_loop_reply_mutex_);
    main_loop_replies_.push_back(callback);
    SetEvent(main_loop_reply_waiting_);
  }

  void DispatchMainLoopReplies() {
    std::vector<std::function<void()>> replies;
    {
      std::unique_lock<std::mutex> lock(main_loop_reply_mutex_);
      std::swap(replies, main_loop_replies_);
      ResetEvent(main_loop_reply_waiting_);
    }
    for (const auto& reply : replies) {
      reply();
    }
  }

  HANDLE OpenFSRootDirectory() {
    return CreateFile((mount_point_ + L"\\").c_str(), GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, nullptr,
        OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  }

  HANDLE OpenFSRootForOverlapped(OVERLAPPED* overlapped) {
    return OpenDirForOverlapped(mount_point_ + L"\\", overlapped);
  }

  HANDLE OpenDirForOverlapped(const std::wstring& path,
                              OVERLAPPED* overlapped) {
    HANDLE handle = CreateFile(path.c_str(), GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, nullptr,
        OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    memset(overlapped, 0, sizeof(OVERLAPPED));
    overlapped->hEvent = CreateEvent(
        nullptr, true, false, L"dokancc_test_overlapped");
    return handle;
  }

  HANDLE Open(const std::wstring& path, DWORD desired_access,
              DWORD share_access = 0) {
    const std::wstring file_name = mount_point_ + path;
    HANDLE handle =
        CreateFile(file_name.c_str(), desired_access, share_access, nullptr,
                   OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    return handle;
  }

  std::unique_ptr<Device> OpenDevice(const FileSystem* fs) {
    auto device = std::make_unique<Device>(logger_.get());
    if (!device->Open(L"\\\\." + fs->device_name())) {
      return nullptr;
    }
    return device;
  }

  bool MountPointIsAReparsePoint() {
    std::wstring mount_point = mount_point_ + L'\\';
    DWORD attributes =
        GetFileAttributes(mount_point.c_str());
    if (attributes == INVALID_FILE_ATTRIBUTES) {
      DOKAN_LOG(ERROR(logger_.get()))
          << " GetFileAttributes failed : " << GetLastError();
      return false;
    }
    return attributes & FILE_ATTRIBUTE_REPARSE_POINT;
  }

  bool MountPointHasVolumeAttached() {
    const DWORD buffer_length = 50;
    WCHAR volum_name[buffer_length];
    memset(volum_name, 0, buffer_length);
    std::wstring mount_point = mount_point_ + L'\\';
    if (!GetVolumeNameForVolumeMountPoint(mount_point.c_str(), volum_name,
                                          buffer_length)) {
      DOKAN_LOG(ERROR(logger_.get()))
          << "GetVolumeNameForVolumeMountPoint failed: " << GetLastError();
      return false;
    }
    DOKAN_LOG(INFO(logger_.get()))
        << "GetVolumeNameForVolumeMountPoint: " << volum_name;
    return true;
  }

  const std::wstring mount_point_;
  const char drive_letter_;
  const uint64_t param_;
  std::unique_ptr<TestFileCallbacks> callbacks_;
  std::unique_ptr<Logger> logger_;
  std::unique_ptr<FileSystem> fs_;
  FreeSpace free_space_;
  bool unmounted_ = false;
  std::vector<std::thread> callback_threads_;
  std::mutex main_loop_reply_mutex_;
  HANDLE main_loop_reply_waiting_;
  std::vector<std::function<void()>> main_loop_replies_;
  NTSTATUS free_space_result_ = STATUS_SUCCESS;
  std::function<void()> mounted_callback_ = []{};
  bool existence_check_after_unmount_ = true;
};

}  // namespace test
}  // namespace dokan

#endif DOKAN_TEST_FILE_SYSTEM_TEST_HELPER_H_
