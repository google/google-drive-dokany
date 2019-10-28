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

#include <stdio.h>
#include <Windows.h>
#include <sddl.h>  // Must be after Windows.h.
#include <WinUser.h>

#include <functional>
#include <mutex>  // NOLINT
#include <string>
#include <thread>  // NOLINT
#include <vector>

#include "file_system.h"
#include "gtest.h"
#include "kernel_defs.h"
#include "logger.h"
#include "test_file_callbacks.h"
#include "test_logger.h"
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

class FileSystemTestHelper : public VolumeCallbacks {
 public:
  FileSystemTestHelper(const std::wstring& mount_point, uint64_t param)
      : mount_point_(mount_point),
        drive_letter_(mount_point[0]),
        param_(param) {
    callbacks_.reset(new TestFileCallbacks(
        std::bind(&FileSystemTestHelper::InvokeCallback, this, _1)));
    fs_.reset(new FileSystem(callbacks_.get(), this, &logger_));
    main_loop_reply_waiting_ =
        CreateEvent(nullptr, true, false, L"main loop reply");
    free_space_.total_bytes = 1024 * 1024 * 1024;
    free_space_.free_bytes_for_all_users = 1024 * 1024 * 512;
    free_space_.free_bytes_for_process_user = 1024 * 1024 * 256;
  }

  virtual ~FileSystemTestHelper() {
    CloseHandle(main_loop_reply_waiting_);
  }

  bool unmounted() const {
    return unmounted_;
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
    CheckDriveExistence(drive_letter_, true);
  }

  void Unmounted() override {
    CheckDriveExistence(drive_letter_, false);
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
      DOKAN_LOG_ERROR(
          &logger_,
          "SDDL \"%S\" could not be converted to a security descriptor;"
          " error %u", sddl.c_str(), GetLastError());
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
    EXPECT_EQ(exists, (drives & (1 << (toupper(letter) - 'A'))) != 0);
  }

  void RunFS(StartupOptions options,
             const std::function<void (FileSystem*)>& test_logic) {
    if (param_ & kSuppressFileNameInEventContext) {
      options.flags |= DOKAN_OPTION_SUPPRESS_FILE_NAME_IN_EVENT_CONTEXT;
    }
    if (param_ & kAssumePagingIoIsLocked) {
      options.flags |= DOKAN_OPTION_ASSUME_PAGING_IO_IS_LOCKED;
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

  HANDLE Open(const std::wstring& path, DWORD desired_access) {
    const std::wstring file_name = mount_point_ + path;
    HANDLE handle = CreateFile(file_name.c_str(), desired_access, 0, nullptr,
                               OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    return handle;
  }

  const std::wstring mount_point_;
  const char drive_letter_;
  const uint64_t param_;
  std::unique_ptr<TestFileCallbacks> callbacks_;
  TestLogger logger_;
  std::unique_ptr<FileSystem> fs_;
  FreeSpace free_space_;
  bool unmounted_ = false;
  std::vector<std::thread> callback_threads_;
  std::mutex main_loop_reply_mutex_;
  HANDLE main_loop_reply_waiting_;
  std::vector<std::function<void()>> main_loop_replies_;
  NTSTATUS free_space_result_ = STATUS_SUCCESS;
};

}  // namespace test
}  // namespace dokan

#endif DOKAN_TEST_FILE_SYSTEM_TEST_HELPER_H_
