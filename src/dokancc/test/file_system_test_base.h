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

// Integrated test that makes sure the file system works when mounted with mock
// callbacks.

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

enum CallbackThreading {
  SYNC,
  ASYNC_EPHEMERAL_THREAD
};

class FileSystemTestBase : public ::testing::TestWithParam<CallbackThreading>,
                           public VolumeCallbacks {
 public:
  FileSystemTestBase() {
    callbacks_.reset(new TestFileCallbacks(
        std::bind(&FileSystemTestBase::InvokeCallback, this, _1)));
    fs_.reset(new FileSystem(callbacks_.get(), this, &logger_));
    main_loop_reply_waiting_ =
        CreateEvent(nullptr, true, false, L"main loop reply");
    free_space_.total_bytes = 1024 * 1024 * 1024;
    free_space_.free_bytes_for_all_users = 1024 * 1024 * 512;
    free_space_.free_bytes_for_process_user = 1024 * 1024 * 256;
  }

  virtual ~FileSystemTestBase() {
    CloseHandle(main_loop_reply_waiting_);
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

 protected:
  void CheckDriveExistence(char letter, bool exists) {
    DWORD drives = GetLogicalDrives();
    EXPECT_EQ(exists, (drives & (1 << (toupper(letter) - 'A'))) != 0);
  }

  void RunFS(const std::function<void (FileSystem*)>& test_logic) {
    MountResult result = fs_->Mount(mount_point_, options_);
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
    switch (GetParam()) {
      case SYNC:
        callback();
        break;
      case ASYNC_EPHEMERAL_THREAD:
        callback_threads_.emplace_back([this, callback]() {
          PostToMainLoop(callback);
        });
        break;
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

  void RemoveSidFromThread(const std::wstring& sddl) {
    HANDLE token = INVALID_HANDLE_VALUE;
    ASSERT_TRUE(OpenProcessToken(GetCurrentProcess(),
                                 TOKEN_QUERY | TOKEN_DUPLICATE,
                                 &token));
    ASSERT_NE(INVALID_HANDLE_VALUE, token);
    HANDLE new_token = INVALID_HANDLE_VALUE;
    SID_AND_ATTRIBUTES sids_to_disable;
    ConvertStringSidToSid(sddl.c_str(), &sids_to_disable.Sid);
    sids_to_disable.Attributes = 0;
    CreateRestrictedToken(token, 0, 1, &sids_to_disable, 0, nullptr, 0, nullptr,
                          &new_token);
    ASSERT_NE(INVALID_HANDLE_VALUE, new_token);
    ASSERT_TRUE(ImpersonateLoggedOnUser(new_token));
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

  // Checks the result of an overlapped ReadDirectoryChangesW call.
  void CheckNotification(HANDLE handle,
                         const char* buffer,
                         OVERLAPPED* overlapped,
                         DWORD expected_action,
                         const std::wstring& expected_file_name) {
    DWORD bytes_read = 0;
    GetOverlappedResult(handle, overlapped, &bytes_read, true);
    EXPECT_GE(bytes_read, sizeof(FILE_NOTIFY_INFORMATION));
    const auto info = reinterpret_cast<const FILE_NOTIFY_INFORMATION*>(buffer);
    EXPECT_EQ(expected_action, info->Action);
    EXPECT_EQ(expected_file_name,
        std::wstring(info->FileName, info->FileNameLength / sizeof(wchar_t)));
  }

  // Checks the result of an overlapped ReadDirectoryChangesW call that expects
  // 2 records.
  void CheckNotification(HANDLE handle,
                         const char* buffer,
                         OVERLAPPED* overlapped,
                         DWORD expected_action1,
                         const std::wstring& expected_file_name1,
                         DWORD expected_action2,
                         const std::wstring& expected_file_name2) {
    CheckNotification(handle, buffer, overlapped, expected_action1,
                      expected_file_name1);
    const auto info = reinterpret_cast<const FILE_NOTIFY_INFORMATION*>(
        buffer + reinterpret_cast<const FILE_NOTIFY_INFORMATION*>(buffer)
            ->NextEntryOffset);
    EXPECT_EQ(expected_action2, info->Action);
    EXPECT_EQ(expected_file_name2,
        std::wstring(info->FileName, info->FileNameLength / sizeof(wchar_t)));
  }

  const std::wstring mount_point_ = L"R:";
  const char drive_letter_ = mount_point_[0];
  StartupOptions options_;
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
