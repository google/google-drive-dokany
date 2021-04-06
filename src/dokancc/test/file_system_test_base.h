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

// Base class for dokan tests that mount a drive.

#ifndef DOKAN_TEST_FILE_SYSTEM_TEST_BASE_H_
#define DOKAN_TEST_FILE_SYSTEM_TEST_BASE_H_

#include "file_system_test_helper.h"
#include "gtest/gtest.h"
#include "test_file_callbacks.h"
#include "test_logger.h"

#include <regex>

using std::placeholders::_1;

namespace dokan {
namespace test {

const wchar_t kDefaultMountPoint[] = L"R:";

class FileSystemTestBase : public ::testing::TestWithParam<uint64_t> {
 public:
  FileSystemTestBase()
    : helper_(kDefaultMountPoint, GetParam()),
      logger_(helper_.logger_.get()), callbacks_(helper_.callbacks_.get()),
      mount_point_(helper_.mount_point_), fs_(helper_.fs_.get()) {}

 protected:
  void RunFS(const std::function<void (FileSystem*)>& test_logic) {
    helper_.RunFS(options_, test_logic);
  }

  HANDLE Open(const std::wstring& path_within_drive, DWORD desired_access,
              DWORD share_access = 0) {
    return helper_.Open(path_within_drive, desired_access, share_access);
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

  void ExpectValidDeviceName(const std::wstring& name) {
    const std::regex name_regex(
        "\\\\Device\\\\Volume"
        "\\{[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\}");
    std::string narrow_name;
    EXPECT_TRUE(util::Narrow(name, &narrow_name));
    if (!std::regex_match(narrow_name, name_regex)) {
      ADD_FAILURE() << "Unexpected device name: " << name;
    }
  }

  void CheckInvalidFunction(ULONG code) {
    RunFS([&] (FileSystem* fs) {
      std::unique_ptr<Device> device = helper_.OpenDevice(fs);
      ASSERT_NE(device, nullptr);
      char input[1024];
      char output[1024];
      EXPECT_FALSE(device->Control(code, input, sizeof(input), output,
                                   sizeof(output)));
      EXPECT_EQ(GetLastError(), ERROR_INVALID_FUNCTION);
      fs->Unmount();
    });
  }

  FileSystemTestHelper helper_;
  StartupOptions options_;
  // Avoid having to refer to these via helper_ for single-helper tests.
  Logger* const logger_;
  TestFileCallbacks* const callbacks_;
  const std::wstring mount_point_;
  FileSystem* const fs_;
};

}  // namespace test
}  // namespace dokan

#endif  // DOKAN_TEST_FILE_SYSTEM_TEST_BASE_H_
