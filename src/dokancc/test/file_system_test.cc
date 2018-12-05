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

#include "file_system_test_base.h"

#include <versionhelpers.h>

namespace dokan {
namespace test {

const wchar_t kEveryoneSddl[] = L"WD";
const wchar_t kAllowEveryoneSddl[] = L"D:(A;;GA;;;WD)";
const wchar_t kAllowEveryoneReadonlySddl[] = L"D:(A;;GR;;;WD)";
const wchar_t kOwnerAccountOperatorsSddl[] = L"O:AO";
const wchar_t kGroupAdminsSddl[] = L"G:BA";

class MountTest : public FileSystemTestBase {};

TEST_F(MountTest, NeverMounted) {
  // This test is basically just proving that the destructor doesn't assert if
  // we never mount.
  EXPECT_FALSE(logger_.HasErrors());
}

TEST_P(MountTest, MountAndUnmountAfterPostStart) {
  RunFS([](FileSystem* fs) {
    fs->WaitUntilPostStartDone();
    fs->Unmount();
  });

  EXPECT_TRUE(unmounted_);
  EXPECT_FALSE(logger_.HasErrors());
}

TEST_P(MountTest, MountAndUnmountImmediately) {
  RunFS([](FileSystem* fs) {
    fs->Unmount();
  });

  EXPECT_TRUE(unmounted_);
  // Note that errors from PostStart may be logged during this test.
}

TEST_P(MountTest, MountAndUnmountOnIoThread) {
  RunFS([this](FileSystem* fs) {
    PostToMainLoop([=] {
      fs->Unmount();
    });
  });

  EXPECT_TRUE(unmounted_);
  // Note that errors from PostStart may be logged during this test.
}

TEST_P(MountTest, MountAndUnmountBeforeLoop) {
  fs_->Mount(mount_point_, options_);
  fs_->Unmount();
  EXPECT_FALSE(fs_->ReceiveIo());
  EXPECT_TRUE(unmounted_);
  // Note that errors from PostStart may be logged during this test.
}

INSTANTIATE_TEST_CASE_P(MountTests, MountTest, testing::Values(
    CallbackThreading::SYNC, CallbackThreading::ASYNC_EPHEMERAL_THREAD));

class OpenCloseTest : public FileSystemTestBase {};

TEST_P(OpenCloseTest, CreateAndClose) {
  const wchar_t path[] = L"\\test_CreateAndClose.txt";
  callbacks_->SetUpFile(path);
  const Observer* observer = callbacks_->AddObserver(
      path,
      FILE_GENERIC_READ,
      FILE_ATTRIBUTE_NORMAL,
      FILE_SHARE_READ | FILE_SHARE_WRITE,
      FILE_OPEN,
      FILE_NON_DIRECTORY_FILE | FILE_SYNCHRONOUS_IO_NONALERT);
  RunFS([&](FileSystem* fs) {
    const std::wstring file_name = mount_point_ + path;
    HANDLE handle = CreateFile(file_name.c_str(), GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_EQ(STATUS_SUCCESS, observer->create_result);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_TRUE(CloseHandle(handle));
    EXPECT_TRUE(observer->cleanup_invoked);
    // The Cleanup from the above CloseHandle will definitely get dispatched if
    // we just end it here, but the Close is async in the kernel and will never
    // get dispatched unless we do subsequent I/O that keeps the file system
    // running. Hence the extra open here is to prove that the Close for
    // the test file happens if the FS stays running.
    handle = OpenFSRootDirectory();
    EXPECT_TRUE(observer->close_invoked);
    fs->Unmount();
  });
}

TEST_P(OpenCloseTest, Create_AlternateStream) {
  const wchar_t path[] = L"\\test_Create_AlternateStream.txt";
  const wchar_t stream_name[] = L"Zone.Identifier";
  options_.flags |= StartupFlags::DOKAN_OPTION_ALT_STREAM;
  callbacks_->SetUpFile(std::wstring(path) + L":" + std::wstring(stream_name));
  const Observer* observer = callbacks_->AddObserver(
      std::wstring(path) + L":" + std::wstring(stream_name),
      FILE_GENERIC_READ,
      FILE_ATTRIBUTE_NORMAL,
      FILE_SHARE_READ | FILE_SHARE_WRITE,
      FILE_OPEN,
      FILE_NON_DIRECTORY_FILE | FILE_SYNCHRONOUS_IO_NONALERT);
  RunFS([&](FileSystem* fs) {
    const std::wstring file_name = mount_point_ + path + L":" + stream_name;
    HANDLE handle = CreateFile(file_name.c_str(), GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_EQ(STATUS_SUCCESS, observer->create_result);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_TRUE(CloseHandle(handle));
    EXPECT_TRUE(observer->cleanup_invoked);

    // The Cleanup from the above CloseHandle will definitely get dispatched if
    // we just end it here, but the Close is async in the kernel and will never
    // get dispatched unless we do subsequent I/O that keeps the file system
    // running. Hence the extra open here is to prove that the Close for
    // the test file happens if the FS stays running.
    handle = OpenFSRootDirectory();
    EXPECT_TRUE(observer->close_invoked);
    fs->Unmount();
  });
}

TEST_P(OpenCloseTest, Create_Error) {
  const wchar_t path[] = L"\\test_Create_Error.txt";
  const Observer* observer = callbacks_->AddObserver(
      path,
      FILE_GENERIC_READ,
      FILE_ATTRIBUTE_NORMAL,
      FILE_SHARE_READ | FILE_SHARE_WRITE,
      FILE_OPEN,
      FILE_NON_DIRECTORY_FILE | FILE_SYNCHRONOUS_IO_NONALERT);
  RunFS([&](FileSystem* fs) {
    const std::wstring file_name = mount_point_ + path;
    HANDLE handle = CreateFile(file_name.c_str(), GENERIC_READ,
                               FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                               OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_EQ(STATUS_OBJECT_NAME_NOT_FOUND, observer->create_result);
    EXPECT_EQ(INVALID_HANDLE_VALUE, handle);
    EXPECT_EQ(ERROR_FILE_NOT_FOUND, GetLastError());
    fs->Unmount();
    EXPECT_FALSE(observer->cleanup_invoked);
    EXPECT_FALSE(observer->close_invoked);
  });
}

TEST_P(OpenCloseTest, SupersedeRoot) {
  RunFS([&](FileSystem* fs) {
    HANDLE handle = INVALID_HANDLE_VALUE;
    OBJECT_ATTRIBUTES attributes;
    IO_STATUS_BLOCK iosb;
    memset(&attributes, 0, sizeof(OBJECT_ATTRIBUTES));
    memset(&iosb, 0, sizeof(IO_STATUS_BLOCK));
    UNICODE_STRING name;
    const std::wstring full_name = L"\\??\\" + mount_point_ +
        std::wstring(L"\\");
    RtlInitUnicodeString(&name, full_name.c_str());
    attributes.Length = sizeof(OBJECT_ATTRIBUTES);
    attributes.ObjectName = &name;
    NTSTATUS status = NtCreateFile(
        &handle, FILE_ALL_ACCESS, &attributes, &iosb,
        /*AllocationSize=*/nullptr, /*FileAttributes=*/0, /*ShareAccess=*/0,
        FILE_SUPERSEDE, 0, /*EaBuffer=*/nullptr, /*EaLength=*/0);
    EXPECT_EQ(STATUS_ACCESS_DENIED, status);
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(OpenCloseTests, OpenCloseTest, testing::Values(
    CallbackThreading::SYNC, CallbackThreading::ASYNC_EPHEMERAL_THREAD));

class VolumeInfoTest : public FileSystemTestBase {};

TEST_P(VolumeInfoTest, GetVolumeInfo) {
  options_.volume_serial_number = 0x12191922;
  options_.volume_name = L"Foo bar";
  options_.file_system_type_name = L"APFS";
  options_.file_system_attributes =
      FILE_CASE_SENSITIVE_SEARCH | FILE_CASE_PRESERVED_NAMES;
  options_.max_component_length = 123;
  RunFS([&](FileSystem* fs) {
    wchar_t volume_name[33];
    wchar_t file_system_name[33];
    DWORD serial_number = 0;
    DWORD max_component_length = 0;
    DWORD file_system_attributes = 0;
    // Fill explicitly with garbage to detect non-termination.
    memset(volume_name, 17, sizeof(volume_name));
    memset(file_system_name, 17, sizeof(file_system_name));
    bool result = GetVolumeInformation(mount_point_.c_str(), volume_name, 32,
                                       &serial_number, &max_component_length,
                                       &file_system_attributes,
                                       file_system_name, 32);
    EXPECT_TRUE(result);
    EXPECT_EQ(0x12191922, serial_number);
    EXPECT_EQ(std::wstring(L"Foo bar"), volume_name);
    EXPECT_EQ(std::wstring(L"APFS"), file_system_name);
    EXPECT_EQ(FILE_CASE_SENSITIVE_SEARCH | FILE_CASE_PRESERVED_NAMES,
              file_system_attributes);
    EXPECT_EQ(123, max_component_length);
    fs->Unmount();
  });
}

TEST_P(VolumeInfoTest, GetFreeSpace_Success) {
  RunFS([&](FileSystem* fs) {
    ULARGE_INTEGER free_bytes_for_process_user = {0};
    ULARGE_INTEGER total_bytes = {0};
    ULARGE_INTEGER free_bytes_for_all_users = {0};
    bool result = GetDiskFreeSpaceEx(
        mount_point_.c_str(),
        &free_bytes_for_process_user,
        &total_bytes,
        &free_bytes_for_all_users);
    EXPECT_TRUE(result);
    EXPECT_EQ(1024 * 1024 * 1024, total_bytes.QuadPart);
    EXPECT_EQ(1024 * 1024 * 512, free_bytes_for_all_users.QuadPart);
    EXPECT_EQ(1024 * 1024 * 256, free_bytes_for_process_user.QuadPart);
    fs->Unmount();
  });
}

TEST_P(VolumeInfoTest, GetFreeSpace_Error) {
  free_space_result_ = STATUS_IO_DEVICE_ERROR;
  RunFS([&](FileSystem* fs) {
    ULARGE_INTEGER free_bytes_for_process_user = {0};
    ULARGE_INTEGER total_bytes = {0};
    ULARGE_INTEGER free_bytes_for_all_users = {0};
    bool result = GetDiskFreeSpaceEx(
        mount_point_.c_str(),
        &free_bytes_for_process_user,
        &total_bytes,
        &free_bytes_for_all_users);
    EXPECT_FALSE(result);
    EXPECT_EQ(0, total_bytes.QuadPart);
    EXPECT_EQ(0, free_bytes_for_all_users.QuadPart);
    EXPECT_EQ(0, free_bytes_for_process_user.QuadPart);
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(VolumeInfoTests, VolumeInfoTest, testing::Values(
    CallbackThreading::SYNC, CallbackThreading::ASYNC_EPHEMERAL_THREAD));

class MetadataTest : public FileSystemTestBase {};

TEST_P(MetadataTest, GetInfo_Error) {
  const wchar_t path[] = L"\\test_GetInfo_Error";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    FILE_BASIC_INFO info = {0};
    callbacks_->ForgetFile(path);
    bool result = GetFileInformationByHandleEx(handle, FileBasicInfo, &info,
                                               sizeof(info));
    EXPECT_FALSE(result);
    // The error code is changed by FileInfoHandler.
    EXPECT_EQ(ERROR_INVALID_PARAMETER, GetLastError());
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_IdInfo) {
  if (!IsWindows8OrGreater()) {
    // ID info is unsupported below Windows 8 according to this page. If we try
    // it, it "succeeds" but the data gets zeroed out.
    //https://msdn.microsoft.com/en-us/library/windows/desktop/aa364228(v=vs.85).aspx
    DOKAN_LOG_INFO(
        &logger_,
        "Skipping ID info test because it is unsupported on this OS version.");
    return;
  }
  const wchar_t path[] = L"\\test_GetInfo_IdInfo";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    FILE_ID_INFO info = {0};
    bool result = GetFileInformationByHandleEx(handle, FileIdInfo, &info,
                                               sizeof(info));
    EXPECT_TRUE(result);
    EXPECT_EQ(options_.volume_serial_number, info.VolumeSerialNumber);
    // The system appears to replace dokan's zero value for result.FileId
    // with something we can't predict.
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_NameInfo) {
  const wchar_t path[] = L"\\test_GetInfo_NameInfo";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle =  Open(path, GENERIC_READ);
    auto info = util::MakeUniqueVarStruct<FILE_NAME_INFO>(1024);
    bool result = GetFileInformationByHandleEx(handle, FileNameInfo, info.get(),
                                               1024);
    EXPECT_TRUE(result);
    EXPECT_EQ(wcslen(path) * sizeof(path[0]), info->FileNameLength);
    EXPECT_EQ(path, std::wstring(info->FileName));
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_NameInfo_Overflow) {
  const wchar_t path[] = L"\\test_GetInfo_NameInfo_Overflow";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    size_t size = sizeof(FILE_NAME_INFO) + 5;
    auto info = util::MakeUniqueVarStruct<FILE_NAME_INFO>(size);
    bool result = GetFileInformationByHandleEx(handle, FileNameInfo, info.get(),
                                               size);
    EXPECT_FALSE(result);
    EXPECT_EQ(ERROR_MORE_DATA, GetLastError());
    EXPECT_EQ(wcslen(path) * sizeof(path[0]), info->FileNameLength);
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_AttributeTagInfo) {
  const wchar_t path[] = L"\\test_GetInfo_AttributeTagInfo";
  FileInfo data_to_return = {0};
  data_to_return.file_attributes = FILE_ATTRIBUTE_SYSTEM;
  callbacks_->SetUpFile(path, data_to_return);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    FILE_ATTRIBUTE_TAG_INFO info = {0};
    bool result = GetFileInformationByHandleEx(handle, FileAttributeTagInfo,
                                               &info, sizeof(info));
    EXPECT_TRUE(result);
    EXPECT_EQ(FILE_ATTRIBUTE_SYSTEM, info.FileAttributes);
    EXPECT_EQ(0, info.ReparseTag);
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_BasicInfo) {
  const wchar_t path[] = L"\\test_GetInfo_BasicInfo";
  FileInfo data_to_return = {0};
  data_to_return.creation_time.dwHighDateTime = 0x11111111;
  data_to_return.creation_time.dwLowDateTime= 0x22222222;
  data_to_return.last_write_time.dwHighDateTime = 0x33333333;
  data_to_return.last_write_time.dwLowDateTime = 0x44444444;
  data_to_return.last_access_time.dwHighDateTime = 0x55555555;
  data_to_return.last_access_time.dwLowDateTime = 0x66666666;
  data_to_return.file_attributes = FILE_ATTRIBUTE_HIDDEN;
  callbacks_->SetUpFile(path, data_to_return);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    FILE_BASIC_INFO info = {0};
    bool result = GetFileInformationByHandleEx(handle, FileBasicInfo, &info,
                                               sizeof(info));
    EXPECT_TRUE(result);
    EXPECT_EQ(0x11111111, info.CreationTime.HighPart);
    EXPECT_EQ(0x22222222, info.CreationTime.LowPart);
    EXPECT_EQ(0x33333333, info.LastWriteTime.HighPart);
    EXPECT_EQ(0x44444444, info.LastWriteTime.LowPart);
    EXPECT_EQ(0x55555555, info.LastAccessTime.HighPart);
    EXPECT_EQ(0x66666666, info.LastAccessTime.LowPart);
    EXPECT_EQ(FILE_ATTRIBUTE_HIDDEN, info.FileAttributes);
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_StandardInfo) {
const wchar_t path[] = L"\\test_GetInfo_StandardInfo";
  FileInfo data_to_return = {0};
  data_to_return.file_size = 0x0000000100004359;
  callbacks_->SetUpFile(path, data_to_return);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    FILE_STANDARD_INFO info = {0};
    bool result = GetFileInformationByHandleEx(handle, FileStandardInfo, &info,
                                               sizeof(info));
    EXPECT_TRUE(result);
    EXPECT_FALSE(info.DeletePending);
    EXPECT_FALSE(info.Directory);
    EXPECT_EQ(1, info.AllocationSize.HighPart);
    EXPECT_EQ(17408, info.AllocationSize.LowPart);
    EXPECT_EQ(1, info.EndOfFile.HighPart);
    EXPECT_EQ(17241, info.EndOfFile.LowPart);
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_CompressionInfo) {
  const wchar_t path[] = L"\\test_GetInfo_CompressionInfo";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    FILE_COMPRESSION_INFO info = {0};
    bool result = GetFileInformationByHandleEx(handle, FileCompressionInfo,
                                               &info, sizeof(info));
    EXPECT_FALSE(result);
    EXPECT_EQ(ERROR_INVALID_FUNCTION, GetLastError());
    fs->Unmount();
  });
}

TEST_P(MetadataTest, FindFirstFile_NoResults) {
  const wchar_t path[] = L"\\test_FindFirstFile_NoResults";
  callbacks_->SetUpDir(path);
  RunFS([&](FileSystem* fs) {
    WIN32_FIND_DATA data = {0};
    HANDLE handle = FindFirstFile((mount_point_ + path + L"\\foobar").c_str(),
                                  &data);
    EXPECT_EQ(INVALID_HANDLE_VALUE, handle);
    EXPECT_EQ(ERROR_FILE_NOT_FOUND, GetLastError());
    fs->Unmount();
  });
}

TEST_P(MetadataTest, FindFirstFile_OneResult) {
  const wchar_t path[] = L"\\test_FindFirstFile_OneResult";
  std::vector<FileNameAndInfo> entries(1);
  entries[0].name = L"foobar";
  entries[0].info.creation_time.dwHighDateTime = 0x11111111;
  entries[0].info.creation_time.dwLowDateTime = 0x22222222;
  entries[0].info.last_access_time.dwHighDateTime = 0x33333333;
  entries[0].info.last_access_time.dwLowDateTime = 0x44444444;
  entries[0].info.last_write_time.dwHighDateTime = 0x55555555;
  entries[0].info.last_write_time.dwLowDateTime = 0x66666666;
  entries[0].info.file_attributes = FILE_ATTRIBUTE_SYSTEM;
  entries[0].info.file_size = 0x123456789abcdef;
  callbacks_->SetUpDir(path, entries);
  RunFS([&](FileSystem* fs) {
    WIN32_FIND_DATA data = {0};
    HANDLE handle = FindFirstFile((mount_point_ + path + L"\\foobar").c_str(),
                                  &data);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_EQ(std::wstring(L"foobar"), std::wstring(data.cFileName));
    EXPECT_EQ(0x11111111, data.ftCreationTime.dwHighDateTime);
    EXPECT_EQ(0x22222222, data.ftCreationTime.dwLowDateTime);
    EXPECT_EQ(0x33333333, data.ftLastAccessTime.dwHighDateTime);
    EXPECT_EQ(0x44444444, data.ftLastAccessTime.dwLowDateTime);
    EXPECT_EQ(0x55555555, data.ftLastWriteTime.dwHighDateTime);
    EXPECT_EQ(0x66666666,  data.ftLastWriteTime.dwLowDateTime);
    EXPECT_EQ(0x1234567, data.nFileSizeHigh);
    EXPECT_EQ(0x89abcdef, data.nFileSizeLow);
    EXPECT_EQ(FILE_ATTRIBUTE_SYSTEM, data.dwFileAttributes);
    EXPECT_FALSE(FindNextFile(handle, &data));
    EXPECT_TRUE(FindClose(handle));
    fs->Unmount();
  });
}

TEST_P(MetadataTest, FindFirstFile_TwoResults) {
  const wchar_t path[] = L"\\test_FindFirstFile_TwoResults";
  std::vector<FileNameAndInfo> entries(2);
  entries[0].name = L"The first file.txt";
  entries[0].info.creation_time.dwHighDateTime = 0x11111111;
  entries[0].info.creation_time.dwLowDateTime = 0x22222222;
  entries[0].info.last_access_time.dwHighDateTime = 0x33333333;
  entries[0].info.last_access_time.dwLowDateTime = 0x44444444;
  entries[0].info.last_write_time.dwHighDateTime = 0x55555555;
  entries[0].info.last_write_time.dwLowDateTime = 0x66666666;
  entries[0].info.file_attributes = FILE_ATTRIBUTE_SYSTEM;
  entries[0].info.file_size = 0x123456789abcdef;

  entries[1].name = L"2nd file~";
  entries[1].info.creation_time.dwHighDateTime = 0x77777777;
  entries[1].info.creation_time.dwLowDateTime = 0x88888888;
  entries[1].info.last_access_time.dwHighDateTime = 0x99999999;
  entries[1].info.last_access_time.dwLowDateTime = 0xaaaaaaaa;
  entries[1].info.last_write_time.dwHighDateTime = 0xbbbbbbbb;
  entries[1].info.last_write_time.dwLowDateTime = 0xcccccccc;
  entries[1].info.file_attributes = FILE_ATTRIBUTE_DIRECTORY |
                                    FILE_ATTRIBUTE_HIDDEN;
  entries[1].info.file_size = 1;
  callbacks_->SetUpDir(path, entries);
  RunFS([&](FileSystem* fs) {
    WIN32_FIND_DATA data = {0};
    HANDLE handle = FindFirstFile(
        (mount_point_ + path + L"\\*file*").c_str(),
        &data);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_EQ(std::wstring(L"2nd file~"), std::wstring(data.cFileName));
    EXPECT_EQ(0x77777777, data.ftCreationTime.dwHighDateTime);
    EXPECT_EQ(0x88888888, data.ftCreationTime.dwLowDateTime);
    EXPECT_EQ(0x99999999, data.ftLastAccessTime.dwHighDateTime);
    EXPECT_EQ(0xaaaaaaaa, data.ftLastAccessTime.dwLowDateTime);
    EXPECT_EQ(0xbbbbbbbb, data.ftLastWriteTime.dwHighDateTime);
    EXPECT_EQ(0xcccccccc, data.ftLastWriteTime.dwLowDateTime);
    EXPECT_EQ(0, data.nFileSizeHigh);
    EXPECT_EQ(1, data.nFileSizeLow);
    EXPECT_EQ(FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_HIDDEN,
        data.dwFileAttributes);
    EXPECT_TRUE(FindNextFile(handle, &data));

    EXPECT_EQ(std::wstring(L"The first file.txt"),
              std::wstring(data.cFileName));
    EXPECT_EQ(0x11111111, data.ftCreationTime.dwHighDateTime);
    EXPECT_EQ(0x22222222, data.ftCreationTime.dwLowDateTime);
    EXPECT_EQ(0x33333333, data.ftLastAccessTime.dwHighDateTime);
    EXPECT_EQ(0x44444444, data.ftLastAccessTime.dwLowDateTime);
    EXPECT_EQ(0x55555555, data.ftLastWriteTime.dwHighDateTime);
    EXPECT_EQ(0x66666666, data.ftLastWriteTime.dwLowDateTime);
    EXPECT_EQ(0x1234567, data.nFileSizeHigh);
    EXPECT_EQ(0x89abcdef, data.nFileSizeLow);
    EXPECT_EQ(FILE_ATTRIBUTE_SYSTEM, data.dwFileAttributes);
    EXPECT_FALSE(FindNextFile(handle, &data));
    EXPECT_TRUE(FindClose(handle));
    fs->Unmount();
  });
}

TEST_P(MetadataTest, FindFirstFile_WrongCase) {
  const wchar_t path[] = L"\\test_FindFirstFile_WrongCase";
  std::vector<FileNameAndInfo> entries(1);
  entries[0].name = L"foo baR";
  callbacks_->SetUpDir(path, entries);
  RunFS([&](FileSystem* fs) {
    WIN32_FIND_DATA data = {0};
    HANDLE handle = FindFirstFile((mount_point_ + path + L"\\FoO bAr").c_str(),
                                  &data);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_EQ(std::wstring(L"foo baR"), std::wstring(data.cFileName));
    EXPECT_FALSE(FindNextFile(handle, &data));
    EXPECT_TRUE(FindClose(handle));
    fs->Unmount();
  });
}

TEST_P(MetadataTest, FindFirstFile_LargeNumber) {
  // We use enough entries here to trigger a buffer overflow and multiple
  // requests to the DLL. The other tests generally don't trigger that.
  const wchar_t path[] = L"\\test_FindFirstFile_LargeNumber";
  std::vector<FileNameAndInfo> entries(5000);
  for (size_t i = 0; i < entries.size(); ++i) {
    // We start the numbering at 1000 to avoid sorting issues.
    entries[i].name = std::wstring(L"file") + std::to_wstring(i + 1000);
  }
  callbacks_->SetUpDir(path, entries);
  RunFS([&](FileSystem* fs) {
    WIN32_FIND_DATA data = {0};
    HANDLE handle = FindFirstFile((mount_point_ + path + L"\\file*").c_str(),
                                  &data);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_EQ(L"file1000", std::wstring(data.cFileName));
    for (size_t i = 1; i < entries.size(); ++i) {
      EXPECT_TRUE(FindNextFile(handle, &data));
      EXPECT_EQ(std::wstring(L"file") + std::to_wstring(i + 1000),
                std::wstring(data.cFileName));
    }
    EXPECT_FALSE(FindNextFile(handle, &data));
    EXPECT_TRUE(FindClose(handle));
    fs->Unmount();
  });
}

TEST_P(MetadataTest, FindFilesViaGetInformation) {
  // GetFileInformationByHandle is worth testing separately from FindFirstFile
  // because it always does a one-request listing of the files, and conveys more
  // of the data.
  const wchar_t path[] = L"\\test_FindFilesViaGetInformation";
  std::vector<FileNameAndInfo> entries(2);
  entries[0].name = L"The first file.txt";
  entries[0].info.creation_time.dwHighDateTime = 0x11111111;
  entries[0].info.creation_time.dwLowDateTime = 0x22222222;
  entries[0].info.last_access_time.dwHighDateTime = 0x33333333;
  entries[0].info.last_access_time.dwLowDateTime = 0x44444444;
  entries[0].info.last_write_time.dwHighDateTime = 0x55555555;
  entries[0].info.last_write_time.dwLowDateTime = 0x66666666;
  entries[0].info.file_attributes = FILE_ATTRIBUTE_SYSTEM;
  entries[0].info.file_size = 0x123456789abcdef;

  entries[1].name = L"2nd file~";
  entries[1].info.creation_time.dwHighDateTime = 0x77777777;
  entries[1].info.creation_time.dwLowDateTime = 0x88888888;
  entries[1].info.last_access_time.dwHighDateTime = 0x99999999;
  entries[1].info.last_access_time.dwLowDateTime = 0xaaaaaaaa;
  entries[1].info.last_write_time.dwHighDateTime = 0xbbbbbbbb;
  entries[1].info.last_write_time.dwLowDateTime = 0xcccccccc;
  entries[1].info.file_attributes = FILE_ATTRIBUTE_DIRECTORY |
                                    FILE_ATTRIBUTE_HIDDEN;
  entries[1].info.file_size = 1;
  callbacks_->SetUpDir(path, entries);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    char data[2048];
    bool result = GetFileInformationByHandleEx(handle, FileIdBothDirectoryInfo,
                                               data, 2048);
    EXPECT_TRUE(result);
    auto entry = reinterpret_cast<FILE_ID_BOTH_DIR_INFO*>(data);

    EXPECT_EQ(std::wstring(L"."), std::wstring(entry->FileName));
    EXPECT_NE(0, entry->NextEntryOffset);
    EXPECT_EQ(FILE_ATTRIBUTE_DIRECTORY, entry->FileAttributes);
    entry = reinterpret_cast<FILE_ID_BOTH_DIR_INFO*>(
        reinterpret_cast<char*>(entry) + entry->NextEntryOffset);

    EXPECT_EQ(std::wstring(L".."), std::wstring(entry->FileName));
    EXPECT_NE(0, entry->NextEntryOffset);
    EXPECT_EQ(FILE_ATTRIBUTE_DIRECTORY, entry->FileAttributes);
    entry = reinterpret_cast<FILE_ID_BOTH_DIR_INFO*>(
        reinterpret_cast<char*>(entry) + entry->NextEntryOffset);
    EXPECT_EQ(std::wstring(L"2nd file~"),
        std::wstring(entry->FileName));
    EXPECT_EQ(0x77777777, entry->CreationTime.HighPart);
    EXPECT_EQ(0x88888888, entry->CreationTime.LowPart);
    EXPECT_EQ(0x99999999, entry->LastAccessTime.HighPart);
    EXPECT_EQ(0xaaaaaaaa, entry->LastAccessTime.LowPart);
    EXPECT_EQ(0xbbbbbbbb, entry->LastWriteTime.HighPart);
    EXPECT_EQ(0xcccccccc, entry->LastWriteTime.LowPart);
    EXPECT_EQ(1, entry->EndOfFile.QuadPart);
    EXPECT_EQ(512, entry->AllocationSize.QuadPart);
    EXPECT_EQ(FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_HIDDEN,
        entry->FileAttributes);
    EXPECT_EQ(0, entry->EaSize);
    EXPECT_NE(0, entry->NextEntryOffset);

    entry = reinterpret_cast<FILE_ID_BOTH_DIR_INFO*>(
        reinterpret_cast<char*>(entry) + entry->NextEntryOffset);
    EXPECT_EQ(std::wstring(L"The first file.txt"),
              std::wstring(entry->FileName));
    EXPECT_EQ(0x11111111, entry->CreationTime.HighPart);
    EXPECT_EQ(0x22222222, entry->CreationTime.LowPart);
    EXPECT_EQ(0x33333333, entry->LastAccessTime.HighPart);
    EXPECT_EQ(0x44444444, entry->LastAccessTime.LowPart);
    EXPECT_EQ(0x55555555, entry->LastWriteTime.HighPart);
    EXPECT_EQ(0x66666666, entry->LastWriteTime.LowPart);
    EXPECT_EQ(0x123456789abcdef, entry->EndOfFile.QuadPart);
    EXPECT_EQ(0x123456789abce00, entry->AllocationSize.QuadPart);
    EXPECT_EQ(FILE_ATTRIBUTE_SYSTEM, entry->FileAttributes);
    EXPECT_EQ(0, entry->EaSize);
    EXPECT_EQ(0, entry->NextEntryOffset);
    fs->Unmount();
  });
}

TEST_P(MetadataTest, ChangeSize) {
  const wchar_t path[] = L"\\test_ChangeSize";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ | GENERIC_WRITE);
    FILE_END_OF_FILE_INFO data{0};
    data.EndOfFile.QuadPart = 0x123456789a;
    bool result = SetFileInformationByHandle(handle, FileEndOfFileInfo, &data,
                                             sizeof(data));
    EXPECT_TRUE(result);
    EXPECT_EQ(0x123456789a, callbacks_->file_info(path).file_size);
    fs->Unmount();
  });
}

TEST_P(MetadataTest, ChangeAttributesAndTimes) {
  const wchar_t path[] = L"\\test_ChangeAttributesAndTimes";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ | GENERIC_WRITE);
    FILE_BASIC_INFO data{0};
    data.FileAttributes = FILE_ATTRIBUTE_SYSTEM | FILE_ATTRIBUTE_HIDDEN;
    data.LastAccessTime.QuadPart = 0x3333333344444444;
    data.CreationTime.QuadPart = 0x1111111122222222;
    bool result = SetFileInformationByHandle(handle, FileBasicInfo, &data,
                                             sizeof(data));
    EXPECT_TRUE(result);
    const FileInfo final_info = callbacks_->file_info(path);
    EXPECT_EQ(FILE_ATTRIBUTE_SYSTEM | FILE_ATTRIBUTE_HIDDEN,
              final_info.file_attributes);
    EXPECT_EQ(0x33333333, final_info.last_access_time.dwHighDateTime);
    EXPECT_EQ(0x44444444, final_info.last_access_time.dwLowDateTime);
    EXPECT_EQ(0x11111111, final_info.creation_time.dwHighDateTime);
    EXPECT_EQ(0x22222222, final_info.creation_time.dwLowDateTime);
    EXPECT_EQ(0, final_info.last_write_time.dwHighDateTime);
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(MetadataTests, MetadataTest, testing::Values(
    CallbackThreading::SYNC, CallbackThreading::ASYNC_EPHEMERAL_THREAD));

class MoveDeleteTest : public FileSystemTestBase {};

TEST_P(MoveDeleteTest, Delete_File_Success) {
  const wchar_t path[] = L"\\test_Delete_File_Success";
  callbacks_->SetUpFile(path);
  const Observer* observer = callbacks_->AddObserver(path);
  RunFS([&](FileSystem* fs) {
    bool result = DeleteFile((mount_point_ + path).c_str());
    EXPECT_TRUE(observer->get_delete_approval_invoked);
    EXPECT_TRUE(result);
    EXPECT_FALSE(callbacks_->file_exists(path));
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, Delete_File_ViaCreateFlag) {
  const wchar_t path[] = L"\\test_Delete_File_ViaCreateFlag";
  callbacks_->SetUpFile(path);
  const Observer* observer = callbacks_->AddObserver(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = CreateFile(
        (mount_point_ + path).c_str(), GENERIC_ALL, 0, nullptr, CREATE_NEW,
        FILE_FLAG_DELETE_ON_CLOSE, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    CloseHandle(handle);
    EXPECT_FALSE(observer->get_delete_approval_invoked);
    EXPECT_FALSE(callbacks_->file_exists(path));
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, Delete_Directory_Success) {
  const wchar_t path[] = L"\\test_Delete_Directory_Success";
  callbacks_->SetUpDir(path);
  const Observer* observer = callbacks_->AddObserver(path);
  RunFS([&](FileSystem* fs) {
    bool result = RemoveDirectory((mount_point_ + path).c_str());
    EXPECT_TRUE(result);
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, Delete_File_ReadOnly) {
  const wchar_t path[] = L"\\test_Delete_File_ReadOnly";
  FileInfo info{0};
  info.file_attributes = FILE_ATTRIBUTE_NORMAL | FILE_ATTRIBUTE_READONLY;
  callbacks_->SetUpFile(path, info);
  const Observer* observer = callbacks_->AddObserver(path);
  RunFS([&](FileSystem* fs) {
    // This delete should be stopped by dokancc without invoking the
    // GetDeleteApproval callback.
    bool result = DeleteFile((mount_point_ + path).c_str());
    EXPECT_FALSE(result);
    EXPECT_EQ(ERROR_ACCESS_DENIED, GetLastError());
    EXPECT_FALSE(observer->get_delete_approval_invoked);
    EXPECT_TRUE(callbacks_->file_exists(path));
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, Delete_Directory_ReadOnly) {
  const wchar_t path[] = L"\\test_Delete_Directory_ReadOnly";
  FileInfo info{0};
  info.file_attributes =
      FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_READONLY;
  callbacks_->SetUpFile(path, info);
  const Observer* observer = callbacks_->AddObserver(path);
  RunFS([&](FileSystem* fs) {
    // This delete should be stopped by dokancc without invoking the
    // GetDeleteApproval callback.
    bool result = RemoveDirectory((mount_point_ + path).c_str());
    EXPECT_FALSE(result);
    EXPECT_EQ(ERROR_ACCESS_DENIED, GetLastError());
    EXPECT_FALSE(observer->get_delete_approval_invoked);
    EXPECT_TRUE(callbacks_->file_exists(path));
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, Delete_File_CallbackFailure) {
  const wchar_t path[] = L"\\test_Delete_File_CallbackFailure";
  callbacks_->SetUpFile(path);
  callbacks_->SetGetDeleteApprovalResult(STATUS_ACCESS_DENIED);
  const Observer* observer = callbacks_->AddObserver(path);
  RunFS([&](FileSystem* fs) {
    bool result = DeleteFile((mount_point_ + path).c_str());
    EXPECT_FALSE(result);
    EXPECT_EQ(ERROR_ACCESS_DENIED, GetLastError());
    EXPECT_TRUE(observer->get_delete_approval_invoked);
    EXPECT_TRUE(callbacks_->file_exists(path));
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, Delete_Directory_CallbackFailure) {
  const wchar_t path[] = L"\\test_Delete_Directory_CallbackFailure";
  callbacks_->SetUpDir(path);
  callbacks_->SetGetDeleteApprovalResult(STATUS_DIRECTORY_NOT_EMPTY);
  const Observer* observer = callbacks_->AddObserver(path);
  RunFS([&](FileSystem* fs) {
    bool result = RemoveDirectory((mount_point_ + path).c_str());
    EXPECT_FALSE(result);
    EXPECT_EQ(ERROR_DIR_NOT_EMPTY, GetLastError());
    EXPECT_TRUE(observer->get_delete_approval_invoked);
    EXPECT_TRUE(callbacks_->file_exists(path));
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, Move_Success) {
  const wchar_t source[] = L"\\test_Move_Success_src\\file";
  const wchar_t dest[] = L"\\test_Move_Success_dest\\file";
  callbacks_->SetUpDir(util::StripLastPathComponent(source));
  callbacks_->SetUpDir(util::StripLastPathComponent(dest));
  callbacks_->SetUpFile(source);
  RunFS([&](FileSystem* fs) {
    bool result = MoveFile(
        (mount_point_ + source).c_str(),
        (mount_point_ + dest).c_str());
    EXPECT_TRUE(result);
    EXPECT_TRUE(callbacks_->file_exists(dest));
    EXPECT_FALSE(callbacks_->file_exists(source));
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, Move_NoReplace_DestExists) {
  const wchar_t source[] = L"\\test_Move_NoReplace_DestExists_src\\file";
  const wchar_t dest[] = L"\\test_Move_NoReplace_DestExists_dest\\file";
  callbacks_->SetUpDir(util::StripLastPathComponent(source));
  callbacks_->SetUpDir(util::StripLastPathComponent(dest));
  callbacks_->SetUpFile(source);
  callbacks_->SetUpFile(dest);
  RunFS([&](FileSystem* fs) {
    bool result = MoveFile(
        (mount_point_ + source).c_str(),
        (mount_point_ + dest).c_str());
    EXPECT_FALSE(result);
    EXPECT_EQ(ERROR_ALREADY_EXISTS, GetLastError());
    EXPECT_TRUE(callbacks_->file_exists(source));
    EXPECT_TRUE(callbacks_->file_exists(dest));
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, Move_Replace_DestExists) {
  const wchar_t source[] = L"\\test_Move_Replace_DestExists_src\\file";
  const wchar_t dest[] = L"\\test_Move_Replace_DestExists_dest\\file";
  callbacks_->SetUpDir(util::StripLastPathComponent(source));
  callbacks_->SetUpDir(util::StripLastPathComponent(dest));
  callbacks_->SetUpFile(source);
  callbacks_->SetUpFile(dest);
  RunFS([&](FileSystem* fs) {
    bool result = MoveFileEx(
        (mount_point_ + source).c_str(),
        (mount_point_ + dest).c_str(),
        MOVEFILE_REPLACE_EXISTING);
    EXPECT_TRUE(result);
    EXPECT_FALSE(callbacks_->file_exists(source));
    EXPECT_TRUE(callbacks_->file_exists(dest));
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(MoveDeleteTests, MoveDeleteTest, testing::Values(
    CallbackThreading::SYNC, CallbackThreading::ASYNC_EPHEMERAL_THREAD));

class ReadWriteTest : public FileSystemTestBase {};

TEST_P(ReadWriteTest, Read_Success) {
  const wchar_t path[] = L"\\test_Read_Success";
  const char content[] = "It was a dark and stormy night.";
  callbacks_->SetUpFile(path, content);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    char buffer[32] = {0};
    DWORD bytes_read = 0;
    bool result = ReadFile(handle, buffer, 31, &bytes_read, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(31, bytes_read);
    EXPECT_EQ(content, std::string(buffer));
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Read_EOF) {
  const wchar_t path[] = L"\\test_Read_EOF";
  const char content[] = "It was a dark and stormy night.";
  callbacks_->SetUpFile(path, content);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    char buffer[32] = {0};
    DWORD bytes_read = 0;
    bool result = ReadFile(handle, buffer, 32, &bytes_read, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(31, bytes_read);
    EXPECT_EQ(content, std::string(buffer));
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Read_Triple) {
  const wchar_t path[] = L"\\test_Read_Triple";
  const char content1[] = "It was a dark and stormy night,";
  const char content2[] = " and the builds failed in torrents";
  const char content3[] =
      " except when kokoro was down at occasional intervals.";
  callbacks_->SetUpFile(path, std::string(content1) + std::string(content2) +
            std::string(content3));
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    char buffer1[32] = {0};
    DWORD bytes_read = 0;
    bool result = ReadFile(handle, buffer1, 31, &bytes_read, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(31, bytes_read);
    EXPECT_EQ(content1, std::string(buffer1));

    char buffer2[35] = {0};
    result = ReadFile(handle, buffer2, 34, &bytes_read, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(34, bytes_read);
    EXPECT_EQ(content2, std::string(buffer2));

    char buffer3[54] = {0};
    result = ReadFile(handle, buffer3, 53, &bytes_read, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(53, bytes_read);
    EXPECT_EQ(content3, std::string(buffer3));

    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Read_Failure) {
  const wchar_t path[] = L"\\test_Read_Failure";
  callbacks_->SetUpFile(path);
  callbacks_->SetReadResult(STATUS_IO_DEVICE_ERROR);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    char buffer[32] = {0};
    DWORD bytes_read = 0;
    bool result = ReadFile(handle, buffer, 32, &bytes_read, NULL);
    EXPECT_FALSE(result);
    EXPECT_EQ(0, bytes_read);
    EXPECT_EQ(std::string(), std::string(buffer));
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Read_Failure_Subsequent_Success) {
  const wchar_t path[] = L"\\test_Read_Failure_Subsequent_Success";
  const char content[] = "It was a dark and stormy night.";
  callbacks_->SetUpFile(path, content);
  callbacks_->SetReadResult(STATUS_IO_DEVICE_ERROR);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    char buffer[32] = {0};
    DWORD bytes_read = 0;
    bool result = ReadFile(handle, buffer, 32, &bytes_read, NULL);
    EXPECT_FALSE(result);
    EXPECT_EQ(0, bytes_read);
    EXPECT_EQ(std::string(), std::string(buffer));

    callbacks_->SetReadResult(STATUS_SUCCESS);
    result = ReadFile(handle, buffer, 31, &bytes_read, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(31, bytes_read);
    EXPECT_EQ(content, std::string(buffer));
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Write_Success) {
  const wchar_t path[] = L"\\test_Write_Success";
  const char content[] = "It was a dark and stormy night.";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ | GENERIC_WRITE);
    DWORD bytes_written = 0;
    bool result = WriteFile(handle, content, 31, &bytes_written, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(31, bytes_written);
    CloseHandle(handle);
    EXPECT_EQ(std::string(content), callbacks_->file_content(path));
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Write_Large) {
  const wchar_t path[] = L"\\test_Write_Large";
  std::vector<char> data(1024 * 1024 * 64 + 1);
  for (int i = 0; i < data.size(); i++) {
    data[i] = 'a' + (i % 26);
  }
  data[data.size() - 1] = 0;
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ | GENERIC_WRITE);
    DWORD bytes_written = 0;
    bool result = WriteFile(handle, data.data(), data.size(), &bytes_written,
                            NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(data.size(), bytes_written);
    CloseHandle(handle);
    EXPECT_EQ(std::string(data.data()), callbacks_->file_content(path));
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Write_Triple) {
  const wchar_t path[] = L"\\test_Write_Triple";
  const char content1[] = "It was a dark and stormy night,";
  const char content2[] = " and the builds failed in torrents";
  const char content3[] =
      " except when kokoro was down at occasional intervals.";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ | GENERIC_WRITE);
    DWORD bytes_written = 0;
    bool result = WriteFile(handle, content1, 31, &bytes_written, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(31, bytes_written);

    result = WriteFile(handle, content2, 34, &bytes_written, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(34, bytes_written);

    result = WriteFile(handle, content3, 53, &bytes_written, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(53, bytes_written);

    CloseHandle(handle);
    EXPECT_EQ(
        std::string(content1) + std::string(content2) + std::string(content3),
        callbacks_->file_content(path));
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Write_Failure) {
  const wchar_t path[] = L"\\test_Write_Failure";
  callbacks_->SetUpFile(path);
  callbacks_->SetWriteResult(STATUS_IO_DEVICE_ERROR);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ | GENERIC_WRITE);
    DWORD bytes_written = 0;
    bool result = WriteFile(handle, "foobar", 6, &bytes_written, NULL);
    EXPECT_FALSE(result);
    EXPECT_EQ(0, bytes_written);
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Write_Failure_Subsequent_Success) {
  const wchar_t path[] = L"\\test_Write_Failure_Subsequent_Success";
  callbacks_->SetUpFile(path);
  callbacks_->SetWriteResult(STATUS_IO_DEVICE_ERROR);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ | GENERIC_WRITE);
    DWORD bytes_written = 0;
    bool result = WriteFile(handle, "foobar", 6, &bytes_written, NULL);
    EXPECT_FALSE(result);
    EXPECT_EQ(0, bytes_written);

    callbacks_->SetWriteResult(STATUS_SUCCESS);
    result = WriteFile(handle, "foo", 3, &bytes_written, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(3, bytes_written);
    CloseHandle(handle);
    EXPECT_EQ("foo", callbacks_->file_content(path));
    fs->Unmount();
  });
}

// TODO(drivefs-team): Add tests that require the NT API:
// - NtQueryInformationFile for GetInfo information classes that are not usable
//   with GetFileInformationByHandleEx.
// - NtWriteFile for writing to the end of the file without an explicit offset.

class SecurityTest : public FileSystemTestBase {};

TEST_P(SecurityTest, SecurityDescriptorEnforcement) {
  SetSecurityDescriptor(kAllowEveryoneSddl,
                        &options_.volume_security_descriptor,
                        &options_.volume_security_descriptor_length);
  RunFS([&] (FileSystem* fs) {
    // Make sure we can access the device normally.
    HANDLE handle = OpenFSRootDirectory();
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    CloseHandle(handle);

    // If we say we're not in Everyone, we should not be able to access the
    // device.
    RemoveSidFromThread(kEveryoneSddl);
    handle = OpenFSRootDirectory();
    EXPECT_EQ(INVALID_HANDLE_VALUE, handle);
    EXPECT_EQ(ERROR_ACCESS_DENIED, GetLastError());

    RevertToSelf();
    fs->Unmount();
  });
}

TEST_P(SecurityTest, GetSecurity_Success) {
  std::wstring sddl;
  sddl += kOwnerAccountOperatorsSddl;
  sddl += kGroupAdminsSddl;
  sddl += kAllowEveryoneSddl;
  SetSecurityDescriptor(sddl, &options_.volume_security_descriptor,
                        &options_.volume_security_descriptor_length);
  RunFS([&] (FileSystem* fs) {
    HANDLE handle = OpenFSRootDirectory();
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    SECURITY_INFORMATION info = DACL_SECURITY_INFORMATION |
        OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION;
    auto sd = util::MakeUniqueVarStruct<SECURITY_DESCRIPTOR>(80);
    DWORD length_needed = 0;
    bool result = GetUserObjectSecurity(handle, &info, sd.get(), 80,
                                        &length_needed);
    EXPECT_TRUE(result);
    EXPECT_EQ(80, length_needed);
    LPWSTR sd_string = nullptr;
    DWORD sd_string_length = 0;
    result = ConvertSecurityDescriptorToStringSecurityDescriptor(
        sd.get(), SECURITY_DESCRIPTOR_REVISION, info, &sd_string,
        &sd_string_length);
    EXPECT_TRUE(result);
    EXPECT_EQ(sddl, std::wstring(sd_string));
    fs->Unmount();
  });
}

TEST_P(SecurityTest, GetSecurity_InsufficientBuffer) {
  std::wstring sddl;
  sddl += kOwnerAccountOperatorsSddl;
  sddl += kGroupAdminsSddl;
  sddl += kAllowEveryoneSddl;
  SetSecurityDescriptor(sddl, &options_.volume_security_descriptor,
                        &options_.volume_security_descriptor_length);
  RunFS([&] (FileSystem* fs) {
    HANDLE handle = OpenFSRootDirectory();
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    SECURITY_INFORMATION info = DACL_SECURITY_INFORMATION |
        OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION;
    auto sd = util::MakeUniqueVarStruct<SECURITY_DESCRIPTOR>(79);
    DWORD length_needed = 0;
    bool result = GetUserObjectSecurity(handle, &info, sd.get(), 79,
                                        &length_needed);
    EXPECT_FALSE(result);
    EXPECT_EQ(80, length_needed);
    EXPECT_EQ(ERROR_INSUFFICIENT_BUFFER, GetLastError());
    fs->Unmount();
  });
}

TEST_P(SecurityTest, GetSecurity_ReadonlyDescriptor) {
  std::wstring base_sddl;
  base_sddl += kOwnerAccountOperatorsSddl;
  base_sddl += kGroupAdminsSddl;
  base_sddl += kAllowEveryoneSddl;
  SetSecurityDescriptor(base_sddl, &options_.volume_security_descriptor,
                        &options_.volume_security_descriptor_length);
  std::wstring readonly_sddl;
  readonly_sddl += kOwnerAccountOperatorsSddl;
  readonly_sddl += kGroupAdminsSddl;
  readonly_sddl += kAllowEveryoneReadonlySddl;
  SetSecurityDescriptor(readonly_sddl, &options_.readonly_security_descriptor,
                        &options_.readonly_security_descriptor_length);

  const wchar_t readonly_path[] =
      L"\\test_GetSecurity_ReadonlyDescriptor\\readonly_file";
  const wchar_t writable_path[] =
      L"\\test_GetSecurity_ReadonlyDescriptor\\writable_file";

  callbacks_->SetUpFile(readonly_path);
  callbacks_->SetUpFile(writable_path);
  callbacks_->SetUseReadonlySecurityDescriptor(readonly_path, true);
  RunFS([&](FileSystem* fs) {
    std::wstring actual_readonly_sddl =
        ReadSecurityDescriptorAsSddl(readonly_path, 1024, false);
    std::wstring actual_base_sddl =
        ReadSecurityDescriptorAsSddl(writable_path, 1024, false);
    EXPECT_EQ(readonly_sddl, actual_readonly_sddl);
    EXPECT_EQ(base_sddl, actual_base_sddl);
    fs->Unmount();
  });
}

TEST_P(SecurityTest, GetSecurity_NoSecurityDescriptor) {
  RunFS([&] (FileSystem* fs) {
    HANDLE handle = OpenFSRootDirectory();
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    SECURITY_INFORMATION info = DACL_SECURITY_INFORMATION |
        OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION;
    auto sd = util::MakeUniqueVarStruct<SECURITY_DESCRIPTOR>(1024);
    DWORD length_needed = 0;
    bool result = GetUserObjectSecurity(handle, &info, sd.get(), 1024,
                                        &length_needed);
    EXPECT_FALSE(result);
    EXPECT_EQ(ERROR_NOT_SUPPORTED, GetLastError());
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(SecurityTests, SecurityTest, testing::Values(
    CallbackThreading::SYNC, CallbackThreading::ASYNC_EPHEMERAL_THREAD));

TEST_P(ReadWriteTest, Flush_Success) {
  const wchar_t path[] = L"\\test_Flush_Success";
  callbacks_->SetUpFile(path);
  const Observer* observer = callbacks_->AddObserver(
      path,
      FILE_GENERIC_READ | FILE_GENERIC_WRITE,
      FILE_ATTRIBUTE_NORMAL,
      FILE_SHARE_READ | FILE_SHARE_WRITE,
      FILE_OPEN,
      FILE_NON_DIRECTORY_FILE | FILE_SYNCHRONOUS_IO_NONALERT);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ | GENERIC_WRITE);
    DWORD bytes_written = 0;
    bool result = WriteFile(handle, "foo", 3, &bytes_written, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(3, bytes_written);
    EXPECT_EQ("foo", callbacks_->file_content(path));
    EXPECT_TRUE(FlushFileBuffers(handle));
    EXPECT_TRUE(observer->flush_invoked);
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Flush_Error) {
  const wchar_t path[] = L"\\test_Flush_Error";
  callbacks_->SetUpFile(path);
  callbacks_->SetFlushResult(STATUS_IO_DEVICE_ERROR);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ | GENERIC_WRITE);
    DWORD bytes_written = 0;
    bool result = WriteFile(handle, "foo", 3, &bytes_written, NULL);
    EXPECT_TRUE(result);
    EXPECT_EQ(3, bytes_written);
    EXPECT_EQ("foo", callbacks_->file_content(path));
    EXPECT_FALSE(FlushFileBuffers(handle));
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(ReadWriteTests, ReadWriteTest, testing::Values(
    CallbackThreading::SYNC, CallbackThreading::ASYNC_EPHEMERAL_THREAD));

class NotificationTest : public FileSystemTestBase {};

TEST_P(NotificationTest, NotifyCreate_File) {
  const wchar_t path[] = L"\\test_NotifyCreate_File";
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, false, FILE_NOTIFY_CHANGE_FILE_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(fs->notification_handler()->NotifyCreate(path, false));
    CheckNotification(handle, buffer, &overlapped, FILE_ACTION_ADDED, path + 1);
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, NotifyCreate_Dir) {
  const wchar_t path[] = L"\\test_NotifyCreate_Dir";
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, false, FILE_NOTIFY_CHANGE_DIR_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(fs->notification_handler()->NotifyCreate(path, true));
    CheckNotification(handle, buffer, &overlapped, FILE_ACTION_ADDED, path + 1);
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, NotifyUpdate) {
  const wchar_t path[] = L"\\test_NotifyUpdate";
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, false, FILE_NOTIFY_CHANGE_ATTRIBUTES, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(fs->notification_handler()->NotifyUpdate(path));
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_MODIFIED, path + 1);
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, NotifyUpdate_NonRoot) {
  const wchar_t path[] = L"\\test_NotifyUpdate_NonRoot";
  const wchar_t change_path[] = L"\\test_NotifyUpdate_NonRoot\\foobar";
  callbacks_->SetUpDir(path);
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = OpenDirForOverlapped(mount_point_ + path, &overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, false, FILE_NOTIFY_CHANGE_ATTRIBUTES, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(fs->notification_handler()->NotifyUpdate(change_path));
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_MODIFIED, L"foobar");
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, NotifyDelete_File) {
  const wchar_t path[] = L"\\test_NotifyDelete_File";
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, false, FILE_NOTIFY_CHANGE_FILE_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(fs->notification_handler()->NotifyDelete(path, false));
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_REMOVED, path + 1);
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, NotifyDelete_Dir) {
  const wchar_t path[] = L"\\test_NotifyDelete_Dir";
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, false, FILE_NOTIFY_CHANGE_DIR_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(fs->notification_handler()->NotifyDelete(path, true));
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_REMOVED, path + 1);
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, NotifyRename_File_SameParent) {
  const wchar_t old_path[] = L"\\test_NotifyRename_File_SameParent_old";
  const wchar_t new_path[] = L"\\test_NotifyRename_File_SameParent_new";
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, false, FILE_NOTIFY_CHANGE_FILE_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(fs->notification_handler()->NotifyRename(
        old_path, new_path, false, true));
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_RENAMED_OLD_NAME, old_path + 1,
        FILE_ACTION_RENAMED_NEW_NAME, new_path + 1);
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, NotifyRename_File_NewParent) {
  // TODO(drivefs-team): There is some weirdness with this case that may
  // indicate one or more driver bugs. 1. We reliably need 2 separate reads to
  // get the 2nd event, while a rename in the same parent does not require
  // that. 2. I can't get a read for either subdirectory (instead of the FS
  // root) to return anything other than a STATUS_INVALID_PARAMETER that is not
  // coming from dokancc.
  const wchar_t old_path[] = L"\\test_NotifyRename_File_NewParent1\\file";
  const wchar_t new_path[] = L"\\test_NotifyRename_File_NewParent2\\file";
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, true, FILE_NOTIFY_CHANGE_FILE_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(fs->notification_handler()->NotifyRename(
        old_path, new_path, false, false));
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_REMOVED,
        L"test_NotifyRename_File_NewParent1\\file");
    result = ReadDirectoryChangesW(
        handle, buffer, 1024, true, FILE_NOTIFY_CHANGE_FILE_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_ADDED,
        L"test_NotifyRename_File_NewParent2\\file");
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, NotifyRename_Dir_SameParent) {
  const wchar_t old_path[] = L"\\test_NotifyRename_Dir_SameParent_old";
  const wchar_t new_path[] = L"\\test_NotifyRename_Dir_SameParent_new";
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, false, FILE_NOTIFY_CHANGE_DIR_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(fs->notification_handler()->NotifyRename(
        old_path, new_path, true, true));
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_RENAMED_OLD_NAME, old_path + 1,
        FILE_ACTION_RENAMED_NEW_NAME, new_path + 1);
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, NotifyRename_Dir_NewParent) {
  // TODO(drivefs-team): This case is weird in the same ways as the file
  // variant.
  const wchar_t old_path[] = L"\\test_NotifyRename_Dir_NewParent1\\dir";
  const wchar_t new_path[] = L"\\test_NotifyRename_Dir_NewParent2\\dir";
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, true, FILE_NOTIFY_CHANGE_DIR_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(fs->notification_handler()->NotifyRename(
        old_path, new_path, true, false));
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_REMOVED, old_path + 1);
    result = ReadDirectoryChangesW(
        handle, buffer, 1024, true, FILE_NOTIFY_CHANGE_DIR_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_ADDED, new_path + 1);
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(NotificationTests, NotificationTest, testing::Values(
    CallbackThreading::SYNC, CallbackThreading::ASYNC_EPHEMERAL_THREAD));

}  // namespace test
}  // namespace dokan
