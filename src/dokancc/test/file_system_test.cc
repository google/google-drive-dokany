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
#include <windows.h>

namespace dokan {
namespace test {

const wchar_t kEveryoneSddl[] = L"WD";
const wchar_t kAllowEveryoneSddl[] = L"D:(A;;GA;;;WD)";
const wchar_t kAllowEveryoneReadonlySddl[] = L"D:(A;;GR;;;WD)";
const wchar_t kOwnerAccountOperatorsSddl[] = L"O:AO";
const wchar_t kGroupAdminsSddl[] = L"G:BA";

const auto kParams = testing::Values(
    kCallbackSync,
    // Current DriveFS prod config.
    kCallbackAsyncEphemeralThread | kAllowRequestBatching |
        kFcbGarbageCollection |
        kDisabledNetworkPhysicalNameQuery,
    // Likely next DriveFS prod config.
    kCallbackAsyncEphemeralThread | kAllowRequestBatching |
        kFcbGarbageCollection |
        kDisabledNetworkPhysicalNameQuery |
        // New possible prod config
        kAllowFullBatching | kAssumePagingIoIsLocked | kDispatchDriverLogs |
        kPullEventAhead | kStoreFcbAvlTable);

class MountTest : public FileSystemTestBase {};

TEST_P(MountTest, NeverMounted) {
  // This test is basically just proving that the destructor doesn't assert if
  // we never mount.
  EXPECT_FALSE(dynamic_cast<TestLogger*>(logger_)->HasErrors());
}

TEST_P(MountTest, MountAndUnmountAfterPostStart) {
  RunFS([](FileSystem* fs) {
    fs->WaitUntilPostStartDone();
    fs->Unmount();
  });

  EXPECT_TRUE(helper_.unmounted());
  // Note that errors from PostStart may be logged during this test.
}

TEST_P(MountTest, MountAndUnmountImmediately) {
  RunFS([](FileSystem* fs) {
    fs->Unmount();
  });

  EXPECT_TRUE(helper_.unmounted());
  // Note that errors from PostStart may be logged during this test.
}

TEST_P(MountTest, MountAndUnmountOnIoThread) {
  RunFS([this](FileSystem* fs) {
    helper_.PostToMainLoop([=] {
      fs->Unmount();
    });
  });

  EXPECT_TRUE(helper_.unmounted());
  // Note that errors from PostStart may be logged during this test.
}

TEST_P(MountTest, MountAndUnmountBeforeLoop) {
  fs_->Mount(mount_point_, options_);
  fs_->Unmount();
  EXPECT_FALSE(fs_->ReceiveIo());
  EXPECT_TRUE(helper_.unmounted());
  // Note that errors from PostStart may be logged during this test.
}

TEST_P(MountTest, MountTwoDifferentDriveLettersAtOnce) {
  // This used to fail because we would use an overlapped event with the same
  // name both times.
  std::thread t1([&] {
    FileSystemTestHelper helper1(L"Q:", GetParam());
    helper1.callbacks_->SetUpFile(L"\\foo.txt", "foo");
    helper1.RunFS(options_, [&](FileSystem* fs) {
      HANDLE handle = helper1.Open(L"\\foo.txt", GENERIC_READ);
      char buffer[4] = {0};
      DWORD bytes_read = 0;
      bool result = ReadFile(handle, buffer, 3, &bytes_read, NULL);
      EXPECT_TRUE(result);
      EXPECT_EQ(std::string(buffer), std::string("foo"));
      fs->Unmount();
    });
    EXPECT_TRUE(helper1.unmounted());
  });
  std::thread t2([&] {
    FileSystemTestHelper helper2(L"R:", GetParam());
    helper2.callbacks_->SetUpFile(L"\\bar.txt", "bar");
    helper2.RunFS(options_, [&](FileSystem* fs) {
      HANDLE handle = helper2.Open(L"\\bar.txt", GENERIC_READ);
      char buffer[4] = {0};
      DWORD bytes_read = 0;
      bool result = ReadFile(handle, buffer, 3, &bytes_read, NULL);
      EXPECT_TRUE(result);
      EXPECT_EQ(std::string(buffer), std::string("bar"));
      fs->Unmount();
    });
    EXPECT_TRUE(helper2.unmounted());
  });
  t1.join();
  t2.join();
}

TEST_P(MountTest, MountSameDriveTwiceAtOnce) {
  std::mutex mutex;
  std::condition_variable cv;
  const std::wstring file_on_first_drive = mount_point_ + L"\\foo.txt";
  const std::wstring file_on_second_drive = mount_point_ + L"\\bar.txt";
  std::thread t1([&] {
    FileSystemTestHelper helper1(mount_point_, GetParam());
    helper1.callbacks_->SetUpFile(L"\\foo.txt", "foo");
    helper1.DisableExistenceCheckAfterUnmount();
    helper1.RunFS(options_, [&](FileSystem* fs) {
      // Prove the drive is bound to this helper's callbacks.
      EXPECT_NE(GetFileAttributes(file_on_first_drive.c_str()), -1);
      // Allow the 2nd thread to proceed.
      cv.notify_one();
    });
    EXPECT_TRUE(helper1.unmounted());
  });
  std::thread t2([&] {
    FileSystemTestHelper helper2(mount_point_, GetParam());
    helper2.callbacks_->SetUpFile(L"\\bar.txt", "bar");
    // Wait until the 1st thread has mounted and checked its drive before we
    // supersede it with a new drive.
    {
      std::unique_lock<std::mutex> lock(mutex);
      cv.wait(lock);
    }
    // This RunFS call should unmount the first drive and replace it with a new
    // one.
    helper2.RunFS(options_, [&](FileSystem* fs) {
      // Prove we are not talking to the callbacks from helper1.
      EXPECT_EQ(GetFileAttributes(file_on_first_drive.c_str()), -1);
      EXPECT_NE(GetFileAttributes(file_on_second_drive.c_str()), -1);
      fs->Unmount();
    });
    EXPECT_TRUE(helper2.unmounted());
  });
  t1.join();
  t2.join();
}

TEST_P(MountTest, MountAndFakeDriveReassignment) {
  // This is a test to hit the code path where the mount manager assigns a
  // different-than-requested drive letter. Since we are faking an invocation to
  // the driver from the mount manager, this gets the dokan driver's internal
  // structs to update, but not the actual usable mount point.
  RunFS([this](FileSystem* fs) {
    std::wstring name = L"\\DosDevices\\Q:";
    USHORT name_length = + name.size() * sizeof(wchar_t);
    std::vector<char> mountdev_name_buffer(sizeof(MOUNTDEV_NAME) + name_length);
    MOUNTDEV_NAME* mountdev_name = reinterpret_cast<MOUNTDEV_NAME*>(
        mountdev_name_buffer.data());
    mountdev_name->NameLength = name_length;
    wcscpy(mountdev_name->Name, name.c_str());
    std::unique_ptr<Device> device =
        helper_.OpenDevice(fs, GENERIC_READ | GENERIC_WRITE);
    ASSERT_NE(device, nullptr);
    EXPECT_TRUE(device->Control(IOCTL_MOUNTDEV_LINK_CREATED, mountdev_name,
                                mountdev_name_buffer.size()));

    // To prove it worked, unmount by the new mount point.
    Device global_device(logger_);
    ASSERT_TRUE(global_device.OpenGlobalDevice());
    DOKAN_UNICODE_STRING_INTERMEDIATE drive_to_delete;
    drive_to_delete.Length = sizeof(wchar_t);
    drive_to_delete.MaximumLength = drive_to_delete.Length;
    drive_to_delete.Buffer[0] = 'Q';
    EXPECT_TRUE(global_device.Control(FSCTL_EVENT_RELEASE, drive_to_delete));
  });

  EXPECT_TRUE(helper_.unmounted());
}

TEST_P(MountTest, MountDirectory) {
  const std::wstring path = L"\\test_CreateAndClose.txt";
  const std::wstring mount_point = L"C:\\MountDirectory";
  EXPECT_TRUE(CreateDirectory(mount_point.c_str(), NULL));
  FileSystemTestHelper helper(mount_point, GetParam());
  helper.callbacks_->SetUpFile(path);
  helper.RunFS(options_, [&](FileSystem* fs) {
    HANDLE handle = CreateFile((mount_point + path).c_str(), GENERIC_READ,
                               FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                               OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    CloseHandle(handle);
    fs->Unmount();
  });
  EXPECT_TRUE(helper.unmounted());
  EXPECT_TRUE(RemoveDirectory(mount_point.c_str()));
  // Note that errors from PostStart may be logged during this test.
}

TEST_P(MountTest, MountInvalidDirectory) {
  const std::wstring mount_point = L"C:\\FileNotAMountPoint.txt";
  HANDLE mount_point_handle = CreateFile(
      mount_point.c_str(), GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
      nullptr, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  CloseHandle(mount_point_handle);
  EXPECT_EQ(fs_->Mount(mount_point, options_),
            MountResult::NO_MOUNT_POINT_ASSIGNED);
}

TEST_P(MountTest, MountAndGetVolumeSizeBeforeEventWait) {
  // This used to trigger a deadlock due to the volume info call dispatching to
  // user mode while the I/O loop is not running.
  helper_.SetMountedCallback([&] {
    HANDLE handle = helper_.OpenFSRootDirectory();
    IO_STATUS_BLOCK iosb;
    memset(&iosb, 0, sizeof(IO_STATUS_BLOCK));
    // Note: we don't care about this data until we change it to be accurate.
    char data[4096];
    NTSTATUS status = NtQueryVolumeInformationFile(
        handle, &iosb, data, sizeof(data), FileFsFullSizeInformation);
    EXPECT_EQ(status, STATUS_SUCCESS);
    CloseHandle(handle);
  });
  helper_.RunFS(options_, [&](FileSystem* fs) {
    fs->WaitUntilPostStartDone();
    fs->Unmount();
  });
  EXPECT_TRUE(helper_.unmounted());
  // Note that errors from PostStart may be logged during this test.
}

INSTANTIATE_TEST_CASE_P(MountTests, MountTest, kParams);

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
    handle = helper_.OpenFSRootDirectory();
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
    handle = helper_.OpenFSRootDirectory();
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

TEST_P(OpenCloseTest, GoodFileNameCaseAfterBadCase) {
  const wchar_t path[] = L"\\test_CaseSwitch.txt";
  const wchar_t lower_path[] = L"\\test_caseswitch.txt";
  callbacks_->SetUpFile(path);
  const Observer* lower_observer = callbacks_->AddObserver(
      lower_path,
      FILE_GENERIC_READ,
      FILE_ATTRIBUTE_NORMAL,
      FILE_SHARE_READ | FILE_SHARE_WRITE,
      FILE_OPEN,
      FILE_NON_DIRECTORY_FILE | FILE_SYNCHRONOUS_IO_NONALERT);
  const Observer* observer = callbacks_->AddObserver(
      path,
      FILE_GENERIC_READ,
      FILE_ATTRIBUTE_NORMAL,
      FILE_SHARE_READ | FILE_SHARE_WRITE,
      FILE_OPEN,
      FILE_NON_DIRECTORY_FILE | FILE_SYNCHRONOUS_IO_NONALERT);
  RunFS([&](FileSystem* fs) {
    const std::wstring file_name = mount_point_ + path;
    const std::wstring lower_file_name = mount_point_ + lower_path;
    HANDLE lower_handle = CreateFile(
        lower_file_name.c_str(), GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_EQ(lower_observer->create_result, STATUS_OBJECT_NAME_NOT_FOUND);
    EXPECT_EQ(lower_handle, INVALID_HANDLE_VALUE);

    // The point is to make sure lower_file_name doesn't somehow get passed to
    // the Create callback below as a consequence of the above usage with that
    // case. FCB garbage collection, for example, specifically avoids this.
    // Note however that with concurrent or overlapping-scoped successful opens,
    // the case you get has always been unspecified behavior.
    HANDLE handle = CreateFile(
        file_name.c_str(), GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_EQ(observer->create_result, STATUS_SUCCESS);
    EXPECT_NE(handle, INVALID_HANDLE_VALUE);
    EXPECT_TRUE(CloseHandle(handle));
    EXPECT_TRUE(observer->cleanup_invoked);
    fs->Unmount();
  });
}

NTSTATUS OpenRelativeFile(HANDLE handle_root,
                          const std::wstring& relative_file_name,
                          PHANDLE handle_file,
                          ULONG DesiredAccess = SYNCHRONIZE | GENERIC_READ,
                          ULONG ShareAccess = 0) {
  OBJECT_ATTRIBUTES attributes_file;
  IO_STATUS_BLOCK iosb_file;
  memset(&attributes_file, 0, sizeof(OBJECT_ATTRIBUTES));
  memset(&iosb_file, 0, sizeof(IO_STATUS_BLOCK));
  UNICODE_STRING name_file;
  RtlInitUnicodeString(&name_file, relative_file_name.c_str());
  attributes_file.Length = sizeof(OBJECT_ATTRIBUTES);
  attributes_file.ObjectName = &name_file;
  attributes_file.RootDirectory = handle_root;
  return NtCreateFile(handle_file, DesiredAccess, &attributes_file,
                      &iosb_file,
                      /*AllocationSize=*/nullptr, /*FileAttributes=*/0,
                      ShareAccess, FILE_OPEN, /*CreateOptions=*/0,
                      /*EaBuffer=*/nullptr, /*EaLength=*/0);
}

NTSTATUS OpenFile(const std::wstring& file_name, PHANDLE handle_file,
                  ULONG DesiredAccess = FILE_ALL_ACCESS) {
  OBJECT_ATTRIBUTES attributes_dir;
  IO_STATUS_BLOCK iosb_dir;
  memset(&attributes_dir, 0, sizeof(OBJECT_ATTRIBUTES));
  memset(&iosb_dir, 0, sizeof(IO_STATUS_BLOCK));
  UNICODE_STRING name_dir;
  const std::wstring full_dir_name = L"\\??\\" + file_name;
  RtlInitUnicodeString(&name_dir, full_dir_name.c_str());
  attributes_dir.Length = sizeof(OBJECT_ATTRIBUTES);
  attributes_dir.ObjectName = &name_dir;
  return NtCreateFile(handle_file, DesiredAccess, &attributes_dir, &iosb_dir,
                      /*AllocationSize=*/nullptr, /*FileAttributes=*/0,
                      /*ShareAccess=*/FILE_SHARE_READ | FILE_SHARE_WRITE,
                      FILE_OPEN,
                      FILE_DIRECTORY_FILE | FILE_OPEN_FOR_BACKUP_INTENT,
                      /*EaBuffer=*/nullptr, /*EaLength=*/0);
}

TEST_P(OpenCloseTest, RelativeFile) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  const wchar_t stream_name[] = L":Zone.Identifier";
  options_.flags |= StartupFlags::DOKAN_OPTION_ALT_STREAM;
  callbacks_->SetUpFile(dir_path);
  callbacks_->SetUpFile(file_path);
  callbacks_->SetUpFile(std::wstring(file_path) +
                        std::wstring(stream_name));
  RunFS([&](FileSystem* fs) {
    // Open parent directory
    HANDLE handle_dir = INVALID_HANDLE_VALUE;
    OBJECT_ATTRIBUTES attributes_dir;
    IO_STATUS_BLOCK iosb_dir;
    memset(&attributes_dir, 0, sizeof(OBJECT_ATTRIBUTES));
    memset(&iosb_dir, 0, sizeof(IO_STATUS_BLOCK));
    UNICODE_STRING name_dir;
    const std::wstring full_dir_name = L"\\??\\" + mount_point_ + dir_path;
    RtlInitUnicodeString(&name_dir, full_dir_name.c_str());
    attributes_dir.Length = sizeof(OBJECT_ATTRIBUTES);
    attributes_dir.ObjectName = &name_dir;
    NTSTATUS status = NtCreateFile(
        &handle_dir, FILE_LIST_DIRECTORY, &attributes_dir,
        &iosb_dir,
        /*AllocationSize=*/nullptr, /*FileAttributes=*/0,
        /*ShareAccess=*/FILE_SHARE_READ | FILE_SHARE_WRITE, FILE_OPEN,
        FILE_DIRECTORY_FILE | FILE_OPEN_FOR_BACKUP_INTENT,
        /*EaBuffer=*/nullptr, /*EaLength=*/0);
    EXPECT_EQ(STATUS_SUCCESS, status);

    HANDLE handle_file = INVALID_HANDLE_VALUE;
    // Open child file with relative name with non relative path
    status = OpenRelativeFile(handle_dir, L"\\file.txt", &handle_file);
    EXPECT_EQ(STATUS_INVALID_PARAMETER, status);

    // Open child file with relative name
    status = OpenRelativeFile(handle_dir, L"file.txt", &handle_file);
    EXPECT_EQ(STATUS_SUCCESS, status);

    HANDLE handle_stream = INVALID_HANDLE_VALUE;
    // Open stream with file as relative path
    status = OpenRelativeFile(handle_file, stream_name, &handle_stream);
    EXPECT_EQ(STATUS_SUCCESS, status);
    NtClose(handle_stream);

    HANDLE duplicate_handle_file = INVALID_HANDLE_VALUE;
    // Open a new handle to an existing FILE_OBJECT
    // Failure is expected as the previous open has ShareAccess 0
    status = OpenRelativeFile(handle_file, L"", &duplicate_handle_file);
    EXPECT_EQ(STATUS_SHARING_VIOLATION, status);

    // TODO(adrienj): This is used when the caller wants to open a new handle to
    // an existing FILE_OBJECT. This is not the same as opening a new handle to
    // the existing FILE_OBJECT (duplicating the handle) because the end result
    // of this is to open a new FILE_OBJECT for the same underlying stream, and
    // the two FILE_OBJECTs are not linked in any other way.
    /*NtClose(handle_file);
    status = OpenRelativeFile(handle_dir, L"file.txt", &handle_file,
                              SYNCHRONIZE | GENERIC_READ,
                              FILE_SHARE_READ | FILE_SHARE_WRITE);
    EXPECT_EQ(STATUS_SUCCESS, status);
    status = OpenRelativeFile(handle_file, L"", &duplicate_handle_file);
    EXPECT_EQ(STATUS_SUCCESS, status);
    NtClose(duplicate_handle_file);*/
    NtClose(handle_file);

    NtClose(handle_dir);
    fs->Unmount();
  });
}

TEST_P(OpenCloseTest, RelativeFileSameFCB) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  callbacks_->SetUpFile(dir_path);
  callbacks_->SetUpFile(file_path);
  RunFS([&](FileSystem* fs) {
    NTSTATUS status;

    // Open parent directory
    HANDLE handle_dir = INVALID_HANDLE_VALUE;
    OBJECT_ATTRIBUTES attributes_dir;
    IO_STATUS_BLOCK iosb_dir;
    memset(&attributes_dir, 0, sizeof(OBJECT_ATTRIBUTES));
    memset(&iosb_dir, 0, sizeof(IO_STATUS_BLOCK));
    UNICODE_STRING name_dir;
    const std::wstring full_dir_name = L"\\??\\" + mount_point_ + dir_path;
    RtlInitUnicodeString(&name_dir, full_dir_name.c_str());
    attributes_dir.Length = sizeof(OBJECT_ATTRIBUTES);
    attributes_dir.ObjectName = &name_dir;
    status = NtCreateFile(
        &handle_dir, FILE_ALL_ACCESS, &attributes_dir, &iosb_dir,
        /*AllocationSize=*/nullptr, /*FileAttributes=*/0,
        /*ShareAccess=*/FILE_SHARE_READ | FILE_SHARE_WRITE, FILE_OPEN,
        FILE_DIRECTORY_FILE | FILE_OPEN_FOR_BACKUP_INTENT,
        /*EaBuffer=*/nullptr, /*EaLength=*/0);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // Open child file with relative name
    HANDLE handle_file = INVALID_HANDLE_VALUE;
    OpenRelativeFile(handle_dir, L"file.txt", &handle_file);

    HANDLE handle2 = CreateFile((mount_point_ + file_path).c_str(), GENERIC_READ,
                               FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                               OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    // STATUS_SHARING_VIOLATION expected here as handle_file ShareAccess is 0
    EXPECT_EQ(INVALID_HANDLE_VALUE, handle2);
    NtClose(handle_file);

    handle2 = CreateFile((mount_point_ + file_path).c_str(), GENERIC_READ,
                        FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle2);
    CloseHandle(handle2);

    NtClose(handle_dir);
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(OpenCloseTests, OpenCloseTest, kParams);

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
  helper_.set_free_space_result(STATUS_IO_DEVICE_ERROR);
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

TEST_P(VolumeInfoTest, GetVolumeMetrics) {
  // Note: this is not a deep check of the metric values, just proof that we
  // can get them.
  const wchar_t path[] = L"\\test_GetVolumeMetrics.txt";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    // First guarantee that there is at least 1 FCB allocation.
    HANDLE handle = Open(path, GENERIC_READ);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_TRUE(CloseHandle(handle));

    std::unique_ptr<Device> device = helper_.OpenDevice(fs);
    VOLUME_METRICS metrics;
    EXPECT_TRUE(fs->GetVolumeMetrics(&metrics));
    EXPECT_GT(metrics.FcbAllocations, 0);
    EXPECT_LE(metrics.FcbDeletions, metrics.FcbAllocations);
    fs->Unmount();
  });
}

TEST_P(VolumeInfoTest, GetVolumeLabelCount) {
  RunFS([&](FileSystem* fs) {
    std::unique_ptr<Device> device =
        helper_.OpenDevice(fs, GENERIC_READ | GENERIC_WRITE);
    VOLUME_METRICS metrics;
    EXPECT_TRUE(fs->GetVolumeMetrics(&metrics));
    EXPECT_EQ(metrics.FsctlVolumeLabelCount, 0);

    std::vector<char> buffer(256);
    EXPECT_TRUE(device->ControlWithOutputOnly(FSCTL_VOLUME_LABEL, buffer.data(),
                                              buffer.size()));
    EXPECT_TRUE(fs->GetVolumeMetrics(&metrics));
    EXPECT_EQ(metrics.FsctlVolumeLabelCount, 1);
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(VolumeInfoTests, VolumeInfoTest, kParams);

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
    DOKAN_LOG_(INFO) << "Skipping ID info test because it is "
                        "unsupported on this OS version.";
    return;
  }
  const wchar_t path[] = L"\\test_GetInfo_IdInfo";
  FileInfo data_to_return = {0};
  data_to_return.file_index = 0x123456789123456;
  callbacks_->SetUpFile(path, data_to_return);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    FILE_ID_INFO info = {0};
    bool result = GetFileInformationByHandleEx(handle, FileIdInfo, &info,
                                               sizeof(info));
    EXPECT_TRUE(result);
    EXPECT_EQ(options_.volume_serial_number, info.VolumeSerialNumber);
    FILE_ID_128 expected_fileid;
    memset(&expected_fileid, 0, sizeof(expected_fileid));
    expected_fileid.Identifier[0] = 0x56;
    expected_fileid.Identifier[1] = 0x34;
    expected_fileid.Identifier[2] = 0x12;
    expected_fileid.Identifier[3] = 0x89;
    expected_fileid.Identifier[4] = 0x67;
    expected_fileid.Identifier[5] = 0x45;
    expected_fileid.Identifier[6] = 0x23;
    expected_fileid.Identifier[7] = 0x01;
    EXPECT_TRUE(0 == std::memcmp(&info.FileId.Identifier,
                                 &expected_fileid.Identifier,
                                 sizeof(expected_fileid.Identifier)));
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
    const ULONG base_size = FIELD_OFFSET(FILE_NAME_INFORMATION, FileName);
    HANDLE handle = Open(path, GENERIC_READ);

    // Note: Sending a buffer size of FIELD_OFFSET(FILE_NAME_INFORMATION,
    // FileName) is a valid buffer overflow that would just set FileNameLength
    // but we cannot test it as ntdll block the request before it goes to the
    // driver.

    auto info = util::MakeUniqueVarStruct<FILE_NAME_INFORMATION>(1024);
    IO_STATUS_BLOCK iosb;
    memset(&iosb, 0, sizeof(iosb));
    NTSTATUS status = NtQueryInformationFile(
        handle, &iosb, info.get(), sizeof(FILE_NAME_INFORMATION),
        FileNameInformation);
    EXPECT_EQ(status, STATUS_BUFFER_OVERFLOW);
    EXPECT_EQ(iosb.Information, base_size + sizeof(WCHAR) * 2);
    EXPECT_EQ(L"\\t", std::wstring(info->FileName, 2));
    EXPECT_EQ(info->FileNameLength, wcslen(path) * sizeof(wchar_t));

    // You can't get half a character back.
    info = util::MakeUniqueVarStruct<FILE_NAME_INFORMATION>(1024);
    memset(&iosb, 0, sizeof(iosb));
    status = NtQueryInformationFile(
        handle, &iosb, info.get(), sizeof(FILE_NAME_INFORMATION) + 1,
        FileNameInformation);
    EXPECT_EQ(status, STATUS_BUFFER_OVERFLOW);
    EXPECT_EQ(iosb.Information, base_size + sizeof(WCHAR) * 2);
    EXPECT_EQ(L"\\t", std::wstring(info->FileName, 2));
    EXPECT_EQ(info->FileNameLength, wcslen(path) * sizeof(wchar_t));

    // Get back 1 character more than the fixed size of the struct.
    info = util::MakeUniqueVarStruct<FILE_NAME_INFORMATION>(1024);
    memset(&iosb, 0, sizeof(iosb));
    status = NtQueryInformationFile(
        handle, &iosb, info.get(), sizeof(FILE_NAME_INFORMATION) + 2,
        FileNameInformation);
    EXPECT_EQ(status, STATUS_BUFFER_OVERFLOW);
    EXPECT_EQ(iosb.Information, base_size + sizeof(WCHAR) * 3);
    EXPECT_EQ(L"\\te", std::wstring(info->FileName, 3));
    EXPECT_EQ(info->FileNameLength, wcslen(path) * sizeof(wchar_t));

    // You can't get back 1.5 characters.
    info = util::MakeUniqueVarStruct<FILE_NAME_INFORMATION>(1024);
    memset(&iosb, 0, sizeof(iosb));
    status = NtQueryInformationFile(
        handle, &iosb, info.get(), sizeof(FILE_NAME_INFORMATION) + 3,
        FileNameInformation);
    EXPECT_EQ(status, STATUS_BUFFER_OVERFLOW);
    EXPECT_EQ(iosb.Information, base_size + sizeof(WCHAR) * 3);
    EXPECT_EQ(L"\\te", std::wstring(info->FileName, 3));
    EXPECT_EQ(info->FileNameLength, wcslen(path) * sizeof(wchar_t));

    // Get back 2 more than the fixed size of the struct.
    info = util::MakeUniqueVarStruct<FILE_NAME_INFORMATION>(1024);
    memset(&iosb, 0, sizeof(iosb));
    status = NtQueryInformationFile(
        handle, &iosb, info.get(), sizeof(FILE_NAME_INFORMATION) + 4,
        FileNameInformation);
    EXPECT_EQ(status, STATUS_BUFFER_OVERFLOW);
    EXPECT_EQ(iosb.Information, base_size + sizeof(WCHAR) * 4);
    EXPECT_EQ(L"\\tes", std::wstring(info->FileName, 4));
    EXPECT_EQ(info->FileNameLength, wcslen(path) * sizeof(wchar_t));
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_NameInfo_Root) {
  RunFS([&](FileSystem* fs) {
    HANDLE handle = helper_.OpenFSRootDirectory();
    FILE_NAME_INFORMATION info;
    memset(&info, 0, sizeof(info));
    IO_STATUS_BLOCK iosb;
    memset(&iosb, 0, sizeof(iosb));
    NTSTATUS status = NtQueryInformationFile(
        handle, &iosb, &info, sizeof(info), FileNameInformation);
    EXPECT_EQ(status, STATUS_SUCCESS);
    EXPECT_EQ(iosb.Information,
              FIELD_OFFSET(FILE_NAME_INFORMATION, FileName) + sizeof(WCHAR));
    EXPECT_EQ(std::wstring(info.FileName, 1), L"\\");
    EXPECT_EQ(info.FileNameLength, sizeof(WCHAR));
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_NameInfo_SingleCharName) {
  const wchar_t path[] = L"\\x";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    FILE_NAME_INFORMATION info;
    memset(&info, 0, sizeof(info));
    IO_STATUS_BLOCK iosb;
    memset(&iosb, 0, sizeof(iosb));
    NTSTATUS status = NtQueryInformationFile(
        handle, &iosb, &info, sizeof(info), FileNameInformation);
    EXPECT_EQ(status, STATUS_SUCCESS);
    EXPECT_EQ(iosb.Information, FIELD_OFFSET(FILE_NAME_INFORMATION, FileName) +
                                    sizeof(WCHAR) * 2);
    EXPECT_EQ(std::wstring(info.FileName, 2), L"\\x");
    EXPECT_EQ(info.FileNameLength, sizeof(WCHAR) * 2);
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_NetworkPhysicalName) {
  const wchar_t path[] = L"\\my_file.txt";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    auto info =
        util::MakeUniqueVarStruct<FILE_NETWORK_PHYSICAL_NAME_INFORMATION>(1024);
    IO_STATUS_BLOCK iosb;
    memset(&iosb, 0, sizeof(iosb));
    NTSTATUS status = NtQueryInformationFile(
        handle, &iosb, info.get(), 1024, FileNetworkPhysicalNameInformation);
    if (GetParam() & kDisabledNetworkPhysicalNameQuery) {
      EXPECT_EQ(status, STATUS_INVALID_PARAMETER);
    } else {
      EXPECT_EQ(status, STATUS_SUCCESS);
      EXPECT_EQ(info->FileNameLength, 0);
    }
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_AllInfo_Root) {
  RunFS([&](FileSystem* fs) {
    HANDLE handle = helper_.OpenFSRootDirectory();
    FILE_ALL_INFORMATION info;
    memset(&info, 0, sizeof(info));
    IO_STATUS_BLOCK iosb;
    memset(&iosb, 0, sizeof(iosb));
    NTSTATUS status = NtQueryInformationFile(handle, &iosb, &info, sizeof(info),
                                             FileAllInformation);
    EXPECT_EQ(status, STATUS_SUCCESS);
    EXPECT_EQ(iosb.Information,
              FIELD_OFFSET(FILE_ALL_INFORMATION, NameInformation.FileName) +
                  sizeof(WCHAR));
    EXPECT_EQ(std::wstring(info.NameInformation.FileName, 1), L"\\");
    EXPECT_EQ(info.NameInformation.FileNameLength, sizeof(WCHAR));
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_AllInfo_SingleCharName) {
  const wchar_t path[] = L"\\x";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    FILE_ALL_INFORMATION info;
    memset(&info, 0, sizeof(info));
    IO_STATUS_BLOCK iosb;
    memset(&iosb, 0, sizeof(iosb));
    NTSTATUS status = NtQueryInformationFile(handle, &iosb, &info, sizeof(info),
                                             FileAllInformation);
    EXPECT_EQ(status, STATUS_SUCCESS);
    EXPECT_EQ(iosb.Information,
              FIELD_OFFSET(FILE_ALL_INFORMATION, NameInformation.FileName) +
                  sizeof(WCHAR) * 2);
    EXPECT_EQ(std::wstring(info.NameInformation.FileName, 2), L"\\x");
    EXPECT_EQ(info.NameInformation.FileNameLength, sizeof(WCHAR) * 2);
    CloseHandle(handle);
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
  data_to_return.creation_time.dwLowDateTime = 0x22222222;
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
    EXPECT_EQ(ERROR_INVALID_PARAMETER, GetLastError());
    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_AllInfo) {
  const wchar_t path[] = L"\\test_GetInfo_AllInfo";
  FileInfo data_to_return = {0};
  data_to_return.file_size = 0x0000000100004359;
  data_to_return.file_index = 0x1234;
  data_to_return.creation_time.dwHighDateTime = 0x11111111;
  data_to_return.creation_time.dwLowDateTime = 0x22222222;
  data_to_return.last_write_time.dwHighDateTime = 0x33333333;
  data_to_return.last_write_time.dwLowDateTime = 0x44444444;
  data_to_return.last_access_time.dwHighDateTime = 0x55555555;
  data_to_return.last_access_time.dwLowDateTime = 0x66666666;
  data_to_return.file_attributes = FILE_ATTRIBUTE_HIDDEN;
  callbacks_->SetUpFile(path, data_to_return);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    auto info = util::MakeUniqueVarStruct<FILE_ALL_INFORMATION>(1024);
    IO_STATUS_BLOCK iosb;
    memset(&iosb, 0, sizeof(iosb));
    NTSTATUS status = NtQueryInformationFile(handle, &iosb, info.get(), 1024,
                                             FileAllInformation);
    EXPECT_EQ(status, STATUS_SUCCESS);
    EXPECT_GT(iosb.Information, sizeof(FILE_ALL_INFORMATION));
    EXPECT_EQ(info->NameInformation.FileNameLength,
              wcslen(path) * sizeof(wchar_t));
    EXPECT_EQ(path, std::wstring(info->NameInformation.FileName,
                                 info->NameInformation.FileNameLength /
                                     sizeof(wchar_t)));

    EXPECT_FALSE(info->StandardInformation.DeletePending);
    EXPECT_FALSE(info->StandardInformation.Directory);
    EXPECT_EQ(1, info->StandardInformation.AllocationSize.HighPart);
    EXPECT_EQ(17408, info->StandardInformation.AllocationSize.LowPart);
    EXPECT_EQ(1, info->StandardInformation.EndOfFile.HighPart);
    EXPECT_EQ(17241, info->StandardInformation.EndOfFile.LowPart);
    EXPECT_EQ(0x11111111, info->BasicInformation.CreationTime.HighPart);
    EXPECT_EQ(0x22222222, info->BasicInformation.CreationTime.LowPart);
    EXPECT_EQ(0x33333333, info->BasicInformation.LastWriteTime.HighPart);
    EXPECT_EQ(0x44444444, info->BasicInformation.LastWriteTime.LowPart);
    EXPECT_EQ(0x55555555, info->BasicInformation.LastAccessTime.HighPart);
    EXPECT_EQ(0x66666666, info->BasicInformation.LastAccessTime.LowPart);
    EXPECT_EQ(FILE_ATTRIBUTE_HIDDEN, info->BasicInformation.FileAttributes);
    EXPECT_EQ(data_to_return.file_index,
              info->InternalInformation.IndexNumber.QuadPart);

    CloseHandle(handle);

    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_AllInfo_Overflow) {
  const wchar_t path[] = L"\\test_GetInfo_AllInfo";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    const ULONG base_size =
        FIELD_OFFSET(FILE_ALL_INFORMATION, NameInformation.FileName);
    // Same as GetInfo_NameInfo_Overflow but for FILE_ALL_INFORMATION

    // Note: Sending a buffer size of FIELD_OFFSET(FILE_ALL_INFORMATION,
    // NameInformation.FileName) is a valid buffer overflow that would just set
    // FileNameLength but we cannot test it as ntdll block the request before it
    // goes to the driver.
    HANDLE handle = Open(path, GENERIC_READ);
    auto info = util::MakeUniqueVarStruct<FILE_ALL_INFORMATION>(1024);
    IO_STATUS_BLOCK iosb;
    memset(&iosb, 0, sizeof(iosb));
    NTSTATUS status = NtQueryInformationFile(handle, &iosb, info.get(),
                                             sizeof(FILE_ALL_INFORMATION),
                                             FileAllInformation);
    EXPECT_EQ(status, STATUS_BUFFER_OVERFLOW);
    EXPECT_EQ(iosb.Information, base_size + sizeof(WCHAR) * 2);
    EXPECT_EQ(L"\\t", std::wstring(info->NameInformation.FileName, 2));
    EXPECT_EQ(info->NameInformation.FileNameLength,
              wcslen(path) * sizeof(wchar_t));

    // You can't get half a character back.
    info = util::MakeUniqueVarStruct<FILE_ALL_INFORMATION>(1024);
    memset(&iosb, 0, sizeof(iosb));
    status = NtQueryInformationFile(handle, &iosb, info.get(),
                                    sizeof(FILE_ALL_INFORMATION) + 1,
                                    FileAllInformation);
    EXPECT_EQ(status, STATUS_BUFFER_OVERFLOW);
    EXPECT_EQ(iosb.Information, base_size + sizeof(WCHAR) * 2);
    EXPECT_EQ(L"\\t", std::wstring(info->NameInformation.FileName, 2));
    EXPECT_EQ(info->NameInformation.FileNameLength,
              wcslen(path) * sizeof(wchar_t));

    // Get back 1 character more than the fixed size of the struct.
    info = util::MakeUniqueVarStruct<FILE_ALL_INFORMATION>(1024);
    memset(&iosb, 0, sizeof(iosb));
    status = NtQueryInformationFile(handle, &iosb, info.get(),
                                    sizeof(FILE_ALL_INFORMATION) + 2,
                                    FileAllInformation);
    EXPECT_EQ(status, STATUS_BUFFER_OVERFLOW);
    EXPECT_EQ(iosb.Information, base_size + sizeof(WCHAR) * 3);
    EXPECT_EQ(L"\\te", std::wstring(info->NameInformation.FileName, 3));
    EXPECT_EQ(info->NameInformation.FileNameLength,
              wcslen(path) * sizeof(wchar_t));

    // You can't get back 1.5 characters.
    info = util::MakeUniqueVarStruct<FILE_ALL_INFORMATION>(1024);
    memset(&iosb, 0, sizeof(iosb));
    status = NtQueryInformationFile(handle, &iosb, info.get(),
                                    sizeof(FILE_ALL_INFORMATION) + 3,
                                    FileAllInformation);
    EXPECT_EQ(status, STATUS_BUFFER_OVERFLOW);
    EXPECT_EQ(iosb.Information, base_size + sizeof(WCHAR) * 3);
    EXPECT_EQ(L"\\te", std::wstring(info->NameInformation.FileName, 3));
    EXPECT_EQ(info->NameInformation.FileNameLength,
              wcslen(path) * sizeof(wchar_t));

    // Get back 2 more than the fixed size of the struct.
    info = util::MakeUniqueVarStruct<FILE_ALL_INFORMATION>(1024);
    memset(&iosb, 0, sizeof(iosb));
    status = NtQueryInformationFile(handle, &iosb, info.get(),
                                    sizeof(FILE_ALL_INFORMATION) + 4,
                                    FileAllInformation);
    EXPECT_EQ(status, STATUS_BUFFER_OVERFLOW);
    EXPECT_EQ(iosb.Information, base_size + sizeof(WCHAR) * 4);
    EXPECT_EQ(L"\\tes", std::wstring(info->NameInformation.FileName, 4));
    EXPECT_EQ(info->NameInformation.FileNameLength,
              wcslen(path) * sizeof(wchar_t));
    CloseHandle(handle);

    fs->Unmount();
  });
}

TEST_P(MetadataTest, GetInfo_InternalInfo) {
  const wchar_t path[] = L"\\test_InternalInfo_AllInfo";
  FileInfo data_to_return = {0};
  data_to_return.file_size = 0x0000000100004359;
  data_to_return.file_index = 0x1234;
  data_to_return.creation_time.dwHighDateTime = 0x11111111;
  data_to_return.creation_time.dwLowDateTime = 0x22222222;
  data_to_return.last_write_time.dwHighDateTime = 0x33333333;
  data_to_return.last_write_time.dwLowDateTime = 0x44444444;
  data_to_return.last_access_time.dwHighDateTime = 0x55555555;
  data_to_return.last_access_time.dwLowDateTime = 0x66666666;
  data_to_return.file_attributes = FILE_ATTRIBUTE_HIDDEN;
  callbacks_->SetUpFile(path, data_to_return);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    FILE_INTERNAL_INFORMATION data{0};
    IO_STATUS_BLOCK iosb;
    memset(&iosb, 0, sizeof(iosb));
    NTSTATUS status = NtQueryInformationFile(handle, &iosb, &data,
                                             sizeof(FILE_INTERNAL_INFORMATION),
                                             FileInternalInformation);
    EXPECT_EQ(status, STATUS_SUCCESS);
    EXPECT_EQ(iosb.Information, sizeof(FILE_INTERNAL_INFORMATION));
    EXPECT_EQ(data_to_return.file_index, data.IndexNumber.QuadPart);

    CloseHandle(handle);

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

INSTANTIATE_TEST_CASE_P(MetadataTests, MetadataTest, kParams);

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

TEST_P(MoveDeleteTest, Move_Replace_DestExistsWithFcbGC) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t source[] = L"\\dir\\file";
  const wchar_t dest[] = L"\\dir\\file2";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpFile(source);
  callbacks_->SetUpFile(dest);
  RunFS([&](FileSystem* fs) {
    // Open parent directory - \dir
    HANDLE handle_dir = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + dir_path, &handle_dir);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // Open child file with relative name - \dir\file.txt
    HANDLE handle_file = INVALID_HANDLE_VALUE;
    status = OpenRelativeFile(handle_dir, L"file", &handle_file,
                              DELETE | SYNCHRONIZE,
                              FILE_SHARE_READ | FILE_SHARE_WRITE);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // Open handle on destination to generate a Fcb GC
    HANDLE handle_file2 =
        CreateFile((mount_point_ + dest).c_str(), 0,
                   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                   nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle_file2);
    CloseHandle(handle_file2);

    // \dir\file -> \dir\file2
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->FileNameLength = wcslen(L"file2") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L"file2",
           file_rename_information->FileNameLength);
    file_rename_information->ReplaceIfExists = TRUE;
    status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(L"\\dir\\file2"));

    NtClose(handle_file);
    NtClose(handle_dir);
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, Move_Replace_DestExistsWithOpenedHandle) {
  // Replacing a file with an existing handle open even with FILE_SHARE_DELETE
  // is denied on NTFS. A Dokan bug makes it possible so this test just exists
  // to cover the code that handles the Fcb being replaced.
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t source[] = L"\\dir\\file";
  const wchar_t dest[] = L"\\dir\\file2";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpFile(source);
  callbacks_->SetUpFile(dest);
  RunFS([&](FileSystem* fs) {
    // Open parent directory - \dir
    HANDLE handle_dir = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + dir_path, &handle_dir);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // Open child file with relative name - \dir\file.txt
    HANDLE handle_file = INVALID_HANDLE_VALUE;
    status = OpenRelativeFile(handle_dir, L"file", &handle_file,
                              DELETE | SYNCHRONIZE,
                              FILE_SHARE_READ | FILE_SHARE_WRITE);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // Open handle on destination file with delete shared
    HANDLE handle_file2 =
        CreateFile((mount_point_ + dest).c_str(), 0,
                   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                   nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle_file2);

    // \dir\file -> \dir\file2
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->FileNameLength = wcslen(L"file2") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L"file2",
           file_rename_information->FileNameLength);
    file_rename_information->ReplaceIfExists = TRUE;
    status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);  // NTFS - STATUS_ACCESS_DENIED
    EXPECT_TRUE(callbacks_->file_exists(L"\\dir\\file2"));

    CloseHandle(handle_file2);
    NtClose(handle_file);
    NtClose(handle_dir);
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, FullyQualifiedRename) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t dir2_path[] = L"\\dir2";
  const wchar_t dir3_path[] = L"\\dir3";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  const wchar_t file_renamed_path[] = L"\\dir3\\file2.txt";
  const wchar_t file_root_path[] = L"\\root_file.txt";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpDir(dir3_path);
  callbacks_->SetUpFile(file_path);
  RunFS([&](FileSystem* fs) {
    // \dir\file.txt -> \dir3\file2.txt
    EXPECT_TRUE(MoveFile((mount_point_ + file_path).c_str(),
                         (mount_point_ + file_renamed_path).c_str()));
    EXPECT_TRUE(callbacks_->file_exists(file_renamed_path));

    // \dir -> \dir2
    EXPECT_TRUE(MoveFile((mount_point_ + dir_path).c_str(),
                         (mount_point_ + dir2_path).c_str()));
    EXPECT_TRUE(callbacks_->file_exists(dir2_path));

    WCHAR current_directory[MAX_PATH];
    EXPECT_TRUE(GetCurrentDirectory(MAX_PATH, current_directory) > 0);
    EXPECT_TRUE(SetCurrentDirectory((mount_point_ + dir3_path).c_str()));
    // cd \dir3 : \dir3\file2.txt -> \file.txt
    // This is endup still being a fully qualified rename.
    EXPECT_TRUE(MoveFile(L"file2.txt", L"..\\file.txt"));
    EXPECT_TRUE(callbacks_->file_exists(L"\\file.txt"));
    EXPECT_TRUE(SetCurrentDirectory(current_directory));
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, SimpleFileRename) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpFile(file_path);
  RunFS([&](FileSystem* fs) {
    // Open parent directory - \dir
    HANDLE handle_dir = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + dir_path, &handle_dir);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // Open child file with relative name - \dir\file.txt
    HANDLE handle_file = INVALID_HANDLE_VALUE;
    status = OpenRelativeFile(handle_dir, L"file.txt", &handle_file,
                              DELETE | SYNCHRONIZE,
                              FILE_SHARE_READ | FILE_SHARE_WRITE);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // \dir\file.txt -> \dir\file2.txt
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->FileNameLength =
        wcslen(L"file2.txt") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L"file2.txt",
           file_rename_information->FileNameLength);
    status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(L"\\dir\\file2.txt"));

    NtClose(handle_file);
    NtClose(handle_dir);
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, SimpleDirectoryRename) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t sub_dir[] = L"\\dir\\sub_dir";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpDir(sub_dir);
  RunFS([&](FileSystem* fs) {
    //NTSTATUS status;

    // Open parent directory - \dir
    HANDLE handle_dir = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + dir_path, &handle_dir);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // Open child dir with relative name - \dir\sub_dir
    HANDLE handle_sub_dir = INVALID_HANDLE_VALUE;
    status = OpenRelativeFile(handle_dir, L"sub_dir", &handle_sub_dir,
                              DELETE | SYNCHRONIZE,
                              FILE_SHARE_READ | FILE_SHARE_WRITE);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // \dir\sub_dir -> \dir\sub_dir2
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->FileNameLength =
        wcslen(L"sub_dir2") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L"sub_dir2",
           file_rename_information->FileNameLength);
    status = NtSetInformationFile(
        handle_sub_dir, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(L"\\dir\\sub_dir2"));

    NtClose(handle_sub_dir);
    NtClose(handle_dir);
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, SimpleRenameEndingWithBackslash) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpFile(file_path);
  RunFS([&](FileSystem* fs) {
    // Open parent directory - \dir
    HANDLE handle_dir = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + dir_path, &handle_dir);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // Open child file with relative name - \dir\file.txt
    HANDLE handle_file = INVALID_HANDLE_VALUE;
    status = OpenRelativeFile(handle_dir, L"file.txt", &handle_file,
                              DELETE | SYNCHRONIZE,
                              FILE_SHARE_READ | FILE_SHARE_WRITE);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // \dir\file.txt -> \dir\file2.txt (\dir\file2.txt\)
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->FileNameLength =
        wcslen(L"file2.txt\\") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L"file2.txt\\",
           file_rename_information->FileNameLength);
    status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(L"\\dir\\file2.txt"));

    NtClose(handle_file);
    NtClose(handle_dir);
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, RelativeRename) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t dir2_path[] = L"\\dir2";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpDir(dir2_path);
  callbacks_->SetUpFile(file_path);
  RunFS([&](FileSystem* fs) {
    // Open file \dir\file.txt
    HANDLE handle_file =
        CreateFile((mount_point_ + file_path).c_str(), GENERIC_READ | DELETE,
                   FILE_SHARE_READ, nullptr, OPEN_EXISTING, 0, nullptr);

    // Open destination directory - \dir2
    HANDLE handle_dest_dir = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + dir2_path, &handle_dest_dir,
                               SYNCHRONIZE | FILE_ADD_FILE);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // \dir\file.txt -> \dir2\file.txt
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->RootDirectory = handle_dest_dir;
    file_rename_information->FileNameLength =
        wcslen(L"file2.txt") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L"file2.txt",
           file_rename_information->FileNameLength);
    status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(L"\\dir2\\file2.txt"));

    NtClose(handle_dest_dir);
    CloseHandle(handle_file);

    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, RelativeRenameWithDeviceAsRootDirectory) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t dir2_path[] = L"\\dir2";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpDir(dir2_path);
  callbacks_->SetUpFile(file_path);
  RunFS([&](FileSystem* fs) {
    // Open file \dir\file.txt
    HANDLE handle_file =
        CreateFile((mount_point_ + file_path).c_str(), GENERIC_READ | DELETE,
                   FILE_SHARE_READ, nullptr, OPEN_EXISTING, 0, nullptr);

    // Open dokan device
    HANDLE handle_dest_dir = INVALID_HANDLE_VALUE;
    OBJECT_ATTRIBUTES attributes_dir;
    IO_STATUS_BLOCK iosb_dir;
    memset(&attributes_dir, 0, sizeof(OBJECT_ATTRIBUTES));
    memset(&iosb_dir, 0, sizeof(IO_STATUS_BLOCK));
    UNICODE_STRING name_dir;
    const std::wstring raw_device_name = L"\\Device" + fs_->device_name();
    RtlInitUnicodeString(&name_dir, raw_device_name.c_str());
    attributes_dir.Length = sizeof(OBJECT_ATTRIBUTES);
    attributes_dir.ObjectName = &name_dir;
    NTSTATUS status = NtCreateFile(
        &handle_dest_dir, SYNCHRONIZE /*| FILE_ADD_FILE*/, &attributes_dir,
        &iosb_dir,
        /*AllocationSize=*/nullptr, /*FileAttributes=*/0,
        /*ShareAccess=*/FILE_SHARE_READ | FILE_SHARE_WRITE, FILE_OPEN,
        FILE_DIRECTORY_FILE | FILE_OPEN_FOR_BACKUP_INTENT,
        /*EaBuffer=*/nullptr, /*EaLength=*/0);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // \dir\file.txt -> \dir2\file.txt
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->RootDirectory = handle_dest_dir;
    file_rename_information->FileNameLength =
        wcslen(L"\\dir2\\file2.txt") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L"\\dir2\\file2.txt",
           file_rename_information->FileNameLength);
    status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(L"\\dir2\\file2.txt"));

    NtClose(handle_dest_dir);
    CloseHandle(handle_file);

    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, RelativeRenameEndingWithBackslash) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t dir2_path[] = L"\\dir2";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpDir(dir2_path);
  callbacks_->SetUpFile(file_path);
  RunFS([&](FileSystem* fs) {
    // Open file \dir\file.txt
    HANDLE handle_file =
        CreateFile((mount_point_ + file_path).c_str(), GENERIC_READ | DELETE,
                   FILE_SHARE_READ, nullptr, OPEN_EXISTING, 0, nullptr);

    // Open destination directory - \dir2
    HANDLE handle_dest_dir = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + dir2_path, &handle_dest_dir,
                               SYNCHRONIZE | FILE_ADD_FILE);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // \dir\file.txt -> \dir2\file2.txt (\dir2\file2.txt\)
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->RootDirectory = handle_dest_dir;
    file_rename_information->FileNameLength =
        wcslen(L"file2.txt\\") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L"file2.txt\\",
           file_rename_information->FileNameLength);
    status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(L"\\dir2\\file2.txt"));

    NtClose(handle_dest_dir);
    CloseHandle(handle_file);

    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, RelativeRenameWithRelativeRootDirectory) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t dir2_path[] = L"\\dir2";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpDir(dir2_path);
  callbacks_->SetUpFile(file_path);
  RunFS([&](FileSystem* fs) {
    // Open file \dir\file.txt
    HANDLE handle_file =
        CreateFile((mount_point_ + file_path).c_str(), GENERIC_READ | DELETE,
                   FILE_SHARE_READ, nullptr, OPEN_EXISTING, 0, nullptr);

    // Open parent directory - \dir
    HANDLE handle_dest_root = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + L"\\", &handle_dest_root);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // Open child file with relative name - \dir\file.txt
    HANDLE handle_dest_dir = INVALID_HANDLE_VALUE;
    status = OpenRelativeFile(handle_dest_root, L"dir2", &handle_dest_dir,
                              SYNCHRONIZE | FILE_ADD_FILE,
                              FILE_SHARE_READ | FILE_SHARE_WRITE);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // \dir\file.txt -> \dir2\file.txt
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->RootDirectory = handle_dest_dir;
    file_rename_information->FileNameLength =
        wcslen(L"file2.txt") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L"file2.txt",
           file_rename_information->FileNameLength);
    status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(L"\\dir2\\file2.txt"));

    NtClose(handle_dest_dir);
    NtClose(handle_dest_root);
    CloseHandle(handle_file);

    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, RelativeRenameWithRelativeRootDirWithOpenTargetDir) {
  // This is how WSL2 rename files with: mv dir/file.txt dir2/file2.txt
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t dir2_path[] = L"\\dir2";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpDir(dir2_path);
  callbacks_->SetUpFile(file_path);
  RunFS([&](FileSystem* fs) {
    // Open parent directory - \dir
    HANDLE handle_root = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + L"\\", &handle_root);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // Open child file with relative name - \dir\file.txt
    HANDLE handle_file = INVALID_HANDLE_VALUE;
    status = OpenRelativeFile(handle_root, L"dir\\file.txt", &handle_file,
                              DELETE | SYNCHRONIZE,
                              FILE_SHARE_READ | FILE_SHARE_WRITE);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // \dir\file.txt -> \dir2\file.txt
    // This rename is special as the filename also include a directory path.
    // This will produce a CreateFile request on dir2\file2.txt relative to \
    // with SL_OPEN_TARGET_DIRECTORY
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->RootDirectory = handle_root;
    file_rename_information->FileNameLength =
        wcslen(L"dir2\\file2.txt") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L"dir2\\file2.txt",
           file_rename_information->FileNameLength);
    status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(L"\\dir2\\file2.txt"));

    NtClose(handle_file);
    NtClose(handle_root);

    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, RelativeStreamRename) {
  // It is a weird case that is not supported by NTFS but we are capable to
  // process it
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  const wchar_t file_stream1_path[] = L"\\dir\\file.txt:stream1";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpFile(file_path);
  callbacks_->SetUpFile(file_stream1_path);
  options_.flags |= StartupFlags::DOKAN_OPTION_ALT_STREAM;
  RunFS([&](FileSystem* fs) {
    // Open file \dir\file.txt:stream1
    HANDLE handle_file =
        CreateFile((mount_point_ + file_path).c_str(), GENERIC_READ | DELETE,
                   +FILE_SHARE_READ, nullptr, OPEN_EXISTING, 0, nullptr);
    // Open destination directory - \dir1
    HANDLE handle_dest_dir = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + dir_path, &handle_dest_dir,
                               +SYNCHRONIZE | FILE_ADD_FILE);
    EXPECT_EQ(STATUS_SUCCESS, status);
    // \dir\file.txt:stream1 -> \dir\file2.txt:stream2
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->RootDirectory = handle_dest_dir;
    file_rename_information->FileNameLength =
        wcslen(L"file.txt:stream2") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L"file.txt:stream2",
           file_rename_information->FileNameLength);
    status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(L"\\dir\\file.txt:stream2"));
    NtClose(handle_dest_dir);
    CloseHandle(handle_file);
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, RelativeInvalidStreamRename) {
  const wchar_t dir_path[] = L"\\dir";
  const wchar_t file_path[] = L"\\dir\\file.txt";
  const wchar_t file_stream1_path[] = L"\\dir\\file.txt:stream1";
  callbacks_->SetUpDir(dir_path);
  callbacks_->SetUpFile(file_path);
  callbacks_->SetUpFile(file_stream1_path);
  options_.flags |= StartupFlags::DOKAN_OPTION_ALT_STREAM;
  RunFS([&](FileSystem* fs) {
    // Open file \dir\file.txt
    HANDLE handle_file =
        CreateFile((mount_point_ + file_path).c_str(), GENERIC_READ | DELETE,
                   +FILE_SHARE_READ, nullptr, OPEN_EXISTING, 0, nullptr);
    // Open destination directory - \dir1
    HANDLE handle_dest_dir = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + dir_path, &handle_dest_dir,
                               +SYNCHRONIZE | FILE_ADD_FILE);
    EXPECT_EQ(STATUS_SUCCESS, status);
    // \dir\file.txt:stream1 -> \dir\:stream2 <- INVALID
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->RootDirectory = handle_dest_dir;
    file_rename_information->FileNameLength =
        wcslen(L":stream2") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L":stream2",
           file_rename_information->FileNameLength);
    status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_INVALID_PARAMETER, status);
    EXPECT_FALSE(callbacks_->file_exists(L"\\dir\\file.txt:stream2"));
    NtClose(handle_dest_dir);
    CloseHandle(handle_file);
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, SimpleStreamRename) {
  const wchar_t file_path[] = L"\\file.txt";
  const wchar_t file_stream1_path[] = L"\\file.txt:stream1";
  callbacks_->SetUpFile(file_path);
  callbacks_->SetUpFile(file_stream1_path);
  options_.flags |= StartupFlags::DOKAN_OPTION_ALT_STREAM;
  RunFS([&](FileSystem* fs) {
    // Open parent directory - \file.txt
    HANDLE handle_file = INVALID_HANDLE_VALUE;
    NTSTATUS status = OpenFile(mount_point_ + file_path, &handle_file);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // Open attached stream with relative name - \file.txt:stream1
    HANDLE handle_file_stream = INVALID_HANDLE_VALUE;
    status = OpenRelativeFile(handle_file, L":stream1", &handle_file_stream,
                     DELETE | SYNCHRONIZE, FILE_SHARE_READ | FILE_SHARE_WRITE);
    EXPECT_EQ(STATUS_SUCCESS, status);

    // \file.txt:stream1 -> \file.txt:stream2
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->FileNameLength =
        wcslen(L":stream2") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L":stream2",
           file_rename_information->FileNameLength);
    status = NtSetInformationFile(
        handle_file_stream, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(L"\\file.txt:stream2"));

    NtClose(handle_file_stream);
    NtClose(handle_file);
    fs->Unmount();
  });
}

TEST_P(MoveDeleteTest, SimpleMainStreamRename) {
  const wchar_t file_path[] = L"\\file.txt";
  const wchar_t file_stream1_path[] = L"\\file.txt:stream1";
  callbacks_->SetUpFile(file_path);
  options_.flags |= StartupFlags::DOKAN_OPTION_ALT_STREAM;
  RunFS([&](FileSystem* fs) {
    // Open \file.txt main stream without the $DATA stream name
    HANDLE handle_file =
        CreateFile((mount_point_ + file_path).c_str(), GENERIC_READ | DELETE,
                   FILE_SHARE_READ, nullptr, OPEN_EXISTING, 0, nullptr);

    // \file.txt -> \file.txt:stream1
    IO_STATUS_BLOCK iosb_rename_file;
    memset(&iosb_rename_file, 0, sizeof(IO_STATUS_BLOCK));
    std::vector<uint8_t> buffer(sizeof(FILE_RENAME_INFORMATION) + MAX_PATH);
    PFILE_RENAME_INFORMATION file_rename_information =
        (PFILE_RENAME_INFORMATION)buffer.data();
    file_rename_information->FileNameLength =
        wcslen(L":stream1") * sizeof(WCHAR);
    memcpy(file_rename_information->FileName, L":stream1",
           file_rename_information->FileNameLength);
    NTSTATUS status = NtSetInformationFile(
        handle_file, &iosb_rename_file, file_rename_information,
        FIELD_OFFSET(FILE_RENAME_INFORMATION, FileName) +
            file_rename_information->FileNameLength,
        FileRenameInformation);
    EXPECT_EQ(STATUS_SUCCESS, status);
    EXPECT_TRUE(callbacks_->file_exists(file_stream1_path));

    NtClose(handle_file);
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(MoveDeleteTests, MoveDeleteTest, kParams);

class ReadWriteTest : public FileSystemTestBase {};

TEST_P(ReadWriteTest, HandleFlags) {
  const wchar_t path[] = L"\\test_HandleFlags";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    callbacks_->SetHandleMatcher([=](const dokan::FileHandle* handle) {
      return handle->path() == path && handle->has_readonly_desired_access() &&
             !handle->allows_sharing(FILE_SHARE_READ) &&
             !handle->allows_sharing(FILE_SHARE_WRITE) &&
             !handle->allows_sharing(FILE_SHARE_DELETE);
    });
    CloseHandle(Open(path, GENERIC_READ));
    EXPECT_TRUE(callbacks_->CheckHandleMatcher());

    callbacks_->SetHandleMatcher([=](const dokan::FileHandle* handle) {
      return handle->path() == path && !handle->has_readonly_desired_access() &&
             !handle->allows_sharing(FILE_SHARE_READ) &&
             !handle->allows_sharing(FILE_SHARE_WRITE) &&
             !handle->allows_sharing(FILE_SHARE_DELETE);
    });
    CloseHandle(Open(path, GENERIC_WRITE));
    EXPECT_TRUE(callbacks_->CheckHandleMatcher());
    CloseHandle(Open(path, GENERIC_READ | GENERIC_WRITE));
    EXPECT_TRUE(callbacks_->CheckHandleMatcher());
    CloseHandle(Open(path, GENERIC_ALL));
    EXPECT_TRUE(callbacks_->CheckHandleMatcher());

    callbacks_->SetHandleMatcher([=](const dokan::FileHandle* handle) {
      return handle->path() == path && handle->has_readonly_desired_access() &&
             handle->allows_sharing(FILE_SHARE_READ) &&
             !handle->allows_sharing(FILE_SHARE_WRITE) &&
             !handle->allows_sharing(FILE_SHARE_DELETE);
    });
    CloseHandle(Open(path, GENERIC_READ, FILE_SHARE_READ));
    EXPECT_TRUE(callbacks_->CheckHandleMatcher());
    callbacks_->SetHandleMatcher([=](const dokan::FileHandle* handle) {
      return handle->path() == path && handle->has_readonly_desired_access() &&
             handle->allows_sharing(FILE_SHARE_READ) &&
             handle->allows_sharing(FILE_SHARE_WRITE) &&
             !handle->allows_sharing(FILE_SHARE_DELETE);
    });
    CloseHandle(Open(path, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE));
    EXPECT_TRUE(callbacks_->CheckHandleMatcher());
    callbacks_->SetHandleMatcher([=](const dokan::FileHandle* handle) {
      return handle->path() == path && handle->has_readonly_desired_access() &&
             handle->allows_sharing(FILE_SHARE_READ) &&
             handle->allows_sharing(FILE_SHARE_WRITE) &&
             handle->allows_sharing(FILE_SHARE_DELETE);
    });
    CloseHandle(Open(path, GENERIC_READ,
                     FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE));
    EXPECT_TRUE(callbacks_->CheckHandleMatcher());
    fs->Unmount();
  });
}

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

TEST_P(ReadWriteTest, Read_4GB_Boundary_TooHigh) {
  // This test would originally BSOD or assert depending on whether normal
  // assertions were enabled.
  const wchar_t path[] = L"\\test_Read_4GB_Boundary_TooHigh";
  FileInfo info{0};
  info.file_size = 0xffffffff;
  // Note: it's too time consuming to do the whole failure range.
  const uint64_t low_size = FileSystem::kMaxReadSize + 1;
  callbacks_->SetUpFile(path, info);
  callbacks_->SetFakeReadSuccess(true);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    std::vector<char> buffer(info.file_size);
    DWORD bytes_read = 0;
    for (DWORD chunk_size = info.file_size; chunk_size >= low_size;
         --chunk_size) {
      EXPECT_FALSE(ReadFile(handle, buffer.data(), chunk_size, &bytes_read,
                            nullptr));
    }
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Read_4GB_Boundary_Success) {
  if (!IsWindows8OrGreater()) {
    // The read tends to fail in Windows 7 test VMs, which is not reproducible
    // using a comparable VM image directly. We think this may be due to the
    // build ramdisk using too much of the available RAM.
    DOKAN_LOG_(INFO) << "Skipping 4 GB boundary successful read test.";
    return;
  }
  const wchar_t path[] = L"\\test_Read_4GB_Boundary_Success";
  FileInfo info{0};
  info.file_size = FileSystem::kMaxReadSize;
  callbacks_->SetUpFile(path, info);
  callbacks_->SetFakeReadSuccess(true);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ);
    std::vector<char> buffer(info.file_size);
    DWORD bytes_read = 0;
    EXPECT_TRUE(ReadFile(handle, buffer.data(), FileSystem::kMaxReadSize,
                         &bytes_read, nullptr));
    EXPECT_EQ(FileSystem::kMaxReadSize, bytes_read);
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

TEST_P(ReadWriteTest, Write_4GB_Boundary_Success) {
  if (!IsWindows8OrGreater()) {
    // The write tends to fail in Windows 7 test VMs, which is not reproducible
    // using a comparable VM image directly. We think this may be due to the
    // build ramdisk using too much of the available RAM.
    DOKAN_LOG_(INFO) << "Skipping 4 GB boundary successful write test.";
    return;
  }
  const wchar_t path[] = L"\\test_Write_4GB_Boundary_Success";
  FileInfo info{0};
  info.file_size = 0xffffffff - 1024;
  callbacks_->SetUpFile(path, info);
  callbacks_->SetFakeWriteSuccess(true);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_WRITE);
    std::vector<char> buffer(info.file_size);
    DWORD bytes_written = 0;
    EXPECT_TRUE(WriteFile(handle, buffer.data(), info.file_size, &bytes_written,
                          nullptr));
    EXPECT_EQ(info.file_size, bytes_written);
    fs->Unmount();
  });
}

TEST_P(ReadWriteTest, Write_4GB_Boundary_Failure) {
  // This test would originally BSOD.
  const wchar_t path[] = L"\\test_Write_4GB_Boundary_Failure";
  FileInfo info{0};
  info.file_size = 0xffffffff;
  callbacks_->SetUpFile(path, info);
  callbacks_->SetWriteResult(STATUS_INVALID_PARAMETER);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_WRITE);
    std::vector<char> buffer(info.file_size);
    DWORD bytes_written = 0;
    EXPECT_FALSE(WriteFile(handle, buffer.data(), info.file_size,
                           &bytes_written, nullptr));
    fs->Unmount();
  });
}

// Note that this does not hit the hang described in b/140938460; there is an
// unknown ingredient this test lacks that triggers the use of the kernel
// background thread to do the write.
TEST_P(ReadWriteTest, MemoryMappedWriteAndFlush) {
  const wchar_t path[] = L"\\test_MemoryMappedWriteAndFlush";
  FileInfo info{0};
  info.file_size = 262144;
  callbacks_->SetUpFile(path, info);
  RunFS([&](FileSystem* fs) {
    HANDLE handle = Open(path, GENERIC_READ | GENERIC_WRITE);
    HANDLE mapping = CreateFileMapping(handle, nullptr, PAGE_READWRITE, 0,
                                       info.file_size, nullptr);
    EXPECT_NE(mapping, INVALID_HANDLE_VALUE);
    char* data = reinterpret_cast<char*>(MapViewOfFile(mapping, FILE_MAP_WRITE,
                                                       0, 0, 131072));
    ASSERT_NE(data, nullptr);
    for (size_t i = 0; i < 131072; ++i) {
      data[i] = i % 256;
    }
    IO_STATUS_BLOCK status_block{0};
    HANDLE process = GetCurrentProcess();
    SIZE_T flush_size = 131072;
    NTSTATUS status = NtFlushVirtualMemory(process,
                                           reinterpret_cast<void**>(&data),
                                           &flush_size, &status_block);
    EXPECT_EQ(status, STATUS_SUCCESS);
    UnmapViewOfFile(data);
    CloseHandle(mapping);
    fs->Unmount();
  });
}

// TODO(drivefs-team): Add tests that require the NT API:
// - NtQueryInformationFile for GetInfo information classes that are not usable
//   with GetFileInformationByHandleEx.
// - NtWriteFile for writing to the end of the file without an explicit offset.

class SecurityTest : public FileSystemTestBase {};

TEST_P(SecurityTest, SecurityDescriptorEnforcement) {
  helper_.SetSecurityDescriptor(kAllowEveryoneSddl,
                                &options_.volume_security_descriptor,
                                &options_.volume_security_descriptor_length);
  RunFS([&] (FileSystem* fs) {
    // Make sure we can access the device normally.
    HANDLE handle = helper_.OpenFSRootDirectory();
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    CloseHandle(handle);

    // If we say we're not in Everyone, we should not be able to access the
    // device.
    RemoveSidFromThread(kEveryoneSddl);
    handle = helper_.OpenFSRootDirectory();
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
  helper_.SetSecurityDescriptor(sddl, &options_.volume_security_descriptor,
                        &options_.volume_security_descriptor_length);
  RunFS([&] (FileSystem* fs) {
    HANDLE handle = helper_.OpenFSRootDirectory();
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
  helper_.SetSecurityDescriptor(sddl, &options_.volume_security_descriptor,
                                &options_.volume_security_descriptor_length);
  RunFS([&] (FileSystem* fs) {
    HANDLE handle = helper_.OpenFSRootDirectory();
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

TEST_P(SecurityTest, GetSecurity_NoBuffer) {
  // The reason this test is worth having in addition to InsufficientBuffer is
  // that this one is more likely to hit an access violation due to something
  // erroneously taking the required size to be the reply payload size. The
  // other test is more of an off-by-one test.
  std::wstring sddl;
  sddl += kOwnerAccountOperatorsSddl;
  sddl += kGroupAdminsSddl;
  sddl += kAllowEveryoneSddl;
  helper_.SetSecurityDescriptor(sddl, &options_.volume_security_descriptor,
                                &options_.volume_security_descriptor_length);
  RunFS([&] (FileSystem* fs) {
    HANDLE handle = helper_.OpenFSRootDirectory();
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    SECURITY_INFORMATION info = DACL_SECURITY_INFORMATION |
        OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION;
    auto sd = util::MakeUniqueVarStruct<SECURITY_DESCRIPTOR>(79);
    DWORD length_needed = 0;
    bool result = GetUserObjectSecurity(handle, &info, sd.get(), 0,
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
  helper_.SetSecurityDescriptor(base_sddl, &options_.volume_security_descriptor,
                                &options_.volume_security_descriptor_length);
  std::wstring readonly_sddl;
  readonly_sddl += kOwnerAccountOperatorsSddl;
  readonly_sddl += kGroupAdminsSddl;
  readonly_sddl += kAllowEveryoneReadonlySddl;
  helper_.SetSecurityDescriptor(readonly_sddl,
                                &options_.readonly_security_descriptor,
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
        helper_.ReadSecurityDescriptorAsSddl(readonly_path, 1024, false);
    std::wstring actual_base_sddl =
        helper_.ReadSecurityDescriptorAsSddl(writable_path, 1024, false);
    EXPECT_EQ(readonly_sddl, actual_readonly_sddl);
    EXPECT_EQ(base_sddl, actual_base_sddl);
    fs->Unmount();
  });
}

TEST_P(SecurityTest, GetSecurity_NoSecurityDescriptor) {
  RunFS([&] (FileSystem* fs) {
    HANDLE handle = helper_.OpenFSRootDirectory();
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

INSTANTIATE_TEST_CASE_P(SecurityTests, SecurityTest, kParams);

class MemorySafetyTest : public FileSystemTestBase {};

TEST_P(MemorySafetyTest, GlobalReleaseBufferOverflow) {
  Device device(logger_);
  ASSERT_TRUE(device.OpenGlobalDevice());
  char buffer[800];
  memset(buffer, 0x41, sizeof(buffer));
  *(WORD*)buffer = 0x320;
  *(WORD*)(buffer + 2) = 0x31a;
  *(uint64_t*)(buffer + 776) = 0x4242424242424242;
  ASSERT_FALSE(device.Control(FSCTL_EVENT_RELEASE, buffer, sizeof(buffer)));
  ASSERT_EQ(GetLastError(), ERROR_INSUFFICIENT_BUFFER);
}

TEST_P(MemorySafetyTest, EventStartTooSmall) {
  Device device(logger_);
  ASSERT_TRUE(device.OpenGlobalDevice());
  EVENT_START buffer;
  ASSERT_FALSE(device.Control(FSCTL_EVENT_START, &buffer, sizeof(buffer) - 1));
  EXPECT_EQ(GetLastError(), ERROR_NO_SYSTEM_RESOURCES);
}

TEST_P(MemorySafetyTest, EventInfoTooSmall) {
  RunFS([&] (FileSystem* fs) {
    std::unique_ptr<Device> device = helper_.OpenDevice(fs);
    ASSERT_NE(device, nullptr);
    EVENT_INFORMATION buffer;
    EXPECT_FALSE(
        device->Control(FSCTL_EVENT_INFO, &buffer, sizeof(buffer) - 1));
    EXPECT_EQ(GetLastError(), ERROR_INSUFFICIENT_BUFFER);
    fs->Unmount();
  });
}

TEST_P(MemorySafetyTest, EventWriteTooSmall) {
  RunFS([&] (FileSystem* fs) {
    std::unique_ptr<Device> device = helper_.OpenDevice(fs);
    ASSERT_NE(device, nullptr);
    EVENT_INFORMATION buffer;
    EXPECT_FALSE(
        device->Control(FSCTL_EVENT_WRITE, &buffer, sizeof(buffer) - 1));
    EXPECT_EQ(GetLastError(), ERROR_INSUFFICIENT_BUFFER);
    fs->Unmount();
  });
}

TEST_P(MemorySafetyTest, MountdevNameTooSmall) {
  RunFS([&](FileSystem* fs) {
    std::unique_ptr<Device> device = helper_.OpenDevice(fs);
    ASSERT_NE(device, nullptr);
    MOUNTDEV_NAME buffer;
    EXPECT_FALSE(
        device->ControlWithOutputOnly(IOCTL_MOUNTDEV_QUERY_DEVICE_NAME, &buffer,
                                      FIELD_OFFSET(MOUNTDEV_NAME, Name) - 1));
    EXPECT_EQ(GetLastError(), ERROR_INSUFFICIENT_BUFFER);
    fs->Unmount();
  });
}

TEST_P(MemorySafetyTest, MountdevNameStringBoundaries) {
  RunFS([&] (FileSystem* fs) {
    std::unique_ptr<Device> device = helper_.OpenDevice(fs);
    ASSERT_NE(device, nullptr);
    MOUNTDEV_NAME buffer;
    memset(&buffer, 0, sizeof(buffer));

    // With no space for a name, it should give us the length.
    EXPECT_FALSE(device->ControlWithOutputOnly(IOCTL_MOUNTDEV_QUERY_DEVICE_NAME,
                                               &buffer));
    EXPECT_EQ(GetLastError(), ERROR_MORE_DATA);
    EXPECT_GT(buffer.NameLength, 0);

    // With inadequate space for the whole name, it should only give us the
    // length.
    std::vector<char> full_buffer(sizeof(buffer) + buffer.NameLength
                                  - sizeof(wchar_t));
    const MOUNTDEV_NAME* full_buffer_data = reinterpret_cast<MOUNTDEV_NAME*>(
        full_buffer.data());
    EXPECT_FALSE(device->ControlWithOutputOnly(IOCTL_MOUNTDEV_QUERY_DEVICE_NAME,
                                               full_buffer.data(),
                                               full_buffer.size() - 1));
    EXPECT_EQ(GetLastError(), ERROR_MORE_DATA);
    EXPECT_EQ(full_buffer_data->NameLength, buffer.NameLength);

    // With enough space, it should give us the name.
    memset(full_buffer.data(), 0, full_buffer.size());
    EXPECT_TRUE(device->ControlWithOutputOnly(IOCTL_MOUNTDEV_QUERY_DEVICE_NAME,
                                              full_buffer.data(),
                                              full_buffer.size()));
    EXPECT_EQ(full_buffer_data->NameLength, buffer.NameLength);
    std::wstring full_data_name = std::wstring(
        full_buffer_data->Name, full_buffer_data->NameLength / sizeof(wchar_t));
    ExpectValidDeviceName(full_data_name);

    // It should not use extra space.
    full_buffer.resize(full_buffer.size() + 1);
    memset(full_buffer.data(), 0, full_buffer.size());
    full_buffer[full_buffer.size() - 1] = 23;  // Arbitrary value.
    full_buffer_data = reinterpret_cast<MOUNTDEV_NAME*>(full_buffer.data());
    EXPECT_TRUE(device->ControlWithOutputOnly(IOCTL_MOUNTDEV_QUERY_DEVICE_NAME,
                                              full_buffer.data(),
                                              full_buffer.size()));
    EXPECT_EQ(full_buffer_data->NameLength, buffer.NameLength);
    full_data_name = std::wstring(
        full_buffer_data->Name, full_buffer_data->NameLength / sizeof(wchar_t));
    ExpectValidDeviceName(full_data_name);
    EXPECT_EQ(full_buffer[full_buffer.size() - 1], 23);
    fs->Unmount();
  });
}

TEST_P(MemorySafetyTest, SuggestedLinkNameTooSmall) {
  RunFS([&] (FileSystem* fs) {
    std::unique_ptr<Device> device = helper_.OpenDevice(fs);
    ASSERT_NE(device, nullptr);
    MOUNTDEV_SUGGESTED_LINK_NAME buffer;
    memset(&buffer, 0, sizeof(buffer));

    // With no space for a name, it should give us the length.
    EXPECT_FALSE(device->ControlWithOutputOnly(
        IOCTL_MOUNTDEV_QUERY_SUGGESTED_LINK_NAME, &buffer,
        FIELD_OFFSET(MOUNTDEV_SUGGESTED_LINK_NAME, Name) - 1));
    EXPECT_EQ(GetLastError(), ERROR_INSUFFICIENT_BUFFER);
    EXPECT_EQ(buffer.NameLength, 0);
    fs->Unmount();
  });
}

TEST_P(MemorySafetyTest, SuggestedLinkNameStringBoundaries) {
  RunFS([&] (FileSystem* fs) {
    std::unique_ptr<Device> device = helper_.OpenDevice(fs);
    ASSERT_NE(device, nullptr);
    MOUNTDEV_SUGGESTED_LINK_NAME buffer;
    memset(&buffer, 0, sizeof(buffer));
    const std::wstring expected_name = std::wstring(L"\\DosDevices\\")
        + mount_point_;

    // With no space for a name, it should give us the length.
    EXPECT_FALSE(device->ControlWithOutputOnly(
        IOCTL_MOUNTDEV_QUERY_SUGGESTED_LINK_NAME, &buffer));
    EXPECT_EQ(GetLastError(), ERROR_MORE_DATA);
    EXPECT_EQ(buffer.NameLength, expected_name.size() * sizeof(wchar_t));
    EXPECT_EQ(buffer.Name[0], 0);
    EXPECT_TRUE(buffer.UseOnlyIfThereAreNoOtherLinks);

    // With inadequate space for the whole name, it should only give us the
    // length.
    std::vector<char> full_buffer(sizeof(buffer) + buffer.NameLength
                                  - sizeof(wchar_t));
    const MOUNTDEV_SUGGESTED_LINK_NAME* full_buffer_data
        = reinterpret_cast<MOUNTDEV_SUGGESTED_LINK_NAME*>(full_buffer.data());
    EXPECT_FALSE(device->ControlWithOutputOnly(
        IOCTL_MOUNTDEV_QUERY_SUGGESTED_LINK_NAME, full_buffer.data(),
        full_buffer.size() - 1));
    EXPECT_EQ(GetLastError(), ERROR_MORE_DATA);
    EXPECT_EQ(full_buffer_data->NameLength, buffer.NameLength);
    EXPECT_EQ(buffer.Name[0], 0);
    EXPECT_TRUE(buffer.UseOnlyIfThereAreNoOtherLinks);

    // With enough space, it should give us the name.
    memset(full_buffer.data(), 0, full_buffer.size());
    EXPECT_TRUE(device->ControlWithOutputOnly(
        IOCTL_MOUNTDEV_QUERY_SUGGESTED_LINK_NAME, full_buffer.data(),
        full_buffer.size()));
    EXPECT_EQ(full_buffer_data->NameLength, buffer.NameLength);
    std::wstring full_data_name = std::wstring(
        full_buffer_data->Name, full_buffer_data->NameLength / sizeof(wchar_t));
    EXPECT_EQ(full_data_name, expected_name);
    EXPECT_TRUE(full_buffer_data->UseOnlyIfThereAreNoOtherLinks);

    // It should not use extra space.
    full_buffer.resize(full_buffer.size() + 1);
    memset(full_buffer.data(), 0, full_buffer.size());
    full_buffer[full_buffer.size() - 1] = 23;  // Arbitrary value.
    full_buffer_data = reinterpret_cast<MOUNTDEV_SUGGESTED_LINK_NAME*>(
        full_buffer.data());
    EXPECT_TRUE(device->ControlWithOutputOnly(
        IOCTL_MOUNTDEV_QUERY_SUGGESTED_LINK_NAME, full_buffer.data(),
        full_buffer.size()));
    EXPECT_EQ(full_buffer_data->NameLength, buffer.NameLength);
    full_data_name = std::wstring(
        full_buffer_data->Name, full_buffer_data->NameLength / sizeof(wchar_t));
    EXPECT_EQ(full_data_name, expected_name);
    EXPECT_TRUE(full_buffer_data->UseOnlyIfThereAreNoOtherLinks);
    EXPECT_EQ(full_buffer[full_buffer.size() - 1], 23);
    fs->Unmount();
  });
}

TEST_P(MemorySafetyTest, VolumeMetricsTooSmall) {
  RunFS([&](FileSystem* fs) {
    std::unique_ptr<Device> device = helper_.OpenDevice(fs);
    VOLUME_METRICS metrics;
    EXPECT_FALSE(device->ControlWithOutputOnly(FSCTL_GET_VOLUME_METRICS,
                                               &metrics, sizeof(metrics) - 1));
    EXPECT_EQ(GetLastError(), ERROR_INSUFFICIENT_BUFFER);
    fs->Unmount();
  });
}

TEST_P(MemorySafetyTest, IoctlInputBreak) {
  RunFS([&](FileSystem* fs) {
    std::unique_ptr<Device> device = helper_.OpenDevice(fs);
    // GET_IRP_BUFFER_OR_BREAK should break on this.
    char data[100];
    EXPECT_FALSE(
        device->ControlWithOutputOnly(IOCTL_STORAGE_QUERY_PROPERTY, data, 100));
    EXPECT_EQ(GetLastError(), ERROR_INSUFFICIENT_BUFFER);
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(MemorySafetyTests, MemorySafetyTest,
                        testing::Values(kCallbackSync));

class DisabledFunctionTest : public FileSystemTestBase {};

TEST_P(DisabledFunctionTest, FsctlGetVersion) {
  CheckInvalidFunction(FSCTL_GET_VERSION);
}

TEST_P(DisabledFunctionTest, FsctlEventMountpointList) {
  CheckInvalidFunction(FSCTL_EVENT_MOUNTPOINT_LIST);
}

TEST_P(DisabledFunctionTest, IoctlRedirQueryPath) {
  CheckInvalidFunction(IOCTL_REDIR_QUERY_PATH);
}

TEST_P(DisabledFunctionTest, IoctlRedirQueryPathEx) {
  CheckInvalidFunction(IOCTL_REDIR_QUERY_PATH_EX);
}

TEST_P(DisabledFunctionTest, FsctlResetTimeout) {
  CheckInvalidFunction(FSCTL_RESET_TIMEOUT);
}

TEST_P(DisabledFunctionTest, FsctlGetAccessToken) {
  CheckInvalidFunction(FSCTL_GET_ACCESS_TOKEN);
}

INSTANTIATE_TEST_CASE_P(DisabledFunctionTests, DisabledFunctionTest,
                        testing::Values(kCallbackSync));

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

INSTANTIATE_TEST_CASE_P(ReadWriteTests, ReadWriteTest, kParams);

class NotificationTest : public FileSystemTestBase {};

TEST_P(NotificationTest, NotifyCreate_File) {
  const wchar_t path[] = L"\\test_NotifyCreate_File";
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
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
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
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
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
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
    HANDLE handle = helper_.OpenDirForOverlapped(mount_point_ + path,
                                                 &overlapped);
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
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
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
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
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
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
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
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
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
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
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
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
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

TEST_P(NotificationTest, Delete_File_ViaCreateFlag_NotifyOnce) {
  const wchar_t path[] = L"\\test_Delete_File_ViaCreateFlag_NotifyOnce";
  callbacks_->SetUpFile(path);
  const Observer* observer = callbacks_->AddObserver(path);
  RunFS([&](FileSystem* fs) {
    // Open two handles, one DELETE_ON_CLOSE one normal.
    HANDLE doc_handle = CreateFile((mount_point_ + path).c_str(), GENERIC_READ,
                                   FILE_SHARE_READ | FILE_SHARE_DELETE, nullptr,
                                   CREATE_NEW, FILE_FLAG_DELETE_ON_CLOSE, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, doc_handle);
    HANDLE normal_handle =
        CreateFile((mount_point_ + path).c_str(), GENERIC_READ,
                   FILE_SHARE_READ | FILE_SHARE_DELETE, nullptr, OPEN_EXISTING,
                   FILE_ATTRIBUTE_NORMAL, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, normal_handle);
    OVERLAPPED overlapped;
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(handle, buffer, 1024, false,
                                        FILE_NOTIFY_CHANGE_FILE_NAME, nullptr,
                                        &overlapped, nullptr);
    EXPECT_TRUE(result);
    // Close the second handle and expect that we haven't yet sent a
    // notification.
    CloseHandle(normal_handle);
    DWORD bytes_read = 0;
    bool got_info =
        GetOverlappedResult(handle, &overlapped, &bytes_read, false);
    if (got_info) {
      EXPECT_GE(bytes_read, sizeof(FILE_NOTIFY_INFORMATION));
      const auto info =
          reinterpret_cast<const FILE_NOTIFY_INFORMATION*>(buffer);
      ADD_FAILURE() << "Unexpected file notification. "
                    << "Action: " << info->Action << " File: "
                    << std::wstring(info->FileName,
                                    info->FileNameLength / sizeof(wchar_t));
    }
    EXPECT_TRUE(callbacks_->file_exists(path));
    // Close the first handle and ensure that the file is deleted.
    CloseHandle(doc_handle);
    CheckNotification(handle, buffer, &overlapped, FILE_ACTION_REMOVED,
                      L"test_Delete_File_ViaCreateFlag_NotifyOnce");
    EXPECT_FALSE(callbacks_->file_exists(path));
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

// Below are variants of the above that test notifications that are
// automatically sent by the driver in response to a rename/move being done on
// the dokan FS from some app. The above ones are for when the owner of the
// FileSystem object fires an explicit notification to indicate "cloud"
// changes or similar.

TEST_P(NotificationTest, ImplicitRenameNotification_File_SameParent) {
  const wchar_t old_path[] = L"\\test_ImplicitRename_File_SameParent_old";
  const wchar_t new_path[] = L"\\test_ImplicitRename_File_SameParent_new";
  callbacks_->SetUpFile(old_path);
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, false, FILE_NOTIFY_CHANGE_FILE_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(MoveFile((mount_point_ + old_path).c_str(),
                         (mount_point_ + new_path).c_str()));
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_RENAMED_OLD_NAME, old_path + 1,
        FILE_ACTION_RENAMED_NEW_NAME, new_path + 1);
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, ImplicitRenameNotification_File_NewParent) {
  // TODO(drivefs-team): See note in the NotifyRename variant.
  const wchar_t old_path[] = L"\\test_ImplicitRename_File_NewParent1\\file";
  const wchar_t new_path[] = L"\\test_ImplicitRename_File_NewParent2\\file";
  callbacks_->SetUpFile(old_path);
  callbacks_->SetUpDir(L"\\test_ImplicitRename_File_NewParent2");
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, true, FILE_NOTIFY_CHANGE_FILE_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(MoveFile((mount_point_ + old_path).c_str(),
                         (mount_point_ + new_path).c_str()));
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_REMOVED,
        L"test_ImplicitRename_File_NewParent1\\file");
    result = ReadDirectoryChangesW(
        handle, buffer, 1024, true, FILE_NOTIFY_CHANGE_FILE_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_ADDED,
        L"test_ImplicitRename_File_NewParent2\\file");
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, ImplicitRenameNotification_Dir_SameParent) {
  const wchar_t old_path[] = L"\\test_ImplicitRename_Dir_SameParent_old";
  const wchar_t new_path[] = L"\\test_ImplicitRename_Dir_SameParent_new";
  callbacks_->SetUpDir(old_path);
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, false, FILE_NOTIFY_CHANGE_DIR_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(MoveFile((mount_point_ + old_path).c_str(),
                         (mount_point_ + new_path).c_str()));
    CheckNotification(
        handle, buffer, &overlapped, FILE_ACTION_RENAMED_OLD_NAME, old_path + 1,
        FILE_ACTION_RENAMED_NEW_NAME, new_path + 1);
    CloseHandle(overlapped.hEvent);
    fs->Unmount();
  });
}

TEST_P(NotificationTest, ImplicitRenameNotification_Dir_NewParent) {
  // TODO(drivefs-team): This case is weird in the same ways as the file
  // variant.
  const wchar_t old_path[] = L"\\test_ImplicitRename_Dir_NewParent1\\dir";
  const wchar_t new_path[] = L"\\test_ImplicitRename_Dir_NewParent2\\dir";
  callbacks_->SetUpDir(old_path);
  callbacks_->SetUpDir(L"\\test_ImplicitRename_Dir_NewParent2");
  RunFS([&](FileSystem* fs) {
    OVERLAPPED overlapped;
    HANDLE handle = helper_.OpenFSRootForOverlapped(&overlapped);
    char buffer[1024];
    bool result = ReadDirectoryChangesW(
        handle, buffer, 1024, true, FILE_NOTIFY_CHANGE_DIR_NAME, nullptr,
        &overlapped, nullptr);
    EXPECT_TRUE(result);
    EXPECT_TRUE(MoveFile((mount_point_ + old_path).c_str(),
                         (mount_point_ + new_path).c_str()));
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

INSTANTIATE_TEST_CASE_P(NotificationTests, NotificationTest, kParams);

class DeviceIOBufferTest : public FileSystemTestBase {};

TEST_P(DeviceIOBufferTest, Large_NtSetInformationFile) {
  const wchar_t file_path[] = L"\\foo.txt";
  callbacks_->SetUpFile(L"\\foo.txt", "foo");
  RunFS([&](FileSystem* fs) {
    HANDLE handle =
        CreateFile((mount_point_ + file_path).c_str(), GENERIC_ALL,
                   FILE_SHARE_READ, nullptr, OPEN_ALWAYS, 0, nullptr);
    IO_STATUS_BLOCK status_block = {};
    int buffer_size = 65536;
    void* buffer = malloc(buffer_size);
    memset(buffer, 0x00, buffer_size);
    NTSTATUS status = NtSetInformationFile(
        handle, &status_block, buffer, buffer_size, FileDispositionInformation);
    EXPECT_EQ(status, STATUS_INVALID_PARAMETER);

    std::unique_ptr<Device> device = helper_.OpenDevice(fs);
    VOLUME_METRICS metrics;
    EXPECT_TRUE(fs->GetVolumeMetrics(&metrics));
    EXPECT_GT(metrics.LargeIRPRegistrationCanceled, 0);

    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(DeviceIOBufferTests, DeviceIOBufferTest, kParams);

class DriverLogsTest : public FileSystemTestBase {
 public:
  static void GenerateActivity(const std::wstring& mount_point,
                               const std::wstring& file_path) {
    HANDLE handle =
        CreateFile((mount_point + file_path).c_str(), GENERIC_ALL,
                   FILE_SHARE_READ, nullptr, OPEN_ALWAYS, 0, nullptr);
    char buffer[4] = {0};
    DWORD bytes_read = 0;
    bool result = ReadFile(handle, buffer, 3, &bytes_read, NULL);
    EXPECT_TRUE(result);
    CloseHandle(handle);
  }

  static bool HasDriverLogs(const std::vector<std::string>& trace) {
    return std::find_if(trace.begin(), trace.end(), [](const std::string& str) {
             return str.find("[DokanDispatchCreate]") != std::string::npos;
           }) != trace.end();
  }
};

TEST_P(DriverLogsTest, DispatchDriverLogs) {
  if (!(GetParam() & kDispatchDriverLogs)) {
    return;
  }

  const wchar_t file_path[] = L"\\foo.txt";
  callbacks_->SetUpFile(file_path, "foo");
  RunFS([&](FileSystem* fs) {
    GenerateActivity(mount_point_, file_path);
    fs->Unmount();
    EXPECT_TRUE(HasDriverLogs(dynamic_cast<TestLogger*>(logger_)->GetTrace()));
  });
}

TEST_P(DriverLogsTest, MultiDispatchDriverLogs) {
  if (!(GetParam() & kDispatchDriverLogs)) {
    return;
  }

  const wchar_t file_path[] = L"\\foo.txt";
  callbacks_->SetUpFile(file_path, "foo");
  RunFS([&](FileSystem* fs) {
    uint8_t activity_count = 10;
    for (int x = 0; x < activity_count; ++x) {
      GenerateActivity(mount_point_, file_path);
    }
    fs->Unmount();
    std::vector<std::string> trace =
        dynamic_cast<TestLogger*>(logger_)->GetTrace();
    EXPECT_TRUE(
        std::count_if(trace.begin(), trace.end(), [](const std::string& str) {
          return str.find("[DokanDispatchCreate]") != std::string::npos;
        }) > activity_count);
  });
}

TEST_P(DriverLogsTest, OnlyVolumeLogsReceived) {
  if (!(GetParam() & kDispatchDriverLogs)) {
    return;
  }

  uint8_t activity_count = 10;
  std::thread t1([&] {
    FileSystemTestHelper helper1(L"Q:", GetParam());
    const wchar_t file_path[] = L"\\foo_OnlyVolumeLogsReceived.txt";
    helper1.callbacks_->SetUpFile(file_path, "foo");
    helper1.RunFS(options_, [&](FileSystem* fs) {
      for (int x = 0; x < activity_count; ++x) {
        GenerateActivity(helper1.mount_point_, file_path);
      }
      fs->Unmount();
      std::vector<std::string> trace =
          dynamic_cast<TestLogger*>(helper1.logger_.get())->GetTrace();
      EXPECT_TRUE(HasDriverLogs(trace));
      auto foo_it =
          std::find_if(trace.begin(), trace.end(), [&](const std::string& str) {
            return str.find("\\foo_OnlyVolumeLogsReceived.txt") != std::string::npos;
          });
      auto bar_it =
          std::find_if(trace.begin(), trace.end(), [&](const std::string& str) {
            return str.find("\\bar_OnlyVolumeLogsReceived.txt") != std::string::npos;
          });
      if (bar_it != trace.end()) {
        DOKAN_LOG_(ERROR) << *foo_it;
      }
      EXPECT_TRUE(foo_it != trace.end());
      EXPECT_TRUE(bar_it == trace.end());
    });
    EXPECT_TRUE(helper1.unmounted());
  });
  std::thread t2([&] {
    FileSystemTestHelper helper2(L"R:", GetParam());
    const wchar_t file_path[] = L"\\bar_OnlyVolumeLogsReceived.txt";
    helper2.callbacks_->SetUpFile(file_path, "bar");
    helper2.RunFS(options_, [&](FileSystem* fs) {
      for (int x = 0; x < activity_count; ++x) {
        GenerateActivity(helper2.mount_point_, file_path);
      }
      fs->Unmount();
      std::vector<std::string> trace =
          dynamic_cast<TestLogger*>(helper2.logger_.get())->GetTrace();
      EXPECT_TRUE(HasDriverLogs(trace));
      auto foo_it =
          std::find_if(trace.begin(), trace.end(), [&](const std::string& str) {
            return str.find("\\foo_OnlyVolumeLogsReceived.txt") != std::string::npos;
          });
      auto bar_it =
          std::find_if(trace.begin(), trace.end(), [&](const std::string& str) {
            return str.find("\\bar_OnlyVolumeLogsReceived.txt") != std::string::npos;
          });
      if (foo_it != trace.end()) {
        DOKAN_LOG_(ERROR) << *foo_it;
      }
      EXPECT_TRUE(foo_it == trace.end());
      EXPECT_TRUE(bar_it != trace.end());
    });
    EXPECT_TRUE(helper2.unmounted());
  });
  t1.join();
  t2.join();
}

INSTANTIATE_TEST_CASE_P(DriverLogsTests, DriverLogsTest, kParams);

}  // namespace test
}  // namespace dokan
