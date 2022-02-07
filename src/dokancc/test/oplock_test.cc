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

namespace dokan {
namespace test {
namespace {

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

}  // namespace

class OplockTest : public FileSystemTestBase {
 public:
  OplockTest() {
    memset(&oplock_output_, 0, sizeof(oplock_output_));
    memset(&oplock_overlapped_, 0, sizeof(oplock_overlapped_));
    oplock_overlapped_.hEvent =
        CreateEvent(nullptr, true, false, L"Oplocktest");
  }

  ~OplockTest() {
    if (acknowledge_thread_ && acknowledge_thread_->joinable()) {
      acknowledge_thread_->join();
    }
    CloseHandle(oplock_overlapped_.hEvent);
  }

 protected:
  bool AcquireOplock(HANDLE file_handle, DWORD requested_level) {
    REQUEST_OPLOCK_INPUT_BUFFER input;
    memset(&input, 0, sizeof(input));
    input.StructureVersion = REQUEST_OPLOCK_CURRENT_VERSION;
    input.StructureLength = sizeof(REQUEST_OPLOCK_INPUT_BUFFER);
    input.RequestedOplockLevel = requested_level;
    input.Flags = REQUEST_OPLOCK_INPUT_FLAG_REQUEST;
    bytes_returned_ = 0;
    bool result = DeviceIoControl(
      file_handle, FSCTL_REQUEST_OPLOCK, &input, sizeof(input),
      &oplock_output_, sizeof(oplock_output_), &bytes_returned_,
      &oplock_overlapped_);
    DWORD error = GetLastError();
    if (error != ERROR_IO_PENDING) {
      DOKAN_LOG_(INFO) << "Oplock acquisition failed; error " << error;
    }
    file_handle_with_oplock_ = file_handle;
    return !result && error == ERROR_IO_PENDING;
  }

  // Starts a background thread that will acknowledge an oplock break that
  // requires acknowledgement (e.g. an RWH oplock).
  void AcknowledgeByClosingWhenOplockBreaks() {
    assert(!acknowledge_thread_);
    assert(file_handle_with_oplock_ != INVALID_HANDLE_VALUE);
    acknowledge_thread_.reset(new std::thread([this] {
      DOKAN_LOG_(TRACE) << "Waiting Oplock to break before closing.";
      EXPECT_EQ(WAIT_OBJECT_0,
                WaitForSingleObject(oplock_overlapped_.hEvent, 35000));
      DOKAN_LOG_(TRACE) << "Oplock break received. Closing the file now.";
      EXPECT_TRUE(CloseHandle(file_handle_with_oplock_));
      oplock_broken_ = true;
    }));
  }

  void WriteToFile(const std::wstring& file_name) {
    HANDLE write_handle = CreateFile(
        file_name.c_str(), GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
        nullptr, OPEN_EXISTING, 0, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, write_handle);
    DWORD bytes_written = 0;
    WriteFile(write_handle, "foobar", 6, &bytes_written, nullptr);
    EXPECT_TRUE(CloseHandle(write_handle));
  }

  void ExpectOplockBroken() {
    if (!oplock_broken_) {
      EXPECT_EQ(WAIT_OBJECT_0,
                WaitForSingleObject(oplock_overlapped_.hEvent, 35000));
    }
  }

  HANDLE OpenRequiringOplock(const std::wstring& relative_path,
                             ACCESS_MASK access_mask,
                             ULONG share_access) {
    // Note: On Windows 8 and above, we could use CreateFile2 for this, but
    // using NtCreateFile ensures compatibility with Windows 7.
    HANDLE handle = INVALID_HANDLE_VALUE;
    OBJECT_ATTRIBUTES attributes;
    IO_STATUS_BLOCK iosb;
    memset(&attributes, 0, sizeof(OBJECT_ATTRIBUTES));
    memset(&iosb, 0, sizeof(IO_STATUS_BLOCK));
    UNICODE_STRING name;
    const std::wstring full_name = L"\\??\\" + mount_point_ + relative_path;
    RtlInitUnicodeString(&name, full_name.c_str());
    attributes.Length = sizeof(OBJECT_ATTRIBUTES);
    attributes.ObjectName = &name;
    NTSTATUS status = NtCreateFile(
        &handle, access_mask, &attributes, &iosb, /*AllocationSize=*/nullptr,
        /*FileAttributes=*/0, share_access, FILE_OPEN,
        FILE_OPEN_REQUIRING_OPLOCK, /*EaBuffer=*/nullptr, /*EaLength=*/0);
    return status == STATUS_SUCCESS ? handle : INVALID_HANDLE_VALUE;
  }

  HANDLE file_handle_with_oplock_;
  REQUEST_OPLOCK_OUTPUT_BUFFER oplock_output_;
  OVERLAPPED oplock_overlapped_;
  DWORD bytes_returned_;
  std::atomic<bool> oplock_broken_{false};
  std::unique_ptr<std::thread> acknowledge_thread_;
};

TEST_P(OplockTest, AcquireAndBreakROplock) {
  const wchar_t path[] = L"\\test_AcquireAndBreakROplock.txt";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    const std::wstring file_name = mount_point_ + path;
    HANDLE handle = CreateFile(
        file_name.c_str(), GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr, OPEN_EXISTING,
        FILE_FLAG_OVERLAPPED, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_TRUE(AcquireOplock(handle, OPLOCK_LEVEL_CACHE_READ));
    WriteToFile(file_name);
    ExpectOplockBroken();
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(OplockTest, AcquireAndBreakRHOplock) {
  const wchar_t path[] = L"\\test_AcquireAndBreakRHOplock.txt";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    const std::wstring file_name = mount_point_ + path;
    HANDLE handle = CreateFile(
        file_name.c_str(), GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr, OPEN_EXISTING,
        FILE_FLAG_OVERLAPPED, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_TRUE(AcquireOplock(
        handle, OPLOCK_LEVEL_CACHE_READ | OPLOCK_LEVEL_CACHE_HANDLE));
    WriteToFile(file_name);
    ExpectOplockBroken();
    CloseHandle(handle);
    fs->Unmount();
  });
}

TEST_P(OplockTest, AcquireAndBreakRWOplock) {
  const wchar_t path[] = L"\\test_AcquireAndBreakRWOplock.txt";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    const std::wstring file_name = mount_point_ + path;
    HANDLE handle = CreateFile(
        file_name.c_str(), GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr, OPEN_EXISTING,
        FILE_FLAG_OVERLAPPED, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_TRUE(AcquireOplock(
        handle, OPLOCK_LEVEL_CACHE_READ | OPLOCK_LEVEL_CACHE_WRITE));
    AcknowledgeByClosingWhenOplockBreaks();
    WriteToFile(file_name);
    ExpectOplockBroken();
    fs->Unmount();
  });
}

TEST_P(OplockTest, AcquireAndBreakRWHOplock) {
  const wchar_t path[] = L"\\test_AcquireAndBreakRWHOplock.txt";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    const std::wstring file_name = mount_point_ + path;
    HANDLE handle = CreateFile(
        file_name.c_str(), GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr, OPEN_EXISTING,
        FILE_FLAG_OVERLAPPED, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_TRUE(AcquireOplock(
        handle,
        OPLOCK_LEVEL_CACHE_READ | OPLOCK_LEVEL_CACHE_WRITE |
        OPLOCK_LEVEL_CACHE_HANDLE));
    AcknowledgeByClosingWhenOplockBreaks();
    WriteToFile(file_name);
    ExpectOplockBroken();
    fs->Unmount();
  });
}

TEST_P(OplockTest, DISABLED_TriggerForcedRWHBreakInCreate) {
  // Disabled: This test pass on our env but fails on Kokoro gcp_presubmit.
  // This could be a race condition that need to be investiagted.
  // TODO(adrienj)
  const wchar_t path[] = L"\\test_TrigerForcedRWHBreakInCreate.txt";
  callbacks_->SetUpFile(path);
  RunFS([&](FileSystem* fs) {
    const std::wstring file_name = mount_point_ + path;
    HANDLE handle = CreateFile(
        file_name.c_str(), GENERIC_READ,
        FILE_SHARE_READ, nullptr, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
    EXPECT_NE(INVALID_HANDLE_VALUE, handle);
    EXPECT_TRUE(AcquireOplock(
        handle,
        OPLOCK_LEVEL_CACHE_READ | OPLOCK_LEVEL_CACHE_WRITE |
        OPLOCK_LEVEL_CACHE_HANDLE));
    AcknowledgeByClosingWhenOplockBreaks();
    HANDLE write_handle = CreateFile(
        file_name.c_str(), GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
        nullptr, OPEN_EXISTING,
        0, 0);
    // This succeeds because we acknowledge the oplock break by closing the
    // handle that doesn't have the required share access.
    EXPECT_NE(INVALID_HANDLE_VALUE, write_handle);
    ExpectOplockBroken();
    fs->Unmount();
  });
}

TEST_P(OplockTest, OpenRequiringOplockRace) {
  // The original dokan driver had a race condition where you could submit an
  // atomic open+oplock request for a nonexistent file, make it exist before
  // the FCB got torn down, and submit a 2nd atomic oplock request, at which
  // point the oplock struct in the FCB would have a dangling pointer to the
  // FILE_OBJECT for the first request, and the 2nd request would de-reference
  // that to check the oplock key in the FILE_OBJECT extension. Often, the
  // freed FILE_OBJECT would have been repurposed and still valid enough to not
  // cause any problem, but occasionally it would BSOD. This test does it enough
  // times to reliably hit the BSOD without the fix.
  const wchar_t path[] = L"\\test_OpenRequiringOplockRace";
  RunFS([&](FileSystem* fs) {
    std::vector<std::thread> threads;
    for (int i = 0; i < 4; i++) {
      threads.emplace_back([&] {
        for (int j = 0; j < 1000; j++) {
          HANDLE handle = OpenRequiringOplock(
              path, GENERIC_READ,
              FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE);
          if (handle != INVALID_HANDLE_VALUE) {
            CloseHandle(handle);
          }
          // Occasionally make the file exist or stop existing.
          switch (rand() % 5) {
            case 0: callbacks_->SetUpFile(path, "foobar");
                    break;
            case 1: callbacks_->ForgetFile(path);
                    break;
            default: break;
          }
        }
      });
    }
    for (std::thread& thread : threads) {
      thread.join();
    }
    fs->Unmount();
  });
}

TEST_P(OplockTest, OpenRequiringOplockTimeout) {
  // This test is for the timeout variation of the bug tested by
  // OpenRequiringOplockRace. Here, the first request times out rather than
  // targeting a nonexistent file. Due to DOKAN_CHECK_INTERVAL, it can take
  // a while to hit it this way without the fix, and this test would only
  // sporadically hit it due to the low number of iterations.
  const wchar_t path[] = L"\\test_OpenRequiringOplockTimeout";
  options_.request_timeout_millis = 200;
  RunFS([&](FileSystem* fs) {
    fs->WaitUntilPostStartDone();
    for (int i = 0; i < 3; i++) {
      callbacks_->SetUpFile(path, "foobar");
      callbacks_->SetCreateDelay(path, 22000);
      HANDLE handle = OpenRequiringOplock(
          path, GENERIC_READ,
          FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE);
      if (handle != INVALID_HANDLE_VALUE) {
        CloseHandle(handle);
      }
      callbacks_->SetCreateDelay(path, 0);
      handle = OpenRequiringOplock(
          path, GENERIC_READ,
          FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE);
      if (handle != INVALID_HANDLE_VALUE) {
        CloseHandle(handle);
      }
    }
    fs->Unmount();
  });
}

TEST_P(OplockTest, DISABLED_OpenRequiringOplockCancellation) {
  const wchar_t path[] = L"\\test_OpenRequiringOplockCancellation.txt";
  callbacks_->SetUpFile(path, "It was a dark and stormy night");
  RunFS([&](FileSystem* fs) {
    const std::wstring file_name = mount_point_ + path;
    fs->WaitUntilPostStartDone();
    // When this triggered a BSOD, it could take up to 3 or so iterations to
    // have the use-after-free of the canceled IRP actually matter.
    for (int i = 0; i < 10; i++) {
      callbacks_->SetCreateDelay(path, 300);
      DWORD thread_id = GetCurrentThreadId();
      std::thread canceler([&] {
        Sleep(75);
        HANDLE thread_handle = OpenThread(THREAD_ALL_ACCESS, false, thread_id);
        CancelSynchronousIo(thread_handle);
        CloseHandle(thread_handle);
      });
      // We don't assert that this fails on the off chance that it occasionally
      // succeeds due to a timing fluke.
      HANDLE canceled_handle = OpenRequiringOplock(
          path, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE);
      canceler.join();
      if (canceled_handle != INVALID_HANDLE_VALUE) {
        CloseHandle(canceled_handle);
      }
      // Write to the file, which would BSOD on the corrupt left over atomic
      // oplock request if the driver did not clean it up in the cancellation.
      callbacks_->SetCreateDelay(path, 0);
      HANDLE handle = CreateFile(file_name.c_str(), GENERIC_WRITE,
                                 FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                                 OPEN_EXISTING, 0, 0);
      EXPECT_NE(INVALID_HANDLE_VALUE, handle);
      DWORD bytes_written = 0;
      EXPECT_TRUE(WriteFile(handle, "foo", 3, &bytes_written, 0));
      CloseHandle(handle);
    }
    fs->Unmount();
  });
}

INSTANTIATE_TEST_CASE_P(OplockTests, OplockTest, kParams);

}  // namespace test
}  // namespace dokan
