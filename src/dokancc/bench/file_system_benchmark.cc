/*
  Dokan : user-mode file system library for Windows

  Copyright (C) 2020 Google, Inc.
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

#include <algorithm>
#include <numeric>
#include <random>

#include "file_system_test_base.h"

namespace dokan {
namespace test {

class NullLogger : public Logger {
 public:
  bool IsEnabledForLevel(LogLevel log_level, const LogSite& site) override {
    return false;
  }

  void Log(LogLevel log_level, const LogSite& site, std::string_view msg){};
};

static const uint64_t kListFilesCount = 100;
static const uint64_t kFileSize = 4096 * 1024;

static const std::wstring kMountPoint = L"G:";
static const std::wstring kBenchInternalDirPath = L"\\bench_dir";
static const std::wstring kBenchInternalFilePath =
    kBenchInternalDirPath + L"\\file_";
static const std::wstring kBenchInternalReadIOFilePath = L"\\read_io_file";
static const std::wstring kBenchInternalWriteIOFilePath = L"\\write_io_file";
static const std::wstring kBenchFilePath = kMountPoint + kBenchInternalFilePath;
static const std::wstring kBenchDirePath = kMountPoint + kBenchInternalDirPath;
static const std::wstring kBenchReadIOFilePath =
    kMountPoint + kBenchInternalReadIOFilePath;
static const std::wstring kBenchWriteIOFilePath =
    kMountPoint + kBenchInternalWriteIOFilePath;

class BenchmarkTest : public ::testing::TestWithParam<int> {
 public:
  void SetUp() override {
    uint64_t param = kCallbackSync | kSuppressFileNameInEventContext |
                     kAllowRequestBatching | kFcbGarbageCollection;
    fs_helper_ = std::make_unique<FileSystemTestHelper>(
        kMountPoint, param, std::make_unique<NullLogger>());
  }

 protected:
  void RunFS(const std::function<void()>& test_logic) {
    StartupOptions options;
    fs_helper_->RunFS(options, [&](FileSystem* fs) {
      StartTimer();
      test_logic();
      StopTimer();
      fs->Unmount();
    });
  }

  void StartTimer() {
    // We use a custom timer because the tests count the SetUp & TearDown in the
    // calculation of the elapsed time.
    begin_time_ = std::chrono::steady_clock::now();
  }

  void StopTimer() {
    if (timer_stopped) {
      return;
    }
    auto end_time = std::chrono::steady_clock::now();
    auto time_elapse = std::chrono::duration_cast<std::chrono::milliseconds>(
                           end_time - begin_time_)
                           .count();
    std::cout << "time_elapsed_ms: " << time_elapse << std::endl;
    testing::Test::RecordProperty("time_elapsed_ms", time_elapse);
    timer_stopped = true;
  }

 protected:
  std::unique_ptr<FileSystemTestHelper> fs_helper_;

 private:
  std::chrono::steady_clock::time_point begin_time_;
  bool timer_stopped = false;
};

// The benchmark create file system is init with this file tree before each
// test: \bench_dir\file_[0-file_count] \bench_dir_[0-file_count]
class BenchmarkCreate : public BenchmarkTest {
 public:
  void SetUp() override {
    BenchmarkTest::SetUp();
    const uint64_t file_count = GetParam();
    // Allows file create during the test.
    fs_helper_->callbacks_->SetAllowCreateFiles(true);
    // Setup files for test using existing files.
    std::vector<FileNameAndInfo> entries(file_count);
    for (uint64_t i = 0; i < file_count; ++i) {
      entries[i].name = L"file_" + std::to_wstring(i);
      entries[i].info.file_attributes = FILE_ATTRIBUTE_NORMAL;
    }
    fs_helper_->callbacks_->SetUpDir(kBenchInternalDirPath, entries);
    // Setup empty directories for RemoveDirectory
    for (uint64_t i = 0; i < file_count; ++i) {
      fs_helper_->callbacks_->SetUpDir(
          kBenchInternalDirPath + std::to_wstring(i), {});
    }
  }
};

// Create new files and close right after.
TEST_P(BenchmarkCreate, NewFile) {
  HANDLE handle;
  const auto base_file_name = kBenchFilePath + L"_new_";

  RunFS([&] {
    for (uint64_t i = 0; i < GetParam(); ++i) {
      const auto file_name = base_file_name + std::to_wstring(i);
      handle = CreateFile(file_name.c_str(), GENERIC_READ,
                          FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                          CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
      EXPECT_NE(INVALID_HANDLE_VALUE, handle);
      EXPECT_TRUE(CloseHandle(handle));
    }
  });
}

// Create new directories.
TEST_P(BenchmarkCreate, NewDir) {
  const auto base_dir_name = kBenchDirePath + L"_new_";
  RunFS([&] {
    for (uint64_t i = 0; i < GetParam(); ++i) {
      const auto dir_name = base_dir_name + std::to_wstring(i);
      EXPECT_TRUE(CreateDirectory(dir_name.c_str(), nullptr));
    }
  });
}

// Open all existing files.
TEST_P(BenchmarkCreate, Open) {
  HANDLE handle;

  RunFS([&] {
    for (uint64_t i = 0; i < GetParam(); ++i) {
      const auto file_name = kBenchFilePath + std::to_wstring(i);
      handle = CreateFile(file_name.c_str(), GENERIC_READ,
                          FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                          OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      EXPECT_NE(INVALID_HANDLE_VALUE, handle);
      EXPECT_TRUE(CloseHandle(handle));
    }
  });
}

// Open repeatedly the same existing file.
TEST_P(BenchmarkCreate, RepeatedOpen) {
  HANDLE handle;
  const auto file_name = kBenchFilePath + L"0";

  RunFS([&] {
    for (uint64_t i = 0; i < GetParam(); ++i) {
      handle = CreateFile(file_name.c_str(), GENERIC_READ,
                          FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                          OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      EXPECT_NE(INVALID_HANDLE_VALUE, handle);
      EXPECT_TRUE(CloseHandle(handle));
    }
  });
}

// Overwrite all existing files.
TEST_P(BenchmarkCreate, Overwrite) {
  HANDLE handle;

  RunFS([&] {
    for (uint64_t i = 0; i < GetParam(); ++i) {
      const auto file_name = kBenchFilePath + std::to_wstring(i);
      handle = CreateFile(file_name.c_str(), GENERIC_READ,
                          FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                          CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      EXPECT_NE(INVALID_HANDLE_VALUE, handle);
      EXPECT_TRUE(CloseHandle(handle));
    }
  });
}

// Open all existing files, keep the handles open and randomly closed them.
TEST_P(BenchmarkCreate, KeepAllOpenAndCloseRandomly) {
  std::vector<HANDLE> handles;
  HANDLE handle;

  RunFS([&] {
    for (uint64_t i = 0; i < GetParam(); ++i) {
      const auto file_name = kBenchFilePath + std::to_wstring(i);
      handle = CreateFile(file_name.c_str(), GENERIC_READ,
                          FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                          OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      EXPECT_NE(INVALID_HANDLE_VALUE, handle);
      handles.push_back(handle);
    }

    // Random close handles
    std::shuffle(handles.begin(), handles.end(),
                 std::mt19937{std::random_device{}()});
    for (auto&& handle : handles) {
      EXPECT_TRUE(CloseHandle(handle));
    }
  });
}

// Open all existing files, keep the handles open and open a new handle
// sequentially on the same files. Here we focus on the existing Fcb Lookup
// happening when reopening an active file.
TEST_P(BenchmarkCreate, KeepAllOpenAndReOpenActiveFcb) {
  std::vector<HANDLE> handles;
  HANDLE handle;

  RunFS([&] {
    for (uint64_t i = 0; i < GetParam(); ++i) {
      const auto file_name = kBenchFilePath + std::to_wstring(i);
      handle = CreateFile(file_name.c_str(), GENERIC_READ,
                          FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                          OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      EXPECT_NE(INVALID_HANDLE_VALUE, handle);
      handles.push_back(handle);
    }

    for (uint64_t i = 0; i < GetParam(); ++i) {
      const auto file_name = kBenchFilePath + std::to_wstring(i);
      HANDLE second_handle = CreateFile(
          file_name.c_str(), GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
          nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      EXPECT_NE(INVALID_HANDLE_VALUE, second_handle);
      CloseHandle(second_handle);
    }

    for (auto&& handle : handles) {
      EXPECT_TRUE(CloseHandle(handle));
    }
  });
}

INSTANTIATE_TEST_CASE_P(BenchmarkCreates, BenchmarkCreate,
                        testing::Values(1000, 2000, 3000, 4000, 5000));

using BenchmarkList = BenchmarkCreate;

// List all files from a directory.
TEST_P(BenchmarkList, AllFiles) {
  WIN32_FIND_DATAW find_data;
  HANDLE handle;

  RunFS([&] {
    for (uint64_t x = 0; x < GetParam(); ++x) {
      handle = FindFirstFile((kBenchDirePath + L"\\*").c_str(), &find_data);
      EXPECT_NE(INVALID_HANDLE_VALUE, handle);
      while (FindNextFile(handle, &find_data))
        ;
      EXPECT_TRUE(FindClose(handle));
    }
  });
}

// List a specific file in a directory.
TEST_P(BenchmarkList, ExactFile) {
  WIN32_FIND_DATAW find_data;
  HANDLE handle;

  RunFS([&] {
    for (uint64_t i = 0; i < GetParam(); ++i) {
      const auto file_name = kBenchFilePath + std::to_wstring(i);
      handle = FindFirstFile(file_name.c_str(), &find_data);
      EXPECT_NE(INVALID_HANDLE_VALUE, handle);
      while (FindNextFile(handle, &find_data))
        ;
      EXPECT_TRUE(FindClose(handle));
    }
  });
}

INSTANTIATE_TEST_CASE_P(BenchmarkLists, BenchmarkList,
                        testing::Values(1000, 2000, 3000, 4000, 5000));

using BenchmarkFileAttribute = BenchmarkCreate;

// Get file attributes of an existing file.
TEST_P(BenchmarkFileAttribute, GetAttributes) {
  const auto param = GetParam();
  DWORD file_attributes;

  RunFS([&] {
    for (uint64_t x = 0; x < kListFilesCount; ++x) {
      for (uint64_t i = 0; i < GetParam(); ++i) {
        const auto file_name = kBenchFilePath + std::to_wstring(i);
        file_attributes = GetFileAttributes(file_name.c_str());
        EXPECT_NE(INVALID_FILE_ATTRIBUTES, file_attributes);
      }
    }
  });
}

// Randomly get file attributes of an existing file.
TEST_P(BenchmarkFileAttribute, GetAttributesRand) {
  const auto param = GetParam();
  DWORD file_attributes;

  // Shuffle file access
  std::vector<uint64_t> file_numbers(GetParam());
  std::iota(file_numbers.begin(), file_numbers.end(), 0);
  std::shuffle(file_numbers.begin(), file_numbers.end(),
               std::mt19937{std::random_device{}()});

  RunFS([&] {
    for (uint64_t x = 0; x < kListFilesCount; ++x) {
      for (auto&& file_number : file_numbers) {
        const auto file_name = kBenchFilePath + std::to_wstring(file_number);
        file_attributes = GetFileAttributes(file_name.c_str());
        EXPECT_NE(INVALID_FILE_ATTRIBUTES, file_attributes);
      }
    }
  });
}

// Set file attributes of an existing file.
TEST_P(BenchmarkFileAttribute, SetAttributes) {
  const auto param = GetParam();
  DWORD file_attributes;

  RunFS([&] {
    for (uint64_t x = 0; x < kListFilesCount; ++x) {
      for (uint64_t i = 0; i < GetParam(); ++i) {
        const auto file_name = kBenchFilePath + std::to_wstring(i);
        EXPECT_NE(SetFileAttributes(file_name.c_str(), FILE_ATTRIBUTE_HIDDEN),
                  0);
      }
    }
  });
}

// Randomly set file attributes of an existing file.
TEST_P(BenchmarkFileAttribute, SetAttributesRand) {
  const auto param = GetParam();
  DWORD file_attributes;

  // Shuffle file access
  std::vector<uint64_t> file_numbers(GetParam());
  std::iota(file_numbers.begin(), file_numbers.end(), 0);
  std::shuffle(file_numbers.begin(), file_numbers.end(),
               std::mt19937{std::random_device{}()});

  RunFS([&] {
    for (uint64_t x = 0; x < kListFilesCount; ++x) {
      for (auto&& file_number : file_numbers) {
        const auto file_name = kBenchFilePath + std::to_wstring(file_number);
        EXPECT_NE(SetFileAttributes(file_name.c_str(), FILE_ATTRIBUTE_HIDDEN),
                  0);
      }
    }
  });
}

INSTANTIATE_TEST_CASE_P(BenchmarkFileAttributes, BenchmarkFileAttribute,
                        testing::Values(100, 200, 300, 400, 500));

using BenchmarkDelete = BenchmarkCreate;

// Delete existing file.
TEST_P(BenchmarkDelete, File) {
  RunFS([&] {
    for (uint64_t i = 0; i < GetParam(); ++i) {
      const auto file_name = kBenchFilePath + std::to_wstring(i);
      EXPECT_TRUE(DeleteFile(file_name.c_str()));
    }
  });
}

// Randomly delete existing file.
TEST_P(BenchmarkDelete, FileRand) {
  std::vector<uint64_t> file_numbers(GetParam());
  std::iota(file_numbers.begin(), file_numbers.end(), 0);
  std::shuffle(file_numbers.begin(), file_numbers.end(),
               std::mt19937{std::random_device{}()});

  RunFS([&] {
    for (auto&& file_number : file_numbers) {
      const auto file_name = kBenchFilePath + std::to_wstring(file_number);
      EXPECT_TRUE(DeleteFile(file_name.c_str()));
    }
  });
}

// Delete directories.
TEST_P(BenchmarkDelete, Directory) {
  const auto param = GetParam();

  RunFS([&] {
    for (uint64_t i = 0; i < GetParam(); ++i) {
      const auto dir_name = kBenchDirePath + std::to_wstring(i);
      EXPECT_TRUE(RemoveDirectory(dir_name.c_str()));
    }
  });
}

INSTANTIATE_TEST_CASE_P(BenchmarkDeletes, BenchmarkDelete,
                        testing::Values(1000, 2000, 3000, 4000, 5000));

// The benchmark file system is init with this file tree before each test:
// \read_io_file
// \write_io_file
class BenchmarkIO : public BenchmarkTest {
 public:
  void SetUp() override {
    BenchmarkTest::SetUp();
    // Setup read / write files
    fs_helper_->callbacks_->SetUpFile(kBenchInternalReadIOFilePath,
                                      std::vector<char>(kFileSize));
    fs_helper_->callbacks_->SetUpFile(kBenchInternalWriteIOFilePath,
                                      std::vector<char>());

    GetSystemInfo(&system_info_);
    sector_size_ = fs_helper_->fs_->startup_options().allocation_unit_size;
  }

  // Read or write entirely a file repeatedly using a specific |buffer_size|.
  void DoIoTest(const std::wstring& file_name, ULONG flags_and_attributes,
                DWORD buffer_size, bool read) {
    RunFS([&] {
      HANDLE handle = CreateFile(
          file_name.c_str(), GENERIC_READ | GENERIC_WRITE, 0, nullptr,
          OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | flags_and_attributes, 0);
      EXPECT_NE(INVALID_HANDLE_VALUE, handle);

      StartTimer();
      std::vector<char> buffer(buffer_size);
      for (uint64_t i = 0; i < GetParam(); ++i) {
        EXPECT_EQ(SetFilePointer(handle, 0, 0, FILE_BEGIN), 0);
        for (uint64_t x = 0; x < kFileSize / buffer_size; ++x) {
          bool result;
          DWORD number_of_bytes;
          if (read) {
            result =
                ReadFile(handle, &buffer[0], buffer_size, &number_of_bytes, 0);
          } else {
            result =
                WriteFile(handle, &buffer[0], buffer_size, &number_of_bytes, 0);
          }
          EXPECT_EQ(buffer_size, number_of_bytes);
          EXPECT_TRUE(result);
        }
        if (!read) {
          EXPECT_TRUE(FlushFileBuffers(handle));
        }
      }
      StopTimer();

      EXPECT_TRUE(CloseHandle(handle));
    });
  }

  // Read or write entirely a memory mapped file repeatedly using a specific
  // |buffer_size|.
  void DoMmapTest(const std::wstring& file_name, DWORD buffer_size, bool read) {
    RunFS([&] {
      HANDLE handle =
          CreateFile(file_name.c_str(), GENERIC_READ | GENERIC_WRITE, 0,
                     nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      EXPECT_NE(INVALID_HANDLE_VALUE, handle);

      HANDLE mmap_handle =
          CreateFileMapping(handle, 0, PAGE_READWRITE, 0, kFileSize, 0);
      EXPECT_NE(nullptr, mmap_handle);

      char* buffer = reinterpret_cast<char*>(
          MapViewOfFile(mmap_handle, FILE_MAP_ALL_ACCESS, 0, 0, 0));
      EXPECT_NE(nullptr, buffer);

      StartTimer();
      for (uint64_t i = 0; i < GetParam(); ++i) {
        for (uint64_t buffer_index = 0; buffer_index < kFileSize / buffer_size;
             ++buffer_index) {
          if (read) {
            uint64_t data = 0;
            for (uint64_t p = 0; p < buffer_size; ++p) {
              data += buffer[buffer_index * buffer_size + p];
            }
            EXPECT_EQ(0, data);
          } else {
            for (uint64_t p = 0; p < buffer_size; ++p) {
              buffer[buffer_index * buffer_size + p] = p % 256;
            }
          }
        }
        FlushViewOfFile(buffer, 0);
      }
      StopTimer();

      EXPECT_TRUE(UnmapViewOfFile(buffer));
      EXPECT_TRUE(CloseHandle(mmap_handle));
      EXPECT_TRUE(CloseHandle(handle));
    });
  }

 protected:
  SYSTEM_INFO system_info_;
  uint32_t sector_size_;
};

// Single Sector size read & write with cache or without cache.
// Note: We are actually very slow to |file_size| with |sector_size| that why we
// use a smaller number of iterations compared to paged and mmap test.
using BenchmarkIOSector = BenchmarkIO;

TEST_P(BenchmarkIOSector, ReadCc) {
  DoIoTest(kBenchReadIOFilePath, 0, sector_size_, true);
}

TEST_P(BenchmarkIOSector, WriteCc) {
  DoIoTest(kBenchWriteIOFilePath, 0, sector_size_, false);
}

TEST_P(BenchmarkIOSector, ReadNoCc) {
  DoIoTest(kBenchReadIOFilePath, FILE_FLAG_NO_BUFFERING, sector_size_, true);
}

TEST_P(BenchmarkIOSector, WriteNoCc) {
  DoIoTest(kBenchWriteIOFilePath, FILE_FLAG_NO_BUFFERING, sector_size_, false);
}

INSTANTIATE_TEST_CASE_P(BenchmarkIOSectors, BenchmarkIOSector,
                        testing::Values(10, 20, 30, 40, 50));

// Single Page size read & write with cache or without cache.
using BenchmarkIOPage = BenchmarkIO;

TEST_P(BenchmarkIOPage, ReadCc) {
  DoIoTest(kBenchReadIOFilePath, 0, system_info_.dwPageSize, true);
}

TEST_P(BenchmarkIOPage, WriteCc) {
  DoIoTest(kBenchWriteIOFilePath, 0, system_info_.dwPageSize, false);
}

TEST_P(BenchmarkIOPage, ReadNoCc) {
  DoIoTest(kBenchReadIOFilePath, FILE_FLAG_NO_BUFFERING,
           system_info_.dwPageSize, true);
}

TEST_P(BenchmarkIOPage, WriteNoCc) {
  DoIoTest(kBenchWriteIOFilePath, FILE_FLAG_NO_BUFFERING,
           system_info_.dwPageSize, false);
}

// Multi Page size read & write with cache or without cache.
// Note: Write size will reach the EVENT_CONTEXT_MAX_SIZE and will create a
// larger write buffer request.
TEST_P(BenchmarkIOPage, ReadMultiCc) {
  DoIoTest(kBenchReadIOFilePath, 0, system_info_.dwPageSize * 16, true);
}

TEST_P(BenchmarkIOPage, WriteMultiCc) {
  DoIoTest(kBenchWriteIOFilePath, 0, system_info_.dwPageSize * 16, false);
}

TEST_P(BenchmarkIOPage, ReadMultiNoCc) {
  DoIoTest(kBenchReadIOFilePath, FILE_FLAG_NO_BUFFERING,
           system_info_.dwPageSize * 16, true);
}

TEST_P(BenchmarkIOPage, WriteMultiNoCc) {
  DoIoTest(kBenchWriteIOFilePath, FILE_FLAG_NO_BUFFERING,
           system_info_.dwPageSize * 16, false);
}

INSTANTIATE_TEST_CASE_P(BenchmarkIOPages, BenchmarkIOPage,
                        testing::Values(100, 200, 300, 400, 500));

// Memory mapped read and write by pagging size
using BenchmarkIOMmap = BenchmarkIO;

TEST_P(BenchmarkIOMmap, Read) {
  DoMmapTest(kBenchReadIOFilePath, system_info_.dwPageSize, true);
}

TEST_P(BenchmarkIOMmap, Write) {
  DoMmapTest(kBenchReadIOFilePath, system_info_.dwPageSize, false);
}

INSTANTIATE_TEST_CASE_P(BenchmarkIOMmaps, BenchmarkIOMmap,
                        testing::Values(100, 200, 300, 400, 500));
}  // namespace test
}  // namespace dokan
