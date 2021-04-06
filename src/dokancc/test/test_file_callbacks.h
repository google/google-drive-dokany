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

#ifndef DOKAN_TEST_TEST_FILE_CALLBACKS_H_
#define DOKAN_TEST_TEST_FILE_CALLBACKS_H_

#include <algorithm>
#include <atomic>
#include <map>
#include <memory>
#include <mutex>
#include <vector>

#include "file_callbacks.h"

namespace dokan {
namespace test {

// An Observer can be associated with either a path or a path when opened with
// specific parameters.
struct Observer {
  // These fields are set at set-up time.
  std::wstring path;
  bool match_by_path_only = false;
  ACCESS_MASK desired_access = 0;
  uint32_t file_attributes = 0;
  uint32_t share_access = 0;
  uint32_t create_disposition = 0;
  uint32_t create_options = 0;

  // These fields are set by file system activity and verified by the test as
  // desired.
  std::atomic<bool> create_invoked{false};
  std::atomic<NTSTATUS> create_result{-1};
  std::atomic<bool> cleanup_invoked{false};
  std::atomic<bool> close_invoked{false};
  std::atomic<bool> flush_invoked{false};
  std::atomic<bool> get_delete_approval_invoked{false};

  bool Matches(const std::wstring& path_value,
               ACCESS_MASK desired_access_value,
               uint32_t file_attributes_value,
               uint32_t share_access_value,
               uint32_t create_disposition_value,
               uint32_t create_options_value) const {
    return path == path_value && match_by_path_only || (
           desired_access == desired_access_value &&
           file_attributes == file_attributes_value &&
           create_disposition == create_disposition_value &&
           create_options == create_options_value);
  }
};

// The context type used in a FileHandle in tests.
struct Context {
  Context() : observer(new Observer()) {}

  std::shared_ptr<Observer> observer;
};

static std::shared_ptr<Observer> GetObserver(const FileHandle* handle) {
  assert(handle->context());
  return reinterpret_cast<Context*>(handle->context())->observer;
}

// Fake file callbacks for testing that use in-memory maps of paths to file info
// and contents. To configure the starting scenario for a test, use SetUpFile
// and related functions on the main test thread before running the file system.
// File system operations will then generally mutate the structure you have set
// up. It is expected that the test does not mutate the fake file system
// contents in a multi-threaded fashion.
class TestFileCallbacks : public FileCallbacks {
 public:
  using CallbackInvoker = std::function<void(const std::function<void()>)>;
  using StatusCallbackInvoker =
      std::function<void(const util::StatusCallback&, NTSTATUS)>;

  TestFileCallbacks(const CallbackInvoker& callback_invoker)
      : callback_invoker_(
            [=](const util::StatusCallback& callback, NTSTATUS status) {
              callback_invoker(std::bind(callback, status));
            }) {
    FileInfo root_info;
    root_info.file_attributes = FILE_ATTRIBUTE_DIRECTORY;
    SetUpFile(L"\\", root_info);
  }

  void Create(FileHandle* handle,
              ACCESS_MASK desired_access,
              uint32_t file_attributes,
              uint32_t share_access,
              uint32_t create_disposition,
              uint32_t create_options,
              const util::StatusCallback& callback) override {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    size_t delay = create_delay_[handle->path()];
    if (delay) {
      Sleep(delay);
    }
    if (!handle_matched_ && handle_matcher_ && handle_matcher_(handle)) {
      handle_matched_ = true;
    }
    util::StatusCallback callback_wrapper = [=](NTSTATUS status) {
      Context* context = nullptr;
      if (status == STATUS_SUCCESS) {
        context = new Context();
        handle->set_context(context);
      }
      for (auto& observer : observers_) {
        if (!observer->create_invoked && observer->Matches(
                MapKey(handle), desired_access, file_attributes, share_access,
                create_disposition, create_options)) {
          observer->create_invoked = true;
          observer->create_result = status;
          if (status == STATUS_SUCCESS) {
            context->observer = observer;
          }
          break;
        }
      }
      callback(status);
    };
    if (readonly_files_.count(handle->path())) {
      handle->set_use_readonly_security_descriptor(true);
    }
    if (handle->is_root() && create_disposition == FILE_SUPERSEDE) {
      callback_invoker_(callback_wrapper, STATUS_ACCESS_DENIED);
      return;
    }
    const auto key = MapKey(handle);
    auto it = file_info_.find(key);
    if (it == file_info_.end() && allow_create_files &&
        (create_disposition == FILE_SUPERSEDE ||
         create_disposition == FILE_CREATE ||
         create_disposition == FILE_OVERWRITE_IF)) {
      SetUpFile(key);
      it = file_info_.find(key);
    }
    if (it != file_info_.end()) {
      if (it->second.file_attributes & FILE_ATTRIBUTE_DIRECTORY) {
        handle->set_directory(true);
      }
      callback_invoker_(callback_wrapper, STATUS_SUCCESS);
      return;
    }
    callback_invoker_(callback_wrapper, STATUS_OBJECT_NAME_NOT_FOUND);
  }

  void GetInfo(const FileHandle* handle,
               FileInfo* info,
               const util::StatusCallback& callback) override {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    const auto it = file_info_.find(MapKey(handle));
    if (it != file_info_.end()) {
      *info = it->second;
      callback_invoker_(callback, STATUS_SUCCESS);
      return;
    }
    callback_invoker_(callback, STATUS_OBJECT_NAME_NOT_FOUND);
  }

  void Read(const FileHandle* handle,
            int64_t offset,
            uint32_t length,
            uint32_t* actual_length_read,
            char* buffer,
            const util::StatusCallback& callback) override {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    if (read_result_ != STATUS_SUCCESS) {
      callback_invoker_(callback, read_result_);
      return;
    }
    if (fake_read_success_) {
      *actual_length_read = length;
      callback_invoker_(callback, STATUS_SUCCESS);
      return;
    }
    *actual_length_read = 0;
    const auto it = file_content_.find(MapKey(handle));
    if (it != file_content_.end() && offset <= it->second.size()) {
      *actual_length_read =
          std::min(length, static_cast<uint32_t>(it->second.size() - offset));
      memcpy(buffer, it->second.data() + offset, *actual_length_read);
    }
    callback_invoker_(callback, STATUS_SUCCESS);
  }

  void Write(const FileHandle* handle,
             int64_t offset,
             uint32_t length,
             const char* buffer,
             const util::StatusCallback& callback) override {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    if (write_result_ != STATUS_SUCCESS) {
      callback_invoker_(callback, write_result_);
      return;
    }
    if (fake_write_success_) {
      callback_invoker_(callback, STATUS_SUCCESS);
      return;
    }
    std::vector<char>& content = file_content_[MapKey(handle)];
    FileInfo& info = file_info_[MapKey(handle)];
    if (offset + length > content.size()) {
      content.resize(offset + length);
      info.file_size = content.size();
    }
    memcpy(content.data() + offset, buffer, length);
    callback_invoker_(callback, STATUS_SUCCESS);
  }

  void Flush(const FileHandle* handle,
             const util::StatusCallback& callback) override {
    GetObserver(handle)->flush_invoked = true;
    callback_invoker_(callback, flush_result_);
  }

  void ChangeSize(const FileHandle* handle,
                  uint64_t size,
                  const util::StatusCallback& callback) override {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    FileInfo& info = file_info_[MapKey(handle)];
    info.file_size = size;
    // Avoid resizing content to very high values when testing ChangeSize.
    if (file_content_.count(MapKey(handle))) {
      std::vector<char>& content = file_content_[MapKey(handle)];
      content.resize(size);
    }
    callback_invoker_(callback, STATUS_SUCCESS);
  }

  void ChangeAttributesAndTimes(const FileHandle* handle,
                                uint32_t attributes,
                                const FileTimes& times,
                                const util::StatusCallback& callback) override {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    FileInfo& info = file_info_[MapKey(handle)];
    info.file_attributes = attributes;
    if (FileTimes::is_set(times.creation_time)) {
      info.creation_time = times.creation_time;
    }
    if (FileTimes::is_set(times.last_access_time)) {
      info.last_access_time = times.last_access_time;
    }
    if (FileTimes::is_set(times.last_write_time)) {
      info.last_write_time = times.last_write_time;
    }
    callback_invoker_(callback, STATUS_SUCCESS);
  }

  void GetDeleteApproval(const FileHandle* handle,
                         const util::StatusCallback& callback) override {
    GetObserver(handle)->get_delete_approval_invoked = true;
    callback_invoker_(callback, get_delete_approval_result_);
  }

  void Move(const FileHandle* handle,
            const std::wstring& new_full_path,
            bool replace_if_existing,
            const util::StatusCallback& callback) override {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    if (!replace_if_existing && file_info_.count(new_full_path)) {
      callback_invoker_(callback, STATUS_OBJECT_NAME_COLLISION);
      return;
    }
    std::vector<char>& source_content = file_content_[MapKey(handle)];
    FileInfo& source_info = file_info_[MapKey(handle)];
    std::vector<char>& dest_content = file_content_[new_full_path];
    FileInfo& dest_info = file_info_[new_full_path];
    dest_content = std::move(source_content);
    dest_info = source_info;
    ForgetFile(handle->path());
    callback_invoker_(callback, STATUS_SUCCESS);
  }

  void FindFiles(const FileHandle* handle,
                 const dokan::FindFilesCallback& output,
                 const util::StatusCallback& callback) override {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    std::vector<FileNameAndInfo> out_entries;
    out_entries.push_back(CreateCurrentDirEntry());
    if (!handle->is_root()) {
      out_entries.push_back(CreateParentDirEntry());
    }
    for (const auto& entry : file_info_) {
      if (entry.first.size() > 1 &&
          util::StripLastPathComponent(entry.first) == handle->path()) {
        FileNameAndInfo out_entry;
        out_entry.name = entry.first.substr(entry.first.rfind(L'\\') + 1);
        out_entry.info = entry.second;
        out_entries.push_back(out_entry);
      }
    }
    output(out_entries);
    callback_invoker_(callback, STATUS_SUCCESS);
  }

  void Cleanup(const FileHandle* handle,
               const util::StatusCallback& callback) override {
    GetObserver(handle)->cleanup_invoked = true;
    if (handle->should_delete_on_cleanup()) {
      ForgetFile(handle->path());
    }
    callback_invoker_(callback, STATUS_SUCCESS);
  }

  void Close(FileHandle* handle) override {
    assert(handle->context());
    GetObserver(handle)->close_invoked = true;
    delete reinterpret_cast<Context*>(handle->context());
  }

  void SetCreateDelay(const std::wstring& path, size_t delay_ms) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    create_delay_[path] = delay_ms;
  }

  void SetAllowCreateFiles(bool allow) {
    allow_create_files = allow;
  }

  void SetUpFile(const std::wstring& path, const FileInfo& info = {}) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    file_info_[path] = info;
  }

  void SetUpFile(const std::wstring& path, const std::string& content) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    std::vector<char>& stored_content = file_content_[path];
    FileInfo& stored_info = file_info_[path];
    stored_content.resize(content.size());
    memcpy(stored_content.data(), content.c_str(), content.size());
    stored_info.file_size = content.size();
  }

  void SetUpFile(const std::wstring& path, const std::vector<char>& content) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    FileInfo& stored_info = file_info_[path];
    file_content_[path] = content;
    stored_info.file_size = content.size();
  }

  void SetHandleMatcher(std::function<bool(const FileHandle*)> matcher) {
    handle_matcher_ = matcher;
  }

  bool CheckHandleMatcher() {
    assert(handle_matcher_);
    bool result = handle_matched_;
    handle_matched_ = false;
    return result;
  }

  void ForgetFile(const std::wstring& path) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    file_content_.erase(path);
    file_info_.erase(path);
  }

  void SetUpDir(const std::wstring& path,
                const std::vector<FileNameAndInfo>& entries = {}) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    FileInfo info;
    info.file_attributes = FILE_ATTRIBUTE_DIRECTORY;
    file_info_[path] = info;
    for (const FileNameAndInfo& entry : entries) {
      SetUpFile(path + std::wstring(L"\\") + entry.name, entry.info);
    }
  }

  void SetUseReadonlySecurityDescriptor(const std::wstring& path, bool value) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    if (value) {
      readonly_files_.insert(path);
    } else {
      readonly_files_.erase(path);
    }
  }

  std::string file_content(const std::wstring& path) const {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    const auto it = file_content_.find(path);
    assert(it != file_content_.end());
    std::vector<char> str = it->second;
    str.push_back(0);
    return std::string(str.data());
  }

  FileInfo file_info(const std::wstring& path) const {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    const auto it = file_info_.find(path);
    assert(it != file_info_.end());
    return it->second;
  }

  bool file_exists(const std::wstring& path) const {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    return file_info_.count(path) != 0;
  }

  void SetFakeReadSuccess(bool value) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    fake_read_success_ = value;
  }

  void SetFakeWriteSuccess(bool value) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    fake_write_success_ = value;
  }

  void SetReadResult(NTSTATUS value) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    read_result_ = value;
  }

  void SetWriteResult(NTSTATUS value) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    write_result_ = value;
  }

  void SetFlushResult(NTSTATUS value) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    flush_result_ = value;
  }

  void SetGetDeleteApprovalResult(NTSTATUS value) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    get_delete_approval_result_ = value;
  }

  // Adds an observer for all uses of the given path. This should be done on the
  // main test thread before starting the file system, and the observer should
  // not be modified by the test code after the file system starts.
  Observer* AddObserver(const std::wstring& path) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    auto observer = std::make_shared<Observer>();
    Observer* raw_observer = observer.get();
    observers_.push_back(std::move(observer));
    raw_observer->path = path;
    raw_observer->match_by_path_only = true;
    return raw_observer;
  }

  // Use this overload when trying to match a specific handle opened by the
  // test and not ones opened by filter drivers. A spurious open by a filter
  // driver is virtually guaranteed not to use the exact same parameters, and
  // if it does, the properties being tested probably apply.
  Observer* AddObserver(const std::wstring& path,
                        ACCESS_MASK desired_access,
                        uint32_t file_attributes,
                        uint32_t share_access,
                        uint32_t create_disposition,
                        uint32_t create_options) {
    std::lock_guard<std::recursive_mutex> lock(mutex_);
    Observer* observer = AddObserver(path);
    observer->match_by_path_only = false;
    observer->desired_access = desired_access;
    observer->file_attributes = file_attributes;
    observer->share_access = share_access;
    observer->create_disposition = create_disposition;
    observer->create_options = create_options;
    return observer;
  }

 private:
  std::wstring MapKey(const FileHandle* handle) {
    std::wstring key = handle->path();
    if (handle->has_alternate_stream_name()) {
      key += L":";
      key += handle->alternate_stream_name();
    }
    return key;
  }

  FileNameAndInfo CreateCurrentDirEntry() {
    FileNameAndInfo entry;
    entry.name = L".";
    entry.info = {0};
    entry.info.file_attributes = FILE_ATTRIBUTE_DIRECTORY;
    return entry;
  }

  FileNameAndInfo CreateParentDirEntry() {
    FileNameAndInfo entry;
    entry.name = L"..";
    entry.info = {0};
    entry.info.file_attributes = FILE_ATTRIBUTE_DIRECTORY;
    return entry;
  }

  mutable std::recursive_mutex mutex_;
  StatusCallbackInvoker callback_invoker_;
  std::map<std::wstring, FileInfo> file_info_;
  std::map<std::wstring, std::vector<char>> file_content_;
  std::map<std::wstring, size_t> create_delay_;
  std::vector<std::shared_ptr<Observer>> observers_;
  std::set<std::wstring> readonly_files_;
  std::atomic<bool> fake_read_success_{false};
  std::atomic<bool> fake_write_success_{false};
  std::atomic<NTSTATUS> read_result_{STATUS_SUCCESS};
  std::atomic<NTSTATUS> write_result_{STATUS_SUCCESS};
  std::atomic<NTSTATUS> flush_result_{STATUS_SUCCESS};
  std::atomic<NTSTATUS> get_delete_approval_result_{STATUS_SUCCESS};
  std::function<bool(const FileHandle*)> handle_matcher_;
  bool handle_matched_{false};
  bool allow_create_files{false};
};

}  // namespace test
}  // namespace dokan

#endif DOKAN_TEST_TEST_FILE_CALLBACKS_H_
