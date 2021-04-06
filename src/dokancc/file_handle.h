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

#ifndef DOKAN_FILE_HANDLE_H_
#define DOKAN_FILE_HANDLE_H_

#include <windows.h>

#include <cassert>
#include <string>
#include <memory>
#include <vector>

#include "api.h"
#include "file_info.h"

namespace dokan {

inline void ValidateSharingFlags(DWORD flags) {
  static const DWORD kAllSharingFlags =
      FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE;
  assert((flags & ~kAllSharingFlags) == 0);
}

// A FileHandle represents an open file within a particular process, with some
// state owned by the dokan library and some state (wrapped in a context object)
// that is owned by the library's consumer. The lifetime of a handle is managed
// by the dokan library, which expects the consumer to set the context object in
// the Create file system callback and destroy it in the Close callback. Note
// that the "particular process" may be the kernel itself, i.e. the System
// process, but a handle is not used by more than one process.
class FileHandle {
 public:
  DOKANCC_API FileHandle(const std::wstring& path, ULONG process_id,
                         DWORD desired_access, DWORD share_access,
                         bool directory);

  // Force the destructor to exist in the DLL and not the caller's binary when
  // the DLL is being used; otherwise the implicit destruction of the member
  // strings might use a mismatched allocator and crash. While FileHandles are
  // not normally created or destroyed by the caller of the library, in a test
  // they can be.
  DOKANCC_API ~FileHandle();

  // Returns the path relative to the file system root, without any alternate
  // stream name.
  const std::wstring& path() const {
    return path_;
  }

  // Returns the name of the alternate stream referenced by this handle.
  const std::wstring& alternate_stream_name() const {
    return alternate_stream_name_;
  }

  // Returns whether this handle references an alternate stream as opposed to
  // an actual file or directory.
  bool has_alternate_stream_name() const {
    return !alternate_stream_name_.empty();
  }

  // Whether this handle points to the root directory of the file system.
  bool is_root() const {
    return path_.size() == 1;
  }

  // Returns the ID of the process that owns the handle.
  ULONG process_id() const {
    return process_id_;
  }

  // Returns true if the handle was opened with only read access.
  bool has_readonly_desired_access() const {
    static const DWORD kReadMask = STANDARD_RIGHTS_READ | FILE_READ_DATA |
                                   FILE_READ_ATTRIBUTES | FILE_READ_EA |
                                   SYNCHRONIZE;
    return (desired_access_ & ~kReadMask) == 0;
  }

  // Returns true if the handle was opened granting the given FILE_SHARE_XXX
  // permissions.
  bool allows_sharing(DWORD flags) const {
    ValidateSharingFlags(flags);
    return (share_access_ & flags) == flags;
  }

  // Used by the app that owns the file system to associate an arbitrary object
  // with the handle during the Create callback.
  void set_context(void* context) {
    context_ = context;
  }

  // Returns the arbitrary object that the app that owns the file system has
  // associated with this handle.
  void* context() const {
    return context_;
  }

  // Whether the handle points to a directory as opposed to a file.
  bool directory() const {
    return directory_;
  }

  // Changes whether the handle points to a directory. This is for use by the
  // Create callback. The value of the flag when the Create callback is invoked
  // is only valid if a new path is being created. For existing paths, it is
  // only a hint that may or may not have been provided by the caller. The
  // Create callback must set it to the actual value.
  void set_directory(bool value) {
    directory_ = value;
  }

  // Returns whether to use the read-only security descriptor in StartupOptions
  // for this file. If not, then the volume security descriptor is used.
  bool use_readonly_security_descriptor() const {
    return use_readonly_security_descriptor_;
  }

  // Sets whether to use the read-only security descriptor in StartupOptions for
  // this file. This is for use by the Create callback. Using the read-only
  // security descriptor makes Explorer suppress options that don't make sense,
  // like right-click->New->*. However, other apps and the kernel generally do
  // not check for this. By default, the regular volume security descriptor is
  // used.
  void set_use_readonly_security_descriptor(bool value) {
    use_readonly_security_descriptor_ = value;
  }

  // The number of remaining references to this handle in user space. The
  // FileSystem object increments this while relevant requests are outstanding.
  int64_t reference_count() const {
    return reference_count_;
  }

  // Whether the file should be deleted when the Cleanup callback is called with
  // this handle. All deletions of files and directories occur by the FileSystem
  // setting this flag in response to a call like ::DeleteFile, and the
  // FileCallbacks object acting on it during Cleanup.
  bool should_delete_on_cleanup() const {
    return delete_on_cleanup_;
  }

  // This can be used by tests for callback implementations. In production use,
  // only the dokan library should flip this flag.
  void testonly_set_delete_on_cleanup(bool value) {
    delete_on_cleanup_ = value;
  }

 private:
  friend class FileSystem;
  friend class FindHandler;
  friend class ChangeHandler;

  // These are called by the FileSystem object.

  void AddReference() {
    ++reference_count_;
  }

  int64_t RemoveReference() {
    return --reference_count_;
  }

  void set_delete_on_cleanup(bool value) {
    delete_on_cleanup_ = value;
  }

  void set_path(const std::wstring& path) {
    path_ = path;
  }

  // FindHandler uses this (only on directory handles used with find requests)
  // to store the list from the last search, since the kernel may retrieve the
  // list in parts using multiple requests. There is an assumption even in the
  // original code that the caching of the list does not need to be protected
  // against conflicting, concurrently open queries.
  std::vector<FileNameAndInfo>* file_list() {
    if (!file_list_) {
      file_list_.reset(new std::vector<FileNameAndInfo>());
    }
    return file_list_.get();
  }

  std::wstring path_;
  std::wstring alternate_stream_name_;
  const ULONG process_id_;
  const DWORD desired_access_;
  const DWORD share_access_;
  bool directory_;
  bool use_readonly_security_descriptor_ = false;
  void* context_ = nullptr;
  std::unique_ptr<std::vector<FileNameAndInfo>> file_list_;
  int64_t reference_count_ = 1;
  bool delete_on_cleanup_ = false;
};

}  // namespace dokan

#endif // DOKAN_FILE_HANDLE_H_
