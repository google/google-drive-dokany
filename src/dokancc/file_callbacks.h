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

#ifndef DOKAN_FILE_CALLBACKS_H_
#define DOKAN_FILE_CALLBACKS_H_

#include <ntstatus.h>
#include <windows.h>

#include <vector>

#include "file_handle.h"
#include "file_info.h"
#include "util.h"

namespace dokan {

class FileSystem;

constexpr int32_t kEndOfFileWriteOffset = -1;

using FindFilesCallback =
      std::function<void(const std::vector<FileNameAndInfo>&)>;

// A FileCallbacks object provides the user-mode implementation of file system
// operations that target specific files. Each function must invoke the callback
// that is its last parameter, when the work completes or fails.
class __declspec(dllexport) FileCallbacks {
 public:
  virtual ~FileCallbacks() {}

  // Creates or opens a file or directory. The input flags are modeled after
  // NtCreateFile. If this function succeeds, it should set the context in the
  // given handle before invoking the callback. If it fails, it must not set the
  // context. All other functions receive a handle with the context that was
  // previously set by this function. If a new path is being created, then the
  // directory flag in the given handle indicates whether a directory should be
  // created. If an existing path is being opened, this function must set the
  // directory flag to the actual value.
  virtual void Create(FileHandle* handle,
                      ACCESS_MASK desired_access,
                      uint32_t file_attributes,
                      uint32_t share_access,
                      uint32_t create_disposition,
                      uint32_t create_options,
                      const util::StatusCallback& callback) = 0;

  // Gets information about an open file. If this function succeeds, it should
  // set all the fields in the output struct before invoking the callback. If it
  // fails, the output struct is ignored.
  virtual void GetInfo(const FileHandle* handle,
                       FileInfo* output,
                       const util::StatusCallback& callback) = 0;

  // Reads length bytes starting at the given byte offset in the file. If the
  // range is partly or entirely beyond the end of the file, then the bytes
  // in the range that are before the end of file should be read. The content
  // read should be placed in the buffer starting at the beginning (i.e. no
  // offset is applied to the buffer). The actual_length_read should be
  // set to the number of bytes that were read. The status should be
  // STATUS_SUCCESS, unless there was a problem other than the requested range
  // starting or extending beyond the end of the file.
  virtual void Read(const FileHandle* handle,
                    int64_t offset,
                    uint32_t length,
                    uint32_t* actual_length_read,
                    char* buffer,
                    const util::StatusCallback& callback) = 0;

  // Writes length bytes from the given buffer to the given byte offset in the
  // file. If the offset is kEndOfFileWriteOffset, then the offset should be
  // ignored and the content should be written to the end of the file. The write
  // is expected to entirely succeed or fail.
  virtual void Write(const FileHandle* handle,
                     int64_t offset,
                     uint32_t length,
                     const char* buffer,
                     const util::StatusCallback& callback) = 0;

  // Forces persistence of any modifications that have been made to the given
  // file (e.g. via the Write function). It should be possible to immediately
  // power off the machine when the status callback is replied to, without
  // losing the modifications. Note though that this is never guaranteed to be
  // invoked after a write, and even Cleanup/Close are never called in the event
  // of an abrupt unmount. Flush can be invoked directly by apps (e.g. via
  // ::FlushFileBuffers), or by the kernel or other drivers (e.g. via
  // ::CcFlushCache).
  virtual void Flush(const FileHandle* handle,
                     const util::StatusCallback& callback) = 0;

  // Lists all the files in the given directory. If this function succeeds, it
  // should call the output callback once with all the entries found. If it
  // fails, it should not call the output callback. In either case, it should
  // conclude its work by calling the status callback. This function must
  // produce "." and ".." entries in output, or just ".", if the handle
  // represents the root directory.
  virtual void FindFiles(const FileHandle* dir_handle,
                         const FindFilesCallback& output,
                         const util::StatusCallback& callback) = 0;

  // Adjusts the size of a non-directory file up or down. If this function
  // succeeds, then calls to GetInfo and FindFiles, after the callback reports
  // completion, should return this size.
  virtual void ChangeSize(const FileHandle* handle,
                          uint64_t size,
                          const util::StatusCallback& callback) = 0;

  // Changes a file's attributes and zero or more of its timestamps. This should
  // use FileTimes::is_set to determine which times need to be set.
  virtual void ChangeAttributesAndTimes(
      const FileHandle* handle,
      uint32_t attributes,
      const FileTimes& times,
      const util::StatusCallback& callback) = 0;

  // Determines whether should_delete_on_cleanup() is allowed to be true for the
  // given non-read-only file or directory. This is invoked when the flag is
  // about to be set due to a call like ::DeleteFile or ::RemoveDirectory. If it
  // does not reply with STATUS_SUCCESS, then that call fails and the flag does
  // not get set. If it is a nonempty directory, the result should be
  // STATUS_DIRECTORY_NOT_EMPTY. Failing for any other reason is unexpected by
  // Windows Explorer and various apps, and may cause bad behavior, but at least
  // the file will not get deleted. The actual deletion should be done in the
  // Cleanup function, if the flag is ultimately set when that function receives
  // the handle. Success of GetDeleteApproval does not guarantee that the flag
  // will be set when Cleanup is called, since the caller can later change its
  // mind and unset the flag.
  virtual void GetDeleteApproval(const FileHandle* handle,
                                 const util::StatusCallback& callback) = 0;

  // Renames or moves a file or directory, given the new full path including the
  // name. If the replace_if_existing flag is false, and there is already a file
  // or directory at new_full_path, then the result must be
  // STATUS_OBJECT_NAME_COLLISION. For various incorrect error statuses, like
  // STATUS_OBJECT_NAME_EXISTS, ::MoveFile would return true to the calling app
  // despite the error.
  virtual void Move(const FileHandle* handle,
                    const std::wstring& new_full_path,
                    bool replace_if_existing,
                    const util::StatusCallback& callback) = 0;

  // Invoked when the last HANDLE to a file is being closed. This should delete
  // the file if it is marked for delete-on-close according to the handle. The
  // file may still be accessed by the kernel (e.g. to flush caches) after this
  // and before Close is called.
  virtual void Cleanup(const FileHandle* handle,
                       const util::StatusCallback& callback) = 0;

  // Invoked if the kernel and dokan library reference count for a file both
  // drop to 0 while the file system is mounted, i.e. the FileHandle is not at
  // all being used anymore. This function is not guaranteed to ever be invoked
  // in a clean unmount scenario, because the kernel may decide to invoke it too
  // late. This function should synchronously dispose of the context object
  // associated with the handle. On returning from this function, the handle
  // pointer becomes invalid. This function must succeed in cleaning up the
  // resources, so there is no result.
  virtual void Close(FileHandle* handle) = 0;
};

}  // namespace dokan

#endif // DOKAN_FILE_CALLBACKS_H_