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

#include "change_handler.h"

#include <cassert>

#include "file_info.h"

namespace dokan {

void ChangeHandler::ChangeSize(
    const LARGE_INTEGER& size,
    const FileHandle* handle,
    const util::StatusCallback& callback) {
  callbacks_->ChangeSize(handle, size.QuadPart, callback);
}

void ChangeHandler::ChangeDisposition(
    const FILE_DISPOSITION_INFORMATION& info,
    FileHandle* handle,
    EVENT_INFORMATION* reply,
    const util::StatusCallback& callback) {
  if (info.DeleteFile == handle->should_delete_on_cleanup()) {
    callback(STATUS_SUCCESS);
    return;
  }
  if (!info.DeleteFile) {
    handle->set_delete_on_cleanup(false);
    reply->Operation.Delete.DeleteOnClose = false;
    callback(STATUS_SUCCESS);
    return;
  }
  auto raw_info = new FileInfo{0};
  callbacks_->GetInfo(handle, raw_info, [=](NTSTATUS status) {
    std::unique_ptr<FileInfo> info(raw_info);
    if (status == STATUS_SUCCESS &&
        info->file_attributes & FILE_ATTRIBUTE_READONLY) {
      callback(STATUS_CANNOT_DELETE);
      return;
    }

    callbacks_->GetDeleteApproval(handle, [=](NTSTATUS status) {
      if (status == STATUS_SUCCESS) {
        handle->set_delete_on_cleanup(true);
        // We think this is superfluous for dokancc (here and above), and the
        // tests pass without it, but since it exists, let's make it accurate.
        reply->Operation.Delete.DeleteOnClose = true;
      }
      callback(status);
    });
  });
}

void ChangeHandler::ChangeBasicInfo(
    const FILE_BASIC_INFORMATION& info,
    const FileHandle* handle,
    const util::StatusCallback& callback) {
  auto times = new FileTimes{0};
  util::LargeIntegerToTime(info.CreationTime, &times->creation_time);
  util::LargeIntegerToTime(info.LastAccessTime, &times->last_access_time);
  util::LargeIntegerToTime(info.LastWriteTime, &times->last_write_time);
  callbacks_->ChangeAttributesAndTimes(handle, info.FileAttributes, *times,
    [=](NTSTATUS status) {
      delete times;
      callback(status);
    });
}

void ChangeHandler::ChangeName(
    const DOKAN_RENAME_INFORMATION& info,
    FileHandle* handle,
    EVENT_INFORMATION* reply,
    const util::StatusCallback& callback) {
  const std::wstring new_full_path(info.FileName,
                                   info.FileNameLength / sizeof(wchar_t));
  callbacks_->Move(
      handle, new_full_path, info.ReplaceIfExists,
      [=, &info](NTSTATUS status) {
        if (status == STATUS_SUCCESS) {
          handle->set_path(new_full_path);
          memcpy(reply->Buffer, info.FileName, info.FileNameLength);
        }
        callback(status);
      });
}

void ChangeHandler::Change(const EVENT_CONTEXT* request,
                           FileHandle* handle,
                           ULONG info_class,
                           util::UniqueVarStructPtr<EVENT_INFORMATION> reply,
                           const ChangeHandler::ReplyFn& reply_fn) {
  // The driver handles position changes.
  assert(info_class != FilePositionInformation);

  // This is the only intentionally unimplemented info class.
  if (info_class == FileLinkInformation) {
    reply_fn(STATUS_NOT_IMPLEMENTED, std::move(reply));
    return;
  }

  EVENT_INFORMATION* raw_reply = reply.release();
  auto callback = [=](NTSTATUS status) {
    reply_fn(status, std::move(util::MakeUniqueVarStruct(raw_reply)));
  };
  const char* const info = reinterpret_cast<const char*>(request) +
      request->Operation.SetFile.BufferOffset;
  switch (info_class) {
    case FileAllocationInformation:
      // We don't believe there's a point in distinguishing this from logical
      // file size, because (1) a user-mode FS implementation is unlikely to be
      // affected by not bothering to somehow preallocate space; (2) We don't
      // have a way to report it back on subsequent queries either, nor does the
      // old C code.
      ChangeSize(
          reinterpret_cast<const FILE_ALLOCATION_INFORMATION*>(info)
              ->AllocationSize,
          handle, callback);
      break;
    case FileBasicInformation:
      ChangeBasicInfo(
          *reinterpret_cast<const FILE_BASIC_INFORMATION*>(info), handle,
          callback);
      break;
    case FileDispositionInformation:
      ChangeDisposition(
          *reinterpret_cast<const FILE_DISPOSITION_INFORMATION*>(info), handle,
          raw_reply, callback);
      break;
    case FileEndOfFileInformation:
      ChangeSize(
          reinterpret_cast<const FILE_END_OF_FILE_INFORMATION*>(info)
              ->EndOfFile,
          handle, callback);
      break;
    case FileRenameInformation:
      ChangeName(
          *reinterpret_cast<const DOKAN_RENAME_INFORMATION*>(info), handle,
          raw_reply, callback);
      break;
    case FileValidDataLengthInformation:
      ChangeSize(
          reinterpret_cast<const FILE_VALID_DATA_LENGTH_INFORMATION*>(info)
              ->ValidDataLength,
          handle, callback);
      break;
    default:
      // There are a bunch of intentionally unsupported info types.
      callback(STATUS_INVALID_PARAMETER);
  }
}

}  // namespace dokan
