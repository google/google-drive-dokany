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

#ifndef DOKAN_FIND_HANDLER_H_
#define DOKAN_FIND_HANDLER_H_

#include <ntstatus.h>
#include <windows.h>

#include <functional>
#include <memory>

#include "file_callbacks.h"
#include "file_handle.h"
#include "file_info_handler.h"
#include "kernel_defs.h"
#include "logger.h"
#include "public.h"
#include "util.h"

namespace dokan {

// A FileSystem delegates directory search requests to a FindHandler.
class FindHandler {
 public:
  FindHandler(FileCallbacks* callbacks,
              Logger* logger,
              FileInfoHandler* file_info_handler)
      : callbacks_(callbacks),
        logger_(logger),
        file_info_handler_(file_info_handler) {}

  // Signature of FileSystem::CompleteFindFiles
  using ReplyFn = std::function<void (
      NTSTATUS,
      ULONG used_buffer_size,
      ULONG next_request_start_index,
      util::UniqueVarStructPtr<EVENT_INFORMATION> reply)>;

  // Finds files matching the given pattern in the directory represented by the
  // given handle. Asynchronously populates the given reply and then invokes the
  // reply_fn. start_file_index is the zero-based index among matching files to
  // start with. If single_entry is true, then at most one entry is returned.
  // Otherwise, all the ones at or beyond start_file_index are all returned, up
  // to the number that can fit in the reply buffer. The
  // next_request_start_index that this function passes to reply_fn is the
  // start_file_index that should be used to resume the query (which may be one
  // beyond the max valid index, if the end has been reached).
  void FindFiles(EVENT_CONTEXT* request,
                 FileHandle* handle,
                 const std::wstring& pattern,
                 ULONG info_class,
                 ULONG start_file_index,
                 bool single_entry,
                 util::UniqueVarStructPtr<EVENT_INFORMATION> reply,
                 const ReplyFn& reply_fn);

 private:
  // Handles the output of FileCallbacks::FindFiles, populating the last 3
  // parameters with the relevant directory entries and info if successful. If
  // the callback or subsequent processing fails, the return value is an
  // unsuccessful status, and the output parameters are untouched.
  NTSTATUS HandleFindFilesReply(FileHandle* handle,
                                const std::wstring& pattern,
                                ULONG info_class,
                                ULONG start_file_index,
                                bool single_entry,
                                NTSTATUS status,
                                EVENT_INFORMATION* reply,
                                ULONG* used_size,
                                ULONG* next_request_start_index);

  // Populates a find reply and info, given a handle with an already populated
  // file_list that contains only files matching the current request pattern,
  // if any.
  NTSTATUS FindFilesWithPopulatedList(
      FileHandle* handle,
      ULONG info_class,
      ULONG start_file_index,
      bool single_entry,
      EVENT_INFORMATION* reply,
      ULONG* used_size,
      ULONG* next_request_start_index);

  // Invokes the appropriate-typed FillEntry function for the given info class.
  bool InvokeFillEntry(ULONG info_class,
                       const FileNameAndInfo& entry,
                       ULONG index,
                       ULONG free_buffer_size,
                       char* buffer,
                       ULONG* entry_size);

  template <typename T>
  using FillFn = std::function<void (const FileNameAndInfo& source, T* dest)>;

  template <typename T>
  bool FillEntry(const FileNameAndInfo& info,
                 ULONG index,
                 ULONG free_buffer_size,
                 char* entry_buffer,
                 ULONG* entry_size,
                 const FillFn<T>& fill_fn) {
    const ULONG name_size = info.name.size() * sizeof(wchar_t);
    const ULONG required_entry_size =
        util::Align(8, sizeof(T) + name_size - sizeof(wchar_t));
    if (free_buffer_size < required_entry_size) {
      return false;
    }
    T* typed_buffer = reinterpret_cast<T*>(entry_buffer);
    fill_fn(info, typed_buffer);
    typed_buffer->FileIndex = index + 1;
    typed_buffer->FileNameLength = name_size;
    memcpy(typed_buffer->FileName, info.name.c_str(), name_size);
    *entry_size = required_entry_size;
    return true;
  }

  template <typename T>
  bool FillEntryWithNameAndIndexOnly(const FileNameAndInfo& info, ULONG index,
                                     ULONG free_buffer_size, char* entry_buffer,
                                     ULONG* entry_size) {
    return FillEntry<T>(info, index, free_buffer_size, entry_buffer, entry_size,
                        [](const FileNameAndInfo&, T*){});
  }

  template <typename T>
  bool FillEntryWithAllInfo(const FileNameAndInfo& info, ULONG index,
                            ULONG free_buffer_size, char* entry_buffer,
                            ULONG* entry_size) {
    return FillEntry<T>(info, index, free_buffer_size, entry_buffer, entry_size,
                        std::bind(&FindHandler::FillAllInfo<T>, this,
                                  std::placeholders::_1,
                                  std::placeholders::_2));
  }

  // This is used for everything but FileNamesInformation. For the other info
  // classes, we currently return only the fields they all have in common.
  template <typename T>
  void FillAllInfo(const FileNameAndInfo& source, T* dest) {
    file_info_handler_->FillBasicInfo(source.info, dest);
    file_info_handler_->FillSizeAndEofInfo(source.info, dest);
  }

  Logger* const logger_;
  FileCallbacks* const callbacks_;
  FileInfoHandler* const file_info_handler_;
};

}  // namespace dokan

#endif // DOKAN_FIND_HANDLER_H_