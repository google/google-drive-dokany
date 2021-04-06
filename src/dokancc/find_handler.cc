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

#include "find_handler.h"

#include <algorithm>
#include <cassert>

#include "hex_util.h"
#include "util.h"

namespace dokan {

void FindHandler::FindFiles(
     EVENT_CONTEXT* request,
     FileHandle* handle,
     const std::wstring& pattern,
     ULONG info_class,
     ULONG start_file_index,
     bool single_entry,
     util::UniqueVarStructPtr<EVENT_INFORMATION> reply,
     const ReplyFn& reply_fn) {
  // These are the only info classes we support.
  if (info_class != FileDirectoryInformation &&
      info_class != FileFullDirectoryInformation &&
      info_class != FileNamesInformation &&
      info_class != FileIdBothDirectoryInformation &&
      info_class != FileBothDirectoryInformation) {
    reply_fn(STATUS_INVALID_PARAMETER, 0, start_file_index, std::move(reply));
    return;
  }
  if (start_file_index == 0) {
    handle->file_list()->clear();
  }
  if (!handle->file_list()->empty()) {
    ULONG used_size = 0;
    ULONG next_request_start_index = start_file_index;
    NTSTATUS status = FindFilesWithPopulatedList(
        handle, info_class, start_file_index, single_entry, reply.get(),
        &used_size, &next_request_start_index);
    reply_fn(status, used_size, next_request_start_index, std::move(reply));
    return;
  }
  // If we get here, there is no cached file list, so load it and then use it.
  EVENT_INFORMATION* raw_reply = reply.release();
  auto populate_list = [=](const std::vector<FileNameAndInfo>& entries) {
    *handle->file_list() = entries;
  };
  callbacks_->FindFiles(handle, populate_list, [=](NTSTATUS status) {
    auto reply = util::MakeUniqueVarStruct<EVENT_INFORMATION>(raw_reply);
    ULONG used_size = 0;
    ULONG next_request_start_index = start_file_index;
    status = HandleFindFilesReply(
        handle, pattern, info_class, start_file_index, single_entry, status,
        reply.get(), &used_size, &next_request_start_index);
    reply_fn(status, used_size, next_request_start_index, std::move(reply));
  });
}

NTSTATUS FindHandler::HandleFindFilesReply(FileHandle* handle,
                                           const std::wstring& pattern,
                                           ULONG info_class,
                                           ULONG start_file_index,
                                           bool single_entry,
                                           NTSTATUS status,
                                           EVENT_INFORMATION* reply,
                                           ULONG* used_size,
                                           ULONG* next_request_start_index) {
  if (status != STATUS_SUCCESS) {
    handle->file_list()->clear();
    NTSTATUS final_status = (start_file_index == 0) ? STATUS_NO_SUCH_FILE :
        STATUS_NO_MORE_FILES;
    DOKAN_LOG_(TRACE) << "FindFiles for " << handle->path() << " with pattern "
                      << pattern << " and info class " << info_class
                      << " returned status " << Hex(status) << "; changing to "
                      << Hex(final_status);
    return final_status;
  }
  if (!pattern.empty()) {
    util::FileNameMatcher matcher;
    if (!matcher.Init(pattern)) {
      DOKAN_LOG_(ERROR) << "Failed to init matcher for pattern: " << pattern;
      return STATUS_INVALID_PARAMETER;
    }
    const auto retained_end = std::remove_if(
        handle->file_list()->begin(),
        handle->file_list()->end(),
        [&](const FileNameAndInfo& entry) {
          return !matcher.Matches(entry.name);
        });
    handle->file_list()->erase(retained_end, handle->file_list()->end());
  }
  return FindFilesWithPopulatedList(
      handle, info_class, start_file_index, single_entry, reply,
      used_size, next_request_start_index);
}

NTSTATUS FindHandler::FindFilesWithPopulatedList(
    FileHandle* handle,
    ULONG info_class,
    ULONG start_file_index,
    bool single_entry,
    EVENT_INFORMATION* reply,
    ULONG* used_size,
    ULONG* next_request_start_index) {
  std::vector<FileNameAndInfo>& file_list = *handle->file_list();
  const ULONG list_count = file_list.size();
  ULONG free_buffer_size = reply->BufferLength;
  char* entry_buffer = reinterpret_cast<char*>(reply->Buffer);
  *next_request_start_index = start_file_index;
  if (start_file_index >= list_count) {
    handle->file_list()->clear();
    return (start_file_index == 0) ? STATUS_NO_SUCH_FILE : STATUS_NO_MORE_FILES;
  }
  FILE_BOTH_DIR_INFORMATION* prev_entry = nullptr;
  for (ULONG i = start_file_index; ; ++i) {
    ULONG entry_size = 0;
    bool fit = InvokeFillEntry(info_class, file_list[i], i, free_buffer_size,
                               entry_buffer, &entry_size);
    assert(entry_size <= free_buffer_size);
    if (!fit) {
      break;
    }
    free_buffer_size -= entry_size;
    if (prev_entry != nullptr) {
      // Note: all the structs have NextEntryOffset in the same position.
      // It must not be set for the last one returned.
      prev_entry->NextEntryOffset =
          entry_buffer - reinterpret_cast<char*>(prev_entry);
    }
    *next_request_start_index = i + 1;
    if (single_entry || i == list_count - 1) {
      break;
    }
    prev_entry = reinterpret_cast<FILE_BOTH_DIR_INFORMATION*>(entry_buffer);
    entry_buffer += entry_size;
  }
  *used_size = reply->BufferLength - free_buffer_size;
  return STATUS_SUCCESS;
}

bool FindHandler::InvokeFillEntry(ULONG info_class,
                                  const FileNameAndInfo& info,
                                  ULONG index,
                                  ULONG free_buffer_size,
                                  char* entry_buffer,
                                  ULONG* entry_size) {
  switch (info_class) {
    case FileNamesInformation:
      return FillEntryWithNameAndIndexOnly<FILE_NAMES_INFORMATION>(
          info, index, free_buffer_size, entry_buffer, entry_size);
    case FileDirectoryInformation:
      return FillEntryWithAllInfo<FILE_DIRECTORY_INFORMATION>(
          info, index, free_buffer_size, entry_buffer, entry_size);
    case FileFullDirectoryInformation:
      return FillEntryWithAllInfo<FILE_FULL_DIR_INFORMATION>(
          info, index, free_buffer_size, entry_buffer, entry_size);
    case FileBothDirectoryInformation:
      return FillEntryWithAllInfo<FILE_BOTH_DIR_INFORMATION>(
          info, index, free_buffer_size, entry_buffer, entry_size);
    case FileIdBothDirectoryInformation:
      return FillEntryWithAllInfo<FILE_ID_BOTH_DIR_INFORMATION>(
          info, index, free_buffer_size, entry_buffer, entry_size);
    default:
      assert(false);
      return false;
  }
}

}  // namespace dokan
