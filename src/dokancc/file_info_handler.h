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

#ifndef DOKAN_FILE_INFO_HANDLER_H_
#define DOKAN_FILE_INFO_HANDLER_H_

#include <ntstatus.h>
#include <windows.h>

#include <functional>
#include <memory>

#include "file_callbacks.h"
#include "file_handle.h"
#include "file_info.h"
#include "kernel_defs.h"
#include "logger.h"
#include "public.h"
#include "startup_options.h"
#include "util.h"

namespace dokan {

// A FileSystem delegates file-info requests to a FileInfoHandler.
class FileInfoHandler {
 public:
  FileInfoHandler(FileCallbacks* callbacks,
                  Logger* logger,
                  const StartupOptions* startup_options)
      : callbacks_(callbacks),
        logger_(logger),
        startup_options_(startup_options) {}

  // Signature of FileSystem::CompleteGetInfo
  using ReplyFn = std::function<void (
      NTSTATUS, ULONG used_buffer_size,
      util::UniqueVarStructPtr<EVENT_INFORMATION> reply)>;

  // Gets the requested info class, using FileCallbacks::GetInfo if necessary,
  // populates the given reply, and invokes the reply function with it. The
  // reply is a raw variable-sized struct pointer, which this function converts
  // into a UniqueVarStructPtr after the callback returns or gets bypassed.
  void GetInfo(EVENT_CONTEXT* request,
               const FileHandle* handle,
               ULONG info_class,
               util::UniqueVarStructPtr<EVENT_INFORMATION> reply,
               const ReplyFn& reply_fn);

  // A helper that fills all the fields that are in FILE_BASIC_INFORMATION.
  // These same fields are also found in various other structs.
  template <typename T>
  void FillBasicInfo(const FileInfo& source, T* dest) {
    dest->FileAttributes = source.file_attributes;
    util::TimeToLargeInteger(source.creation_time, &dest->CreationTime);
    util::TimeToLargeInteger(source.last_access_time, &dest->LastAccessTime);
    // The old code also duplicates this in both fields.
    util::TimeToLargeInteger(source.last_write_time, &dest->LastWriteTime);
    util::TimeToLargeInteger(source.last_write_time, &dest->ChangeTime);
  }

  // A helper that fills the size and EOF fields that are in various structs
  // like FILE_BASIC_INFORMATION and FILE_NETWORK_OPEN_INFORMATION.
  template <typename T>
  void FillSizeAndEofInfo(const FileInfo& source, T* dest) {
    dest->EndOfFile.QuadPart = source.file_size;
    dest->AllocationSize = dest->EndOfFile;
    dest->AllocationSize.QuadPart = util::Align(
        startup_options_->allocation_unit_size, dest->AllocationSize.QuadPart);
  }

  template <typename T>
  void FillIdInfo(const FileInfo& source, T* dest) {
    dest->VolumeSerialNumber = startup_options_->volume_serial_number;
    std::memcpy(&dest->FileId.Identifier, &source.file_index,
                sizeof(source.file_index));
  }

  template <typename T>
  void FillInternalInfo(const FileInfo& source, T* dest) {
    dest->IndexNumber.QuadPart = source.file_index;
  }

 private:
  void FillStandardInfo(const FileInfo& source,
                        FILE_STANDARD_INFORMATION* dest);

  void FillNetworkOpenInfo(const FileInfo& source,
                           FILE_NETWORK_OPEN_INFORMATION* dest);

  NTSTATUS FillAllInfo(EVENT_CONTEXT* request, const FileInfo& get_info_result,
                       EVENT_INFORMATION* reply, ULONG* used_buffer_size);

  // Converts the status and result from FileCallbacks::GetInfo into a status
  // and FILE_*_INFORMATION struct to return to the driver. The latter struct is
  // placed in the buffer inside the reply, if it fits (otherwise the returned
  // status is STATUS_BUFFER_OVERFLOW). This function sets the used_buffer_size
  //  to indicate how many bytes of that buffer it uses.
  NTSTATUS HandleGetInfoReply(
      PEVENT_CONTEXT request,
      ULONG info_class,
      const FileInfo& get_info_result,
      NTSTATUS status,
      EVENT_INFORMATION* reply,
      ULONG* used_buffer_size);

  const StartupOptions* const startup_options_;
  Logger* const logger_;
  FileCallbacks* const callbacks_;
};

}  // namespace dokan

#endif // DOKAN_FILE_INFO_HANDLER_H_