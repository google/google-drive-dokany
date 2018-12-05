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

#include "file_info_handler.h"

#include <cassert>

using std::placeholders::_1;
using std::placeholders::_2;

namespace dokan {
namespace {

template <typename T>
using FillFn = std::function<void (const FileInfo& source, T* dest)>;

// Interprets the reply's buffer as a fixed-size FILE_*_INFORMATION struct of
// type T and fills it from the result using fill_fn. If the reply's buffer does
// not fit a struct of type T, then the result is STATUS_BUFFER_OVERFLOW.
template <typename T>
NTSTATUS FillFixedSizeInfo(const FillFn<T>& fill_fn,
                           const FileInfo& result,
                           EVENT_INFORMATION* reply,
                           ULONG* used_buffer_size) {
  if (reply->BufferLength < sizeof(T)) {
    return STATUS_BUFFER_OVERFLOW;
  }
  fill_fn(result, reinterpret_cast<T*>(reply->Buffer));
  *used_buffer_size = sizeof(T);
  return STATUS_SUCCESS;
}

void FillAttributeTagInfo(const FileInfo& source,
                          FILE_ATTRIBUTE_TAG_INFORMATION* dest) {
  dest->FileAttributes = source.file_attributes;
  dest->ReparseTag = 0;
}

NTSTATUS FillNetworkPhysicalNameInfo(
    EVENT_CONTEXT* request,
    EVENT_INFORMATION* reply,
    ULONG* used_buffer_size) {
  if (reply->BufferLength < sizeof(FILE_NETWORK_PHYSICAL_NAME_INFORMATION) +
        request->Operation.File.FileNameLength - sizeof(wchar_t)) {
    return STATUS_BUFFER_OVERFLOW;
  }
  auto dest = reinterpret_cast<FILE_NETWORK_PHYSICAL_NAME_INFORMATION*>(
      reply->Buffer);
  dest->FileNameLength = request->Operation.File.FileNameLength;
  memcpy(dest->FileName, request->Operation.File.FileName,
         request->Operation.File.FileNameLength);
  // The one char in the fixed-size area is subtracted from the dynamic length.
  assert(dest->FileNameLength != 0);
  *used_buffer_size = sizeof(FILE_NETWORK_PHYSICAL_NAME_INFORMATION) +
      request->Operation.File.FileNameLength - sizeof(wchar_t);
  return STATUS_SUCCESS;
}

}  // namespace

void FileInfoHandler::FillStandardInfo(const FileInfo& source,
                                       FILE_STANDARD_INFORMATION* dest) {
  FillSizeAndEofInfo(source, dest);
  dest->DeletePending = false;
  dest->Directory = ((source.file_attributes & FILE_ATTRIBUTE_DIRECTORY) != 0);
}

void FileInfoHandler::FillNetworkOpenInfo(const FileInfo& source,
                                          FILE_NETWORK_OPEN_INFORMATION* dest) {
  FillBasicInfo(source, dest);
  FillSizeAndEofInfo(source, dest);
}

NTSTATUS FileInfoHandler::FillAllInfo(
    EVENT_CONTEXT* request,
    const FileInfo& get_info_result,
    EVENT_INFORMATION* reply,
    ULONG* used_buffer_size) {
  const ULONG buffer_size = reply->BufferLength;
  if (buffer_size < sizeof(FILE_ALL_INFORMATION)) {
    return STATUS_BUFFER_OVERFLOW;
  }
  auto dest = reinterpret_cast<FILE_ALL_INFORMATION*>(reply->Buffer);
  FillBasicInfo(get_info_result, &dest->BasicInformation);
  FillStandardInfo(get_info_result, &dest->StandardInformation);

  // TODO(drivefs-team): The driver should start handling the
  // FILE_NAME_INFORMATION piece in the FileAllInformation case like it does
  // when only a name-related info class is requested. We can then delete the
  // code below. Note that it does handle the FILE_POSITION_INFORMATION either
  // way.

  if (buffer_size < sizeof(FILE_ALL_INFORMATION) +
      request->Operation.File.FileNameLength) {
    // Fill the fixed-size portion of the FILE_NAME_INFORMATION structure if
    // that's all we can fit. That includes the length and first char. The old
    // code doesn't try to fit some chars in the var-sized area.
    dest->NameInformation.FileNameLength =
        request->Operation.File.FileNameLength;
    dest->NameInformation.FileName[0] =
        request->Operation.File.FileName[0];
    *used_buffer_size = sizeof(FILE_ALL_INFORMATION);
    return STATUS_BUFFER_OVERFLOW;
  }
  dest->NameInformation.FileNameLength =
      request->Operation.File.FileNameLength;
  memcpy(dest->NameInformation.FileName, request->Operation.File.FileName,
         request->Operation.File.FileNameLength);

  // The one char in the fixed-size area is subtracted from the dynamic length.
  assert(dest->NameInformation.FileNameLength != 0);
  *used_buffer_size = sizeof(FILE_ALL_INFORMATION) +
      dest->NameInformation.FileNameLength - sizeof(wchar_t);
  return STATUS_SUCCESS;
}

NTSTATUS FileInfoHandler::HandleGetInfoReply(
    EVENT_CONTEXT* request,
    ULONG info_class,
    const FileInfo& get_info_result,
    NTSTATUS status,
    EVENT_INFORMATION* reply,
    ULONG* used_buffer_size) {
  *used_buffer_size = 0;
  if (status != STATUS_SUCCESS) {
    // The original code turns any unsuccessful status into
    // STATUS_INVALID_PARAMETER, which may be wise, given that it's widely
    // checked for.
    DOKAN_LOG_TRACE(
        logger_,
        "Turning GetInfo failure status 0x%x into STATUS_INVALID_PARAMETER.",
        status);
    return STATUS_INVALID_PARAMETER;
  }

  switch (info_class) {
    case FileBasicInformation:
      return FillFixedSizeInfo<FILE_BASIC_INFORMATION>(
          std::bind(&FileInfoHandler::FillBasicInfo<FILE_BASIC_INFORMATION>,
              this, _1, _2),
          get_info_result, reply, used_buffer_size);
    case FileStandardInformation:
      return FillFixedSizeInfo<FILE_STANDARD_INFORMATION>(
          std::bind(&FileInfoHandler::FillStandardInfo, this, _1, _2),
          get_info_result, reply, used_buffer_size);
    case FileAttributeTagInformation:
      return FillFixedSizeInfo<FILE_ATTRIBUTE_TAG_INFORMATION>(
          &FillAttributeTagInfo, get_info_result, reply,
          used_buffer_size);
    case FileNetworkOpenInformation:
      return FillFixedSizeInfo<FILE_NETWORK_OPEN_INFORMATION>(
          std::bind(&FileInfoHandler::FillNetworkOpenInfo, this, _1, _2),
          get_info_result, reply, used_buffer_size);
    case FileAllInformation:
      return FillAllInfo(request, get_info_result, reply, used_buffer_size);
    default:
      assert(false);
  }
  return STATUS_INVALID_PARAMETER;
}

void FileInfoHandler::GetInfo(
     EVENT_CONTEXT* request,
     const FileHandle* handle,
     ULONG info_class,
     util::UniqueVarStructPtr<EVENT_INFORMATION> reply,
     const ReplyFn& reply_fn) {
  // These are info classes that are always handled within the driver.
  assert(info_class != FileNameInformation &&
         info_class != FileNormalizedNameInformation &&
         info_class != FilePositionInformation);

  // We only send back the volume serial number and not an ID for the file. This
  // differs from the C library, but we currently don't think it's useful to
  // even support having the GetInfo callback produce a file ID.
  if (info_class == FileIdInformation) {
    auto data = reinterpret_cast<FILE_ID_INFORMATION*>(
        reply->Buffer);
    data->VolumeSerialNumber = startup_options_->volume_serial_number;
    reply_fn(STATUS_SUCCESS, sizeof(FILE_ID_INFORMATION), std::move(reply));
    return;
  }

  // This is a no-op since we don't support IDs for files.
  if (info_class == FileInternalInformation) {
    reply_fn(STATUS_SUCCESS, sizeof(FILE_INTERNAL_INFORMATION),
             std::move(reply));
    return;
  }

  // These are not implemented here or in the C library. We should arguably just
  // fall to the bottom case for these. STATUS_NOT_IMPLEMENTED tends to be a bad
  // status for production code because it's checked for in virtually no calling
  // code, but for now we're keeping it like the C library.
  if (info_class == FileAlternateNameInformation ||
      info_class == FileCompressionInformation) {
    reply_fn(STATUS_NOT_IMPLEMENTED, 0, std::move(reply));
    return;
  }

  // Surprisingly, this does get used for non-network file systems. It may only
  // be filter drivers that do it in practice.
  // TODO(drivefs-team): Make the driver handle this one.
  if (info_class == FileNetworkPhysicalNameInformation) {
    ULONG used_buffer_size = 0;
    NTSTATUS status = FillNetworkPhysicalNameInfo(request, reply.get(),
                                                  &used_buffer_size);
    reply_fn(status, used_buffer_size, std::move(reply));
    return;
  }

  // These are the info classes that require actually invoking
  // FileSystemCallbacks.
  if (info_class == FileBasicInformation ||
      info_class == FileStandardInformation ||
      info_class == FileAllInformation ||
      info_class == FileAttributeTagInformation ||
      info_class == FileNetworkOpenInformation) {
    FileInfo* raw_get_info_result = new FileInfo{0};
    EVENT_INFORMATION* raw_reply = reply.release();
    callbacks_->GetInfo(handle, raw_get_info_result, [=](NTSTATUS status) {
      std::unique_ptr<FileInfo> get_info_result(raw_get_info_result);
      auto reply = util::MakeUniqueVarStruct<EVENT_INFORMATION>(raw_reply);
      ULONG used_buffer_size = 0;
      status = HandleGetInfoReply(request, info_class, *get_info_result, status,
                                  reply.get(), &used_buffer_size);
      reply_fn(status, used_buffer_size, std::move(reply));
    });
    return;
  }
  if (info_class < FileMaximumInformation) {
    DOKAN_LOG_TRACE(logger_,
                    "GetInfo request with known, unhandled info class: %u",
                    info_class);
  } else {
    // An info class that gets us here would be one Microsoft added after this
    // code was written.
    DOKAN_LOG_INFO(logger_,
                   "GetInfo request with new, unhandled info class: %u",
                   info_class);
  }
  reply_fn(STATUS_INVALID_PARAMETER, 0, std::move(reply));
}

}  // namespace dokan
