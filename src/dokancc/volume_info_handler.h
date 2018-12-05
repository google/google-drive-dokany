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

#ifndef DOKAN_VOLUME_INFO_HANDLER_H_
#define DOKAN_VOLUME_INFO_HANDLER_H_

#include <ntstatus.h>
#include <windows.h>

#include <functional>
#include <memory>

#include "file_handle.h"
#include "file_info.h"
#include "kernel_defs.h"
#include "logger.h"
#include "public.h"
#include "startup_options.h"
#include "util.h"
#include "volume_callbacks.h"

namespace dokan {

// A FileSystem delegates volume-info requests to a VolumeInfoHandler.
class VolumeInfoHandler {
 public:
  VolumeInfoHandler(VolumeCallbacks* callbacks,
                    Logger* logger,
                    const StartupOptions* startup_options)
      : callbacks_(callbacks),
        logger_(logger),
        allocation_unit_size_(startup_options->allocation_unit_size),
        volume_info_size_(
            sizeof(FILE_FS_VOLUME_INFORMATION) - sizeof(wchar_t) +
            startup_options->volume_name.size() * sizeof(wchar_t)),
        volume_info_(
            util::MakeUniqueVarStruct<FILE_FS_VOLUME_INFORMATION>(
                volume_info_size_)),
        attribute_info_size_(
            sizeof(FILE_FS_ATTRIBUTE_INFORMATION) - sizeof(wchar_t) +
            startup_options->volume_name.size() * sizeof(wchar_t)),
        attribute_info_(
            util::MakeUniqueVarStruct<FILE_FS_ATTRIBUTE_INFORMATION>(
                attribute_info_size_)) {
    volume_info_->VolumeSerialNumber = startup_options->volume_serial_number;
    volume_info_->VolumeLabelLength =
        startup_options->volume_name.size() * sizeof(wchar_t);
    memcpy(volume_info_->VolumeLabel, startup_options->volume_name.c_str(),
           volume_info_->VolumeLabelLength);
    attribute_info_->FileSystemAttributes =
        startup_options->file_system_attributes;
    attribute_info_->MaximumComponentNameLength =
        startup_options->max_component_length;
    attribute_info_->FileSystemNameLength =
        startup_options->file_system_type_name.size() * sizeof(wchar_t); 
    memcpy(attribute_info_->FileSystemName,
           startup_options->file_system_type_name.c_str(),
           attribute_info_->FileSystemNameLength);
  }

  // Signature of the function that GetVolumeInfo replies to in FileSystem.
  using ReplyFn = std::function<void (
      NTSTATUS, ULONG used_buffer_size,
      util::UniqueVarStructPtr<EVENT_INFORMATION> reply)>;

  void GetVolumeInfo(const EVENT_CONTEXT* request,
                     ULONG info_class,
                     util::UniqueVarStructPtr<EVENT_INFORMATION> reply,
                     const ReplyFn& reply_fn);

 private:
  const uint32_t allocation_unit_size_;
  Logger* const logger_;
  VolumeCallbacks* const callbacks_;

  // Volume info and attribute info are initialized by the constructor, never
  // changed, and copied when requested.
  const size_t volume_info_size_;
  const size_t attribute_info_size_;
  util::UniqueVarStructPtr<FILE_FS_VOLUME_INFORMATION> volume_info_;
  util::UniqueVarStructPtr<FILE_FS_ATTRIBUTE_INFORMATION> attribute_info_;
};

}  // namespace dokan

#endif // DOKAN_FILE_INFO_HANDLER_H_