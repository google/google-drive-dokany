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

#ifndef DOKAN_STARTUP_OPTIONS_H_
#define DOKAN_STARTUP_OPTIONS_H_

#include <windows.h>

namespace dokan {

// Flags that can be used to configure the driver at mount time.
enum StartupFlags {
  // Enable the use of alternate stream paths in the form
  // <file-name>:<stream-name>. If this is not specified then the driver will
  // fail any attempt to access a path with a colon.
  DOKAN_OPTION_ALT_STREAM = (1 << 0),

  // Use the FILE_READ_ONLY_DEVICE device characteristic when mounting.
  DOKAN_OPTION_WRITE_PROTECT = (1 << 1),

  // Use the FILE_REMOVABLE_MEDIA device characteristic when mounting.
  DOKAN_OPTION_REMOVABLE = (1 << 2),

  // Enable the invocation of user-mode file locking callbacks. If this is not
  // specified, then the driver uses FsRtlCheckOplock and related functions to
  // implement locking.
  DOKAN_OPTION_FILELOCK_USER_MODE = (1 << 3),

  // Enable logging of abnormally long kernel lock acquisition waits. This is
  // detrimental to performance and should not be enabled in normal use.
  DOKAN_OPTION_LOCK_DEBUG_ENABLED = (1 << 4),

  // Whether the driver should log oplock requests. This may be detrimental to
  // preformance and should not be enabled in normal use.
  DOKAN_OPTION_LOG_OPLOCKS = (1 << 5),

  // Don't lock the FCB in the driver for paging I/O IRPs where it is not
  // already bypassed by the above flag.
  DOKAN_OPTION_ASSUME_PAGING_IO_IS_LOCKED = (1 << 6),

  // Allow more than one request to be passed per kernel-to-user message.
  DOKAN_OPTION_ALLOW_REQUEST_BATCHING = (1 << 7),

  // Allow more than one request and reply to be passed per kernel-to-user
  // message. This implies DOKAN_OPTION_ALLOW_REQUEST_BATCHING and it's
  // irrelevant whether that is also set.
  DOKAN_OPTION_ALLOW_FULL_BATCHING = (1 << 8),

  // Forward the kernel driver global and volume logs to the userland.
  DOKAN_OPTION_DISPATCH_DRIVER_LOGS = (1 << 9),
  // Disable FileNetworkPhysicalNameInformation query on files.The
  // FileInformationClass query will directly result in STATUS_INVALID_PARAMETER
  // failure instead of being resolved by the driver.
  DOKAN_OPTION_DISABLE_NETWORK_PHYSICAL_QUERY = (1 << 10),

  // Allow I/O requests to be pulled directly by FSCTL_EVENT_WAIT when available
  // instead of waiting to be woken up by the system thread.
  DOKAN_OPTION_PULL_EVENT_AHEAD = (1 << 11),

  // Store the DokanFCB instances in the Avl table instead of the LIST_ENTRY.
  DOKAN_OPTION_FCB_AVL_TABLE = (1 << 12)
};

// The options for mounting a FileSystem.
struct StartupOptions {
 public:
  // The StartupFlags to send to the dokan driver. These are not directly
  // accessible outside dokan.
  uint64_t flags = 0;

  // The serial number used in all file-info and volume-info responses.
  uint32_t volume_serial_number = 0x19831116;

  // The name shown in Explorer etc. for the volume.
  std::wstring volume_name = L"DOKAN";

  // This name can be accessed via volume-info APIs by the kernel, other
  // drivers, and apps, and it may have unforeseen effects.
  std::wstring file_system_type_name = L"FAT32";

  // These flags can be accessed via volume-info APIs by the kernel, other
  // drivers, and apps.
  uint32_t file_system_attributes = FILE_SUPPORTS_REMOTE_STORAGE |
                                    FILE_UNICODE_ON_DISK;

  // Setting this makes the claim that chunks of this size in bytes are deducted
  // from the available space when something needs to be stored. The default
  // value is in line with most real file systems.
  uint32_t allocation_unit_size = 512;

  // The maximum length in chars of a path component on the file system.
  uint32_t max_component_length = 256;

  // How long the driver should wait for the user-mode code to respond to a
  // specific I/O request before failing that request. Note that such a timeout
  // causes unmounting if it occurs before the keepalive handle is activated in
  // FileSystem::PostStart.
  uint64_t request_timeout_millis = 20 * 1000;

  // The number of threads to use for sending I/O replies through the driver.
  // The reply handling logic can be non-trivial, acquire locks, and be
  // customized by filter drivers that could make re-entrant requests, so it is
  // best to have several threads for this.
  size_t reply_thread_count = 10;

  // Size in bytes of volume_security_descriptor, or 0 if not present.
  uint32_t volume_security_descriptor_length = 0;

  // A security descriptor in self-relative format controlling access to the
  // volume itself, or NULL for default security. By default, all users can
  // access and modify files. If this is specified, then APIs like
  // GetUserObjectSecurity will also use it when invoked by apps like Explorer.
  // If it is not specified, then such APIs will return ERROR_NOT_SUPPORTED,
  // which is not normally disruptive.
  PSECURITY_DESCRIPTOR volume_security_descriptor = nullptr;

  // Size in bytes of readonly_security_descriptor, or 0 if not present.
  uint32_t readonly_security_descriptor_length = 0;

  // A security descriptor in self-relative format that should be used for
  // path-specific IRP_MJ_QUERY_SECURITY requests when the Create callback has
  // asked for this behavior via
  // FileHandle::set_use_readonly_security_descriptor. The purpose is to prevent
  // Explorer from offering e.g. New options that don't make sense.
  PSECURITY_DESCRIPTOR readonly_security_descriptor = nullptr;

  // Keep FCBs alive for this amount of time after they are no longer referenced
  // by an open file or memory mapped region. This essentially allows the
  // teardown of virus scanner state to be canceled if the file is soon used
  // again, which can exponentially speed up use cases like zip extraction.
  ULONG fcb_garbage_collection_interval_ms = 0;
};

}  // namespace dokan

#endif // DOKAN_STARTUP_OPTIONS_H_
