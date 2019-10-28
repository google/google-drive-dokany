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

#ifndef DOKAN_VOLUME_CALLBACKS_H_
#define DOKAN_VOLUME_CALLBACKS_H_

#include <ntstatus.h>
#include <windows.h>

#include "api.h"
#include "util.h"

namespace dokan {

class FileSystem;

// Encapsulates the free space information for a file system.
struct FreeSpace {
  uint64_t free_bytes_for_all_users;
  uint64_t free_bytes_for_process_user;
  uint64_t total_bytes;
};

// A VolumeCallbacks object provides the user-mode implementation of the file
// system operations that are not related to specific files. Each function must
// invoke the callback that is its last parameter when the work completes or
// fails.
class DOKANCC_API VolumeCallbacks {
 public:
  virtual ~VolumeCallbacks() {}

  // Determines the free space on the file system, both generally and for the
  // user of the requesting process. If the file system does not have per-user
  // quotas (e.g. it is always in the context of one user), then the two free
  // bytes values should be the same. The total_bytes should count both the used
  // and free bytes for all users.
  virtual void GetVolumeFreeSpace(uint32_t process_id,
                                  FreeSpace* free_space,
                                  const util::StatusCallback& callback) = 0;

  // Invoked when the file system has been mounted but is not yet running. The
  // intent is for the caller of FileSystem::Mount to then execute the I/O loop.
  // This callback runs within the Mount call, so it is guaranteed that no other
  // callback will be invoked before this one returns. However, this means it
  // must not send any synchronous I/O to the mounted file system. The pointer
  // passed in is valid until Unmounted() returns.
  virtual void Mounted(FileSystem* fs) = 0;

  // Invoked when the file system has stopped dispatching I/O but the FileSystem
  // object passed to Mounted() is still valid. This must not send any I/O to
  // the file system. After it returns, the FileSystem object becomes invalid.
  virtual void Unmounted() = 0;
};

}  // namespace dokan

#endif // DOKAN_VOLUME_CALLBACKS_H_
