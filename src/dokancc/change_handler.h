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

#ifndef DOKAN_CHANGE_HANDLER_H_
#define DOKAN_CHANGE_HANDLER_H_

#include <ntstatus.h>

#include <functional>
#include <memory>

#include "file_callbacks.h"
#include "file_handle.h"
#include "kernel_defs.h"
#include "logger.h"
#include "public.h"
#include "util.h"

namespace dokan {

// A FileSystem delegates metadata change requests (e.g. changing the size or
// name of a file) to a ChangeHandler.
class ChangeHandler {
 public:
  ChangeHandler(FileCallbacks* callbacks, Logger* logger)
      : callbacks_(callbacks),
        logger_(logger) {}

  // Signature of FileSystem::CompleteChange
  using ReplyFn = std::function<void (
      NTSTATUS,
      util::UniqueVarStructPtr<EVENT_INFORMATION> reply)>;

  // Changes a file's metadata in the way specified by the given info class,
  // using the input data in the given request. Asynchronously populates any
  // relevant data in the reply (except for generic Status/Context fields) and
  // invokes reply_fn when the operation completes.
  void Change(const EVENT_CONTEXT* request,
              FileHandle* handle,
              ULONG info_class,
              util::UniqueVarStructPtr<EVENT_INFORMATION> reply,
              const ReplyFn& reply_fn);

 private:
  void ChangeSize(const LARGE_INTEGER& size,
                  const FileHandle* handle,
                  const util::StatusCallback& callback);

  void ChangeDisposition(const FILE_DISPOSITION_INFORMATION& info,
                         FileHandle* handle,
                         EVENT_INFORMATION* reply,
                         const util::StatusCallback& callback);

  void ChangeBasicInfo(const FILE_BASIC_INFORMATION& info,
                       const FileHandle* handle,
                       const util::StatusCallback& callback);

  void ChangeName(const DOKAN_RENAME_INFORMATION& info,
                  FileHandle* handle,
                  EVENT_INFORMATION* reply,
                  const util::StatusCallback& callback);

  FileCallbacks* const callbacks_;
  Logger* const logger_;
};

}  // namespace dokan

#endif // DOKAN_CHANGE_HANDLER_H_