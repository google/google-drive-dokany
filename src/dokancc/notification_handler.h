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

#ifndef DOKAN_NOTIFICATION_HANDLER_H_
#define DOKAN_NOTIFICATION_HANDLER_H_

#include <memory>
#include <string>

#include "logger.h"

class FileSystem;

namespace dokan {

// A NotificationHandler can be used by the owner of a FileSystem to indicate
// when files change through another channel (for example, something has
// modified a database or cloud store backing the FileCallbacks, without using
// the dokan file system to do it).
//
// Restrictions:
// - Notifications should not be sent through the NotificationHandler until the
//   Mounted callback for the FileSystem has been invoked.
// - Notifications must be sent from a thread other than the I/O thread. This is
//   because filter drivers may intercept the IOCTL performed synchronously by a
//   notification function and turn it into a series of synchronous I/O
//   operations, some of which need to be dispatched on the I/O thread. In a
//   pure environment with no filter drivers, it would be safe to use the I/O
//   thread.
class __declspec(dllexport) NotificationHandler {
 public:
  virtual ~NotificationHandler() {}

  // Notifies that a file or directory has been created.
  virtual bool NotifyCreate(const std::wstring& path, bool directory) = 0;

  // Notifies that a file or directory has had its contents or metadata changed.
  virtual bool NotifyUpdate(const std::wstring& path) = 0;

  // Notifies that a file or directory has been deleted.
  virtual bool NotifyDelete(const std::wstring& path, bool directory) = 0;

  // Notifies that a file or directory has been renamed.
  virtual bool NotifyRename(const std::wstring& old_path,
                            const std::wstring& new_path,
                            bool directory,
                            bool same_parent) = 0;
};

// This factory is only accessible within the library; clients should use
// FileSystem::notification_handler().
std::unique_ptr<NotificationHandler> CreateNotificationHandler(
    const FileSystem* fs, Logger* logger);

}  // namespace dokan

#endif  // DOKAN_NOTIFICATION_HANDLER_H_
