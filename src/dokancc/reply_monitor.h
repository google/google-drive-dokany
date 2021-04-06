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

#ifndef DOKAN_REPLY_MONITOR_H_
#define DOKAN_REPLY_MONITOR_H_

#include <functional>

namespace dokan {

// Interface for the reply monitoring API in FileSystem, allowing it to easily
// be mocked for testing.
class ReplyMonitor {
 public:
  virtual ~ReplyMonitor() = default;

  // Returns the milliseconds elapsed since we started sending the oldest reply
  // that is in progress. This is useful for detecting kernel latency/deadlocks
  // in reply processing, since replies are sent to the kernel asynchronously as
  // a result of status callback invocation. If there are no replies currently
  // being sent, the result is 0.
  virtual uint64_t GetOldestInProgressReplyAgeMs() = 0;

  // Installs a callback that will be invoked when the number of in-progress
  // kernel replies changes. This is invoked on arbitrary threads.
  virtual void SetReplyStateChangeListener(std::function<void()> listener) = 0;
};

}  // namespace dokan

#endif  // DOKAN_REPLY_MONITOR_H_
