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

#ifndef DOKAN_REPLY_HANDLER_H_
#define DOKAN_REPLY_HANDLER_H_

#include <ntstatus.h>
#include <windows.h>
#include <winioctl.h>

#include <atomic>
#include <memory>
#include <mutex>
#include <queue>
#include <set>
#include <thread>

#include "device.h"
#include "kernel_defs.h"
#include "logger.h"
#include "public.h"
#include "util.h"

namespace dokan {

// Encapsulates a reply to the driver in a ReplyHandler's queue.
struct Reply {
  util::UniqueVarStructPtr<EVENT_INFORMATION> data;
  size_t size;

  Reply()
      : data(util::MakeUniqueVarStruct<EVENT_INFORMATION>(nullptr)),
        size(0) {}

  Reply(util::UniqueVarStructPtr<EVENT_INFORMATION> data_value,
        size_t size_value)
      : data(std::move(data_value)),
        size(size_value) {}
};

// Manages a queue of pending replies to the driver and a thread pool for
// sending them. The reason for this (as opposed to just sending the reply IOCTL
// synchronously) is that replying to the driver can get blocked by a
// DokanCompleteXXX function acquiring a lock and/or doing nontrivial
// processing. The fact that an FCB lock is held during an entire read for
// NtCreateSection tends to exacerbate this problem with I/O against executable
// files. Filter drivers can also add custom IRP completion routines that could
// even do re-entrant I/O that we need to reply to.
class ReplyHandler {
 public:
  ReplyHandler(Device* device, Logger* logger, size_t thread_count);

  ~ReplyHandler() {
    Shutdown();
  }

  // Stops the reply threads when they have finished sending any replies they
  // are already in the process of sending. This function does not return until
  // the threads have stopped. Further replies that come in via SendReply after
  // this is invoked, as well as replies that have not been started by any reply
  // thread yet, are logged but otherwise ignored.
  void Shutdown();

  // Sends a reply of the given size to the driver asynchronously from an
  // available thread. This can be called from any thread.
  void SendReply(util::UniqueVarStructPtr<EVENT_INFORMATION> reply,
                 size_t size);

 private:
  // Runs the loop for one reply thread.
  void Run();

  Device* const device_;
  Logger* const logger_;
  std::atomic<size_t> busy_count_{0};
  std::vector<std::unique_ptr<std::thread>> threads_;

  std::condition_variable state_;
  std::mutex mutex_;

  // These are guarded by mutex_ and part of state_. stopped_ can be read
  // without mutex_.
  std::queue<Reply> pending_replies_;
  std::atomic<bool> stopped_{false};
};

}  // namespace dokan

#endif // DOKAN_REPLY_HANDLER_H_
