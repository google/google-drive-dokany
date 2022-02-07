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

#include <algorithm>
#include <atomic>
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include <thread>

#include "device.h"
#include "kernel_defs.h"
#include "logger.h"
#include "public.h"
#include "reply_monitor.h"
#include "util.h"

namespace dokan {

// Encapsulates a reply to the driver in a ReplyHandler's queue.
struct Reply {
  util::UniqueVarStructPtr<EVENT_INFORMATION> data;
  size_t size;
  uint64_t timestamp_ms;

  Reply(util::UniqueVarStructPtr<EVENT_INFORMATION> data_value,
        size_t size_value)
      : data(std::move(data_value)),
        size(size_value),
        timestamp_ms(util::CurrentWallTimeMs()) {}

  Reply(const Reply&) = delete;
  Reply(Reply&&) = default;
};

// Manages a queue of pending replies to the driver and a thread pool for
// sending them. The reason for this (as opposed to just sending the reply IOCTL
// synchronously) is that replying to the driver can get blocked by a
// DokanCompleteXXX function acquiring a lock and/or doing nontrivial
// processing. The fact that an FCB lock is held during an entire read for
// NtCreateSection tends to exacerbate this problem with I/O against executable
// files. Filter drivers can also add custom IRP completion routines that could
// even do re-entrant I/O that we need to reply to.
class ReplyHandler : public ReplyMonitor {
 public:
  ReplyHandler(Device* device, Logger* logger, size_t thread_count,
               bool allow_batching);

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

  // Returns the milliseconds elapsed since the handler started sending the
  // oldest reply that is in progress.
  uint64_t GetOldestInProgressReplyAgeMs() override;

  // Installs a callback that will be invoked when the number of in-progress
  // kernel replies changes.
  void SetReplyStateChangeListener(std::function<void()> listener) override {
    if (listener) {
      in_progress_reply_state_listener_ = listener;
    } else {
      in_progress_reply_state_listener_ = [] {};
    }
  }

 private:
  // Runs the loop for one reply thread.
  void Run();

  // Sends a batch of replies that are mapped by SerialNumber.
  bool SendReplies(const std::map<uint64_t, Reply>& replies);

  Device* const device_;
  Logger* const logger_;
  const bool allow_batching_;
  std::vector<std::unique_ptr<std::thread>> threads_;

  std::condition_variable state_changed_;
  std::mutex mutex_;

  // state_changed_ should be notified when either of these changes. stopped_
  // can be read without mutex_, but otherwise they are guarded by mutex_.
  // pending_replies_ contains replies that are waiting to be sent to the
  // kernel.
  std::map<uint64_t, Reply> pending_replies_;  // The key is the SerialNumber.
  std::atomic<bool> stopped_{false};

  // Everything below is guarded by mutex_.

  // These replies are currently being sent to the driver, and are owned by the
  // stack frame that is invoking the driver. The size of this is at most the
  // size of threads_. The begin() one is the oldest one.
  std::map<uint64_t, Reply*> in_progress_replies_;
  // Invoked when in_progress_replies_ changes.
  std::function<void()> in_progress_reply_state_listener_ = [] {};
  // The key that will be used for the next Reply that gets added to
  // in_progress_replies_.
  uint64_t next_in_progress_key_ = 0;
};

}  // namespace dokan

#endif // DOKAN_REPLY_HANDLER_H_
