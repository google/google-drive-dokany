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

#include "reply_handler.h"

#include <vector>

namespace dokan {

ReplyHandler::ReplyHandler(Device* device, Logger* logger, size_t thread_count)
    : device_(device),
      logger_(logger),
      threads_(thread_count) {
  for (size_t i = 0; i < thread_count; ++i) {
    threads_[i].reset(new std::thread(std::bind(&ReplyHandler::Run, this)));
  }
}

void ReplyHandler::Shutdown() {
  {
    std::unique_lock<std::mutex> lock(mutex_);
    stopped_ = true;
  }
  state_.notify_all();
  for (auto& thread : threads_) {
    if (thread->joinable()) {
      thread->join();
    }
  }
  if (!pending_replies_.empty()) {
    DOKAN_LOG_INFO(
        logger_,
        "Reply handler has shut down with %u replies still in the queue.",
        pending_replies_.size());
  }
}

void ReplyHandler::SendReply(util::UniqueVarStructPtr<EVENT_INFORMATION> data,
                             size_t size) {
  if (stopped_) {
    DOKAN_LOG_TRACE(logger_, "Sent reply after reply handler was shut down.");
    return;
  }
  {
    std::unique_lock<std::mutex> lock(mutex_);
    pending_replies_.emplace(std::move(data), size);
    state_.notify_one();
  }
}

void ReplyHandler::Run() {
  while (!stopped_) {
    {
      std::unique_lock<std::mutex> lock(mutex_);
      state_.wait(lock, [&] {
        return stopped_ || !pending_replies_.empty();
      });
    }
    Reply reply;
    bool found_reply = false;
    {
      std::unique_lock<std::mutex> lock(mutex_);
      if (!pending_replies_.empty()) {
        reply = std::move(pending_replies_.front());
        pending_replies_.pop();
        found_reply = true;
      }
    }
    if (found_reply) {
      size_t busy_count = ++busy_count_;
      double saturation = static_cast<double>(busy_count) /
          static_cast<double>(threads_.size());
      if (saturation > 0.7) {
        DOKAN_LOG_INFO(logger_, "High number of busy reply threads: %u",
                       busy_count);
      }
      if (!device_->Control(IOCTL_EVENT_INFO, reply.data.get(), reply.size)) {
        DOKAN_LOG_ERROR(logger_, "Failed to send reply.");
      }
      --busy_count_;
    }
  }
}

}  // namespace dokan
