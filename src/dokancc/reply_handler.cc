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

#include <cassert>
#include <utility>
#include <vector>

#include "util.h"

namespace dokan {
namespace {

const size_t kMaxReplyBatchByteSize = 1024 * 1024 * 1024;

}  // namespace

ReplyHandler::ReplyHandler(Device* device, Logger* logger, size_t thread_count,
                           bool allow_batching)
    : device_(device),
      logger_(logger),
      threads_(thread_count),
      allow_batching_(allow_batching) {
  for (size_t i = 0; i < thread_count; ++i) {
    threads_[i].reset(new std::thread(std::bind(&ReplyHandler::Run, this)));
  }
}

void ReplyHandler::Shutdown() {
  {
    std::unique_lock<std::mutex> lock(mutex_);
    stopped_ = true;
  }
  state_changed_.notify_all();
  for (auto& thread : threads_) {
    if (thread->joinable()) {
      thread->join();
    }
  }
  if (!pending_replies_.empty()) {
    DOKAN_LOG_(INFO) << "Reply handler has shut down with "
                     << pending_replies_.size()
                     << " replies still in the queue.";
  }
}

void ReplyHandler::SendReply(util::UniqueVarStructPtr<EVENT_INFORMATION> data,
                             size_t size) {
  if (stopped_) {
    DOKAN_LOG_(TRACE) << "Sent reply after reply handler was shut down.";
    return;
  }
  {
    std::unique_lock<std::mutex> lock(mutex_);
    const bool was_empty = pending_replies_.empty();
    assert(!pending_replies_.count(data->SerialNumber));
    pending_replies_.emplace(
        std::piecewise_construct,
        std::forward_as_tuple(data->SerialNumber),
        std::forward_as_tuple(std::move(data), size));
    // If batching is allowed and there are already pending replies then the one
    // we are adding is guaranteed to be picked up by a prior notify, so avoid
    // a pointless wakeup.
    if (was_empty || !allow_batching_) {
      state_changed_.notify_one();
    }
  }
}

void ReplyHandler::Run() {
  while (!stopped_) {
    {
      std::unique_lock<std::mutex> lock(mutex_);
      state_changed_.wait(lock, [&] {
        return stopped_ || !pending_replies_.empty();
      });
    }
    size_t busy_count = 0;
    uint64_t in_progress_key = 0;
    std::map<uint64_t, Reply> local_replies;
    {
      std::unique_lock<std::mutex> lock(mutex_);
      if (allow_batching_) {
        local_replies = std::move(pending_replies_);
        pending_replies_.clear();
      } else if (!pending_replies_.empty()) {
        auto it = pending_replies_.begin();
        local_replies.emplace(it->second.data->SerialNumber,
                              std::move(it->second));
        pending_replies_.erase(it);
      }
      if (local_replies.empty()) {
        continue;
      }
      in_progress_key = next_in_progress_key_++;
      in_progress_replies_.emplace(in_progress_key,
                                   &local_replies.begin()->second);
      busy_count = in_progress_replies_.size();
      assert(busy_count <= threads_.size());
    }
    in_progress_reply_state_listener_();
    double saturation =
        static_cast<double>(busy_count) / static_cast<double>(threads_.size());
    if (saturation > 0.7) {
      DOKAN_LOG_(INFO) << "High number of busy reply threads: " << busy_count;
    }
    if (!SendReplies(local_replies)) {
      DOKAN_LOG_(ERROR) << "Failed to send reply.";
    }
    {
      std::unique_lock<std::mutex> lock(mutex_);
      in_progress_replies_.erase(in_progress_key);
    }
    in_progress_reply_state_listener_();
  }
}

uint64_t ReplyHandler::GetOldestInProgressReplyAgeMs() {
  std::unique_lock<std::mutex> lock(mutex_);
  if (in_progress_replies_.empty()) {
    return 0;
  }
  return util::CurrentWallTimeMs() -
         in_progress_replies_.cbegin()->second->timestamp_ms;
}

bool ReplyHandler::SendReplies(const std::map<uint64_t, Reply>& replies) {
  if (replies.empty()) {
    return true;
  }
  if (replies.size() == 1) {
    const Reply& reply = replies.begin()->second;
    return device_->Control(FSCTL_EVENT_INFO, reply.data.get(), reply.size);
  }
  size_t total_size = 0;
  bool batchable = true;
  for (const auto& [key, reply] : replies) {
    total_size += reply.size;
    // Buffer overflow replies overload the BufferLength as the required length,
    // and batching in the drive currently assumes BufferLength is the payload
    // length.
    batchable &= (reply.data->Status != STATUS_BUFFER_OVERFLOW);
  }
  if (!batchable || total_size > kMaxReplyBatchByteSize) {
    // If we are sending a huge amount of data, it is most likely a small number
    // of replies that are each large, so there's not much benefit in batching
    // that, and it could be too large; just serialize it.
    bool result = true;
    for (const auto& [key, reply] : replies) {
      if (!device_->Control(FSCTL_EVENT_INFO, reply.data.get(), reply.size)) {
        result = false;
      }
    }
    return result;
  }
  // Send one concatenated reply.
  std::vector<char> batch(total_size);
  size_t offset = 0;
  for (const auto& [key, reply] : replies) {
    memcpy(batch.data() + offset,
           reinterpret_cast<const char*>(reply.data.get()), reply.size);
    offset += reply.size;
  }
  return device_->Control(FSCTL_EVENT_INFO, batch.data(), total_size);
}

}  // namespace dokan
