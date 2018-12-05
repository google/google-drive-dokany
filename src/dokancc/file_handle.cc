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

#include "file_handle.h"

#include "util.h"

namespace dokan {

FileHandle::FileHandle(const std::wstring& path,
                       ULONG process_id,
                       bool directory)
    : path_(path), process_id_(process_id), directory_(directory) {
  assert(!path.empty());
  util::SplitAlternateStreamName(&path_, &alternate_stream_name_);
}

FileHandle::~FileHandle() {}

}  // namespace dokan
