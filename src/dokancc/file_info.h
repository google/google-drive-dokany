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

#ifndef DOKAN_FILE_INFO_H_
#define DOKAN_FILE_INFO_H_

#include <windows.h>

#include <string>

namespace dokan {

// The data returned by the FileCallbacks::GetInfo function. This is the
// subset of BY_HANDLE_FILE_INFORMATION that is actually used.
struct FileInfo {
  uint32_t file_attributes;
  FILETIME creation_time;
  FILETIME last_access_time;
  FILETIME last_write_time;
  uint64_t file_size;
};

// The data returned by the FileCallbacks::FindFiles function for each
// file.
struct FileNameAndInfo {
  std::wstring name;
  FileInfo info;
};

inline bool operator==(const FILETIME& left, const FILETIME& right) {
  return left.dwHighDateTime == right.dwHighDateTime &&
      left.dwLowDateTime == right.dwLowDateTime;
}

// The times that can be changed via FileCallbacks::SetAttributesAndTimes.
struct FileTimes {
  FILETIME creation_time;
  FILETIME last_access_time;
  FILETIME last_write_time;

  // Determines whether a given time in the struct is actually set, indicating
  // that it should be changed.
  static inline bool is_set(const FILETIME& time) {
    return time.dwHighDateTime != 0 || time.dwLowDateTime != 0 &&
        !is_suppression_request(time);
  }

  // Determines whether a given time in the struct is set to -1, which is a
  // Windows magic value for requesting suppression of automatic updates as a
  // result of future operations on the handle (for example, explicitly changing
  // the last_write_time to -1 would be a request not to update the last write
  // time when a write occurs).
  static inline bool is_suppression_request(const FILETIME& time) {
    return time.dwHighDateTime == 0xffffffff &&
        time.dwLowDateTime == 0xffffffff;
  }

  bool operator==(const FileTimes& times) const {
    return creation_time == times.creation_time &&
        last_access_time == times.last_access_time &&
        last_write_time == times.last_write_time;
  }
};

}  // namespace dokan

#endif // DOKAN_FILE_INFO_H_