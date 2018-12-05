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

#include "third_party/dokan/src/dokancc/test/test_logger.h"

#include <stdio.h>

#include <cassert>

namespace dokan {
namespace test {

void TestLogger::VLogInfo(const LogSite& site, const char* format,
                          va_list args) {
  Write("Info", site, format, args, &info_);
}

void TestLogger::VLogError(const LogSite& site, const char* format,
                           va_list args) {
  Write("Error", site, format, args, &error_);
}

void TestLogger::VLogTrace(const LogSite& site, const char* format,
                           va_list args) {
  Write("Trace", site, format, args, &trace_);
}

void TestLogger::Write(const char* prefix, const LogSite& site,
                       const char* format, va_list args,
                       std::vector<std::string>* v) {
  std::lock_guard<std::mutex> lock(mutex_);
  char buffer[2048];
  vsnprintf(buffer, sizeof(buffer) - 1, format, args);
  // If the buffer comes out to be an empty string, the format probably doesn't
  // match the arguments.
  assert(strlen(buffer) != 0);
  v->push_back(buffer);
  printf("%s: %s:%d: %s\n", prefix, site.function, site.line, buffer);
}

}  // namespace test
}  // namespace dokan
