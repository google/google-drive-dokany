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
#include <string_view>

namespace dokan {
namespace test {

void TestLogger::Log(LogLevel log_level, const LogSite& site,
                     std::string_view msg) {
  switch (log_level) {
    case LogLevel::kTrace:
      Write("Trace", site, msg, &trace_);
      return;
    case LogLevel::kInfo:
      Write("Info", site, msg, &info_);
      return;
    case LogLevel::kError:
      Write("Error", site, msg, &error_);
      return;
  }
  assert(false);
}

void TestLogger::Write(const char* prefix, const LogSite& site,
                       std::string_view msg, std::vector<std::string>* v) {
  std::lock_guard<std::mutex> lock(mutex_);
  v->emplace_back(msg);
  printf("%s: %s:%d: %s\n", prefix, site.function, site.line,
         v->back().c_str());
}

}  // namespace test
}  // namespace dokan
