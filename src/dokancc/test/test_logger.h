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

#ifndef DOKAN_TEST_TEST_LOGGER_H_
#define DOKAN_TEST_TEST_LOGGER_H_

// Dokan Logger implementation that is useful in tests.

#include <stdio.h>

#include <mutex>
#include <string>
#include <vector>

#include "third_party/dokan/src/dokancc/logger.h"

namespace dokan {
namespace test {

class TestLogger : public Logger {
 public:
  void VLogInfo(const LogSite& site, const char* format, va_list args) override;

  void VLogError(const LogSite& site, const char* format,
                 va_list args) override;

  void VLogTrace(const LogSite& site, const char* format,
                 va_list args) override;

  bool HasErrors() const { return !error_.empty(); }

 private:
  void Write(const char* prefix, const LogSite& site, const char* format,
             va_list args, std::vector<std::string>* v);

  std::mutex mutex_;
  std::vector<std::string> info_;
  std::vector<std::string> error_;
  std::vector<std::string> trace_;
};

}  // namespace test
}  // namespace dokan

#endif  // DOKAN_TEST_TEST_LOGGER_H_

