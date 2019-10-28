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

#ifndef DOKAN_LOGGER_H_
#define DOKAN_LOGGER_H_

#include <cstdarg>

#include "api.h"

namespace dokan {

struct LogSite {
  const char* function;
  const char* file;
  const int line;
};

#define DOKAN_LOG_SITE LogSite {__FUNCTION__, __FILE__, __LINE__}

#define DOKAN_LOG_TRACE(logger, format, ...)                            \
  {                                                                      \
    const LogSite site = DOKAN_LOG_SITE;                                 \
    if ((logger)->IsTraceEnabled(site)) {                                \
      (logger)->LogTrace(site, format, __VA_ARGS__);                     \
    }                                                                    \
  }
#define DOKAN_LOG_INFO(logger, format, ...) \
    (logger)->LogInfo(DOKAN_LOG_SITE, format, __VA_ARGS__)
#define DOKAN_LOG_ERROR(logger, format, ...) \
    (logger)->LogError(DOKAN_LOG_SITE, format, __VA_ARGS__)

// An object that handles the writing of log messages. Logger functions may be
// invoked concurrently on different threads, and subclasses must be
// thread-safe.
class DOKANCC_API Logger {
 public:
  virtual ~Logger() {}

  void LogTrace(const LogSite& site, const char* format, ...) {
    va_list  args;
    va_start(args, format);
    VLogTrace(site, format, args);
    va_end(args);
  }

  void LogInfo(const LogSite& site, const char* format, ...) {
    va_list args;
    va_start(args, format);
    VLogInfo(site, format, args);
    va_end(args);
  }

  void LogError(const LogSite& site, const char* format, ...) {
    va_list args;
    va_start(args, format);
    VLogError(site, format, args);
    va_end(args);
  }

  virtual bool IsTraceEnabled(const LogSite& site) {
    return true;
  }

  virtual void VLogTrace(const LogSite& site, const char* format,
                         va_list args) = 0;

  virtual void VLogInfo(const LogSite& site, const char* format,
                        va_list args) = 0;

  virtual void VLogError(const LogSite& site, const char* format,
                         va_list args) = 0;
};

}  // namespace dokan

#endif // DOKAN_LOGGER_H
