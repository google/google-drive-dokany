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
#include <memory>
#include <sstream>
#include <string_view>

#include "api.h"

namespace dokan {

enum class LogLevel {
  kTrace,
  kInfo,
  kError,
};

struct LogSite {
  const char* function;
  const char* file;
  const int line;
};

#define DOKAN_LOG_SITE LogSite {__FUNCTION__, __FILE__, __LINE__}

// `DOKAN_LOGGING_INTERNAL_CONDITION` prefixes another macro that expands to a
// temporary `LogMessage` instantiation followed by zero or more streamed
// expressions.  This definition is tricky to read correctly.  It evaluates to
// either
//
//   (void)0;
//
// or
//
//   ::dokan::LogMessageVoidify() &&
//       ::dokan::LogMessage(...) << "message";
//
// If the condition is evaluable at compile time, as is often the case, it
// compiles away to just one side or the other.
// `DOKAN_LOGGING_INTERNAL_CONDITION` can be used consecutively; e.g. if a
// macro's expansion produces more than one condition at different levels of
// expansion. In other words:
//
//   DOKAN_LOGGING_INTERNAL_CONDITION(x) DOKAN_LOGGING_INTERNAL_CONDITION(y)
//
// is equivalent to
//
//   DOKAN_LOGGING_INTERNAL_CONDITION(x && y).
#define DOKAN_LOGGING_INTERNAL_CONDITION(condition) \
  !(condition) ? (void)0 : ::dokan::LogMessageVoidify()&&

// Don't use these macros directly, instead use the below level-specific macro.
#define DOKAN_LOG_INTERNAL_ONLY_LEVEL_SITE(level, site, logger)              \
  DOKAN_LOGGING_INTERNAL_CONDITION((logger)->IsEnabledForLevel(level, site)) \
  ::dokan::LogMessage(logger, level, site)
#define DOKAN_LOG_INTERNAL_ONLY_TRACE_SITE(site, logger) \
  DOKAN_LOG_INTERNAL_ONLY_LEVEL_SITE(::dokan::LogLevel::kTrace, site, logger)
#define DOKAN_LOG_INTERNAL_ONLY_INFO_SITE(site, logger) \
  DOKAN_LOG_INTERNAL_ONLY_LEVEL_SITE(::dokan::LogLevel::kInfo, site, logger)
#define DOKAN_LOG_INTERNAL_ONLY_ERROR_SITE(site, logger) \
  DOKAN_LOG_INTERNAL_ONLY_LEVEL_SITE(::dokan::LogLevel::kError, site, logger)
#define DOKAN_LOG_INTERNAL_ONLY_LEVEL(level, logger) \
  DOKAN_LOG_INTERNAL_ONLY_LEVEL_SITE(level, DOKAN_LOG_SITE, logger)
#define DOKAN_LOG_INTERNAL_ONLY_TRACE(logger) \
  DOKAN_LOG_INTERNAL_ONLY_LEVEL(::dokan::LogLevel::kTrace, logger)
#define DOKAN_LOG_INTERNAL_ONLY_INFO(logger) \
  DOKAN_LOG_INTERNAL_ONLY_LEVEL(::dokan::LogLevel::kInfo, logger)
#define DOKAN_LOG_INTERNAL_ONLY_ERROR(logger) \
  DOKAN_LOG_INTERNAL_ONLY_LEVEL(::dokan::LogLevel::kError, logger)

// Use this macro to log using a "stream" pattern:
//     DOKAN_LOG(TRACE(logger)) << "message";
//     DOKAN_LOG(INFO(logger)) << "message";
//     DOKAN_LOG(ERROR(logger)) << "message";
//     DOKAN_LOG(LEVEL(level, logger)) << "message";
//     DOKAN_LOG(TRACE_SITE(site, logger)) << "message";
//     DOKAN_LOG(INFO_SITE(site, logger)) << "message";
//     DOKAN_LOG(ERROR_SITE(site, logger)) << "message";
//     DOKAN_LOG(LEVEL_SITE(level, site, logger)) << "message";
#define DOKAN_LOG(severity) DOKAN_LOG_INTERNAL_ONLY_##severity

// Use this macro to log using a "stream" pattern, when there is a |Logger|
// instance in scope named |logger_|:
//     DOKAN_LOG_(TRACE) << "message";
//     DOKAN_LOG_(INFO) << "message";
//     DOKAN_LOG_(ERROR) << "message";
#define DOKAN_LOG_(severity) DOKAN_LOG_INTERNAL_ONLY_##severity(logger_)

// An object that handles the writing of log messages. Logger functions may be
// invoked concurrently on different threads, and subclasses must be
// thread-safe.
class DOKANCC_API Logger {
 public:
  virtual ~Logger() {}

  virtual bool IsEnabledForLevel(LogLevel log_level, const LogSite& site) {
    return true;
  }

  virtual void Log(LogLevel log_level, const LogSite& site,
                   std::string_view msg) = 0;
};

// Simple short-lived class to facilitate stream-based logging.
// This class should not be used directly. Use the DOKAN_LOG(*) macro defined
// above.
class DOKANCC_API LogMessage {
 public:
  LogMessage(Logger* logger, LogLevel log_level, const LogSite& log_site)
      : logger_(logger), log_level_(log_level), log_site_(&log_site) {
    if (logger->IsEnabledForLevel(log_level, log_site)) {
      stream_ = std::make_unique<std::ostringstream>();
      *stream_ << std::boolalpha;
    }
  }

  ~LogMessage() {
    if (stream_) {
      logger_->Log(log_level_, *log_site_, stream_->str());
    }
  }

  template <typename T>
  LogMessage& operator<<(const T& something) {
    if (stream_) {
      *stream_ << something;
    }
    return *this;
  }

 private:
  Logger* const logger_;
  const LogLevel log_level_;
  const LogSite* const log_site_;
  std::unique_ptr<std::ostringstream> stream_;
};

// This class is used to explicitly ignore values in the conditional logging
// macros.  This avoids compiler warnings like "value computed is not used" and
// "statement has no effect" if we can evaluate the condition at compile-time.
class LogMessageVoidify {
 public:
  // This has to be an operator with a precedence lower than << but higher than
  // ?:
  void operator&&(const dokan::LogMessage&) {}
  void operator&&(const std::ostream&) {}

  // This overload allows `DOKAN_LOGGING_INTERNAL_CONDITION` to be used
  // consecutively.
  //
  //   DOKAN_LOGGING_INTERNAL_CONDITION(x)
  //   DOKAN_LOGGING_INTERNAL_CONDITION(y)
  //       ::dokan::LogMessage(...) << "message";
  //
  // becomes this:
  //
  //   !x ? void(0) : ::dokan::LogMessageVoidify() &&
  //   !y ? void(0) : ::dokan::LogMessageVoidify() &&
  //       ::dokan::LogMessage(...) << "message";
  //
  // `LogMessageVoidify() && !y` evaluates to just `!y`, so the final part (with
  // the `LogMessage`) is evaluated only if `x && y`.
  bool operator&&(bool b) { return b; }
};

}  // namespace dokan

#endif // DOKAN_LOGGER_H
