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

#include "util.h"

#include <cassert>
#include <codecvt>
#include <locale>
#include <mutex>

#include "logger.h"

namespace dokan {
namespace util {
namespace {

// Historically dokan has had its own version of this to avoid linking against
// the  RPC-related Windows libraries that provide the built-in equivalent.
void UUIDToString(const GUID guid, char* out) {
  sprintf(out,
      "{%08lX-%04hX-%04hX-%02hhX%02hhX-%02hhX%02hhX%02hhX%02hhX%02hhX%02hhX}",
      guid.Data1, guid.Data2, guid.Data3,
      guid.Data4[0], guid.Data4[1], guid.Data4[2], guid.Data4[3],
      guid.Data4[4], guid.Data4[5], guid.Data4[6], guid.Data4[7]);
}

}  // anonymous namespace

bool FileNameMatcher::Init(const std::wstring& pattern) {
  assert(pattern_.Buffer == nullptr);
  UNICODE_STRING unicode_input_pattern;
  RtlInitUnicodeString(&unicode_input_pattern, pattern.c_str());
  NTSTATUS status = RtlUpcaseUnicodeString(&pattern_, &unicode_input_pattern,
                                           true);
  return status == STATUS_SUCCESS;
}

FileNameMatcher::~FileNameMatcher() {
  if (pattern_.Buffer != nullptr) {
    RtlFreeUnicodeString(&pattern_);
    pattern_.Buffer = nullptr;
  }
}

bool FileNameMatcher::Matches(const std::wstring& name) {
  assert(pattern_.Buffer != nullptr);
  if (pattern_.Buffer == nullptr) {
    return false;
  }
  UNICODE_STRING unicode_name;
  RtlInitUnicodeString(&unicode_name, name.c_str());
  // This is what NTFS uses. We think the original dokan code had its own
  // version because it couldn't refer to the "Fs"-prefixed one, but that is in
  // fact an alias with the same address.
  return RtlIsNameInExpression(&pattern_, &unicode_name, true, nullptr);
}

int64_t Align(size_t block_size, int64_t value) {
  int64_t r = value % block_size;
  return r > 0 ? (value + block_size - r) : value;
}

std::wstring SanitizePath(const std::wstring& path) {
  size_t start = 0;
  size_t end = path.size();
  if (path.size() > 1 && path[0] == '\\' && path[1] == '\\') {
    ++start;
  }
  if (path.size() > 2 && path[path.size() - 1] == '\\')  {
    --end;
  }
  return path.substr(start, end);
}

std::wstring StripLastPathComponent(const std::wstring& path) {
  size_t index = path.rfind('\\');
  // It should already be a "sanitized" path.
  assert(index != path.size() - 1 || path.size() == 1);
  if (index == std::string::npos) {
    index = path.rfind('/');
  }
  if (index == 0 || index == std::string::npos) {
    return L"\\";
  }
  return path.substr(0, index);
}

std::wstring ReplaceLastPathComponent(const std::wstring& path,
                                      const std::wstring& name) {
  assert(path.size() > 1);
  std::wstring stripped = StripLastPathComponent(path);
  assert(!stripped.empty());
  return stripped.size() == 1 ? (stripped + name) : (stripped + L'\\' + name);
}

void SplitAlternateStreamName(std::wstring* path, std::wstring* stream_name) {
  size_t colon_index = path->find(':');
  if (colon_index == std::string::npos) {
    *stream_name = L"";
    return;
  }
  *stream_name = path->substr(colon_index + 1);
  *path = path->substr(0, colon_index);
}

std::wstring MakeDevicePath(const std::wstring& device_name) {
  assert(!device_name.empty() && device_name[0] == L'\\');
  return std::wstring(L"\\\\?") + device_name;
}

std::wstring MakeDevicePath(const std::wstring& device_name,
                            const std::wstring& subpath) {
  if (subpath.empty() || subpath[0] == '\\') {
    return MakeDevicePath(device_name) + subpath;
  }
  return MakeDevicePath(device_name) + L'\\' + subpath;
}

bool CheckDriverVersion(const GUID& driver_version, Logger* logger) {
  GUID user_version = DOKAN_DRIVER_VERSION;
  if (!IsEqualGUID(driver_version, user_version)) {
    char driver_version_str[50];
    UUIDToString(driver_version, driver_version_str);
    char user_version_str[50];
    UUIDToString(user_version, user_version_str);
    DOKAN_LOG_ERROR(logger, "Driver version %s does not match DLL version %s",
                    driver_version_str, user_version_str);
    return false;
  }
  return true;
}

bool Narrow(const std::wstring& str, std::string* out) {
  static std::mutex converter_mutex;
  static std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> converter;
  std::lock_guard<std::mutex> auto_lock(converter_mutex);
  try {
    *out = converter.to_bytes(str);
  } catch (const std::range_error& exception) {
    return false;
  }
  return true;
}

}  // namespace util
}  // namespace dokan
