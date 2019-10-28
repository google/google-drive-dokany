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

#ifndef DOKAN_DEVICE_H_
#define DOKAN_DEVICE_H_

#include <memory>
#include <string>
#include <vector>
#include <windows.h>

#include "api.h"
#include "logger.h"

namespace dokan {

// A Device is a wrapper for a device handle, which manages the lifetime of the
// handle and provides convenient functions for issuing control requests.
class Device {
 public:
  explicit Device(Logger* logger) : logger_(logger) {}

  Device(const Device&) = delete;
  Device& operator=(const Device&) = delete;

  // Opens the device with the given name, which logs errors to the given
  // logger. If successful, the handle stays open until the Device object is
  // destroyed.
  DOKANCC_API bool Open(const std::wstring& name);

  bool is_open() const {
    return handle_ != INVALID_HANDLE_VALUE;
  }

  // Closes the device, if it has been opened.
  DOKANCC_API ~Device();

  // Invokes a control function on the device that takes no input and returns no
  // output.
  DOKANCC_API bool Control(ULONG ioctl);

  // Invokes an FSCTL control function on a specific file object owned by the
  // device, which takes no input and returns no ouptut.
  DOKANCC_API bool Control(HANDLE file_handle, ULONG fsctl);

  // Invokes a control function on the device that takes fixed-size input and
  // returns no output.
  template <typename T>
  bool Control(ULONG ioctl, const T& input) {
    return Control(ioctl, &input, sizeof(T));
  }

  // Invokes a control function on the device that takes variable-size input
  // and returns no output.
  template <typename T>
  bool Control(ULONG ioctl, const T* input, size_t size) {
    DWORD bytes_returned = 0;
    bool result = DeviceIoControl(handle_, ioctl, (LPVOID)input, size,
                                  nullptr, 0, &bytes_returned, nullptr);
    if (!result) {
      LogGenericResult(ioctl);
    }
    return result;
  }

  // Invokes a control function on the device that takes input and returns a
  // fixed-size output structure. If the output structure is practically
  // variable-length (e.g. has a one-char placeholder for a variable-length
  // string), this won't work.
  template <typename T, typename U>
  bool Control(ULONG ioctl, const T& input, U* output) {
    return Control(ioctl, input, reinterpret_cast<char*>(output), sizeof(U));
  }

  // Invokes a control function on the device that takes input and returns data
  // of the specified size.
  template <typename T>
  bool Control(ULONG ioctl, const T& input, char* output, ULONG output_size) {
    DWORD bytes_returned = 0;
    bool result = DeviceIoControl(handle_, ioctl, (LPVOID)&input, sizeof(input),
                                  output, output_size, &bytes_returned,
                                  nullptr);
    if (!result) {
      DOKAN_LOG_ERROR(logger_, "IOCTL 0x%x failed on device %S; error: %u",
                      ioctl, name_.c_str(), GetLastError());
    } else if (bytes_returned != output_size) {
      DOKAN_LOG_INFO(logger_,
                     "IOCTL 0x%x on device %S returned %u bytes; expected %u",
                     ioctl, name_.c_str(), bytes_returned, output_size);
    }
    return result;
  }

  // Invokes a control function on the device that takes no input and returns
  // a variable-length output structure. The output vector passed in should
  // be pre-sized to fit the expected data. This function does not change
  // output->size() and does not yield an actual_output_size greater than that.
  DOKANCC_API bool ControlAsync(ULONG ioctl, std::vector<char>* output,
                                OVERLAPPED* overlapped);

  // Retrieves the result from a ControlAsync request. This should not be done
  // until the event in the overlapped structure is signalled.
  DOKANCC_API bool GetAsyncResult(OVERLAPPED* overlapped,
                                  DWORD* actual_output_size, DWORD* error);

 private:
  DOKANCC_API void LogGenericResult(ULONG ioctl);

  std::wstring name_;
  HANDLE handle_ = INVALID_HANDLE_VALUE;
  Logger* const logger_;
};

}  // namespace dokan

#endif // DOKAN_DEVICE_H_
