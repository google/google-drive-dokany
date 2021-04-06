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

#include "device.h"

#include <sstream>

#include "hex_util.h"
#include "util.h"

namespace dokan {

Device::~Device() {
  if (handle_ != INVALID_HANDLE_VALUE) {
    CloseHandle(handle_);
  }
}

bool Device::OpenGlobalDevice() {
  std::wostringstream device_name_stream;
  device_name_stream << L"\\\\.\\" << DOKAN_DEVICE_PREFIX_NAME << L"_"
                     << DOKAN_BUILD_LABEL;
  const std::wstring device = device_name_stream.str();
  bool opened = Open(device);
  if (opened) {
    DOKAN_LOG_(INFO) << "Opened global dokan device: " << device;
  }
  return opened;
}

bool Device::Open(const std::wstring& name) {
  handle_ = CreateFile(name.c_str(), desired_access_,
                       FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
                       FILE_FLAG_OVERLAPPED, NULL);
  name_ = name;
  if (handle_ == INVALID_HANDLE_VALUE) {
    DOKAN_LOG_(ERROR) << "Failed to open device " << name << "; error "
                      << GetLastError();
    return false;
  }
  return true;
}

bool Device::Control(ULONG ioctl) {
  ULONG output_size = 0;
  bool result = DeviceIoControl(handle_, ioctl, nullptr, 0, nullptr, 0,
                                &output_size, nullptr);
  if (!result) {
    LogGenericResult(ioctl);
  }
  return result;
}

bool Device::Control(HANDLE file_handle, ULONG fsctl) {
  ULONG output_size = 0;
  bool result = DeviceIoControl(file_handle, fsctl, nullptr, 0, nullptr, 0,
                                &output_size, nullptr);
  if (!result) {
    LogGenericResult(fsctl);
  }
  return result;
}

bool Device::ControlAsync(ULONG ioctl, std::vector<char>* output,
                          OVERLAPPED* overlapped) {
  bool result = DeviceIoControl(handle_, ioctl, nullptr, 0, output->data(),
                                output->size(), nullptr, overlapped);
  if (!result && GetLastError() == ERROR_IO_PENDING) {
    result = true;
  }
  if (!result) {
    LogGenericResult(ioctl);
  }
  return result;
}

bool Device::GetAsyncResult(OVERLAPPED* overlapped,
                            DWORD* actual_output_size,
                            DWORD* error) {
  if (!GetOverlappedResult(handle_, overlapped, actual_output_size, FALSE)) {
    *error = GetLastError();
    if (*error == ERROR_OPERATION_ABORTED) {
      DOKAN_LOG_(INFO) << "Async IOCTL aborted. This is normal during unmount.";
    } else if (*error == ERROR_OPERATION_IN_PROGRESS) {
      DOKAN_LOG_(ERROR)
          << "GetAsyncResult was called without waiting for completion.";
    } else {
      DOKAN_LOG_(ERROR) << "Async IOCTL failed to produce result, error = "
                        << *error;
    }
    *actual_output_size = 0;
    return false;
  }
  return true;
}

void Device::LogGenericResult(ULONG ioctl) {
  DWORD error = GetLastError();
  if (error == ERROR_FILE_NOT_FOUND) {
    DOKAN_LOG_(INFO) << "The device " << name_
                     << " has been unmounted and did not respond to IOCTL "
                     << Hex(ioctl);
  } else {
    DOKAN_LOG_(ERROR) << "IOCTL " << Hex(ioctl) << " failed on device " << name_
                      << "; error: " << GetLastError();
  }
}

void Device::SetDesiredAccess(DWORD desired_access) {
  desired_access_ = desired_access;
}

}  // namespace dokan
