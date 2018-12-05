#include "volume_info_handler.h"

#include <algorithm>
#include <cassert>

namespace dokan {
namespace {

// Copies info that is known at mount time, which satisfies some request info
// classes. If source_size is larger than sizeof(T) then T has a trailing
// dynamic-length string, which this function truncates in the copy if necessary
// to stay within reply->BufferLength.
template <typename T>
void CopyConstantInfo(const T& source, ULONG source_size,
                      EVENT_INFORMATION* reply, NTSTATUS* status,
                      ULONG* used_buffer_size) {
  if (reply->BufferLength < sizeof(T)) {
    *status = STATUS_BUFFER_OVERFLOW;
    return;
  }
  *used_buffer_size = std::min(reply->BufferLength, source_size);
  memcpy(reply->Buffer, &source, *used_buffer_size);
  *status = STATUS_SUCCESS;
}

// Fills the info that is common to both types of *_SIZE_INFORMATION structs,
// using the data in the given FreeSpace object.
template <typename T>
void FillCommonVolumeSizeInfo(const FreeSpace& free_space,
                              uint32_t allocation_unit_size,
                              T* info) {
  info->TotalAllocationUnits.QuadPart = free_space.total_bytes /
      allocation_unit_size;
  info->SectorsPerAllocationUnit = 1;
  info->BytesPerSector = allocation_unit_size;
}

// Fills a reply buffer for one of the *SizeInformation info classes, using the
// data in the given FreeSpace object. If the buffer is too small for the
// appropriate *_SIZE_INFORMATION struct, then this function returns false and
// does nothing; otherwise, it returns true and sets used_buffer_size to the
// fixed size of the struct.
bool FillSizeInfo(const FreeSpace& free_space, ULONG info_class,
                  uint32_t allocation_unit_size, char* buffer,
                  ULONG buffer_size, ULONG* used_buffer_size) {
  if (info_class == FileFsFullSizeInformation) {
    if (buffer_size < sizeof(FILE_FS_FULL_SIZE_INFORMATION)) {
      return false;
    }
    auto info = reinterpret_cast<FILE_FS_FULL_SIZE_INFORMATION*>(buffer);
    FillCommonVolumeSizeInfo(free_space, allocation_unit_size, info);
    info->CallerAvailableAllocationUnits.QuadPart =
        free_space.free_bytes_for_process_user / allocation_unit_size;
    info->ActualAvailableAllocationUnits.QuadPart =
        free_space.free_bytes_for_all_users / allocation_unit_size;
    *used_buffer_size = sizeof(FILE_FS_FULL_SIZE_INFORMATION);
  } else {
    if (buffer_size < sizeof(FILE_FS_SIZE_INFORMATION)) {
      return false;
    }
    auto info = reinterpret_cast<FILE_FS_SIZE_INFORMATION*>(buffer);
    FillCommonVolumeSizeInfo(free_space, allocation_unit_size, info);
    info->AvailableAllocationUnits.QuadPart =
        free_space.free_bytes_for_process_user / allocation_unit_size;
    *used_buffer_size = sizeof(FILE_FS_SIZE_INFORMATION);
  }
  return true;
}

}  // namespace

void VolumeInfoHandler::GetVolumeInfo(
    const EVENT_CONTEXT* request,
    ULONG info_class,
    util::UniqueVarStructPtr<EVENT_INFORMATION> reply,
    const ReplyFn& reply_fn) {
  NTSTATUS status = STATUS_INVALID_PARAMETER;
  ULONG used_buffer_size = 0;
  switch (info_class) {
    // TODO(drivefs-team): Move these 2 cases to the driver, since all they do
    // is copy data that is available at mount time.
    case FileFsVolumeInformation:
      CopyConstantInfo(*volume_info_, volume_info_size_, reply.get(),
                       &status, &used_buffer_size);
      if (status == STATUS_SUCCESS && used_buffer_size < volume_info_size_) {
        // Decrement the volume name length in the struct, since by default it's
        // a copy of the length in the source struct.
        reinterpret_cast<FILE_FS_VOLUME_INFORMATION*>(reply->Buffer)
            ->VolumeLabelLength -= volume_info_size_ - used_buffer_size;
      }
      reply_fn(status, used_buffer_size, std::move(reply));
      return;
    case FileFsAttributeInformation:
      CopyConstantInfo(*attribute_info_, attribute_info_size_, reply.get(),
                       &status, &used_buffer_size);
      if (status == STATUS_SUCCESS && used_buffer_size < attribute_info_size_) {
        // Decrement the FS name length in the struct, since by default it's a
        // copy of the length in the source struct.
        reinterpret_cast<FILE_FS_ATTRIBUTE_INFORMATION*>(reply->Buffer)
            ->FileSystemNameLength -= attribute_info_size_ - used_buffer_size;
      }
      reply_fn(status, used_buffer_size, std::move(reply));
      return;
    case FileFsSizeInformation:
    case FileFsFullSizeInformation: {
      auto raw_free_space = new FreeSpace{0};
      auto raw_reply = reply.release();
      callbacks_->GetVolumeFreeSpace(
          request->ProcessId, raw_free_space,
          [=](NTSTATUS status) {
            auto unique_reply = util::MakeUniqueVarStruct(raw_reply);
            std::unique_ptr<FreeSpace> free_space(raw_free_space);
            if (status != STATUS_SUCCESS) {
              reply_fn(status, 0, std::move(unique_reply));
              return;
            }
            ULONG used_buffer_size = 0;
            const bool fit = FillSizeInfo(
                *free_space, info_class, allocation_unit_size_,
                reinterpret_cast<char*>(unique_reply->Buffer),
                unique_reply->BufferLength, &used_buffer_size);
            reply_fn(fit ? STATUS_SUCCESS : STATUS_BUFFER_OVERFLOW,
                     used_buffer_size, std::move(unique_reply));
          });
      return;
    }
    default:
      DOKAN_LOG_ERROR(
          logger_,
          "Bypassing GetVolumeInfo with unsupported info class: 0x%x",
          info_class);
      assert(false);
      reply_fn(STATUS_INVALID_PARAMETER, 0, std::move(reply));
      return;
  }
}

}  // namespace dokan
