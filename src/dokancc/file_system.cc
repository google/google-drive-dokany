#include "file_system.h"

#include <dbt.h>
#include <ShlObj.h>

#include <algorithm>
#include <cassert>

#include "util.h"

namespace dokan {

namespace {

const wchar_t kKeepaliveFileName[] = L"\\__drive_fs_keepalive";

// Determines whether the given status represents either success or a benign
// error from CreateFile.
bool IsNormalCreateResult(NTSTATUS status, ULONG disposition) {
  return status == STATUS_SUCCESS ||
      (status == STATUS_OBJECT_NAME_COLLISION &&
          (disposition == FILE_OPEN_IF || disposition == FILE_SUPERSEDE ||
           disposition == FILE_OVERWRITE_IF));
}

bool IsDriveLetterInUse(wchar_t letter) {
  return (GetLogicalDrives() & (1 << (toupper(letter) - 'A'))) != 0;
}

bool PrepareMountRequest(const std::wstring& requested_mount_point,
                         const StartupOptions& options,
                         Logger* logger,
                         EVENT_START* mount_request) {
  assert(!requested_mount_point.empty());
  memset(mount_request, 0, sizeof(EVENT_START));
  mount_request->UserVersion = DOKAN_DRIVER_VERSION;
  mount_request->BaseVolumeGuid = DOKAN_DRIVER_VERSION;
  mount_request->Flags = DOKAN_EVENT_MOUNT_MANAGER |
                         DOKAN_EVENT_RESOLVE_MOUNT_CONFLICTS;
  if (IsDriveLetterInUse(requested_mount_point[0])) {
    mount_request->Flags |= DOKAN_EVENT_DRIVE_LETTER_IN_USE;
  }
  if (options.flags & DOKAN_OPTION_ALT_STREAM) {
    mount_request->Flags |= DOKAN_EVENT_ALTERNATIVE_STREAM_ON;
  }
  if (options.flags & DOKAN_OPTION_REMOVABLE) {
    mount_request->Flags |= DOKAN_EVENT_REMOVABLE;
  }
  if (options.flags & DOKAN_OPTION_WRITE_PROTECT) {
    mount_request->Flags |= DOKAN_EVENT_WRITE_PROTECT;
  }
  if (options.flags & DOKAN_OPTION_FILELOCK_USER_MODE) {
    mount_request->Flags |= DOKAN_EVENT_FILELOCK_USER_MODE;
  }
  if (options.flags & DOKAN_OPTION_LOCK_DEBUG_ENABLED) {
    mount_request->Flags |= DOKAN_EVENT_LOCK_DEBUG_ENABLED;
  }
  if (options.flags & DOKAN_OPTION_ENABLE_OPLOCKS) {
    mount_request->Flags |= DOKAN_EVENT_ENABLE_OPLOCKS;
  }
  if (options.flags & DOKAN_OPTION_LOG_OPLOCKS) {
    mount_request->Flags |= DOKAN_EVENT_LOG_OPLOCKS;
  }
  if (options.flags & DOKAN_OPTION_OPTIMIZE_SINGLE_NAME_SEARCH) {
    mount_request->Flags |= DOKAN_EVENT_OPTIMIZE_SINGLE_NAME_SEARCH;
  }
  memcpy_s(mount_request->MountPoint, sizeof(mount_request->MountPoint),
           requested_mount_point.c_str(),
           requested_mount_point.size() * sizeof(wchar_t));
  mount_request->IrpTimeout = options.request_timeout_millis;
  mount_request->VolumeSecurityDescriptorLength =
      options.volume_security_descriptor_length;
  if (mount_request->VolumeSecurityDescriptorLength >
      VOLUME_SECURITY_DESCRIPTOR_MAX_SIZE) {
    DOKAN_LOG_ERROR(logger, "Volume security descriptor is too large."
                     " Actual: %u, supported: %u",
                     mount_request->VolumeSecurityDescriptorLength,
                     VOLUME_SECURITY_DESCRIPTOR_MAX_SIZE);
    return false;
  } else if (mount_request->VolumeSecurityDescriptorLength != 0) {
    memcpy(mount_request->VolumeSecurityDescriptor,
           options.volume_security_descriptor,
           mount_request->VolumeSecurityDescriptorLength);
  }
  return true;
}

using std::placeholders::_1;
using std::placeholders::_2;
using std::placeholders::_3;
using std::placeholders::_4;

// Initializes a fixed-size reply object during the completion phase of a
// request.
void PrepareReply(NTSTATUS status, EVENT_CONTEXT* request,
                  EVENT_INFORMATION* reply) {
  memset(reply, 0, sizeof(EVENT_INFORMATION));
  reply->SerialNumber = request->SerialNumber;
  reply->Status = status;
  reply->Context = request->Context;
}

// Initializes a fixed-size reply object pertaining to a known FileHandle during
// the completion phase of a request.
void PrepareReply(NTSTATUS status, EVENT_CONTEXT* request, FileHandle* handle,
                  EVENT_INFORMATION* reply) {
  PrepareReply(status, request, reply);
  reply->Context = (ULONG64)handle;
}

// Computes the size of a variable-sized driver reply if the given size is used
// for the variable-sized Buffer inside it. The result is never smaller than
// sizeof(EVENT_INFORMATION); if the buffer_size is less than 8 then the default
// reply size with a fixed 8-byte buffer is used.
size_t ComputeVarReplySize(size_t buffer_size) {
  return std::max(
      sizeof(EVENT_INFORMATION),
      sizeof(EVENT_INFORMATION) - 8 + buffer_size);
}

// Constructs a reply whose size is bigger than the declared size of
// EVENT_INFORMATION, due to additional inline data at the end. For the types of
// requests that need this, the reply must be prepared via this function before
// invoking FileCallbacks. If a FileCallbacks function is invoked, the pointer
// must be release()'d beforehand, and MakeUniqueVarStruct can then be called to
// re-establish ownership after the function replies.
util::UniqueVarStructPtr<EVENT_INFORMATION> PrepareVarReply(
    EVENT_CONTEXT* request, size_t size) {
  auto reply = util::MakeUniqueVarStruct<EVENT_INFORMATION>(size);
  reply->SerialNumber = request->SerialNumber;
  reply->Context = request->Context;
  reply->BufferLength = size - (sizeof(EVENT_INFORMATION) - 8);
  return reply;
}

}  // anonymous namespace

FileSystem::FileSystem(FileCallbacks* file_callbacks,
                       VolumeCallbacks* volume_callbacks,
                       Logger* logger)
      : file_callbacks_(file_callbacks),
        volume_callbacks_(volume_callbacks),
        logger_(logger),
        driver_log_subscriber_(logger),
        io_buffer_(EVENT_CONTEXT_MAX_SIZE),
        global_device_(logger),
        device_(logger) {
  memset(&overlapped_, 0, sizeof(OVERLAPPED));
  overlapped_.hEvent = CreateEvent(
      nullptr, true, false, L"FileSystem::ReceiveIo");
  notification_handler_ = CreateNotificationHandler(this, logger);
  if (!driver_log_subscriber_.Start()) {
    DOKAN_LOG_ERROR(logger_, "Not capturing driver log messages.");
  }
}

MountResult FileSystem::Mount(const std::wstring& requested_mount_point,
                              const StartupOptions& startup_options) {
  assert(!mounted_);
  startup_options_ = startup_options;
  io_thread_id_ = GetCurrentThreadId();
  change_handler_.reset(new ChangeHandler(file_callbacks_, logger_));
  file_info_handler_.reset(
      new FileInfoHandler(file_callbacks_, logger_, &startup_options_));
  volume_info_handler_.reset(
      new VolumeInfoHandler(volume_callbacks_, logger_, &startup_options_));
  find_handler_.reset(
      new FindHandler(file_callbacks_, logger_, file_info_handler_.get()));
  DOKAN_LOG_INFO(logger_, "Mounting file system with requested mount point: %S",
                 requested_mount_point.c_str());
  if (!global_device_.Open(DOKAN_GLOBAL_DEVICE_NAME)) {
    return MountResult::FAILED_TO_OPEN_GLOBAL_DEVICE;
  }
  DOKAN_LOG_INFO(logger_, "Opened global dokan device: %S",
                 DOKAN_GLOBAL_DEVICE_NAME);
  EVENT_START mount_request = {0};
  if (!PrepareMountRequest(requested_mount_point, startup_options_, logger_,
                           &mount_request)) {
    DOKAN_LOG_ERROR(logger_, "Failed to prepare mount request.");
    return MountResult::FAILED_TO_PREPARE_MOUNT_REQUEST;
  }
  EVENT_DRIVER_INFO mount_response = {0};
  bool request_sent = global_device_.Control(
      IOCTL_EVENT_START, mount_request, &mount_response);
  if (!request_sent) {
    DOKAN_LOG_ERROR(logger_, "Failed to issue mount request.");
    return MountResult::FAILED_TO_ISSUE_MOUNT_REQUEST;
  }
  device_name_ = mount_response.DeviceName;
  driver_mount_id_ = mount_response.MountId;
  const std::wstring raw_device_name = L"\\\\." + device_name_;
  if (!device_.Open(raw_device_name)) {
    return MountResult::FAILED_TO_OPEN_MOUNTED_DEVICE;
  }

  if (mount_response.Status == DOKAN_MOUNTED) {
    mount_point_ = L"";
    mount_point_ += mount_response.ActualDriveLetter;
    mount_point_ += L':';
    DOKAN_LOG_INFO(logger_, "Successfully mounted device name: %S"
                   " with mount point: %S",
                   mount_response.DeviceName,
                   mount_point_.c_str());
    mounted_ = true;
    volume_callbacks_->Mounted(this);
    DOKAN_LOG_INFO(logger_, "Mounted callback returned.");
    reply_handler_.reset(new ReplyHandler(&device_, logger_,
                                          startup_options_.reply_thread_count));
    post_start_thread_.reset(new std::thread([this] {
      PostStart();
    }));
    return MountResult::SUCCESS;
  }

  // In any case below here, we are going to cancel.
  if (mount_response.Status == DOKAN_START_FAILED) {
    if (!util::CheckDriverVersion(mount_response.DriverVersion, logger_)) {
      return MountResult::DRIVER_VERSION_MISMATCH;
    }
    DOKAN_LOG_ERROR(logger_, "Generic mount error returned from driver.");
    return MountResult::GENERIC_FAILURE;
  }
  DOKAN_LOG_ERROR(logger_,
                  "Unrecognized result from driver; assuming failure: %x",
                  mount_response.Status);
  return MountResult::UNRECOGNIZED_DRIVER_MOUNT_RESPONSE;
}

void FileSystem::BroadcastExistenceToApps(bool exists) {
  // We make user32 calls in here, which are deadlock-prone when done from the
  // critical path of a file system.
  if (exists) {
    AssertNotCalledOnIoThread();
  }
  assert(!mount_point_.empty());
  // Note: this means it was ever mounted. After that, mount_point_ indicates
  // the actual assigned drive.
  assert(mounted_);
  assert(mount_point_[0] >= 'A' && mount_point_[0] <= 'Z');
  DEV_BROADCAST_VOLUME params;
  memset(&params, 0, sizeof(params));
  params.dbcv_size = sizeof(params);
  params.dbcv_devicetype = DBT_DEVTYP_VOLUME;
  params.dbcv_unitmask = (1 << (toupper(mount_point_[0]) - 'A'));
  DWORD receipients = BSM_APPLICATIONS;
  DWORD device_event = exists ? DBT_DEVICEARRIVAL : DBT_DEVICEREMOVECOMPLETE;
  long result = BroadcastSystemMessage(
      BSF_NOHANG | BSF_FORCEIFHUNG | BSF_NOTIMEOUTIFNOTHUNG, &receipients,
      WM_DEVICECHANGE, device_event, reinterpret_cast<LPARAM>(&params));
  if (result <= 0) {
    DOKAN_LOG_ERROR(logger_, "Failed to broadcast drive existence: %u",
                    GetLastError());
  }
  SHChangeNotify(exists ? SHCNE_DRIVEADD : SHCNE_DRIVEREMOVED, SHCNF_PATH,
                 mount_point_.c_str(), nullptr);
}

FileSystem::~FileSystem() {
  if (!mounted_) {
    return;
  }
  AssertCalledOnIoThread();
  assert(io_stopped_);
  assert(pending_requests_.empty());
  assert(!(post_start_thread_ && post_start_thread_->joinable()));
}

void FileSystem::AssertCalledOnIoThread() const {
  assert(GetCurrentThreadId() == io_thread_id_);
}

void FileSystem::AssertNotCalledOnIoThread() const {
  assert(GetCurrentThreadId() != io_thread_id_);
}

void FileSystem::Unmount() {
  if (!mounted_ || unmount_requested_.exchange(true)) {
    return;
  }
  DOKAN_LOG_INFO(logger_, "Unmounting the file system.");
  if (device_.Control(IOCTL_EVENT_RELEASE)) {
    DOKAN_LOG_INFO(logger_, "Sent unmount request to driver.");
  } else {
    DOKAN_LOG_INFO(logger_, "Failed to send unmount request to driver.");
  }
}

void FileSystem::MaybeFinishUnmount() {
  AssertCalledOnIoThread();
  if (io_stopped_ && pending_requests_.empty()) {
    post_start_thread_->join();
    reply_handler_->Shutdown();
    BroadcastExistenceToApps(false);
    DOKAN_LOG_INFO(logger_, "There are no more pending I/O requests.");
    volume_callbacks_->Unmounted();
    DOKAN_LOG_INFO(logger_, "Unmounted callback completed.");
  }
}

void FileSystem::PostStart() {
  if (!WaitUntilSafeToAccess()) {
    return;
  }
  const std::wstring keepalive_path = util::MakeDevicePath(
      device_name_, kKeepaliveFileName);
  keepalive_handle_ = CreateFile(
      keepalive_path.c_str(), GENERIC_READ, 0, NULL, OPEN_EXISTING, 0, NULL);
  if (keepalive_handle_ == INVALID_HANDLE_VALUE) {
    // We don't consider this a fatal error because the keepalive handle is only
    // needed for abnormal termination cases anyway.
    DOKAN_LOG_ERROR(logger_, "Failed to open keepalive file: %S",
                    keepalive_path.c_str());
  } else {
    DWORD keepalive_bytes_returned = 0;
    BOOL keepalive_active = device_.Control(keepalive_handle_,
                                            FSCTL_ACTIVATE_KEEPALIVE);
    if (!keepalive_active) {
      DOKAN_LOG_ERROR(logger_, "Failed to activate keepalive handle.");
    }
  }

  BroadcastExistenceToApps(true);
  DOKAN_LOG_INFO(logger_, "PostStart completed.");
  util::SetAndNotify(&mutex_, &state_, &post_start_done_);
}

bool FileSystem::ReceiveIo() {
  AssertCalledOnIoThread();
  if (!device_.ControlAsync(IOCTL_EVENT_WAIT, &io_buffer_, &overlapped_)) {
    util::SetAndNotify(&mutex_, &state_, &io_stopped_);
    DOKAN_LOG_INFO(logger_, "The file system has stopped receiving I/O.");
    MaybeFinishUnmount();
    return false;
  }
  if (!safe_to_access_) {
    util::SetAndNotify(&mutex_, &state_, &safe_to_access_);
    DOKAN_LOG_INFO(logger_, "The file system is now safe to access.");
    state_.notify_all();
  }
  return true;
}

void FileSystem::DispatchIo() {
  AssertCalledOnIoThread();

  // It might be ideal if we could pull back a batch of IRPs in here, at least
  // in case more come in after we get signalled that there is one waiting
  // request. However, the driver doesn't really work that way right now. It
  // wants to match one IOCTL_EVENT_WAIT up to one real IRP.
  DWORD actual_buffer_size = 0;
  bool success = device_.GetAsyncResult(&overlapped_, &actual_buffer_size);
  if (!success || actual_buffer_size == 0) {
    return;
  }

  EVENT_CONTEXT* original_request = reinterpret_cast<EVENT_CONTEXT*>(
      &io_buffer_[0]);
  if (original_request->MountId != driver_mount_id_) {
    DOKAN_LOG_ERROR(logger_,
                    "Received request targeting mount ID %u, but expected %u",
                    original_request->MountId, driver_mount_id_);
    return;
  }

  // Copy the request because the I/O buffer could get reused while this
  // request is pending. We could use a buffer pool or something to read into
  // a buffer that's reserved until completion, but we're keeping it simple
  // for now.
  EVENT_CONTEXT* request = reinterpret_cast<EVENT_CONTEXT*>(
      new char[actual_buffer_size]);
  memcpy(request, original_request, actual_buffer_size);
  pending_requests_.insert(request);
  DispatchFn dispatch = GetDispatchFn(request->MajorFunction);
  if (dispatch) {
    (this->*dispatch)(request);
  } else {
    // TODO(drivefs-team): Remove this when there is a dispatch function for
    // every IRP.
    DOKAN_LOG_INFO(logger_,
                   "No function for IRP: %u",
                   static_cast<uint32_t>(request->MajorFunction));
    auto reply = util::MakeUniqueVarStruct<EVENT_INFORMATION>(
        sizeof(EVENT_INFORMATION));
    PrepareReply(STATUS_INVALID_PARAMETER, request, reply.get());
    ReplyToDriver(request, std::move(reply), sizeof(EVENT_INFORMATION));
  }
}

bool FileSystem::WaitUntilSafeToAccess() {
  if (safe_to_access_) {
    return true;
  }
  std::unique_lock<std::mutex> lock(mutex_);
  state_.wait(lock, [&] {
    if (safe_to_access_ || io_stopped_) {
      return true;
    }
    DOKAN_LOG_INFO(logger_,
                   "Waiting for the file system to be safe to access...");
    return false;
  });
  return safe_to_access_;
}

void FileSystem::WaitUntilPostStartDone() {
  if (post_start_done_) {
    return;
  }
  std::unique_lock<std::mutex> lock(mutex_);
  state_.wait(lock, [&] {
    if (!post_start_done_) {
      DOKAN_LOG_INFO(logger_, "Waiting for PostStart to be done...");
      return false;
    }
    return true;
  });
}

FileSystem::DispatchFn FileSystem::GetDispatchFn(ULONG major_irp_function) {
  switch (major_irp_function) {
    case IRP_MJ_CREATE: return &FileSystem::DispatchCreate;
    case IRP_MJ_CLEANUP: return &FileSystem::DispatchCleanup;
    case IRP_MJ_CLOSE: return &FileSystem::DispatchClose;
    case IRP_MJ_DIRECTORY_CONTROL: return &FileSystem::DispatchFindFiles;
    case IRP_MJ_SET_INFORMATION: return &FileSystem::DispatchChange;
    case IRP_MJ_QUERY_INFORMATION: return &FileSystem::DispatchGetInfo;
    case IRP_MJ_QUERY_SECURITY: return &FileSystem::DispatchGetSecurity;
    case IRP_MJ_QUERY_VOLUME_INFORMATION:
        return &FileSystem::DispatchGetVolumeInfo;
    case IRP_MJ_READ: return &FileSystem::DispatchRead;
    case IRP_MJ_WRITE: return &FileSystem::DispatchWrite;
    case IRP_MJ_FLUSH_BUFFERS: return &FileSystem::DispatchFlush;
    default: return nullptr;
  }
}

void FileSystem::DispatchCreate(EVENT_CONTEXT* request) {
  AssertCalledOnIoThread();
  char* request_data = reinterpret_cast<char*>(&request->Operation.Create);
  const std::wstring file_name = util::SanitizePath(reinterpret_cast<wchar_t*>(
      (request_data + request->Operation.Create.FileNameOffset)));

  // The high 8 bits of the CreateOptions represent the disposition.
  const ULONG disposition =
      (request->Operation.Create.CreateOptions >> 24) & 0xff;

  // The low 24 bits of the CreateOptions are the actual create options.
  const DWORD options = request->Operation.Create.CreateOptions &
      FILE_VALID_OPTION_FLAGS;

  FileHandle* const handle = new FileHandle(file_name, request->ProcessId,
                                            options & FILE_DIRECTORY_FILE);
  if ((options & FILE_NON_DIRECTORY_FILE) && (options & FILE_DIRECTORY_FILE)) {
    DOKAN_LOG_ERROR(logger_,
                    "Both FILE_NON_DIRECTORY_FILE and FILE_DIRECTORY_FILE were"
                    " specified in the same create request.");
    CompleteCreate(request, handle, disposition, options,
                   STATUS_INVALID_PARAMETER);
    return;
  }

  const DWORD desired_access =
      request->Operation.Create.SecurityContext.DesiredAccess;
  const DWORD file_attributes = request->Operation.Create.FileAttributes;
  const DWORD share_access = request->Operation.Create.ShareAccess;
  if (!(request->Flags & SL_OPEN_TARGET_DIRECTORY)) {
    // This is the normal case.
    file_callbacks_->Create(
        handle, desired_access, file_attributes, share_access, disposition,
        options,
        [=](NTSTATUS status) {
          CompleteCreate(request, handle, disposition, options, status);
        });
    return;
  }

  // This is an edge case where we need to deal back the parent directory,
  // after going through the motions of doing a CreateFile for the specified
  // file. So this is a create/close for the specified directory, followed by
  // a create for parent. Note that this case is exercised for the destination
  // location when you do a simple MoveFile call.
  DOKAN_LOG_TRACE(
      logger_,
      "Create requested with SL_OPEN_TARGET_DIRECTORY for path %S",
      file_name.c_str());
  file_callbacks_->Create(
      handle, desired_access, file_attributes, share_access, disposition,
      options,
      [=](NTSTATUS status) {
        AssertCalledOnIoThread();
        bool child_exists = IsNormalCreateResult(status, disposition);
        auto dispatch_parent_create = [=] {
          FileHandle* mutable_handle = handle;
          RemoveReference(&mutable_handle);
          DispatchParentCreate(request, file_name, disposition, options,
                               desired_access, file_attributes, share_access);
        };
        if (!child_exists) {
          dispatch_parent_create();
          return;
        }
        file_callbacks_->Cleanup(handle, [=](NTSTATUS status) {
          AssertCalledOnIoThread();
          if (status != STATUS_SUCCESS) {
            CompleteCreate(request, handle, disposition, options, status);
            return;
          }
          dispatch_parent_create();
        });
      });
}

void FileSystem::DispatchParentCreate(EVENT_CONTEXT* request,
                                      const std::wstring& file_name,
                                      ULONG disposition,
                                      DWORD options,
                                      DWORD desired_access,
                                      DWORD file_attributes,
                                      DWORD share_access) {
  AssertCalledOnIoThread();
  // Note: it seems suspicious to not force the disposition to be some flavor of
  // OPEN, but the original code uses the disposition passed in.
  const std::wstring parent_file_name = util::StripLastPathComponent(file_name);
  const DWORD parent_options = (options | FILE_DIRECTORY_FILE) &
      ~FILE_NON_DIRECTORY_FILE;
  FileHandle* const parent_handle = new FileHandle(
      parent_file_name, request->ProcessId, true);
  file_callbacks_->Create(
      parent_handle, desired_access, file_attributes, share_access, disposition,
      parent_options,
      [=](NTSTATUS status) {
        CompleteCreate(request, parent_handle, disposition, options, status);
      });
}

void FileSystem::CompleteCreate(EVENT_CONTEXT* request,
                                FileHandle* handle,
                                ULONG disposition,
                                ULONG options,
                                NTSTATUS status) {
  AssertCalledOnIoThread();
  DOKAN_LOG_TRACE(logger_, "Completing create with callback status 0x%x for %S",
                  status, handle->path().c_str());
  auto reply = util::MakeUniqueVarStruct<EVENT_INFORMATION>(
      sizeof(EVENT_INFORMATION));
  PrepareReply(status, request, handle, reply.get());
  bool success = (status == STATUS_SUCCESS);
  if (!handle->is_root()) {
    success = IsNormalCreateResult(status, disposition);
  }
  if (success) {
    assert(handle->context());
    if (!handle->context()) {
      DOKAN_LOG_ERROR(logger_, "No context was set for %S with disposition %d,"
                      " options %d, status 0x%x", handle->path().c_str(),
                      disposition, options, status);
    }
    reply->Status = STATUS_SUCCESS;
    reply->Operation.Create.Information = FILE_OPENED;
    if (disposition == FILE_CREATE || disposition == FILE_OPEN_IF ||
        disposition == FILE_OVERWRITE_IF || disposition == FILE_SUPERSEDE) {
      reply->Operation.Create.Information = FILE_CREATED;
      if (status == STATUS_OBJECT_NAME_COLLISION) {
        if (disposition == FILE_OPEN_IF) {
          reply->Operation.Create.Information = FILE_OPENED;
        } else if (disposition == FILE_OVERWRITE_IF) {
          reply->Operation.Create.Information = FILE_OVERWRITTEN;
        } else if (disposition == FILE_SUPERSEDE) {
          reply->Operation.Create.Information = FILE_SUPERSEDED;
        }
      }
    }
    if (handle->directory()) {
      reply->Operation.Create.Flags |= DOKAN_FILE_DIRECTORY;
    }
    if (options & FILE_DELETE_ON_CLOSE) {
      handle->set_delete_on_cleanup(true);
    }
  } else {
    // Abnormal result.
    reply->Operation.Create.Information = FILE_DOES_NOT_EXIST;
    if (status == STATUS_OBJECT_NAME_COLLISION) {
      reply->Operation.Create.Information = FILE_EXISTS;
    }
    // The original code supports an edge case here where the CreateFile
    // callback returns STATUS_ACCESS_DENIED for DELETE access to a file but
    // allows FILE_DELETE_CHILD for the parent directory. For now at least, we
    // are assuming that logic is unneeded.
  }
  DOKAN_LOG_TRACE(
      logger_,
      "Replying to driver for Create with status 0x%x, info %u for path %S",
      reply->Status, reply->Operation.Create.Information,
      handle->path().c_str());
  if (reply->Status != STATUS_SUCCESS) {
    if (handle->context()) {
      DOKAN_LOG_ERROR(
          logger_,
          "Unsuccessful Create callback for %S left a context in the handle.",
          handle->path().c_str());
    }
    assert(handle->reference_count() == 1);
    RemoveReference(&handle);
    assert(!handle);
    reply->Context = 0;
  }
  ReplyToDriver(request, std::move(reply), sizeof(EVENT_INFORMATION));
}

void FileSystem::DispatchGetInfo(EVENT_CONTEXT* request) {
  AssertCalledOnIoThread();
  const ULONG info_class = request->Operation.File.FileInformationClass;
  const size_t reply_size = ComputeVarReplySize(
      request->Operation.File.BufferLength);
  util::UniqueVarStructPtr<EVENT_INFORMATION> reply = PrepareVarReply(
      request, reply_size);
  FileHandle* const handle = ReferenceHandle(request);
  if (handle == nullptr) {
    DOKAN_LOG_ERROR(logger_,
                   "DispatchGetInfo with info class %u bypassed due to null"
                   " file handle.", info_class);
    CompleteGetInfo(request, handle, info_class, STATUS_INVALID_PARAMETER, 0,
                    std::move(reply));
    return;
  }
  DOKAN_LOG_TRACE(logger_, "DispatchGetInfo for path %S and info class %u",
                  handle->path().c_str(), info_class);
  auto complete = std::bind(&FileSystem::CompleteGetInfo, this, request, handle,
                            info_class, _1, _2, _3);
  file_info_handler_->GetInfo(request, handle, info_class, std::move(reply),
                              complete);
}

void FileSystem::CompleteGetInfo(
    EVENT_CONTEXT* request,
    FileHandle* handle,
    ULONG info_class,
    NTSTATUS status,
    ULONG used_buffer_size,
    util::UniqueVarStructPtr<EVENT_INFORMATION> reply) {
  AssertCalledOnIoThread();
  if (handle != nullptr) {
    DOKAN_LOG_TRACE(
        logger_, "Replying to GetInfo for path %S and info class %u"
        " with status 0x%x and used buffer size %u", handle->path().c_str(),
        info_class, status, used_buffer_size);
  }
  CompleteRequestWithVarReply(
      request, handle, status, request->Operation.File.BufferLength,
      used_buffer_size, std::move(reply));
}

void FileSystem::DispatchGetSecurity(EVENT_CONTEXT* request) {
  // TODO(drivefs-team): Just handle this whole IRP directly in the driver.
  AssertCalledOnIoThread();
  FileHandle* handle = ReferenceHandle(request);
  const size_t buffer_length = request->Operation.Security.BufferLength;
  const size_t reply_size = ComputeVarReplySize(buffer_length);
  util::UniqueVarStructPtr<EVENT_INFORMATION> reply = PrepareVarReply(
      request, reply_size);
  if (startup_options_.volume_security_descriptor_length == 0) {
    CompleteRequestWithVarReply(
        request, handle, STATUS_NOT_SUPPORTED, buffer_length, 0,
        std::move(reply));
    DOKAN_LOG_TRACE(
        logger_,
        "DispatchGetSecurity failed because no security descriptor was set.");
    return;
  }
  PSECURITY_DESCRIPTOR descriptor = startup_options_.volume_security_descriptor;
  uint32_t descriptor_length =
      startup_options_.volume_security_descriptor_length;
  if (handle != nullptr && handle->use_readonly_security_descriptor()) {
    descriptor = startup_options_.readonly_security_descriptor;
    descriptor_length = startup_options_.readonly_security_descriptor_length;
  }
  if (reply->BufferLength < descriptor_length) {
    // We can't use CompleteRequestWithVarReply here because it has an assertion
    // that reply->BufferLength is no larger than the provided buffer length.
    // The driver uses reply->BufferLength to be the desired buffer length,
    // only in this one case.
    DOKAN_LOG_TRACE(
        logger_,
        "DispatchGetSecurity for path %S and security info 0x%x"
        " received insufficient buffer: %I32d; required size is %I32d",
        handle ? handle->path().c_str() : L"<unknown>",
        request->Operation.Security.SecurityInformation,
        reply->BufferLength,
        descriptor_length);
    reply->Status = STATUS_BUFFER_OVERFLOW;
    reply->BufferLength = descriptor_length;
    ReplyToDriver(request, std::move(reply), reply_size);
    RemoveReference(&handle);
    return;
  }
  memcpy(reply->Buffer, descriptor, descriptor_length);
  DOKAN_LOG_TRACE(
      logger_,
      "DispatchGetSecurity for path %S and security info 0x%x succeeded.",
      handle ? handle->path().c_str() : L"<unknown>",
      request->Operation.Security.SecurityInformation);
  CompleteRequestWithVarReply(
      request, handle, STATUS_SUCCESS, buffer_length, descriptor_length,
      std::move(reply));
}

void FileSystem::DispatchGetVolumeInfo(EVENT_CONTEXT* request) {
  AssertCalledOnIoThread();
  const ULONG info_class = request->Operation.Volume.FsInformationClass;
  const ULONG buffer_size = request->Operation.Volume.BufferLength;
  const ULONG reply_size = ComputeVarReplySize(buffer_size);
  util::UniqueVarStructPtr<EVENT_INFORMATION> reply = PrepareVarReply(
      request, reply_size);
  DOKAN_LOG_TRACE(logger_, "DispatchGetVolumeInfo with info class %u",
                  info_class);
  auto complete = std::bind(&FileSystem::CompleteGetVolumeInfo, this, request,
                            info_class, _1, _2, _3);
  volume_info_handler_->GetVolumeInfo(request, info_class, std::move(reply),
                                      complete);
}

void FileSystem::CompleteGetVolumeInfo(
    EVENT_CONTEXT* request,
    ULONG info_class,
    NTSTATUS status,
    ULONG used_buffer_size,
    util::UniqueVarStructPtr<EVENT_INFORMATION> reply) {
  AssertCalledOnIoThread();
  DOKAN_LOG_TRACE(
        logger_, "Replying to GetVolumeInfo for info class %u with status 0x%x"
        " and used buffer size %u", info_class, status, used_buffer_size);
  CompleteRequestWithVarReply(
      request, /*handle=*/nullptr, status,
      request->Operation.Volume.BufferLength, used_buffer_size,
      std::move(reply));
}

void FileSystem::DispatchChange(EVENT_CONTEXT* request) {
  AssertCalledOnIoThread();
  const ULONG info_class = request->Operation.SetFile.FileInformationClass;
  size_t reply_size = sizeof(EVENT_INFORMATION);
  if (info_class == FileRenameInformation) {
    auto rename_info = reinterpret_cast<DOKAN_RENAME_INFORMATION*>(
        reinterpret_cast<char*>(request) +
        request->Operation.SetFile.BufferOffset);
    reply_size += rename_info->FileNameLength;
  }
  util::UniqueVarStructPtr<EVENT_INFORMATION> reply = PrepareVarReply(
      request, reply_size);
  FileHandle* const handle = ReferenceHandle(request);
  if (handle == nullptr) {
    DOKAN_LOG_ERROR(logger_,
                   "DispatchChange with info class %u bypassed due to null"
                   " file handle.", info_class);
    CompleteChange(request, handle, info_class, STATUS_INVALID_PARAMETER,
                   reply_size, std::move(reply));
    return;
  }
  DOKAN_LOG_TRACE(logger_, "DispatchChange for path %S and info class %u",
                  handle->path().c_str(), info_class);
  auto complete = std::bind(&FileSystem::CompleteChange, this, request, handle,
                            info_class, _1, reply_size, _2);
  change_handler_->Change(request, handle, info_class, std::move(reply),
                          complete);
}

void FileSystem::CompleteChange(
    EVENT_CONTEXT* request,
    FileHandle* handle,
    ULONG info_class,
    NTSTATUS status,
    ULONG buffer_size,
    util::UniqueVarStructPtr<EVENT_INFORMATION> reply) {
  AssertCalledOnIoThread();
  if (handle != nullptr) {
    DOKAN_LOG_TRACE(
        logger_, "Replying to Change for path %S and info class %u"
        " with status 0x%x and used buffer size %u", handle->path().c_str(),
        info_class, status, buffer_size);
  }
  CompleteRequestWithVarReply(
      request, handle, status, buffer_size, buffer_size, std::move(reply));
}

void FileSystem::DispatchFindFiles(EVENT_CONTEXT* request) {
  AssertCalledOnIoThread();
  const ULONG info_class = request->Operation.Directory.FileInformationClass;
  const ULONG start_file_index = request->Operation.Directory.FileIndex;
  const size_t reply_size = ComputeVarReplySize(
      request->Operation.Directory.BufferLength);
  util::UniqueVarStructPtr<EVENT_INFORMATION> reply = PrepareVarReply(
      request, reply_size);
  std::wstring pattern;
  if (request->Operation.Directory.SearchPatternLength != 0) {
    pattern = reinterpret_cast<wchar_t*>(
        (uint64_t)&request->Operation.Directory.SearchPatternBase[0] +
        (uint64_t)request->Operation.Directory.SearchPatternOffset);
  }
  FileHandle* const handle = ReferenceHandle(request);
  if (handle == nullptr) {
    DOKAN_LOG_ERROR(logger_,
                    "DispatchFindFiles with info class %u bypassed due to null"
                    " file handle.", info_class);
    CompleteFindFiles(request, handle, pattern, info_class,
                      STATUS_INVALID_PARAMETER, 0, start_file_index,
                      std::move(reply));
    return;
  }
  DOKAN_LOG_TRACE(logger_, "DispatchFindFiles for path %S, pattern \"%S\", and"
                  " info class %u", handle->path().c_str(), pattern.c_str(),
                  info_class);
  auto complete = std::bind(&FileSystem::CompleteFindFiles, this, request,
                            handle, pattern, info_class, _1, _2, _3, _4);
  const bool single_entry = request->Flags & SL_RETURN_SINGLE_ENTRY;
  find_handler_->FindFiles(request, handle, pattern, info_class,
                           start_file_index, single_entry, std::move(reply),
                           complete);
}

void FileSystem::CompleteFindFiles(
    EVENT_CONTEXT* request,
    FileHandle* handle,
    const std::wstring& pattern,
    ULONG info_class,
    NTSTATUS status,
    ULONG used_buffer_size,
    ULONG next_request_start_index,
    util::UniqueVarStructPtr<EVENT_INFORMATION> reply) {
  AssertCalledOnIoThread();
  if (handle != nullptr) {
    DOKAN_LOG_TRACE(
        logger_, "replying to FindFiles for path %S, pattern \"%S\", and info"
        " class %u with status 0x%x and used buffer size %u",
        handle->path().c_str(), pattern.c_str(), info_class, status,
        used_buffer_size);
  }
  reply->Operation.Directory.Index = next_request_start_index;
  CompleteRequestWithVarReply(
      request, handle, status, request->Operation.Directory.BufferLength,
      used_buffer_size, std::move(reply));
}

void FileSystem::DispatchRead(EVENT_CONTEXT* request) {
  AssertCalledOnIoThread();
  const size_t reply_size = ComputeVarReplySize(
      request->Operation.Read.BufferLength);
  // TODO(drivefs-team): we shouldn't be allocating a buffer of whatever size it
  // asks for (e.g. 4 GB if the file is only 2 bytes), because it's making the
  // request assuming we don't use double the space. Ideally we should be using
  // the real buffer supplied by the calling app if possible. Otherwise it may
  // make sense to chunk the read, so that in a case like that, we would only
  // have e.g. 128K allocated in user mode at once. However, doing either of
  // these things would require a different driver interface than the old
  // library has, and for the time being we are trying to put a new library on
  // top of the same driver.
  util::UniqueVarStructPtr<EVENT_INFORMATION> reply =
      PrepareVarReply(request, reply_size);
  const int64_t offset = request->Operation.Read.ByteOffset.QuadPart;
  const int32_t buffer_length = request->Operation.Read.BufferLength;
  FileHandle* handle = ReferenceHandle(request);
  if (handle == nullptr) {
    DOKAN_LOG_ERROR(logger_,
                   "DispatchRead bypassed due to null file handle.");
    CompleteRead(request, handle, offset, buffer_length,
                 STATUS_INVALID_PARAMETER, std::move(reply));
    return;
  }
  DOKAN_LOG_TRACE(logger_,
                  "DispatchRead for path %S, offset %I64x, length %I32u",
                  handle->path().c_str(), offset, buffer_length);
  EVENT_INFORMATION* raw_reply = reply.release();
  file_callbacks_->Read(
      handle, offset, buffer_length,
      reinterpret_cast<uint32_t*>(&raw_reply->BufferLength),
      reinterpret_cast<char*>(raw_reply->Buffer),
      [=](NTSTATUS status) {
        auto unique_reply = util::MakeUniqueVarStruct(raw_reply);
        CompleteRead(request, handle, offset, buffer_length,
                     status, std::move(unique_reply));
      });
}

void FileSystem::CompleteRead(
    EVENT_CONTEXT* request,
    FileHandle* handle,
    int64_t offset,
    uint32_t buffer_length,
    NTSTATUS status,
    util::UniqueVarStructPtr<EVENT_INFORMATION> reply) {
  AssertCalledOnIoThread();
  const ULONG actual_read_length = reply->BufferLength;
  if (status == STATUS_SUCCESS) {
    reply->Operation.Read.CurrentByteOffset.QuadPart =
        request->Operation.Read.ByteOffset.QuadPart + actual_read_length;
    if (actual_read_length == 0) {
      status = STATUS_END_OF_FILE;
    }
  }
  if (handle != nullptr) {
    DOKAN_LOG_TRACE(
        logger_, "Replying to Read for path %S, offset %I64x, length %I32u,"
        " actual length read %I32u, status 0x%x", handle->path().c_str(),
        offset, buffer_length, actual_read_length, status);
  }
  CompleteRequestWithVarReply(request, handle, status, actual_read_length,
                              actual_read_length, std::move(reply));
}

void FileSystem::DispatchWrite(EVENT_CONTEXT* raw_request) {
  AssertCalledOnIoThread();
  util::UniqueVarStructPtr<EVENT_CONTEXT> request =
      util::MakeUniqueVarStruct(raw_request);
  int64_t offset = request->Operation.Write.ByteOffset.QuadPart;
  FileHandle* handle = ReferenceHandle(raw_request);
  if (handle == nullptr) {
    DOKAN_LOG_ERROR(logger_,
                   "DispatchWrite bypassed due to null file handle.");
    CompleteWrite(std::move(request), handle, offset,
                  STATUS_INVALID_PARAMETER);
    return;
  }
  if (request->Operation.Write.RequestLength > 0) {
    // The majority of writes can't fit their content in the fixed size of
    // EVENT_CONTEXT, we have to allocate a buffer of the requested length and
    // then do another IOCTL to read the full request.
    EVENT_INFORMATION reply_with_buffer;
    PrepareReply(STATUS_SUCCESS, request.get(), handle, &reply_with_buffer);
    const ULONG var_request_size = request->Operation.Write.RequestLength;
    request = util::MakeUniqueVarStruct<EVENT_CONTEXT>(var_request_size);
    pending_requests_.insert(request.get());
    pending_requests_.erase(raw_request);
    raw_request = request.get();
    bool result = device_.Control(
        IOCTL_EVENT_WRITE, reply_with_buffer,
        reinterpret_cast<char*>(request.get()), var_request_size);
    if (!result) {
      // Note: the old code actually ignores an error here.
      DOKAN_LOG_ERROR(logger_,
                      "IOCTL_EVENT_WRITE failed for path %S with request size"
                      " %u", handle->path().c_str(), var_request_size);
      CompleteWrite(std::move(request), handle, offset, STATUS_IO_DEVICE_ERROR);
      return;
    }
  }
  const char* buffer = reinterpret_cast<const char*>(request.get()) +
      request->Operation.Write.BufferOffset;
  const int32_t length = request->Operation.Write.BufferLength;
  offset = request->Operation.Write.ByteOffset.QuadPart;
  if (request->FileFlags & DOKAN_WRITE_TO_END_OF_FILE) {
    offset = kEndOfFileWriteOffset;
  }
  DOKAN_LOG_TRACE(logger_,
                  "DispatchWrite for path %S, offset %I64x, length %I32u",
                  handle->path().c_str(), offset, length);
  raw_request = request.release();
  file_callbacks_->Write(handle, offset, length, buffer, [=](NTSTATUS status) {
    CompleteWrite(std::move(util::MakeUniqueVarStruct(raw_request)),
                  handle, offset, status);
  });
}

void FileSystem::CompleteWrite(
    util::UniqueVarStructPtr<EVENT_CONTEXT> request,
    FileHandle* handle,
    int64_t offset,
    NTSTATUS status) {
  AssertCalledOnIoThread();
  const ULONG written_length = request->Operation.Write.BufferLength;
  if (handle != nullptr) {
    DOKAN_LOG_TRACE(
        logger_, "Replying to Write for path %S, offset %I64x, length %I32u,"
        " status 0x%x", handle->path().c_str(),
        offset, written_length, status);
  }
  auto reply = util::MakeUniqueVarStruct<EVENT_INFORMATION>(
      sizeof(EVENT_INFORMATION));
  PrepareReply(status, request.get(), handle, reply.get());
  if (status == STATUS_SUCCESS) {
    reply->BufferLength = written_length;
    reply->Operation.Write.CurrentByteOffset.QuadPart =
        request->Operation.Write.ByteOffset.QuadPart + written_length;
  }
  ReplyToDriver(request.release(), std::move(reply), sizeof(EVENT_INFORMATION));
  RemoveReference(&handle);
}

void FileSystem::DispatchFlush(EVENT_CONTEXT* request) {
  AssertCalledOnIoThread();
  FileHandle* handle = ReferenceHandle(request);
  if (handle == nullptr) {
    DOKAN_LOG_ERROR(logger_,
                   "DispatchFlush bypassed due to null file handle.");
    CompleteFlush(request, handle, STATUS_INVALID_PARAMETER);
    return;
  }
  file_callbacks_->Flush(
      handle,
      std::bind(&FileSystem::CompleteFlush, this, request, handle, _1));
}

void FileSystem::CompleteFlush(EVENT_CONTEXT* request,
                               FileHandle* handle,
                               NTSTATUS status) {
  AssertCalledOnIoThread();
  if (handle != nullptr) {
    DOKAN_LOG_TRACE(
        logger_,
        "Replying to driver for Flush with path %S",
        handle->path().c_str());
  }
  auto reply = util::MakeUniqueVarStruct<EVENT_INFORMATION>(
      sizeof(EVENT_INFORMATION));
  PrepareReply(status, request, handle, reply.get());
  ReplyToDriver(request, std::move(reply), sizeof(EVENT_INFORMATION));
  RemoveReference(&handle);
}

void FileSystem::CompleteRequestWithVarReply(
    EVENT_CONTEXT* request,
    FileHandle* handle,
    NTSTATUS status,
    ULONG provided_var_buffer_size,
    ULONG used_var_buffer_size,
    util::UniqueVarStructPtr<EVENT_INFORMATION> reply) {
  assert(used_var_buffer_size <= provided_var_buffer_size);
  reply->Status = status;
  reply->BufferLength = used_var_buffer_size;
  // Note: reply->Buffer is overlaid  with a buffer of size
  // provided_var_buffer_size in this case.
  const ULONG reply_size = ComputeVarReplySize(provided_var_buffer_size);
  ReplyToDriver(request, std::move(reply), reply_size);
  RemoveReference(&handle);
}

FileHandle* FileSystem::ReferenceHandle(EVENT_CONTEXT* request) {
  AssertCalledOnIoThread();
  if (request->Context == 0) {
    return nullptr;
  }
  FileHandle* const handle = (FileHandle*)request->Context;
  if (handle != nullptr) {
    handle->AddReference();
  }
  return handle;
}

int64_t FileSystem::RemoveReference(FileHandle** handle) {
  AssertCalledOnIoThread();
  if (*handle == nullptr) {
    return 0;
  }
  int64_t count = (*handle)->RemoveReference();
  if (count < 0) {
    DOKAN_LOG_ERROR(logger_,
                    "Handle reference count has dropped to %I64d for %S",
                    count, (*handle)->path().c_str());
  } else if (count == 0) {
    if ((*handle)->context()) {
      // We should get in here for any case but a failed create.
      file_callbacks_->Close(*handle);
    }
    delete *handle;
    *handle = nullptr;
  }
  return count;
}

void FileSystem::DispatchCleanup(EVENT_CONTEXT* request) {
  AssertCalledOnIoThread();
  FileHandle* const handle = ReferenceHandle(request);
  if (handle == nullptr) {
    DOKAN_LOG_INFO(logger_,
                   "DispatchCleanup bypassed due to null file handle.");
    CompleteCleanup(request, nullptr, STATUS_SUCCESS);
    return;
  }
  DOKAN_LOG_TRACE(logger_, "DispatchCleanup for path %S",
                  handle->path().c_str());
  file_callbacks_->Cleanup(
      handle,
      std::bind(&FileSystem::CompleteCleanup, this, request, handle,
                std::placeholders::_1));
}

void FileSystem::CompleteCleanup(EVENT_CONTEXT* request, FileHandle* handle,
                                 NTSTATUS status) {
  AssertCalledOnIoThread();
  if (status != STATUS_SUCCESS) {
    // Historically, returning an error from cleanup has never really been
    // supported. It's unclear whether it should be. Since that could lead to
    // some bad resource leak problems, we should continue not allowing it until
    // a need is proven.
    DOKAN_LOG_ERROR(logger_, "Cleanup callback returned error 0x%x for path %S",
                    status, handle ? handle->path().c_str() : L"<unknown>");
  } else {
    DOKAN_LOG_TRACE(
        logger_,
        "Replying successfully to driver for Cleanup with path %S",
        handle ? handle->path().c_str() : L"<unknown>");
  }
  auto reply = util::MakeUniqueVarStruct<EVENT_INFORMATION>(
      sizeof(EVENT_INFORMATION));
  PrepareReply(STATUS_SUCCESS, request, handle, reply.get());
  ReplyToDriver(request, std::move(reply), sizeof(EVENT_INFORMATION));
  RemoveReference(&handle);
}

void FileSystem::DispatchClose(EVENT_CONTEXT* request) {
  AssertCalledOnIoThread();
  FileHandle* handle = ReferenceHandle(request);
  if (handle != nullptr) {
    DOKAN_LOG_TRACE(logger_, "DispatchClose with path %S",
                    handle->path().c_str());
    RemoveReference(&handle);  // Our own reference.
    RemoveReference(&handle);  // The creation reference.
    // Normally the above call invokes the Close callback, but on the off chance
    // that IRP_MJ_CLOSE is issued when we still own a pending request using the
    // handle, it will not happen until that request removes its reference. This
    // probably happens when an IRP is canceled and user mode is unaware of the
    // cancellation.
  } else {
    DOKAN_LOG_INFO(logger_, "DispatchClose bypassed due to null file handle.");
  }
  RequestCompleted(request);
}

void FileSystem::ReplyToDriver(
    EVENT_CONTEXT* request,
    util::UniqueVarStructPtr<EVENT_INFORMATION> reply,
    ULONG size) {
  AssertCalledOnIoThread();
  assert(size >= sizeof(EVENT_INFORMATION));
  assert(reply_handler_);
  reply_handler_->SendReply(std::move(reply), size);
  RequestCompleted(request);
}

void FileSystem::RequestCompleted(EVENT_CONTEXT* request) {
  AssertCalledOnIoThread();
  pending_requests_.erase(request);
  util::DeleteVarStruct(request);
  MaybeFinishUnmount();
}

}  // namespace dokan