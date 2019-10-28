/*
  Dokan : user-mode file system library for Windows

  Copyright (C) 2017 - 2018 Google, Inc.
  Copyright (C) 2015 - 2016 Adrien J. <liryna.stark@gmail.com> and Maxime C. <maxime@islog.com>
  Copyright (C) 2007 - 2011 Hiroki Asakawa <info@dokan-dev.net>

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

#include "dokan.h"

#ifdef ALLOC_PRAGMA
#pragma alloc_text(PAGE, DokanOplockComplete)
#endif

VOID DokanCreateIrpCancelRoutine(_Inout_ PDEVICE_OBJECT DeviceObject,
                                 _Inout_ _IRQL_uses_cancel_ PIRP Irp) {
  // Cancellation of a Create is handled like a timeout. The whole effect of
  // this routine is just to set the IRP entry's tick count so as to trigger a
  // timeout, and then force the timeout thread to wake up. This simplifies the
  // complex cleanup that needs to be done and can't be done on the unknown
  // context where the cancel routine runs. For other types of IRPs, the cancel
  // routine actually does cancelation/cleanup.
  IoReleaseCancelSpinLock(Irp->CancelIrql);
  PIRP_ENTRY irpEntry =
      Irp->Tail.Overlay.DriverContext[DRIVER_CONTEXT_IRP_ENTRY];
  if (irpEntry != NULL) {
    Irp->Tail.Overlay.DriverContext[DRIVER_CONTEXT_EVENT] = NULL;
    InterlockedAnd64(&irpEntry->TickCount.QuadPart, 0);
    irpEntry->AsyncStatus = STATUS_CANCELLED;
    PDokanVCB vcb = DeviceObject->DeviceExtension;
    PDokanDCB dcb = vcb->Dcb;
    KeSetEvent(&dcb->ForceTimeoutEvent, 0, FALSE);
  }
}

// Only used for non-Create IRPs.
VOID DokanIrpCancelRoutine(_Inout_ PDEVICE_OBJECT DeviceObject,
                           _Inout_ _IRQL_uses_cancel_ PIRP Irp) {
  KIRQL oldIrql;
  PIRP_ENTRY irpEntry;
  ULONG serialNumber = 0;
  PIO_STACK_LOCATION irpSp = NULL;

  UNREFERENCED_PARAMETER(DeviceObject);

  DDbgPrint("==> DokanIrpCancelRoutine\n");

  // Release the cancel spinlock
  IoReleaseCancelSpinLock(Irp->CancelIrql);

  irpEntry = Irp->Tail.Overlay.DriverContext[DRIVER_CONTEXT_IRP_ENTRY];

  if (irpEntry != NULL) {
    PKSPIN_LOCK lock = &irpEntry->IrpList->ListLock;

    // Acquire the queue spinlock
    ASSERT(KeGetCurrentIrql() <= DISPATCH_LEVEL);
    KeAcquireSpinLock(lock, &oldIrql);

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    ASSERT(irpSp != NULL);

    serialNumber = irpEntry->SerialNumber;

    RemoveEntryList(&irpEntry->ListEntry);
    InitializeListHead(&irpEntry->ListEntry);

    // If Write is canceld before completion and buffer that saves writing
    // content is not freed, free it here
    if (irpSp->MajorFunction == IRP_MJ_WRITE) {
      PVOID eventContext =
          Irp->Tail.Overlay.DriverContext[DRIVER_CONTEXT_EVENT];
      if (eventContext != NULL) {
        DokanFreeEventContext(eventContext);
      }
      Irp->Tail.Overlay.DriverContext[DRIVER_CONTEXT_EVENT] = NULL;
    }

    if (IsListEmpty(&irpEntry->IrpList->ListHead)) {
      // DDbgPrint("    list is empty ClearEvent\n");
      KeClearEvent(&irpEntry->IrpList->NotEmpty);
    }

    irpEntry->Irp = NULL;

    if (irpEntry->CancelRoutineFreeMemory == FALSE) {
      InitializeListHead(&irpEntry->ListEntry);
    } else {
      DokanFreeIrpEntry(irpEntry);
      irpEntry = NULL;
    }

    Irp->Tail.Overlay.DriverContext[DRIVER_CONTEXT_IRP_ENTRY] = NULL;

    KeReleaseSpinLock(lock, oldIrql);
  }

  DDbgPrint("   canceled IRP #%X\n", serialNumber);
  DokanCompleteIrpRequest(Irp, STATUS_CANCELLED, 0);

  DDbgPrint("<== DokanIrpCancelRoutine\n");
}

VOID DokanOplockComplete(IN PVOID Context, IN PIRP Irp)
/*++
Routine Description:
This routine is called by the oplock package when an oplock break has
completed, allowing an Irp to resume execution.  If the status in
the Irp is STATUS_SUCCESS, then we queue the Irp to the Fsp queue.
Otherwise we complete the Irp with the status in the Irp.
Arguments:
Context - Pointer to the EventContext to be queued to the Fsp
Irp - I/O Request Packet.
Return Value:
None.
--*/
{
  PIO_STACK_LOCATION irpSp;

  DDbgPrint("==> DokanOplockComplete\n");
  PAGED_CODE();

  irpSp = IoGetCurrentIrpStackLocation(Irp);

  //
  //  Check on the return value in the Irp.
  //
  if (Irp->IoStatus.Status == STATUS_SUCCESS) {
    DokanRegisterPendingIrp(irpSp->DeviceObject, Irp, (PEVENT_CONTEXT)Context,
                            0);
  } else {
    DokanCompleteIrpRequest(Irp, Irp->IoStatus.Status, 0);
  }

  DDbgPrint("<== DokanOplockComplete\n");
}

// Routine for kernel oplock functions to invoke if they are going to wait for
// e.g. an oplock break acknowledgement and return STATUS_PENDING. This enables
// us to mark the IRP pending without there being a race condition where it
// gets acted on by the acknowledging thread before we do this.
// do the right thing here with conservative impact.
VOID DokanPrePostIrp(IN PVOID Context, IN PIRP Irp)
{
  DDbgPrint("==> DokanPrePostIrp\n");

  UNREFERENCED_PARAMETER(Context);
  IoMarkIrpPending(Irp);

  DDbgPrint("<== DokanPrePostIrp\n");
}

NTSTATUS
RegisterPendingIrpMain(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp,
                       __in ULONG SerialNumber, __in PIRP_LIST IrpList,
                       __in ULONG Flags, __in ULONG CheckMount,
                       __in NTSTATUS CurrentStatus) {
  PIRP_ENTRY irpEntry;
  PIO_STACK_LOCATION irpSp;
  KIRQL oldIrql;
  PDokanVCB vcb = NULL;

  DDbgPrint("==> DokanRegisterPendingIrpMain\n");

  if (GetIdentifierType(DeviceObject->DeviceExtension) == VCB) {
    vcb = DeviceObject->DeviceExtension;
    if (CheckMount && IsUnmountPendingVcb(vcb)) {
      DDbgPrint(" device is not mounted\n");
      return STATUS_NO_SUCH_DEVICE;
    }
  }

  irpSp = IoGetCurrentIrpStackLocation(Irp);

  // Allocate a record and save all the event context.
  irpEntry = DokanAllocateIrpEntry();

  if (NULL == irpEntry) {
    DDbgPrint("  can't allocate IRP_ENTRY\n");
    return STATUS_INSUFFICIENT_RESOURCES;
  }

  RtlZeroMemory(irpEntry, sizeof(IRP_ENTRY));

  InitializeListHead(&irpEntry->ListEntry);

  irpEntry->SerialNumber = SerialNumber;
  irpEntry->FileObject = irpSp->FileObject;
  irpEntry->Irp = Irp;
  irpEntry->IrpSp = irpSp;
  irpEntry->IrpList = IrpList;
  irpEntry->Flags = Flags;
  irpEntry->AsyncStatus = CurrentStatus;

  // Update the irp timeout for the entry
  if (vcb) {
    ExAcquireResourceExclusiveLite(&vcb->Dcb->Resource, TRUE);
    DokanUpdateTimeout(&irpEntry->TickCount, vcb->Dcb->IrpTimeout);
    ExReleaseResourceLite(&vcb->Dcb->Resource);
  } else {
    DokanUpdateTimeout(&irpEntry->TickCount, DOKAN_IRP_PENDING_TIMEOUT);
  }

  // DDbgPrint("  Lock IrpList.ListLock\n");
  ASSERT(KeGetCurrentIrql() <= DISPATCH_LEVEL);
  KeAcquireSpinLock(&IrpList->ListLock, &oldIrql);

  if (irpSp->MajorFunction == IRP_MJ_CREATE) {
    IoSetCancelRoutine(Irp, DokanCreateIrpCancelRoutine);
  } else {
    IoSetCancelRoutine(Irp, DokanIrpCancelRoutine);
  }

  if (Irp->Cancel) {
    if (IoSetCancelRoutine(Irp, NULL) != NULL) {
      // DDbgPrint("  Release IrpList.ListLock %d\n", __LINE__);
      KeReleaseSpinLock(&IrpList->ListLock, oldIrql);

      DokanFreeIrpEntry(irpEntry);

      return STATUS_CANCELLED;
    }
  }

  IoMarkIrpPending(Irp);

  InsertTailList(&IrpList->ListHead, &irpEntry->ListEntry);

  irpEntry->CancelRoutineFreeMemory = FALSE;

  // save the pointer in order to be accessed by cancel routine
  Irp->Tail.Overlay.DriverContext[DRIVER_CONTEXT_IRP_ENTRY] = irpEntry;

  KeSetEvent(&IrpList->NotEmpty, IO_NO_INCREMENT, FALSE);

  // DDbgPrint("  Release IrpList.ListLock\n");
  KeReleaseSpinLock(&IrpList->ListLock, oldIrql);

  DDbgPrint("<== DokanRegisterPendingIrpMain\n");
  return STATUS_PENDING;
}

NTSTATUS
DokanRegisterPendingIrp(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp,
                        __in PEVENT_CONTEXT EventContext, __in ULONG Flags) {
  PDokanVCB vcb = DeviceObject->DeviceExtension;
  NTSTATUS status;

  DDbgPrint("==> DokanRegisterPendingIrp\n");

  if (GetIdentifierType(vcb) != VCB) {
    DDbgPrint("  IdentifierType is not VCB\n");
    return STATUS_INVALID_PARAMETER;
  }

  status = RegisterPendingIrpMain(DeviceObject, Irp, EventContext->SerialNumber,
                                  &vcb->Dcb->PendingIrp, Flags, TRUE,
                                  /*CurrentStatus=*/STATUS_SUCCESS);

  if (status == STATUS_PENDING) {
    DokanEventNotification(&vcb->Dcb->NotifyEvent, EventContext);
  } else {
    DokanFreeEventContext(EventContext);
  }

  DDbgPrint("<== DokanRegisterPendingIrp\n");
  return status;
}

VOID
DokanRegisterPendingRetryIrp(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp) {
  PDokanVCB vcb = DeviceObject->DeviceExtension;
  if (GetIdentifierType(vcb) != VCB) {
    return;
  }
  PIO_STACK_LOCATION irpSp = IoGetCurrentIrpStackLocation(Irp);
  if (irpSp->MajorFunction == IRP_MJ_CREATE) {
    // You can't just re-dispatch a create, since the part before the pending
    // retry has side-effects. DokanDispatchCreate uses this flag to identify
    // retries.
    PDokanCCB ccb = irpSp->FileObject->FsContext2;
    ASSERT(ccb != NULL);
    DokanCCBFlagsSetBit(ccb, DOKAN_RETRY_CREATE);
    OplockDebugRecordFlag(ccb->Fcb, DOKAN_OPLOCK_DEBUG_CREATE_RETRY_QUEUED);
  }
  RegisterPendingIrpMain(DeviceObject, Irp, /*SerialNumber=*/0,
                         &vcb->Dcb->PendingRetryIrp, /*Flags=*/0,
                         /*CheckMount=*/TRUE,
                         /*CurrentStatus=*/STATUS_SUCCESS);
}

VOID
DokanRegisterAsyncCreateFailure(__in PDEVICE_OBJECT DeviceObject,
                                __in PIRP Irp,
                                __in NTSTATUS Status) {
  PDokanVCB vcb = DeviceObject->DeviceExtension;
  if (GetIdentifierType(vcb) != VCB) {
    return;
  }
  RegisterPendingIrpMain(DeviceObject, Irp, /*SerialNumber=*/0,
                         &vcb->Dcb->PendingIrp, /*Flags=*/0,
                         /*CheckMount=*/TRUE, Status);
  KeSetEvent(&vcb->Dcb->ForceTimeoutEvent, 0, FALSE);
}

NTSTATUS
DokanRegisterPendingIrpForEvent(__in PDEVICE_OBJECT DeviceObject,
                                _Inout_ PIRP Irp) {
  PDokanVCB vcb = DeviceObject->DeviceExtension;

  if (GetIdentifierType(vcb) != VCB) {
    DDbgPrint("  IdentifierType is not VCB\n");
    return STATUS_INVALID_PARAMETER;
  }

  if (IsUnmountPendingVcb(vcb)) {
    DDbgPrint("  Volume is dismounted\n");
    return STATUS_NO_SUCH_DEVICE;
  }

  // DDbgPrint("DokanRegisterPendingIrpForEvent\n");
  vcb->HasEventWait = TRUE;

  return RegisterPendingIrpMain(DeviceObject, Irp,
                                0, // SerialNumber
                                &vcb->Dcb->PendingEvent,
                                0, // Flags
                                TRUE,
                                /*CurrentStatus=*/STATUS_SUCCESS);
}

NTSTATUS
DokanRegisterPendingIrpForService(__in PDEVICE_OBJECT DeviceObject,
                                  _Inout_ PIRP Irp) {
  PDOKAN_GLOBAL dokanGlobal;
  DDbgPrint("DokanRegisterPendingIrpForService\n");

  dokanGlobal = DeviceObject->DeviceExtension;
  if (GetIdentifierType(dokanGlobal) != DGL) {
    return STATUS_INVALID_PARAMETER;
  }

  return RegisterPendingIrpMain(DeviceObject, Irp,
                                0, // SerialNumber
                                &dokanGlobal->PendingService,
                                0, // Flags
                                FALSE,
                                /*CurrentStatus=*/STATUS_SUCCESS);
}

// When user-mode file system application returns EventInformation,
// search corresponding pending IRP and complete it
NTSTATUS
DokanCompleteIrp(__in PDEVICE_OBJECT DeviceObject, _Inout_ PIRP Irp) {
  KIRQL oldIrql;
  PLIST_ENTRY thisEntry, nextEntry, listHead;
  PIRP_ENTRY irpEntry;
  PDokanVCB vcb;
  PEVENT_INFORMATION eventInfo;

  eventInfo = (PEVENT_INFORMATION)Irp->AssociatedIrp.SystemBuffer;
  ASSERT(eventInfo != NULL);

  // DDbgPrint("==> DokanCompleteIrp [EventInfo #%X]\n",
  // eventInfo->SerialNumber);

  vcb = DeviceObject->DeviceExtension;
  if (GetIdentifierType(vcb) != VCB) {
    return STATUS_INVALID_PARAMETER;
  }

  if (IsUnmountPendingVcb(vcb)) {
    DDbgPrint("      Volume is not mounted\n");
    return STATUS_NO_SUCH_DEVICE;
  }

  // DDbgPrint("      Lock IrpList.ListLock\n");
  ASSERT(KeGetCurrentIrql() <= DISPATCH_LEVEL);
  KeAcquireSpinLock(&vcb->Dcb->PendingIrp.ListLock, &oldIrql);

  // search corresponding IRP through pending IRP list
  listHead = &vcb->Dcb->PendingIrp.ListHead;

  for (thisEntry = listHead->Flink; thisEntry != listHead;
       thisEntry = nextEntry) {

    PIRP irp;
    PIO_STACK_LOCATION irpSp;

    nextEntry = thisEntry->Flink;

    irpEntry = CONTAINING_RECORD(thisEntry, IRP_ENTRY, ListEntry);

    // check whether this is corresponding IRP

    // DDbgPrint("SerialNumber irpEntry %X eventInfo %X\n",
    // irpEntry->SerialNumber, eventInfo->SerialNumber);

    // this irpEntry must be freed in this if statement
    if (irpEntry->SerialNumber != eventInfo->SerialNumber) {
      continue;
    }

    RemoveEntryList(thisEntry);

    irp = irpEntry->Irp;

    if (irp == NULL) {
      // this IRP is already canceled
      ASSERT(irpEntry->CancelRoutineFreeMemory == FALSE);
      DokanFreeIrpEntry(irpEntry);
      irpEntry = NULL;
      break;
    }

    if (IoSetCancelRoutine(irp, NULL) == NULL) {
      // Cancel routine will run as soon as we release the lock
      InitializeListHead(&irpEntry->ListEntry);
      irpEntry->CancelRoutineFreeMemory = TRUE;
      break;
    }

    // IRP is not canceled yet
    irpSp = irpEntry->IrpSp;

    ASSERT(irpSp != NULL);

    // IrpEntry is saved here for CancelRoutine
    // Clear it to prevent to be completed by CancelRoutine twice
    irp->Tail.Overlay.DriverContext[DRIVER_CONTEXT_IRP_ENTRY] = NULL;
    KeReleaseSpinLock(&vcb->Dcb->PendingIrp.ListLock, oldIrql);

    if (IsUnmountPendingVcb(vcb)) {
      DDbgPrint("      Volume is not mounted second check\n");
      return STATUS_NO_SUCH_DEVICE;
    }

    if (eventInfo->Status == STATUS_PENDING) {
      DDbgPrint(
          "      !!WARNING!! Do not return STATUS_PENDING DokanCompleteIrp!");
    }

    switch (irpSp->MajorFunction) {
    case IRP_MJ_DIRECTORY_CONTROL:
      DokanCompleteDirectoryControl(irpEntry, eventInfo);
      break;
    case IRP_MJ_READ:
      DokanCompleteRead(irpEntry, eventInfo);
      break;
    case IRP_MJ_WRITE:
      DokanCompleteWrite(irpEntry, eventInfo);
      break;
    case IRP_MJ_QUERY_INFORMATION:
      DokanCompleteQueryInformation(irpEntry, eventInfo);
      break;
    case IRP_MJ_QUERY_VOLUME_INFORMATION:
      DokanCompleteQueryVolumeInformation(irpEntry, eventInfo, DeviceObject);
      break;
    case IRP_MJ_CREATE:
      DokanCompleteCreate(irpEntry, eventInfo);
      break;
    case IRP_MJ_CLEANUP:
      DokanCompleteCleanup(irpEntry, eventInfo);
      break;
    case IRP_MJ_LOCK_CONTROL:
      DokanCompleteLock(irpEntry, eventInfo);
      break;
    case IRP_MJ_SET_INFORMATION:
      DokanCompleteSetInformation(irpEntry, eventInfo);
      break;
    case IRP_MJ_FLUSH_BUFFERS:
      DokanCompleteFlush(irpEntry, eventInfo);
      break;
    case IRP_MJ_QUERY_SECURITY:
      DokanCompleteQuerySecurity(irpEntry, eventInfo);
      break;
    case IRP_MJ_SET_SECURITY:
      DokanCompleteSetSecurity(irpEntry, eventInfo);
      break;
    default:
      DDbgPrint("Unknown IRP %d\n", irpSp->MajorFunction);
      // TODO: in this case, should complete this IRP
      break;
    }

    DokanFreeIrpEntry(irpEntry);
    irpEntry = NULL;

    return STATUS_SUCCESS;
  }

  KeReleaseSpinLock(&vcb->Dcb->PendingIrp.ListLock, oldIrql);

  // DDbgPrint("<== AACompleteIrp [EventInfo #%X]\n", eventInfo->SerialNumber);

  // TODO: should return error
  return STATUS_SUCCESS;
}

// Gets the binary owner information from the security descriptor of the device.
// Returns either the information or NULL if it cannot be read. Logs the reason
// for any failure. If the result is not NULL then the caller must free it via
// ExFreePool.
char* GetDeviceOwner(__in PDOKAN_LOGGER Logger,
                     __in const WCHAR* DeviceNameForLog,
                     __in PDEVICE_OBJECT DeviceObject,
                     _Inout_ PULONG OwnerSize) {
  NTSTATUS status = STATUS_SUCCESS;
  HANDLE handle = 0;
  char* result = NULL;
  __try {
    status = ObOpenObjectByPointer(DeviceObject, OBJ_KERNEL_HANDLE, NULL,
                                   READ_CONTROL, 0, KernelMode, &handle);
    if (!NT_SUCCESS(status)) {
      DokanLogInfo(Logger, L"Failed to open device: %s, status: 0x%x",
                   DeviceNameForLog, status);
      __leave;
    }
    status = ZwQuerySecurityObject(handle, OWNER_SECURITY_INFORMATION, NULL, 0,
                                   OwnerSize);
    if (status != STATUS_BUFFER_TOO_SMALL) {
      DokanLogInfo(Logger,
                   L"Failed to query for owner length of device: %s,"
                   L" status: 0x%x", DeviceNameForLog, status);
      __leave;
    }
    result = ExAllocatePool(*OwnerSize);
    status = ZwQuerySecurityObject(handle, OWNER_SECURITY_INFORMATION, result,
                                   *OwnerSize, OwnerSize);
    if (!NT_SUCCESS(status)) {
      DokanLogInfo(Logger, L"Failed to query for owner of device: %s,"
                   L" status: 0x%x", DeviceNameForLog, status);
      ExFreePool(result);
      result = NULL;
      __leave;
    }
  } __finally {
    if (handle != 0) {
      ZwClose(handle);
    }
  }
  return result;
}

// Determines whether the given two devices have the same owner specified by
// their security descriptors. Calling the devices "new" and "existing" is
// useful for the logging done within this function, but otherwise irrelevant.
// Any failure or negative result is logged.
BOOLEAN HasSameOwner(__in PDOKAN_LOGGER Logger,
                     __in PDEVICE_OBJECT NewDevice,
                     __in PDEVICE_OBJECT OldDevice) {
  BOOLEAN result = FALSE;
  char* newOwner = NULL;
  char* oldOwner = NULL;
  ULONG oldOwnerSize = 0;
  ULONG newOwnerSize = 0;
  __try {
    newOwner = GetDeviceOwner(Logger, L"new device", NewDevice, &newOwnerSize);
    if (newOwner == NULL) {
      __leave;
    }
    oldOwner = GetDeviceOwner(Logger, L"old device", OldDevice, &oldOwnerSize);
    if (oldOwner == NULL) {
      __leave;
    }
    if (oldOwnerSize != newOwnerSize ||
        RtlCompareMemory(newOwner, oldOwner, oldOwnerSize) != oldOwnerSize) {
      DokanLogInfo(Logger, L"Retrieved device owners and they do not match.");
      __leave;
    }
    result = TRUE;
  } __finally {
    if (newOwner != NULL) {
      ExFreePool(newOwner);
    }
    if (oldOwner != NULL) {
      ExFreePool(oldOwner);
    }
  }
  return result;
}

// Unmounts the drive indicated by OldControl in order for the caller to replace
// it with the one indicated by NewControl. If this cannot be done due to
// different ownership, or mysteriously fails, returns FALSE; otherwise returns
// TRUE.
BOOLEAN MaybeUnmountOldDrive(__in PDOKAN_LOGGER Logger,
                             __in PDOKAN_GLOBAL DokanGlobal,
                             __in PDOKAN_CONTROL OldControl,
                             __in PDOKAN_CONTROL NewControl) {
  DokanLogInfo(Logger,
      L"Mount point exists and"
      L" DOKAN_EVENT_REPLACE_DOKAN_DRIVE_IF_EXISTS is set: %s",
      NewControl->MountPoint);
  if (!HasSameOwner(Logger, NewControl->DiskDeviceObject,
                    OldControl->DiskDeviceObject)) {
    DokanLogInfo(Logger,
                 L"Not replacing existing drive with different owner.");
    return FALSE;
  }
  DokanLogInfo(Logger, L"Unmounting the existing drive.");
  DokanUnmount(OldControl->Dcb);
  PMOUNT_ENTRY entryAfterUnmount =
      FindMountEntry(DokanGlobal, NewControl, FALSE);
  if (entryAfterUnmount != NULL) {
    DokanLogInfo(
        Logger,
        L"Warning: old mount entry was not removed by unmount attempt.");
    return FALSE;
  }
  DokanLogInfo(Logger, L"The existing mount entry is now gone.");
  return TRUE;
}

// start event dispatching
NTSTATUS
DokanEventStart(__in PDEVICE_OBJECT DeviceObject, _Inout_ PIRP Irp) {
  ULONG outBufferLen;
  ULONG inBufferLen;
  PIO_STACK_LOCATION irpSp = NULL;
  PEVENT_START eventStart = NULL;
  PEVENT_DRIVER_INFO driverInfo = NULL;
  PDOKAN_GLOBAL dokanGlobal = NULL;
  PDokanDCB dcb = NULL;
  NTSTATUS status;
  DEVICE_TYPE deviceType;
  ULONG deviceCharacteristics = 0;
  WCHAR *baseGuidString;
  UNICODE_STRING unicodeGuid;
  ULONG deviceNamePos;
  BOOLEAN useMountManager = FALSE;
  BOOLEAN mountGlobally = TRUE;
  BOOLEAN fileLockUserMode = FALSE;
  BOOLEAN lockDebugEnabled = FALSE;
  PSECURITY_DESCRIPTOR volumeSecurityDescriptor = NULL;
  DOKAN_INIT_LOGGER(logger, DeviceObject->DriverObject, 0);

  DokanLogInfo(&logger, L"Entered event start.");

  dokanGlobal = DeviceObject->DeviceExtension;
  if (GetIdentifierType(dokanGlobal) != DGL) {
    return STATUS_INVALID_PARAMETER;
  }

  irpSp = IoGetCurrentIrpStackLocation(Irp);

  outBufferLen = irpSp->Parameters.DeviceIoControl.OutputBufferLength;
  inBufferLen = irpSp->Parameters.DeviceIoControl.InputBufferLength;

  eventStart = ExAllocatePool(sizeof(EVENT_START));
  baseGuidString = ExAllocatePool(64 * sizeof(WCHAR));

  if (outBufferLen != sizeof(EVENT_DRIVER_INFO) ||
      inBufferLen != sizeof(EVENT_START) || eventStart == NULL ||
      baseGuidString == NULL) {
    if (eventStart) {
      ExFreePool(eventStart);
    }
    if (baseGuidString) {
      ExFreePool(baseGuidString);
    }
    return DokanLogError(&logger, STATUS_INSUFFICIENT_RESOURCES,
                         L"Failed to allocate buffers in event start.");
  }

  RtlCopyMemory(eventStart, Irp->AssociatedIrp.SystemBuffer,
                sizeof(EVENT_START));
  driverInfo = Irp->AssociatedIrp.SystemBuffer;
  driverInfo->Flags = 0;
  GUID baseGuid = eventStart->BaseVolumeGuid;
  GUID dokanDriverVersion = DOKAN_DRIVER_VERSION;
  if (!IsEqualGUID(&eventStart->UserVersion, &dokanDriverVersion)) {
    DokanLogInfo(&logger, L"Driver version check in event start failed.");
    driverInfo->DriverVersion = dokanDriverVersion;
    driverInfo->Status = DOKAN_START_FAILED;
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = sizeof(EVENT_DRIVER_INFO);
    ExFreePool(eventStart);
    ExFreePool(baseGuidString);
    return STATUS_SUCCESS;
  }

  switch (eventStart->DeviceType) {
  case DOKAN_DISK_FILE_SYSTEM:
    deviceType = FILE_DEVICE_DISK_FILE_SYSTEM;
    break;
  case DOKAN_NETWORK_FILE_SYSTEM:
    deviceType = FILE_DEVICE_NETWORK_FILE_SYSTEM;
    deviceCharacteristics |= FILE_REMOTE_DEVICE;
    break;
  default:
    DDbgPrint("  Unknown device type: %d\n", eventStart->DeviceType);
    deviceType = FILE_DEVICE_DISK_FILE_SYSTEM;
  }

  if (eventStart->Flags & DOKAN_EVENT_REMOVABLE) {
    DDbgPrint("  DeviceCharacteristics |= FILE_REMOVABLE_MEDIA\n");
    deviceCharacteristics |= FILE_REMOVABLE_MEDIA;
  }

  if (eventStart->Flags & DOKAN_EVENT_WRITE_PROTECT) {
    DDbgPrint("  DeviceCharacteristics |= FILE_READ_ONLY_DEVICE\n");
    deviceCharacteristics |= FILE_READ_ONLY_DEVICE;
  }

  if (eventStart->Flags & DOKAN_EVENT_MOUNT_MANAGER) {
    DDbgPrint("  Using Mount Manager\n");
    useMountManager = TRUE;
  }

  if (eventStart->Flags & DOKAN_EVENT_CURRENT_SESSION) {
    DDbgPrint("  Mounting on current session only\n");
    mountGlobally = FALSE;
  }

  if (eventStart->Flags & DOKAN_EVENT_FILELOCK_USER_MODE) {
    DDbgPrint("  FileLock in User Mode\n");
    fileLockUserMode = TRUE;
  }

  if (eventStart->Flags & DOKAN_EVENT_LOCK_DEBUG_ENABLED) {
    DokanLogInfo(
        &logger,
        L"Enabling lock debugging. This should be disabled in normal use.");
    DDbgPrint("  Lock debugging enabled\n");
    lockDebugEnabled = TRUE;
  }

  KeEnterCriticalRegion();
  ExAcquireResourceExclusiveLite(&dokanGlobal->Resource, TRUE);

  DOKAN_CONTROL dokanControl;
  RtlZeroMemory(&dokanControl, sizeof(dokanControl));
  RtlStringCchCopyW(dokanControl.MountPoint, MAXIMUM_FILENAME_LENGTH,
                    L"\\DosDevices\\");
  if (wcslen(eventStart->MountPoint) == 1) {
    dokanControl.MountPoint[12] = towupper(eventStart->MountPoint[0]);
    dokanControl.MountPoint[13] = L':';
    dokanControl.MountPoint[14] = L'\0';
  } else {
    RtlStringCchCatW(dokanControl.MountPoint, MAXIMUM_FILENAME_LENGTH,
                     eventStart->MountPoint);
  }

  DDbgPrint("  Checking for MountPoint %ls \n", dokanControl.MountPoint);
  PMOUNT_ENTRY foundEntry = FindMountEntry(dokanGlobal, &dokanControl, FALSE);
  if (foundEntry != NULL &&
      !(eventStart->Flags & DOKAN_EVENT_RESOLVE_MOUNT_CONFLICTS)) {
    // Legacy behavior: fail on existing mount entry with the same mount point.
    // Note: there are edge cases where this entry (which is internal to dokan)
    // may be left around despite the drive being technically unmounted. In such
    // a case, the code outside this driver can't know that. Therefore, it's
    // advisable to set the flag and avoid this branch.
    DokanLogInfo(
        &logger,
        L"Mount point exists; DOKAN_EVENT_RESOLVE_MOUNT_CONFLICTS not set: %s",
        dokanControl.MountPoint);
    driverInfo->DriverVersion = dokanDriverVersion;
    driverInfo->Status = DOKAN_START_FAILED;
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = sizeof(EVENT_DRIVER_INFO);
    ExReleaseResourceLite(&dokanGlobal->Resource);
    KeLeaveCriticalRegion();
    ExFreePool(eventStart);
    ExFreePool(baseGuidString);
    return STATUS_SUCCESS;
  }

  baseGuid.Data2 = (USHORT)(dokanGlobal->MountId & 0xFFFF) ^ baseGuid.Data2;
  baseGuid.Data3 = (USHORT)(dokanGlobal->MountId >> 16) ^ baseGuid.Data3;

  status = RtlStringFromGUID(&baseGuid, &unicodeGuid);
  if (!NT_SUCCESS(status)) {
    ExReleaseResourceLite(&dokanGlobal->Resource);
    KeLeaveCriticalRegion();
    ExFreePool(eventStart);
    ExFreePool(baseGuidString);
    return DokanLogError(&logger, status, L"Failed to convert GUID to string.");
  }
  RtlZeroMemory(baseGuidString, 64 * sizeof(WCHAR));
  RtlStringCchCopyW(baseGuidString, 64,
                    unicodeGuid.Buffer);
  RtlFreeUnicodeString(&unicodeGuid);

  InterlockedIncrement((LONG *)&dokanGlobal->MountId);

  if (eventStart->VolumeSecurityDescriptorLength != 0) {
    DDbgPrint("Using volume security descriptor of length %d\n",
        eventStart->VolumeSecurityDescriptorLength);
    deviceCharacteristics |= FILE_DEVICE_SECURE_OPEN;
    volumeSecurityDescriptor = eventStart->VolumeSecurityDescriptor;
  }

  status = DokanCreateDiskDevice(
      DeviceObject->DriverObject, dokanGlobal->MountId, eventStart->MountPoint,
      eventStart->UNCName, volumeSecurityDescriptor, baseGuidString,
      dokanGlobal, deviceType, deviceCharacteristics, mountGlobally,
      useMountManager, &dokanControl);

  if (!NT_SUCCESS(status)) {
    ExReleaseResourceLite(&dokanGlobal->Resource);
    KeLeaveCriticalRegion();
    ExFreePool(eventStart);
    ExFreePool(baseGuidString);
    return DokanLogError(&logger, status, L"Disk device creation failed.");
  }

  dcb = dokanControl.Dcb;
  dcb->EnableOplocks = (eventStart->Flags & DOKAN_EVENT_ENABLE_OPLOCKS) != 0;
  dcb->LogOplocks = (eventStart->Flags & DOKAN_EVENT_LOG_OPLOCKS) != 0;
  dcb->SuppressFileNameInEventContext =
      (eventStart->Flags & DOKAN_EVENT_SUPPRESS_FILE_NAME_IN_EVENT_CONTEXT)
      != 0;
  dcb->AssumePagingIoIsLocked =
      (eventStart->Flags & DOKAN_EVENT_ASSUME_PAGING_IO_IS_LOCKED) != 0;
  if (dcb->AssumePagingIoIsLocked && !dcb->SuppressFileNameInEventContext) {
    DokanLogInfo(
        &logger,
        L"Warning: DOKAN_EVENT_ASSUME_PAGING_IO_IS_LOCKED is supposed to"
        L" augment DOKAN_EVENT_SUPPRESS_FILE_NAME_IN_EVENT_CONTEXT, which"
        L" has not been set for this drive.");
  }
  dcb->OptimizeSingleNameSearch =
      (eventStart->Flags & DOKAN_EVENT_OPTIMIZE_SINGLE_NAME_SEARCH) != 0;
  dcb->DispatchNonRootOpensBeforeEventWait =
      (eventStart->Flags &
          DOKAN_EVENT_DISPATCH_NON_ROOT_OPENS_BEFORE_EVENT_WAIT) != 0;

  // This has 2 effects that differ from legacy behavior: (1) try to get rid of
  // the occupied drive, if it's a dokan drive owned by the same user; (2) have
  // the mount manager avoid using the occupied drive, if it's not one we're
  // willing to get rid of. By having the mount manager take care of that, we
  // avoid having to figure out a new suitable mount point ourselves. Because of
  // various explicit mount manager IOCTLs that dokan historically issues, we
  // need a stricter flag to avoid clobbering dokan drives than to avoid
  // clobbering real ones.
  dcb->ResolveMountConflicts =
      (eventStart->Flags & DOKAN_EVENT_RESOLVE_MOUNT_CONFLICTS) != 0;
  if (dcb->ResolveMountConflicts) {
    if (foundEntry != NULL) {
      if (MaybeUnmountOldDrive(&logger, dokanGlobal, &foundEntry->MountControl,
                               &dokanControl)) {
        driverInfo->Flags |= DOKAN_DRIVER_INFO_OLD_DRIVE_UNMOUNTED;
      } else {
        driverInfo->Flags |= DOKAN_DRIVER_INFO_OLD_DRIVE_LEFT_MOUNTED;
        dcb->ForceDriveLetterAutoAssignment = TRUE;
      }
    } else if (eventStart->Flags & DOKAN_EVENT_DRIVE_LETTER_IN_USE) {
      // The drive letter is perceived as being in use in user mode, and this
      // driver doesn't own it. In this case we explicitly ask not to use it.
      // Although we ask the mount manager to avoid clobbering existing links
      // in device.c, that only works for persistent ones and not e.g. if you
      // do something like "subst g: c:\temp".
      DokanLogInfo(
          &logger,
          L"Forcing auto-assignment because the drive letter is in use by"
          L" another driver.");
      dcb->ForceDriveLetterAutoAssignment = TRUE;
    }
  }
  if (dcb->ForceDriveLetterAutoAssignment) {
    driverInfo->Flags |= DOKAN_DRIVER_INFO_AUTO_ASSIGN_REQUESTED;
  }
  PMOUNT_ENTRY mountEntry = InsertMountEntry(dokanGlobal, &dokanControl, FALSE);
  if (mountEntry != NULL) {
    DokanLogInfo(&logger, L"Inserted new mount entry.");
  } else {
    return DokanLogError(&logger, STATUS_INSUFFICIENT_RESOURCES,
                         L"Failed to allocate new mount entry.");
  }

  dcb->FileLockInUserMode = fileLockUserMode;
  dcb->LockDebugEnabled = lockDebugEnabled;
  driverInfo->DeviceNumber = dokanGlobal->MountId;
  driverInfo->MountId = dokanGlobal->MountId;
  driverInfo->Status = DOKAN_MOUNTED;
  driverInfo->DriverVersion = dokanDriverVersion;

  // SymbolicName is
  // \\DosDevices\\Global\\Volume{D6CC17C5-1734-4085-BCE7-964F1E9F5DE9}
  // Finds the last '\' and copy into DeviceName.
  // DeviceName is \Volume{D6CC17C5-1734-4085-BCE7-964F1E9F5DE9}
  deviceNamePos = dcb->SymbolicLinkName->Length / sizeof(WCHAR) - 1;
  deviceNamePos = DokanSearchWcharinUnicodeStringWithUlong(
      dcb->SymbolicLinkName, L'\\', deviceNamePos, 0);

  RtlStringCchCopyW(driverInfo->DeviceName,
                    sizeof(driverInfo->DeviceName) / sizeof(WCHAR),
                    &(dcb->SymbolicLinkName->Buffer[deviceNamePos]));

  // Set the irp timeout in milliseconds
  // If the IrpTimeout is 0, we assume that the value was not changed
  dcb->IrpTimeout = DOKAN_IRP_PENDING_TIMEOUT;
  if (eventStart->IrpTimeout > 0) {
    if (eventStart->IrpTimeout > DOKAN_IRP_PENDING_TIMEOUT_RESET_MAX) {
      eventStart->IrpTimeout = DOKAN_IRP_PENDING_TIMEOUT_RESET_MAX;
    }

    if (eventStart->IrpTimeout < DOKAN_IRP_PENDING_TIMEOUT) {
      eventStart->IrpTimeout = DOKAN_IRP_PENDING_TIMEOUT;
    }
    dcb->IrpTimeout = eventStart->IrpTimeout;
  }

  DokanLogInfo(&logger, L"Event start using mount ID: %d; device name: %s.",
               dcb->MountId, driverInfo->DeviceName);

  dcb->UseAltStream = 0;
  if (eventStart->Flags & DOKAN_EVENT_ALTERNATIVE_STREAM_ON) {
    DDbgPrint("  ALT_STREAM_ON\n");
    dcb->UseAltStream = 1;
  }

  DokanStartEventNotificationThread(dcb);

  ExReleaseResourceLite(&dokanGlobal->Resource);
  KeLeaveCriticalRegion();

  IoVerifyVolume(dcb->DeviceObject, FALSE);
  // The mount entry now has the actual mount point, because IoVerifyVolume
  // re-entrantly invokes DokanMountVolume, which calls DokanCreateMountPoint,
  // which re-entrantly issues IOCTL_MOUNTDEV_LINK_CREATED, and that updates the
  // mount entry. We now copy the actual drive letter to the returned info. We
  // expect it to be in the form \DosDevices\G:. If it's a directory mount
  // point, this value is unused by the library.
  if (dcb->ResolveMountConflicts) {
    if (!dcb->MountPointDetermined) {
      // Getting into this block is considered very rare, and we are not even
      // sure how to achieve it naturally. It can be triggered artificially by
      // adding an applicable deleted volume record under
      // HKLM\System\MountedDevices. We don't create such records automatically
      // when ResolveMountConflicts is true. When it is false, we create them
      // with the legacy DOKAN_BASE_GUID, so those records should be ignored.
      DokanLogError(
          &logger, 0, L"Warning: mount point creation is being forced.");
      driverInfo->Flags |= DOKAN_DRIVER_INFO_MOUNT_FORCED;
      DokanCreateMountPoint(dcb);
      if (!dcb->MountPointDetermined) {
        // This is not believed to be possible. We have historical evidence that
        // DokanCreateMountPoint always works, but we don't have proof that it
        // always updates MountPointDetermined synchronously, so we still report
        // success in this case.
        driverInfo->Flags |= DOKAN_DRIVER_INFO_NO_MOUNT_POINT_ASSIGNED;
        DokanLogError(
            &logger, 0, L"Mount point was still not assigned after forcing.");
      }
    }
    if (RtlCompareMemory(mountEntry->MountControl.MountPoint,
                         L"\\DosDevices\\", 24) == 24) {
      driverInfo->ActualDriveLetter = mountEntry->MountControl.MountPoint[12];
      DokanLogInfo(&logger, L"Returning actual mount point %c",
                   driverInfo->ActualDriveLetter);
    } else {
      DokanLogInfo(
          &logger,
          L"Warning: actual mount point %s does not have expected prefix.",
          mountEntry->MountControl.MountPoint);
    }
  }
  Irp->IoStatus.Status = STATUS_SUCCESS;
  Irp->IoStatus.Information = sizeof(EVENT_DRIVER_INFO);
  ExFreePool(eventStart);
  ExFreePool(baseGuidString);

  DokanLogInfo(&logger, L"Finished event start successfully with flags: %I32x",
               driverInfo->Flags);

  return Irp->IoStatus.Status;
}

// user assinged bigger buffer that is enough to return WriteEventContext
NTSTATUS
DokanEventWrite(__in PDEVICE_OBJECT DeviceObject, _Inout_ PIRP Irp) {
  KIRQL oldIrql;
  PLIST_ENTRY thisEntry, nextEntry, listHead;
  PIRP_ENTRY irpEntry;
  PDokanVCB vcb;
  PEVENT_INFORMATION eventInfo;
  PIRP writeIrp;

  eventInfo = (PEVENT_INFORMATION)Irp->AssociatedIrp.SystemBuffer;
  ASSERT(eventInfo != NULL);

  DDbgPrint("==> DokanEventWrite [EventInfo #%X]\n", eventInfo->SerialNumber);

  vcb = DeviceObject->DeviceExtension;

  if (GetIdentifierType(vcb) != VCB) {
    return STATUS_INVALID_PARAMETER;
  }

  ASSERT(KeGetCurrentIrql() <= DISPATCH_LEVEL);
  KeAcquireSpinLock(&vcb->Dcb->PendingIrp.ListLock, &oldIrql);

  // search corresponding write IRP through pending IRP list
  listHead = &vcb->Dcb->PendingIrp.ListHead;

  for (thisEntry = listHead->Flink; thisEntry != listHead;
       thisEntry = nextEntry) {

    PIO_STACK_LOCATION writeIrpSp, eventIrpSp;
    PEVENT_CONTEXT eventContext;
    ULONG info = 0;
    NTSTATUS status;

    nextEntry = thisEntry->Flink;
    irpEntry = CONTAINING_RECORD(thisEntry, IRP_ENTRY, ListEntry);

    // check whehter this is corresponding IRP

    // DDbgPrint("SerialNumber irpEntry %X eventInfo %X\n",
    // irpEntry->SerialNumber, eventInfo->SerialNumber);

    if (irpEntry->SerialNumber != eventInfo->SerialNumber) {
      continue;
    }

    // do NOT free irpEntry here
    writeIrp = irpEntry->Irp;
    if (writeIrp == NULL) {
      // this IRP has already been canceled
      ASSERT(irpEntry->CancelRoutineFreeMemory == FALSE);
      DokanFreeIrpEntry(irpEntry);
      continue;
    }

    if (IoSetCancelRoutine(writeIrp, DokanIrpCancelRoutine) == NULL) {
      // if (IoSetCancelRoutine(writeIrp, NULL) != NULL) {
      // Cancel routine will run as soon as we release the lock
      InitializeListHead(&irpEntry->ListEntry);
      irpEntry->CancelRoutineFreeMemory = TRUE;
      continue;
    }

    writeIrpSp = irpEntry->IrpSp;
    eventIrpSp = IoGetCurrentIrpStackLocation(Irp);

    ASSERT(writeIrpSp != NULL);
    ASSERT(eventIrpSp != NULL);

    eventContext =
        (PEVENT_CONTEXT)
            writeIrp->Tail.Overlay.DriverContext[DRIVER_CONTEXT_EVENT];
    ASSERT(eventContext != NULL);

    // short of buffer length
    if (eventIrpSp->Parameters.DeviceIoControl.OutputBufferLength <
        eventContext->Length) {
      DDbgPrint("  EventWrite: STATUS_INSUFFICIENT_RESOURCE\n");
      status = STATUS_INSUFFICIENT_RESOURCES;
    } else {
      PVOID buffer;
      // DDbgPrint("  EventWrite CopyMemory\n");
      // DDbgPrint("  EventLength %d, BufLength %d\n", eventContext->Length,
      //            eventIrpSp->Parameters.DeviceIoControl.OutputBufferLength);
      if (Irp->MdlAddress)
        buffer = MmGetSystemAddressForMdlNormalSafe(Irp->MdlAddress);
      else
        buffer = Irp->AssociatedIrp.SystemBuffer;

      ASSERT(buffer != NULL);
      RtlCopyMemory(buffer, eventContext, eventContext->Length);

      info = eventContext->Length;
      status = STATUS_SUCCESS;
    }

    DokanFreeEventContext(eventContext);
    writeIrp->Tail.Overlay.DriverContext[DRIVER_CONTEXT_EVENT] = 0;

    KeReleaseSpinLock(&vcb->Dcb->PendingIrp.ListLock, oldIrql);

    Irp->IoStatus.Status = status;
    Irp->IoStatus.Information = info;

    // this IRP will be completed by caller function
    return Irp->IoStatus.Status;
  }

  KeReleaseSpinLock(&vcb->Dcb->PendingIrp.ListLock, oldIrql);

  // if the corresponding IRP not found, the user should already canceled the operation and the IRP already destroyed.
  DDbgPrint("  EventWrite : Cannot found corresponding IRP. User should already canceled the operation. Return STATUS_CANCELLED.");

  return STATUS_CANCELLED;
}
