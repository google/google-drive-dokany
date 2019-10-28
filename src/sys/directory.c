/*
  Dokan : user-mode file system library for Windows

  Copyright (C) 2015 - 2017 Adrien J. <liryna.stark@gmail.com> and Maxime C. <maxime@islog.com>
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

NTSTATUS
DokanQueryDirectory(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp);

NTSTATUS
DokanNotifyChangeDirectory(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp);

// Finds an FCB whose path has the given prefix. The prefix should not end with
// a backslash.
// - An FCB for \foo\bar\abc\xyz could be returned for the prefix "\foo\bar".
// - An FCB for \foo\barabcxyz would not be returned for the prefix "\foo\bar",
//   because the prefix matching is in terms of whole path components.
// - If there are multiple open matching FCBs, an arbitrary one is returned.
// - If there are no open matching FCBs, the function returns NULL.
// - If the function does not return NULL, the caller must call DokanFreeFCB
//   when finished with it, in order for the FCB to ever be closed.
static PDokanFCB DokanFindFCBWithPrefix(__in PDokanVCB Vcb,
                                        __in const UNICODE_STRING* Prefix,
                                        __in BOOLEAN CaseSensitive) {
  PLIST_ENTRY thisEntry = NULL;
  PLIST_ENTRY nextEntry = NULL;
  PLIST_ENTRY listHead = &Vcb->NextFCB;
  PDokanFCB fcb = NULL;
  UNICODE_STRING matchablePart;
  matchablePart.Length = Prefix->Length;
  matchablePart.MaximumLength = Prefix->Length;
  DokanVCBLockRW(Vcb);
  for (thisEntry = listHead->Flink; thisEntry != listHead;
       thisEntry = nextEntry) {
    nextEntry = thisEntry->Flink;
    fcb = CONTAINING_RECORD(thisEntry, DokanFCB, NextFCB);
    if (fcb->FileName.Length == Prefix->Length ||
        (fcb->FileName.Length > Prefix->Length &&
            fcb->FileName.Buffer[Prefix->Length / sizeof(WCHAR)] == L'\\')) {
      matchablePart.Buffer = fcb->FileName.Buffer;
      if (RtlEqualUnicodeString(Prefix, &matchablePart, !CaseSensitive)) {
        break;
      }
    }
    fcb = NULL;
  }
  if (fcb != NULL) {
    InterlockedIncrement(&fcb->FileCount);
  }
  DokanVCBUnlock(Vcb);
  return fcb;
}

// Optimization for Windows 7 path normalization in the Filter Manager. See
// the related DokanDCB flag for more info.
static NTSTATUS DokanOptimizeSingleNameSearch(__in PIRP Irp,
                                              __in PIO_STACK_LOCATION IrpSp,
                                              __in PDokanVCB Vcb,
                                              __in PDokanFCB DirectoryFcb) {
  ULONG bufferLen = IrpSp->Parameters.QueryDirectory.Length;
  const UNICODE_STRING* componentName =
      IrpSp->Parameters.QueryDirectory.FileName;
  if (bufferLen < sizeof(FILE_NAMES_INFORMATION) - 1 + componentName->Length) {
    return STATUS_BUFFER_OVERFLOW;
  }
  if (!DokanVCBTryLockRW(Vcb)) {
    return STATUS_INVALID_PARAMETER;
  }
  DokanFCBLockRO(DirectoryFcb);
  NTSTATUS status = STATUS_SUCCESS;
  PDokanFCB targetFcb = NULL;
  UNICODE_STRING fullName;
  fullName.Length = 0;
  fullName.MaximumLength = DirectoryFcb->FileName.Length +
      componentName->Length + sizeof(WCHAR);
  fullName.Buffer = ExAllocatePool(fullName.MaximumLength);
  __try {
    PVOID buffer = Irp->UserBuffer;
    if (Irp->MdlAddress) {
      buffer = MmGetSystemAddressForMdlNormalSafe(Irp->MdlAddress);
    }
    RtlUnicodeStringCopy(&fullName, &DirectoryFcb->FileName);
    if (fullName.Length == 0 ||
        fullName.Buffer[(fullName.Length - 1) / sizeof(WCHAR)] != L'\\') {
      RtlUnicodeStringCbCatStringN(&fullName, L"\\", 2 * sizeof(WCHAR));
    }
    RtlUnicodeStringCbCatN(&fullName, componentName, componentName->Length);
    targetFcb = DokanFindFCBWithPrefix(Vcb, &fullName, TRUE);
    if (targetFcb == NULL) {
      status = STATUS_OBJECT_NAME_NOT_FOUND;
      __leave;
    }
    FILE_NAMES_INFORMATION* output = (FILE_NAMES_INFORMATION*)buffer;
    output->NextEntryOffset = 0;
    // Note: this is not required to be unique or consistent.
    output->FileIndex = 0;
    output->FileNameLength = componentName->Length;
    const WCHAR* realName = (const WCHAR*)((char*)targetFcb->FileName.Buffer +
        fullName.Length - output->FileNameLength);
    RtlCopyMemory(output->FileName, realName, output->FileNameLength);
  } __finally {
    ExFreePool(fullName.Buffer);
    if (targetFcb != NULL) {
      DokanFreeFCB(Vcb, targetFcb);
    }
    DokanFCBUnlock(DirectoryFcb);
    DokanVCBUnlock(Vcb);
  }
  return status;
}

// This wrapper makes it easier to trace when the optimization happens. The
// status returned is whether the optimization succeeds, and has no bearing on
// whether the non-optimized path would succeed.
static NTSTATUS
DokanMaybeOptimizeSingleNameSearch(__in PIRP Irp,
                                   __in PIO_STACK_LOCATION IrpSp,
                                   __in PDokanVCB Vcb,
                                   __in PDokanFCB DirectoryFcb) {
  if (Vcb->Dcb == NULL || !Vcb->Dcb->OptimizeSingleNameSearch) {
    return STATUS_INVALID_PARAMETER;
  }
  if (!(IrpSp->Flags & SL_RETURN_SINGLE_ENTRY)) {
    return STATUS_INVALID_PARAMETER;
  }
  if (IrpSp->Parameters.QueryDirectory.FileInformationClass !=
      FileNamesInformation) {
    return STATUS_INVALID_PARAMETER;
  }
  return DokanOptimizeSingleNameSearch(Irp, IrpSp, Vcb, DirectoryFcb);
}

NTSTATUS
DokanDispatchDirectoryControl(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp) {
  NTSTATUS status = STATUS_NOT_IMPLEMENTED;
  PFILE_OBJECT fileObject;
  PIO_STACK_LOCATION irpSp;
  PDokanVCB vcb;

  __try {
    DDbgPrint("==> DokanDirectoryControl\n");

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    fileObject = irpSp->FileObject;

    if (fileObject == NULL) {
      DDbgPrint("   fileObject is NULL\n");
      status = STATUS_INVALID_PARAMETER;
      __leave;
    }

    vcb = DeviceObject->DeviceExtension;
    if (GetIdentifierType(vcb) != VCB ||
        !DokanCheckCCB(vcb->Dcb, fileObject->FsContext2)) {
      status = STATUS_INVALID_PARAMETER;
      __leave;
    }

    DDbgPrint("  ProcessId %lu\n", IoGetRequestorProcessId(Irp));
    DokanPrintFileName(fileObject);

    if (irpSp->MinorFunction == IRP_MN_QUERY_DIRECTORY) {
      status = DokanQueryDirectory(DeviceObject, Irp);

    } else if (irpSp->MinorFunction == IRP_MN_NOTIFY_CHANGE_DIRECTORY) {
      status = DokanNotifyChangeDirectory(DeviceObject, Irp);
    } else {
      DDbgPrint("  invalid minor function\n");
      status = STATUS_INVALID_PARAMETER;
    }

  } __finally {

    DokanCompleteIrpRequest(Irp, status, 0);

    DDbgPrint("<== DokanDirectoryControl\n");
  }

  return status;
}

NTSTATUS
DokanQueryDirectory(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp) {
  PFILE_OBJECT fileObject;
  PIO_STACK_LOCATION irpSp;
  PDokanVCB vcb;
  PDokanCCB ccb;
  PDokanFCB fcb;
  NTSTATUS status;
  ULONG eventLength;
  PEVENT_CONTEXT eventContext;
  ULONG index;
  BOOLEAN initial;
  ULONG flags = 0;
  BOOLEAN fcbLocked = FALSE;

  irpSp = IoGetCurrentIrpStackLocation(Irp);
  fileObject = irpSp->FileObject;

  vcb = DeviceObject->DeviceExtension;
  if (GetIdentifierType(vcb) != VCB) {
    return STATUS_INVALID_PARAMETER;
  }

  ccb = fileObject->FsContext2;
  if (ccb == NULL) {
    return STATUS_INVALID_PARAMETER;
  }
  ASSERT(ccb != NULL);

  if (irpSp->Flags & SL_INDEX_SPECIFIED) {
    DDbgPrint("  index specified %d\n",
              irpSp->Parameters.QueryDirectory.FileIndex);
  }
  if (irpSp->Flags & SL_RETURN_SINGLE_ENTRY) {
    DDbgPrint("  return single entry\n");
  }
  if (irpSp->Flags & SL_RESTART_SCAN) {
    DDbgPrint("  restart scan\n");
  }
  if (irpSp->Parameters.QueryDirectory.FileName) {
    DDbgPrint("  pattern:%wZ\n", irpSp->Parameters.QueryDirectory.FileName);
  }

  switch (irpSp->Parameters.QueryDirectory.FileInformationClass) {
  case FileDirectoryInformation:
    DDbgPrint("  FileDirectoryInformation\n");
    break;
  case FileIdFullDirectoryInformation:
    DDbgPrint("  FileIdFullDirectoryInformation\n");
    break;
  case FileFullDirectoryInformation:
    DDbgPrint("  FileFullDirectoryInformation\n");
    break;
  case FileNamesInformation:
    DDbgPrint("  FileNamesInformation\n");
    break;
  case FileBothDirectoryInformation:
    DDbgPrint("  FileBothDirectoryInformation\n");
    break;
  case FileIdBothDirectoryInformation:
    DDbgPrint("  FileIdBothDirectoryInformation\n");
    break;
  default:
    DDbgPrint("  unknown FileInfoClass %d\n",
              irpSp->Parameters.QueryDirectory.FileInformationClass);
    break;
  }

  fcb = ccb->Fcb;
  ASSERT(fcb != NULL);

  OplockDebugRecordMajorFunction(fcb, IRP_MJ_DIRECTORY_CONTROL);
  status = DokanMaybeOptimizeSingleNameSearch(Irp, irpSp, vcb, fcb);
  if (status == STATUS_SUCCESS || status == STATUS_BUFFER_OVERFLOW) {
    return status;
  }

  // make a MDL for UserBuffer that can be used later on another thread context
  if (Irp->MdlAddress == NULL) {
    status = DokanAllocateMdl(Irp, irpSp->Parameters.QueryDirectory.Length);
    if (!NT_SUCCESS(status)) {
      return status;
    }
    flags = DOKAN_MDL_ALLOCATED;
  }

  eventLength = sizeof(EVENT_CONTEXT);
  if (!vcb->Dcb->SuppressFileNameInEventContext) {
    // size of EVENT_CONTEXT is sum of its length and file name length
    DokanFCBLockRO(fcb);
    eventLength += fcb->FileName.Length;
    fcbLocked = TRUE;
  }

  initial = (BOOLEAN)(ccb->SearchPattern == NULL &&
                      !(DokanCCBFlagsIsSet(ccb, DOKAN_DIR_MATCH_ALL)));

  // this is an initial query
  if (initial) {
    DDbgPrint("    initial query\n");
    // and search pattern is provided
    if (irpSp->Parameters.QueryDirectory.FileName) {
      // free current search pattern stored in CCB
      if (ccb->SearchPattern)
        ExFreePool(ccb->SearchPattern);

      // the size of search pattern
      ccb->SearchPatternLength =
          irpSp->Parameters.QueryDirectory.FileName->Length;
      ccb->SearchPattern =
          ExAllocatePool(ccb->SearchPatternLength + sizeof(WCHAR));

      if (ccb->SearchPattern == NULL) {
        if (fcbLocked) {
          DokanFCBUnlock(fcb);
        }
        return STATUS_INSUFFICIENT_RESOURCES;
      }

      RtlZeroMemory(ccb->SearchPattern,
                    ccb->SearchPatternLength + sizeof(WCHAR));

      // copy provided search pattern to CCB
      RtlCopyMemory(ccb->SearchPattern,
                    irpSp->Parameters.QueryDirectory.FileName->Buffer,
                    ccb->SearchPatternLength);

    } else {
      DokanCCBFlagsSetBit(ccb, DOKAN_DIR_MATCH_ALL);
    }
  }

  // if search pattern is provided, add the length of it to store pattern
  if (ccb->SearchPattern) {
    eventLength += ccb->SearchPatternLength;
  }

  eventContext = AllocateEventContext(vcb->Dcb, Irp, eventLength, ccb);

  if (eventContext == NULL) {
    if (fcbLocked) {
      DokanFCBUnlock(fcb);
    }
    return STATUS_INSUFFICIENT_RESOURCES;
  }

  eventContext->Context = ccb->UserContext;
  // DDbgPrint("   get Context %X\n", (ULONG)ccb->UserContext);

  // index which specified index-1 th directory entry has been returned
  // this time, 'index'th entry should be returned
  index = 0;

  if (irpSp->Flags & SL_INDEX_SPECIFIED) {
    index = irpSp->Parameters.QueryDirectory.FileIndex;
    DDbgPrint("    using FileIndex %d\n", index);

  } else if (FlagOn(irpSp->Flags, SL_RESTART_SCAN)) {
    DDbgPrint("    SL_RESTART_SCAN\n");
    index = 0;

  } else {
    index = (ULONG)ccb->Context;
    DDbgPrint("    ccb->Context %d\n", index);
  }

  eventContext->Operation.Directory.FileInformationClass =
      irpSp->Parameters.QueryDirectory.FileInformationClass;
  eventContext->Operation.Directory.BufferLength =
      irpSp->Parameters.QueryDirectory.Length; // length of buffer
  eventContext->Operation.Directory.FileIndex =
      index; // directory index which should be returned this time

  // copying file name(directory name)
  if (!vcb->Dcb->SuppressFileNameInEventContext) {
    eventContext->Operation.Directory.DirectoryNameLength =
        fcb->FileName.Length;
    RtlCopyMemory(eventContext->Operation.Directory.DirectoryName,
                  fcb->FileName.Buffer, fcb->FileName.Length);
    DokanFCBUnlock(fcb);
  }

  // if search pattern is specified, copy it to EventContext
  if (ccb->SearchPatternLength && ccb->SearchPattern) {
    PVOID searchBuffer;

    eventContext->Operation.Directory.SearchPatternLength =
        ccb->SearchPatternLength;
    eventContext->Operation.Directory.SearchPatternOffset =
        eventContext->Operation.Directory.DirectoryNameLength;

    searchBuffer = (PVOID)(
        (SIZE_T)&eventContext->Operation.Directory.SearchPatternBase[0] +
        (SIZE_T)eventContext->Operation.Directory.SearchPatternOffset);

    RtlCopyMemory(searchBuffer, ccb->SearchPattern, ccb->SearchPatternLength);

    DDbgPrint("    ccb->SearchPattern %ws\n", ccb->SearchPattern);
  }

  status = DokanRegisterPendingIrp(DeviceObject, Irp, eventContext, flags);

  return status;
}

NTSTATUS
DokanNotifyChangeDirectory(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp) {
  PDokanCCB ccb;
  PDokanFCB fcb;
  PFILE_OBJECT fileObject;
  PIO_STACK_LOCATION irpSp;
  PDokanVCB vcb;

  DDbgPrint("\tNotifyChangeDirectory\n");

  irpSp = IoGetCurrentIrpStackLocation(Irp);
  fileObject = irpSp->FileObject;

  vcb = DeviceObject->DeviceExtension;
  if (GetIdentifierType(vcb) != VCB) {
    return STATUS_INVALID_PARAMETER;
  }

  ccb = fileObject->FsContext2;
  ASSERT(ccb != NULL);

  fcb = ccb->Fcb;
  ASSERT(fcb != NULL);

  if (!DokanFCBFlagsIsSet(fcb, DOKAN_FILE_DIRECTORY)) {
    return STATUS_INVALID_PARAMETER;
  }

  DDbgPrint("\tFsRtlNotifyFullChangeDirectory FileName %wZ\n", &fcb->FileName);
  
  DokanFCBLockRO(fcb);
  FsRtlNotifyFullChangeDirectory(
      vcb->NotifySync, &vcb->DirNotifyList, ccb, (PSTRING)&fcb->FileName,
      (irpSp->Flags & SL_WATCH_TREE) ? TRUE : FALSE, FALSE,
      irpSp->Parameters.NotifyDirectory.CompletionFilter, Irp, NULL, NULL);
  DokanFCBUnlock(fcb);

  return STATUS_PENDING;
}

VOID DokanCompleteDirectoryControl(__in PIRP_ENTRY IrpEntry,
                                   __in PEVENT_INFORMATION EventInfo) {
  PIRP irp;
  PIO_STACK_LOCATION irpSp;
  NTSTATUS status = STATUS_SUCCESS;
  ULONG info = 0;
  ULONG bufferLen = 0;
  PVOID buffer = NULL;

  DDbgPrint("==> DokanCompleteDirectoryControl\n");

  irp = IrpEntry->Irp;
  irpSp = IrpEntry->IrpSp;

  // buffer pointer which points DirecotryInfo
  if (irp->MdlAddress) {
    // DDbgPrint("   use MDL Address\n");
    buffer = MmGetSystemAddressForMdlNormalSafe(irp->MdlAddress);
  } else {
    // DDbgPrint("   use UserBuffer\n");
    buffer = irp->UserBuffer;
  }
  // usable buffer size
  bufferLen = irpSp->Parameters.QueryDirectory.Length;

  // DDbgPrint("  !!Returning DirecotyInfo!!\n");

  // buffer is not specified or short of length
  if (bufferLen == 0 || buffer == NULL || bufferLen < EventInfo->BufferLength) {
    info = 0;
    status = STATUS_INSUFFICIENT_RESOURCES;

  } else {

    PDokanCCB ccb = IrpEntry->FileObject->FsContext2;
    // ULONG     orgLen = irpSp->Parameters.QueryDirectory.Length;

    //
    // set the information recieved from user mode
    //
    ASSERT(buffer != NULL);

    RtlZeroMemory(buffer, bufferLen);

    // DDbgPrint("   copy DirectoryInfo\n");
    RtlCopyMemory(buffer, EventInfo->Buffer, EventInfo->BufferLength);

    DDbgPrint("    eventInfo->Directory.Index = %lu\n",
              EventInfo->Operation.Directory.Index);
    DDbgPrint("    eventInfo->BufferLength    = %lu\n",
              EventInfo->BufferLength);
    DDbgPrint("    eventInfo->Status = %x (%lu)\n", EventInfo->Status,
              EventInfo->Status);

    // update index which specified n-th directory entry is returned
    // this should be locked before writing?
    ccb->Context = EventInfo->Operation.Directory.Index;

    ccb->UserContext = EventInfo->Context;
    // DDbgPrint("   set Context %X\n", (ULONG)ccb->UserContext);

    // written bytes
    // irpSp->Parameters.QueryDirectory.Length = EventInfo->BufferLength;

    status = EventInfo->Status;

    info = EventInfo->BufferLength;
  }

  if (IrpEntry->Flags & DOKAN_MDL_ALLOCATED) {
    DokanFreeMdl(irp);
    IrpEntry->Flags &= ~DOKAN_MDL_ALLOCATED;
  }

  DokanCompleteIrpRequest(irp, status, info);

  DDbgPrint("<== DokanCompleteDirectoryControl\n");
}
