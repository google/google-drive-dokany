{
    'defines': [
        # This is the basis of the device name returned by QueryDosDevice for a
        # dokan drive. The 2nd and 3rd parts get modified for each new drive
        # mounted within a Windows session, but the same base GUID is used by all
        # versions of the driver.
        # {8BBAEE64-42F7-49A2-BFDF-4882767802DB}
        'DOKAN_BASE_GUID={ 0x8bbaee64, 0x42f7, 0x49a2, { 0xbf, 0xdf, 0x48, 0x82, 0x76, 0x78, 0x2, 0xdb } }',

        # This GUID is used instead of the above when using
        # either the C-based dokan DLL with
        # DOKAN_OPTION_RESOLVE_MOUNT_CONFLICTS, or the dokancc DLL with any
        # options. It prevents the new mount logic from being affected by stale
        # entries that the old logic leaves in the mount manager database.
        # {710DC066-9D89-45A8-BA7A-F575A948586D}
        'DOKAN_BASE_GUID_V2={ 0x710dc066, 0x9d89, 0x45a8, { 0xba, 0x7a, 0xf5, 0x75, 0xa9, 0x48, 0x58, 0x6d } }',
    ],
}