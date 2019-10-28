{
  # Determines the latest signed driver version to be used by DriveFS.
  # Update this after checking in a new signed driver build.
  'variables': {
    'DOKAN_VERSION': '2019-10-23_1918'
  },
  'includes': {
    # This presumes that copy_dokan_version.py or the equivalent has been run
    # (i.e. the dokan_version.gypi from the directory for DOKAN_VERSION has been
    # copied to the google3 root). That is a workaround for gyp not being able
    # to use variables in includes.
    # See https://gyp.gsrc.io/docs/InputFormatReference.md#including-other-files 
    '../../../../../dokan_version.gypi',
  },
}
