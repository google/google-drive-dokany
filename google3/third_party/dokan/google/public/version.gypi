{
  "includes": [
     # For "normal" builds, use the dokan_version created at the biuld top.
     "../../../../dokan_version.gypi",
     # For hacking around and building the .dll against an existing library,
     # you should uncomment the next line and change to point to an existing
     # dokan build.
     # "../../../../../vendor_src/dokan/google_signed/53/dokan_version.gypi",
  ]
}
