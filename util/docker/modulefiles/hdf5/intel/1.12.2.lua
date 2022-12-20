require("posix")

-- The message printed by the module whatis command
whatis("hdf5 v1.12.2")

-- The message printed by the module help command
help([[
Software website - https://www.hdfgroup.org/downloads/hdf5/

Modules used:
   intel/2021.7.1
   zlib/1.2.13
]])

-- Set paths for software binaries, libraries, and headers
local basepath = "/usr/local/default/intel/hdf5"
local binpath  = pathJoin(basepath, "/bin")             -- binaries
local libpath  = pathJoin(basepath, "/lib")             -- libraries
local incpath  = pathJoin(basepath, "/include")         -- include files

-- Update the binary, pkgconfig, and manual paths in user environment
prepend_path("PATH",            binpath)
prepend_path("LD_LIBRARY_PATH", libpath)

-- Set HDF5 variable for application builds
setenv("HDF5", basepath)