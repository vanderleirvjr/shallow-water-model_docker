require("posix")

-- The message printed by the module whatis command
whatis("zlib v1.2.13")

-- The message printed by the module help command
help([[
Software website - https://zlib.net/

Modules used:
   intel/2017.7.1
]])

-- Set paths for software binaries, libraries, and headers
local basepath = "/usr/local/default/gnu/zlib"
local libpath  = pathJoin(basepath, "/lib")             -- libraries
local incpath  = pathJoin(basepath, "/include")         -- include files

-- Update the binary, and manual paths in user environment
prepend_path("LD_LIBRARY_PATH", libpath)

-- Set ZLIB variable for application builds
setenv("ZLIB", basepath)
