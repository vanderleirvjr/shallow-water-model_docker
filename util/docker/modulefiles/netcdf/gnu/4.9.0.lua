require("posix")

-- The message printed by the module whatis command
whatis("netcdf v4.9.0")

-- The message printed by the module help command
help([[
NetCDF is a set of software libraries and self-describing,
machine-independent data formats that support the creation, access,
and sharing of array-oriented scientific data.

This module loads the serial NetCDF interfaces for C (v4.9.0), and Fortran
(v4.5.3).

Software website - http://www.unidata.ucar.edu/software/netcdf/

Modules used:
   intel/2017.7.1
   hdf5/1.12.2
]])

-- Set prerequisites and conflicts
conflict("hdf5")

-- Set paths for software binaries, libraries, headers, and manuals
local basepath = "/usr/local/default/gnu/netcdf"
local binpath  = pathJoin(basepath, "/bin")             -- binaries
local libpath  = pathJoin(basepath, "/lib")             -- libraries
local incpath  = pathJoin(basepath, "/include")         -- include files
local pkgpath  = pathJoin(basepath, "/lib/pkgconfig")   -- pkgconfig
local manpath  = pathJoin(basepath, "/share/man")       -- man pages
local libs     = "-Wl,-Bstatic -lnetcdff -lnetcdf -lhdf5hl_fortran -lhdf5_hl -lhdf5_fortran -lhdf5 -lsz -lz -Wl,-Bdynamic -lm -ldl"

-- Update the binary, pkgconfig, and manual paths in user environment
prepend_path("PATH",            binpath)
prepend_path("PKG_CONFIG_PATH", pkgpath)
prepend_path("MANPATH",         manpath)
prepend_path("LD_LIBRARY_PATH", libpath)

-- Set NetCDF variable for application builds
setenv("NETCDF", basepath)