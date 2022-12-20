require("posix")
family("compiler")

-- The message printed by the module whatis command
whatis("gnu v9.4.0")

-- The message printed by the module help command
help([[
This module loads the GNU 10.2.1 compilers:
 Fortran: gfortran
 C: gcc
 C++: g++
For more information on the individual compilers and their suboptions,
refer to the man pages for the individual compilers. Also see
http://gcc.gnu.org for details.

Software website - https://gcc.gnu.org/

]])

-- Set paths for software binaries, libraries, headers, and manuals
local basepath = "/usr"
local binpath  = pathJoin(basepath, "/bin")             -- binaries
local libpath  = pathJoin(basepath, "/lib64")           -- libraries
local incpath  = pathJoin(basepath, "/include")         -- include files
local manpath  = pathJoin(basepath, "/share/man")       -- man pages
local l32path  = pathJoin(basepath, "/lib/gcc/x86_64-pc-linux-gnu/10")

-- Update the binary and manual paths in user environment
prepend_path("PATH",    binpath)
prepend_path("MANPATH", manpath)

-- Runtime libraries
prepend_path("LD_LIBRARY_PATH", libpath)

-- Set identifier variables
setenv("MP_COMPILER",           "gnu")
setenv("COMPILER",              "gnu")
setenv("LMOD_COMPILER",         "gnu")
setenv("GNU_MAJOR_VERSION",     "9.4")
setenv("GNU_MINOR_VERSION",     "0")
setenv("LMOD_COMPILER_VERSION", "9.4.0")

-- Update modulepath to contain correct dependent module tree
local mroot = "/usr/local"
local mdir = pathJoin(mroot,"gnu/9.4.0")
append_path("MODULEPATH",mdir)

-- Set compiler variables for build systems
setenv("CC",         "gcc")
setenv("CXX",        "g++")
setenv("FC",         "gfortran")
setenv("F77",        "gfortran")
setenv("F90",        "gfortran")
setenv("MPICC_CC",   "gcc")
setenv("MPICXX_CXX", "g++")
setenv("MPIF90_F90", "gfortran")

