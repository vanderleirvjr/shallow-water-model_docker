require("posix")
family("compiler")

-- The message printed by the module whatis command
whatis("intel v2021.7.1")

-- The message printed by the module help command
help([[
This module loads the Intel Compilers:
  C:        icc
  C++:      icpc
  Fortran:  ifort
For more information on the individual compilers and their suboptions
refer to the man page for the individual compilers.

Note that compiler version 2021.7.1 is part of version 2020u4
of the Intel Parallel Studio.

Website: https://software.intel.com/en-us/parallel-studio-xe
]])


-- Update modulepath to contain correct dependent module tree
local mroot = "/usr/local/modulefiles/default"
local mdir = pathJoin(mroot,"intel")
append_path("MODULEPATH",mdir)

-- Set compiler variables for build systems
setenv("CC",            "icc")
setenv("CXX",           "icpc")
setenv("FC",            "ifort")
setenv("F77",           "ifort")
setenv("F90",           "ifort")
setenv("MPICC_CC",      "icc")
setenv("MPICXX_CXX",    "icpc")
setenv("MPIF90_F90",    "ifort")

-- Add the Intel tools from the PSXE
local advpath = pathJoin(basepath, "advisor")
local inspath = pathJoin(basepath, "inspector")
local vtupath = pathJoin(basepath, "vtune_amplifier")

setenv("ADVISOR_2020_DIR",           advpath)
setenv("INSPECTOR_2020_DIR",         inspath)
setenv("VTUNE_AMPLIFIER_2020_DIR",   vtupath)

local toolbin = "bin64"
local advbin  = pathJoin(advpath, toolbin)
local insbin  = pathJoin(inspath, toolbin)
local vtubin  = pathJoin(vtupath, toolbin)

prepend_path("PATH", advbin)
prepend_path("PATH", insbin)
prepend_path("PATH", vtubin)