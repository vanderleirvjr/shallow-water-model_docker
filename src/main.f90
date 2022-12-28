program shallow_water

! This is the main program responsible to call the
! main subroutines.

#ifdef MPI    
    use model_core_mpi, only: initialize, run, finalize

#else 
    use model_core, only: initialize, run, finalize
#endif

    implicit none

    call initialize    

    call run

    call finalize

end program shallow_water