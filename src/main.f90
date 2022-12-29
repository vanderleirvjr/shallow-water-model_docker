program shallow_water

! This is the main program responsible to call the
! main subroutines.

    use model_core, only: initialize, run, finalize

    implicit none

    call initialize    

    call run

    call finalize

end program shallow_water