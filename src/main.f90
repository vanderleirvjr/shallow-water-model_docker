program shallow_water

    use model_core, only: initialize, run, finalize

    implicit none

    call initialize

    call run

    call finalize

end program shallow_water