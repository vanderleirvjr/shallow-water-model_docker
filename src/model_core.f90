module model_core

    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    use system_state, only: initialize_grid, initial_conditions
    use kinds, only: r_kind, i_kind 
    use gridinfo, only: grid2d
    use io_module, only: read_namelist, write_output, namelist_options
    use physics, only: update_state

    implicit none

    type(grid2d) :: grid
    type(namelist_options) :: opt

    contains

        subroutine initialize

            implicit none

            call read_namelist(opt)

            call initialize_grid(opt%nx,opt%ny,opt%dx,opt%dy,grid)

            call initial_conditions(grid)

            return

        end subroutine initialize

        subroutine run

            implicit none

            integer              :: i, iterations
            integer(kind=i_kind) :: stdout
            real(kind=r_kind)    :: elapsed
            real(kind=r_kind)    :: start, finish

            iterations = int(opt%run_time / opt%dt)

            do i = 1, iterations + 1
                
                if ( mod((i-1)*opt%dt,opt%history_interval) == 0 ) then
                    call CPU_TIME(start)
                    write(stdout,'("Writing output file... Time step = ",F6.2)') (i-1)*opt%dt
                    call write_output(grid,opt,i)
                    call CPU_TIME(finish)
                    elapsed = finish - start
                    write(stdout,'("Done. Time elapsed = ",F8.4)') elapsed
                end if

                call update_state(grid,opt%dt)
    
            end do

            return

        end subroutine

        subroutine finalize

            implicit none

            return
        
        end subroutine finalize

end module model_core