module model_core

! This module contains the main subroutines responsible for initialize,
! integrate, and finalize the model.

#ifdef MPI    
    use mpi
    use physics_mpi, only: update_state
#else    
    use physics, only: update_state
#endif

    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    use system_state, only: initialize_grid, initial_conditions
    use kinds, only: r_kind, i_kind 
    use gridinfo, only: grid2d
    use io_module, only: read_namelist, write_output, namelist_options

    implicit none

    public :: initialize, run, finalize
    private

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
            integer              :: ierror, isize, irank = 0
            integer(kind=i_kind) :: stdout
            real(kind=r_kind)    :: elapsed
            real(kind=r_kind)    :: start, finish

            iterations = int(opt%run_time / opt%dt)
           
#ifdef MPI
            call MPI_INIT(ierror)
            call MPI_COMM_RANK(MPI_COMM_WORLD,irank,ierror)
            call MPI_COMM_SIZE(MPI_COMM_WORLD,isize,ierror)
#endif

            do i = 1, iterations + 1                

                
                if ( irank .EQ. 0 ) then
                
                    if ( mod((i-1)*opt%dt*1000,opt%history_interval*1000) == 0 ) then                        
                        write(stdout,'("Writing output file... Time step = ",F6.2)') (i-1)*opt%dt
                        call write_output(grid,opt,i)                        
                        elapsed = finish - start
                        write(stdout,'("Done. Time elapsed = ",F8.4)') elapsed
                    end if
                
                end if

                call update_state(grid,opt%dt,opt%mheight,opt%fc,opt%bc)

#ifdef MPI
                call MPI_BARRIER(MPI_COMM_WORLD,ierror)
#endif                
    
            end do

            return

        end subroutine

        subroutine finalize

            implicit none

#ifdef MPI
            call MPI_FINALIZE(ierror)
#endif

            return
        
        end subroutine finalize

end module model_core