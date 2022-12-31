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

            integer              :: ierror, irank = 0
            real(kind=r_kind)    :: start, end

#ifdef MPI
            call MPI_INIT(ierror)
            call MPI_COMM_RANK(MPI_COMM_WORLD,irank,ierror)
#endif

            if (irank == 0) then
                call CPU_TIME(start)
                write(stdout,*)
                write(stdout,*) '-------------------------------------------'
                write(stdout,*)
                write(stdout,'(12X,A19)') 'Shallow Water Model'
                write(stdout,*)
                write(stdout,*) '-------------------------------------------'
                write(stdout,*)
                write(stdout,'(A25)') 'Initializing the model...'
            end if

            call read_namelist(opt)
               
            call initialize_grid(opt%nx,opt%ny,opt%dx,opt%dy,grid)

            call initial_conditions(grid)

            if (irank == 0) then
                call CPU_TIME(end)
                write(stdout,'(A41,F14.6)') 'Initialization completed. Time elapsed = ', end - start
            end if

#ifdef MPI
            call MPI_BARRIER(MPI_COMM_WORLD,ierror)
#endif

            return

        end subroutine initialize

        subroutine run

            implicit none

            integer              :: i, iterations, j, k
            integer              :: ierror, isize, irank = 0
            integer(kind=i_kind) :: stdout
            real(kind=r_kind)    :: start, end, wstart, wend, tstart, tend

            iterations = int(opt%run_time / opt%dt)
           
#ifdef MPI
            call MPI_COMM_RANK(MPI_COMM_WORLD,irank,ierror)
            call MPI_COMM_SIZE(MPI_COMM_WORLD,isize,ierror)
#endif

            if (irank == 0) then
                call CPU_TIME(tstart)
                write(stdout,'(A28)') 'Integration step starting...'
            end if

            do i = 1, iterations + 1                

                
                if ( irank .EQ. 0 ) then
                
                    if ( mod((i-1)*opt%dt*1000,opt%history_interval*1000) == 0 ) then
                        call CPU_TIME(wstart)                        
                        write(stdout,'("Writing output file... Time step = ",F6.2)') (i-1)*opt%dt
                        call write_output(grid,opt,i)
                        call CPU_TIME(wend)                        
                        write(stdout,'("Done. Time elapsed = ",F14.6)') wend - wstart
                    end if
                
                end if

                if (irank == 0) then
                    call CPU_TIME(start) 
                end if

                call update_state(grid,opt%dt,opt%mheight,opt%fc,opt%bc)

#ifdef MPI
                call MPI_BARRIER(MPI_COMM_WORLD,ierror)
#endif

                if (irank == 0) then
                    call CPU_TIME(end)
                    write(stdout,'(A16,1X,I10,1X,A1,1X,F14.6,A16)') 'Simulation step:', i, ':', end - start, 'elapsed seconds.'
                end if
    
            end do

            if (irank == 0) then
                call CPU_TIME(tend)
                write(stdout,'(A43,F14.6,A16)') 'Simulation finalized. Total time elapsed = ', tend - tstart, 'elapsed seconds.'
            end if

            return

        end subroutine

        subroutine finalize

            implicit none

#ifdef MPI

            integer :: ierror

            call MPI_FINALIZE(ierror)
#endif

            return
        
        end subroutine finalize

end module model_core