module model_core_mpi

! This module contains the main subroutines responsible for initialize,
! integrate, and finalize the model.

    use mpi
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    use system_state, only: initialize_grid, initial_conditions
    use kinds, only: r_kind, i_kind 
    use gridinfo, only: grid2d
    use io_module, only: read_namelist, write_output, namelist_options
    use physics_mpi, only: update_state
    use mpi_functions, only: split_array, split_grid, array2row, row2array

    implicit none

    public :: initialize, run, finalize
    private

    type(grid2d) :: grid
    type(grid2d) :: mgrid
    type(namelist_options) :: opt
    integer :: irank, ierror, isize    
    real(kind=r_kind), allocatable, dimension(:,:) :: height, uwnd, vwnd
    real(kind=r_kind), allocatable, dimension(:) :: hrow, urow, vrow

    contains

        subroutine initialize

            implicit none

            integer :: i, j, k, l
            integer :: iy_size, fy_size, isize
            integer(kind=i_kind) :: nx
            integer(kind=i_kind) :: ny

            call MPI_INIT(ierror)
            call MPI_COMM_RANK(MPI_COMM_WORLD,irank,ierror)
            call MPI_COMM_SIZE(MPI_COMM_WORLD,isize,ierror)

            if ( irank == 0 ) then 

                call read_namelist(opt)

                nx = opt%nx
                ny = opt%ny
                
                call initialize_grid(nx,ny,opt%dx,opt%dy,grid)

                call initial_conditions(grid)

                allocate(uwnd(nx,ny/isize))
                allocate(vwnd(nx,ny/isize))
                allocate(height(nx,ny/isize))
                allocate(urow(nx*ny/isize))
                allocate(vrow(nx*ny/isize))
                allocate(hrow(nx*ny/isize))

                do i = 0, isize - 1 
                    call split_grid(i,grid,mgrid)

                    uwnd = mgrid%uwnd
                    vwnd = mgrid%vwnd
                    height = mgrid%height

                    ! do l = 1, nx 
                    !     print '(3F8.2)', (height(l,j),j=1,ny/isize)
                    ! end do
                    
                    if ( i /= 0 ) then
                        call array2row(nx,ny/isize,mgrid%uwnd,urow)
                        call MPI_SEND(urow,nx*ny/isize,MPI_DOUBLE,i,1,MPI_COMM_WORLD, ierror)
                        call array2row(nx,ny/isize,mgrid%vwnd,vrow)
                        call MPI_SEND(vrow,nx*ny/isize,MPI_DOUBLE,i,2,MPI_COMM_WORLD, ierror)
                        call array2row(nx,ny/isize,mgrid%height,hrow)                        
                        call MPI_SEND(hrow,nx*ny/isize,MPI_DOUBLE,i,3,MPI_COMM_WORLD, ierror)
                    end if

                end do

            end if 

            call MPI_BCAST(nx, 1, MPI_INT, 0, MPI_COMM_WORLD, ierror)
            call MPI_BCAST(ny, 1, MPI_INT, 0, MPI_COMM_WORLD, ierror)

            if ( irank /= 0 ) then
                allocate(uwnd(nx,ny/isize))
                allocate(vwnd(nx,ny/isize))
                allocate(height(nx,ny/isize))
                allocate(urow(nx*ny/isize))
                allocate(vrow(nx*ny/isize))
                allocate(hrow(nx*ny/isize))
            
                call MPI_RECV(urow,nx*ny/isize,MPI_DOUBLE,0,1,MPI_COMM_WORLD,MPI_STATUS_IGNORE, ierror)                
                call MPI_RECV(vrow,nx*ny/isize,MPI_DOUBLE,0,2,MPI_COMM_WORLD,MPI_STATUS_IGNORE, ierror)                
                call MPI_RECV(hrow,nx*ny/isize,MPI_DOUBLE,0,3,MPI_COMM_WORLD,MPI_STATUS_IGNORE, ierror)                
            end if

            call row2array(nx,ny/isize,hrow,uwnd)
            call row2array(nx,ny/isize,hrow,vwnd)
            call row2array(nx,ny/isize,hrow,height)

            call MPI_BARRIER(MPI_COMM_WORLD,ierror)            

            return

        end subroutine initialize

        subroutine run

            implicit none

            integer              :: i, iterations
            integer(kind=i_kind) :: stdout
            real(kind=r_kind)    :: elapsed
            real(kind=r_kind)    :: start, finish

            iterations = int(opt%run_time / opt%dt)
            iterations = 0

            do i = 1, iterations + 1                

                ! if ( irank .EQ. 0 ) then
                
                !     ! if ( mod((i-1)*opt%dt*1000,opt%history_interval*1000) == 0 ) then
                !     !     call CPU_TIME(start)
                !     !     write(stdout,'("Writing output file... Time step = ",F6.2)') (i-1)*opt%dt
                !     !     call write_output(grid,opt,i)
                !     !     call CPU_TIME(finish)
                !     !     elapsed = finish - start
                !     !     write(stdout,'("Done. Time elapsed = ",F8.4)') elapsed
                !     ! end if
                
                ! end if
               
                call update_state(grid,opt%dt,opt%mheight,opt%fc,opt%bc, irank)
                call MPI_BARRIER(MPI_COMM_WORLD,ierror)
    
            end do

            return

        end subroutine

        subroutine finalize

            implicit none

            call MPI_FINALIZE(ierror)

            return
        
        end subroutine finalize

end module model_core_mpi