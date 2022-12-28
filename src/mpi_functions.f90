module mpi_functions

    use mpi
    use kinds, only: r_kind, i_kind
    use gridinfo, only: grid2d

    implicit none

    public :: split_array, split_grid, array2row, row2array
    private

    contains

    subroutine split_grid(irank,grid,mgrid)

        implicit none 

        integer, intent(in)       :: irank
        type(grid2d), intent(in)  :: grid
        type(grid2d), intent(out) :: mgrid
        integer :: isize, i, j, ierror, k

        call MPI_COMM_SIZE(MPI_COMM_WORLD,isize,ierror)

        mgrid%nx = grid%nx
        mgrid%ny = grid%ny/isize
        mgrid%dx = grid%dx
        mgrid%dy = grid%dy

        allocate(mgrid%x(grid%nx))
        allocate(mgrid%y(grid%ny/isize))
        allocate(mgrid%uwnd(grid%nx,grid%ny/isize))
        allocate(mgrid%vwnd(grid%nx,grid%ny/isize))
        allocate(mgrid%height(grid%nx,grid%ny/isize))

        do i = 1, grid%nx 
            grid%x(i) = (i - 1)*mgrid%dx
        end do

        do i = 1, grid%ny/isize
            grid%y(i) = (i - 1)*mgrid%dy
        end do

        do i = 1, grid%nx
            k = 0
            do j = 1 + irank*grid%ny/isize, (irank+1)*grid%ny/isize
                k = k + 1
                mgrid%uwnd(i,k) = grid%uwnd(i,j)
                mgrid%vwnd(i,k) = grid%vwnd(i,j)
                mgrid%height(i,k) = grid%height(i,j)
            end do
        end do 

        return 

    end subroutine split_grid

    subroutine split_array(nx,ny,array,out_array)

        implicit none 

        integer, intent(in)                                         :: nx
        integer, intent(in)                                         :: ny
        real(kind=r_kind), dimension(nx,ny), intent(in)             :: array        
        real(kind=r_kind), dimension(:,:), allocatable, intent(out) :: out_array
        real(kind=r_kind), dimension(:),allocatable                 :: row        

        integer :: isize, ierror, inc, iy_size, fy_size, i, j, l, k, irank

        call MPI_COMM_RANK(MPI_COMM_WORLD,irank,ierror)
        call MPI_COMM_SIZE(MPI_COMM_WORLD,isize,ierror)

        inc = isize       

        allocate(out_array(nx,ny/inc))
        allocate(row(nx*ny/inc))        
        row = -999

        if ( irank == 0 ) then 

            do l = 0, isize - 1

                iy_size = 1 + ny*l/inc
                fy_size = ny*(l+1)/inc

                out_array = -999. 
                k = 0.
                do j = iy_size, fy_size
                    k = k + 1
                    do i = 1, nx                 
                        out_array(i,k) = array(i,j)                
                    end do            
                end do 
            
                call array2row(nx,ny/inc,out_array,row)

                if ( l /= 0 ) then                                         
                    call MPI_SEND(row, nx*ny/inc, MPI_DOUBLE, l, 1, MPI_COMM_WORLD, ierror)
                end if                

            end do

        else 
            call MPI_RECV(row, nx*ny/inc, MPI_DOUBLE, 0, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        end if
    
        call row2array(nx,ny/inc,row,out_array)

        return

    end subroutine split_array

    subroutine array2row(nx,ny,array,row)

        implicit none 

        integer(kind=i_kind), intent(in)                :: nx
        integer(kind=i_kind), intent(in)                :: ny
        real(kind=r_kind), dimension(nx,ny), intent(in) :: array 
        real(kind=r_kind), dimension(nx*ny), intent(out):: row
        
        integer :: i, j, k

        k = 0
        do i = 1, nx 
            do j = 1, ny 
                k = k + 1
                row(k) = array(i,j)
            end do
        end do

        return 

    end subroutine array2row

    subroutine row2array(nx,ny,row,array)

        implicit none 

        integer(kind=i_kind), intent(in)                :: nx
        integer(kind=i_kind), intent(in)                :: ny        
        real(kind=r_kind), dimension(nx*ny), intent(in) :: row
        real(kind=r_kind), dimension(nx,ny), intent(out):: array 
        
        integer :: i, j, k

        k = 0
        do i = 1, nx 
            do j = 1, ny 
                k = k + 1
                array(i,j) = row(k)
            end do
        end do

        return 

    end subroutine row2array

end module mpi_functions
