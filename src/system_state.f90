module system_state

    use gridinfo, only: grid2d
    use kinds, only: i_kind, r_kind
    use constants, only: PI

    implicit none

    contains

        subroutine initialize_grid(nx,ny,dx,dy,grid)

            implicit none 

            integer(kind=i_kind), intent(in) :: nx
            integer(kind=i_kind), intent(in) :: ny
            real(kind=r_kind), intent(in)    :: dx
            real(kind=r_kind), intent(in)    :: dy 
            type(grid2d), intent(inout)      :: grid

            integer                          :: i
    
            allocate(grid%x(nx))
            allocate(grid%y(ny))
            allocate(grid%uwnd(nx,ny))
            allocate(grid%vwnd(nx,ny))
            allocate(grid%height(nx,ny))
            grid%nx = nx 
            grid%ny = ny

            do i = 1, nx
                grid%x(i) = (i - 1)*dx 
            end do

            do i = 1, ny
                grid%y(i) = (i - 1)*dy 
            end do

            return

        end subroutine initialize_grid

        subroutine initial_conditions(grid)

            implicit none

            type(grid2d), intent(inout) :: grid
            integer                     :: i, j

            do i = 1, grid%nx
                do j = 1, grid%ny 
                    grid%uwnd(i,j) = 2.
                    grid%vwnd(i,j) = -1.
                    grid%height(i,j) = 2*cos( 3.141592 * sqrt((i-50.)*(i-50.) + (j-50.)*(j-50.)) / 50)
                end do 
            end do

            return

        end subroutine initial_conditions

end module system_state