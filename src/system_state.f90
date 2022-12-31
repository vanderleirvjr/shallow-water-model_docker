module system_state

! This module initializes the grid variables and initial conditions
! associated with it.

    use gridinfo, only: grid2d
    use kinds, only: i_kind, r_kind
    use constants, only: PI

    implicit none

    public :: initialize_grid, initial_conditions, grid2d
    private

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

            grid%dx = dx 
            grid%dy = dy

            return

        end subroutine initialize_grid

        subroutine initial_conditions(grid)

            implicit none

            type(grid2d), intent(inout)        :: grid
            real(kind=r_kind)                  :: xcenter, ycenter, dist, last
            integer                            :: i, j, points

            xcenter = MAXVAL(grid%x(:))/2
            ycenter = MAXVAL(grid%y(:))/2
            points = SIZE(grid%x(:))

            do i = 1, grid%nx
                do j = 1, grid%ny 
                    grid%uwnd(i,j) = 0.3
                    grid%vwnd(i,j) = 0.
                    dist = sqrt( (grid%x(i)-xcenter)**2 + (grid%y(j)-ycenter)**2 )
                    grid%height(i,j) = 1000 + (1.4*exp(-dist/3000))*cos( (2*PI/9000) * dist )    
                end do 
            end do

            return

        end subroutine initial_conditions

end module system_state