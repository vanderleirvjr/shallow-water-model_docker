module system_state

    use gridinfo, only: grid2d
    use kinds, only: i_kind, r_kind

    implicit none

    contains

        subroutine initialize_grid(nx,ny,grid)

            implicit none 

            integer(kind=i_kind), intent(in) :: nx
            integer(kind=i_kind), intent(in) :: ny
            type(grid2d), intent(inout) :: grid
    
            allocate(grid%uwnd(nx,ny))
            allocate(grid%vwnd(nx,ny))
            allocate(grid%height(nx,ny))
            grid%nx = nx 
            grid%ny = ny

            return

        end subroutine initialize_grid

        subroutine initial_conditions(grid)

            implicit none

            type(grid2d), intent(inout) :: grid

            return

        end subroutine initial_conditions

end module system_state