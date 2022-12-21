module physics

    use kinds, only: i_kind, r_kind
    use system_state, only: grid2d
     
    implicit none

    contains

    subroutine update_wnd(grid)

        implicit none

        type(grid2d), intent(inout) :: grid
        type(grid2d)                :: new_grid
        integer                     :: i, j

        new_grid = grid

        do i = 1, grid%nx
            do j = 1, grid%ny
                new_grid%uwnd(i,j) = grid%uwnd(i,j) + 0.1
            end do
        end do

        do i = 1, grid%nx
            do j = 1, grid%ny
                new_grid%vwnd(i,j) = grid%vwnd(i,j) - 0.1
            end do
        end do

        grid = new_grid

        return

    end subroutine update_wnd

    subroutine update_height(grid)

        implicit none

        type(grid2d), intent(inout) :: grid
        type(grid2d)                :: new_grid
        integer                     :: i, j

        new_grid = grid

        do i = 1, grid%nx
            do j = 1, grid%ny
                new_grid%height(i,j) = grid%height(i,j) + 0.1
            end do
        end do

        grid = new_grid

        return 

    end subroutine update_height


end module physics 