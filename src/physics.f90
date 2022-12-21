module physics

    use kinds, only: i_kind, r_kind
    use system_state, only: grid2d
    use constants, only: GRAV, mheight
     
    implicit none

    public :: update_state
    private

    contains

    subroutine update_state(grid,dt)

        implicit none

        type(grid2d), intent(inout)                    :: grid
        real(kind=r_kind), intent(in)                  :: dt
        real(kind=r_kind), dimension(:,:), allocatable :: uwnd, vwnd, height
        integer                                        :: i, j
        real(kind=r_kind)                              :: term1, term2, term3, term4

        allocate(uwnd(grid%nx,grid%ny))
        allocate(vwnd(grid%nx,grid%ny))
        allocate(height(grid%nx,grid%ny))

        do i = 2, grid%nx - 1 
            do j = 2, grid%ny - 1
                term1 = grid%uwnd(i,j) * ( grid%uwnd(i + 1,j) - 2*grid%uwnd(i,j) + grid%uwnd(i - 1,j) ) / grid%dx
                term2 = grid%vwnd(i,j) * ( grid%uwnd(i,j+1) - 2*grid%uwnd(i,j) + grid%uwnd(i,j-1) ) / grid%dy
                term3 = GRAV * ( grid%height(i+1,j) - 2*grid%height(i,j) + grid%height(i-1,j) ) / grid%dx 
                uwnd(i,j) = grid%uwnd(i,j) - ( dt / 2 ) * ( term1 + term2 + term3 ) 
            end do
        end do

        do i = 2, grid%nx - 1
            do j = 2, grid%ny - 1 
                term1 = grid%uwnd(i,j) * ( grid%vwnd(i+1,j) - 2*grid%vwnd(i,j) + grid%vwnd(i-1,j) ) / grid%dx
                term2 = grid%vwnd(i,j) * ( grid%vwnd(i,j+1) - 2*grid%vwnd(i,j) + grid%vwnd(i,j-1) ) / grid%dy 
                term3 = GRAV * ( grid%height(i,j+1) - 2*grid%height(i,j) + grid%height(i,j-1) ) / grid%dy 
                vwnd(i,j) = grid%vwnd(i,j) - ( dt / 2 ) * ( term1 + term2 + term3 ) 
            end do
        end do

        do i = 2, grid%nx - 1
            do j = 2, grid%ny - 1
                term1 = grid%uwnd(i,j) * ( grid%height(i+1,j) - 2*grid%height(i,j) + grid%height(i-1,j) ) / grid%dx 
                term2 = (mheight + grid%height(i,j) ) * ( grid%uwnd(i+1,j) - 2*grid%uwnd(i,j) + grid%uwnd(i-1,j) ) / grid%dx
                term3 = grid%vwnd(i,j) * ( grid%height(i,j+1) - 2*grid%height(i,j) + grid%height(i,j-1) ) / grid%dy
                term4 = (mheight + grid%height(i,j) ) * ( grid%uwnd(i,j+1) -2*grid%uwnd(i,j) + grid%uwnd(i,j-1) ) / grid%dy
                height(i,j) = grid%height(i,j) - ( dt / 2 ) * ( term1 + term2 + term3 + term4 )
            end do
        end do

        grid%uwnd = uwnd
        grid%vwnd = vwnd
        grid%height = height

        return

    end subroutine update_state

end module physics 