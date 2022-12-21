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
        integer                                        :: i, j, yr, yl, xr, xl
        real(kind=r_kind)                              :: term1, term2, term3, term4

        allocate(uwnd(grid%nx,grid%ny))
        allocate(vwnd(grid%nx,grid%ny))
        allocate(height(grid%nx,grid%ny))

        do i = 1, grid%nx 
            do j = 1, grid%ny
                xl = i - 1
                xr = i + 1
                yl = j - 1
                yr = j + 1
                if ( xl == 0 ) then
                    xl = grid%nx
                end if
                if ( xr > grid%nx ) then
                    xr = 1
                end if
                if ( yl == 0 ) then
                    yl = grid%ny
                end if
                if ( yr > grid%ny ) then
                    yr = 1
                end if
                term1 = grid%uwnd(i,j) * ( grid%uwnd(xr,j) - 2*grid%uwnd(i,j) + grid%uwnd(xl,j) ) / grid%dx
                term2 = grid%vwnd(i,j) * ( grid%uwnd(i,yr) - 2*grid%uwnd(i,j) + grid%uwnd(i,yl) ) / grid%dy
                term3 = GRAV * ( grid%height(xr,j) - 2*grid%height(i,j) + grid%height(xl,j) ) / grid%dx 
                uwnd(i,j) = grid%uwnd(i,j) - ( dt / 2 ) * ( term1 + term2 + term3 ) 
            end do
        end do

        do i = 1, grid%nx
            do j = 1, grid%ny
                xl = i - 1
                xr = i + 1
                yl = j - 1
                yr = j + 1
                if ( xl == 0 ) then
                    xl = grid%nx
                end if
                if ( xr > grid%nx ) then
                    xr = 1
                end if
                if ( yl == 0 ) then
                    yl = grid%ny
                end if
                if ( yr > grid%ny ) then
                    yr = 1
                end if
                term1 = grid%uwnd(i,j) * ( grid%vwnd(xr,j) - 2*grid%vwnd(i,j) + grid%vwnd(xl,j) ) / grid%dx
                term2 = grid%vwnd(i,j) * ( grid%vwnd(i,yr) - 2*grid%vwnd(i,j) + grid%vwnd(i,yl) ) / grid%dy 
                term3 = GRAV * ( grid%height(i,yr) - 2*grid%height(i,j) + grid%height(i,yl) ) / grid%dy 
                vwnd(i,j) = grid%vwnd(i,j) - ( dt / 2 ) * ( term1 + term2 + term3 ) 
            end do
        end do

        do i = 1, grid%nx
            do j = 1, grid%ny
                xl = i - 1
                xr = i + 1
                yl = j - 1
                yr = j + 1
                if ( xl == 0 ) then
                    xl = grid%nx
                end if
                if ( xr > grid%nx ) then
                    xr = 1
                end if
                if ( yl == 0 ) then
                    yl = grid%ny
                end if
                if ( yr > grid%ny ) then
                    yr = 1
                end if
                term1 = grid%uwnd(i,j) * ( grid%height(xr,j) - 2*grid%height(i,j) + grid%height(xl,j) ) / grid%dx 
                term2 = (mheight + grid%height(i,j) ) * ( grid%uwnd(xr,j) - 2*grid%uwnd(i,j) + grid%uwnd(xl,j) ) / grid%dx
                term3 = grid%vwnd(i,j) * ( grid%height(i,yr) - 2*grid%height(i,j) + grid%height(i,yl) ) / grid%dy
                term4 = (mheight + grid%height(i,j) ) * ( grid%uwnd(i,yr) -2*grid%uwnd(i,j) + grid%uwnd(i,yl) ) / grid%dy
                height(i,j) = grid%height(i,j) - ( dt / 2 ) * ( term1 + term2 + term3 + term4 )
            end do
        end do

        grid%uwnd = uwnd
        grid%vwnd = vwnd
        grid%height = height

        return

    end subroutine update_state

end module physics 