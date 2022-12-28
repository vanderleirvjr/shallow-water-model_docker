module physics_mpi

! This module contains subroutines to update the system state.
! It is used the finite differences method FTCS (Forward Time Centered Space)
! to estimate the future state of the variables uwnd, vwnd, and height.

! The boundary conditions is updated each time step using the subroutine update_bc.
! The boundaries of each variable are duplicated into a new variable which
! in turn is used to calculate the next time step. The variable is then copied back
! into the grid variable

    use kinds, only: i_kind, r_kind
    use system_state, only: grid2d
    use constants, only: GRAV
     
    implicit none

    public :: update_state
    private

    contains

    subroutine update_state(grid,dt,mheight,fc,bc,irank)

        implicit none

        type(grid2d), intent(inout)                    :: grid
        real(kind=r_kind), intent(in)                  :: dt
        real(kind=r_kind), intent(in)                  :: mheight
        real(kind=r_kind), intent(in)                  :: bc
        real(kind=r_kind), intent(in)                  :: fc
        integer(kind=i_kind), intent(in)               :: irank
        real(kind=r_kind), dimension(:,:), allocatable :: uwnd, vwnd, height
        real(kind=r_kind), dimension(:,:), allocatable :: tuwnd, tvwnd, theight
        integer                                        :: i, j
        real(kind=r_kind)                              :: term1, term2, term3, term4


        ! call split_array(grid%nx,grid%ny,grid%height,height)
        ! call split_array(grid%nx,grid%ny,grid%uwnd,uwnd)
        ! call split_array(grid%nx,grid%ny,grid%vwnd,vwnd)

        ! 1 + irank*ny/isize 
        ! (irank + 1)*ny/isize 

        allocate(uwnd(grid%nx + 2 ,grid%ny + 2 ))
        allocate(vwnd(grid%nx + 2 ,grid%ny + 2 ))
        allocate(height(grid%nx + 2 ,grid%ny + 2 ))
        allocate(tuwnd(grid%nx + 2 ,grid%ny + 2 ))
        allocate(tvwnd(grid%nx + 2 ,grid%ny + 2 ))
        allocate(theight(grid%nx + 2 ,grid%ny + 2 ))


        print*, irank 
        return

        call update_bc(uwnd,vwnd,height,grid)

        theight = height
        tuwnd = uwnd
        tvwnd = vwnd

        do i = 2, grid%nx + 1
            do j = 2, grid%ny + 1
                term1 = uwnd(i,j) * ( uwnd(i+1,j) - 2*uwnd(i,j) + uwnd(i-1,j) ) / grid%dx * grid%dx
                term2 = vwnd(i,j) * ( uwnd(i,j+1) - 2*uwnd(i,j) + uwnd(i,j-1) ) / grid%dy * grid%dy
                term3 = GRAV * ( height(i+1,j) - 2*height(i,j) + height(i-1,j) ) / grid%dx * grid%dx
                tuwnd(i,j) = uwnd(i,j) - dt * ( term1 + term2 + term3 - fc*vwnd(i,j)) 
            end do
        end do

        do i = 2, grid%nx + 1
            do j = 2, grid%ny + 1
                term1 = uwnd(i,j) * ( vwnd(i+1,j) - 2*vwnd(i,j) + vwnd(i-1,j) ) / grid%dx * grid%dx
                term2 = vwnd(i,j) * ( vwnd(i,j+1) - 2*vwnd(i,j) + vwnd(i,j-1) ) / grid%dy * grid%dy
                term3 = GRAV * ( height(i,j+1) - 2*height(i,j) + height(i,j-1) ) / grid%dy * grid%dy
                tvwnd(i,j) = vwnd(i,j) - dt * ( term1 + term2 + term3 + fc*uwnd(i,j) ) 
            end do
        end do

        uwnd = tuwnd
        vwnd = tvwnd

        do i = 2, grid%nx + 1
            do j = 2, grid%ny + 1
                term1 = uwnd(i,j) * ( height(i+1,j) - 2*height(i,j) + height(i-1,j) ) / grid%dx * grid%dx 
                term2 = (mheight + height(i,j) ) * ( uwnd(i+1,j) - 2*uwnd(i,j) + uwnd(i-1,j) ) / grid%dx * grid%dx 
                term3 = vwnd(i,j) * ( height(i,j+1) - 2*height(i,j) + height(i,j-1) ) / grid%dy * grid%dy
                term4 = (mheight + height(i,j) ) * ( vwnd(i,j+1) -2*vwnd(i,j) + vwnd(i,j-1) ) / grid%dy * grid%dy
                theight(i,j) = height(i,j) - dt * ( term1 + term2 + term3 + term4 )
            end do
        end do

        height = theight

        do i = 1, grid%nx
            do j = 1, grid%ny
                grid%height(i,j) = height(i+1,j+1)
                grid%uwnd(i,j) = uwnd(i+1,j+1)
                grid%vwnd(i,j) = vwnd(i+1,j+1) 
            end do
        end do
        
        return

    end subroutine update_state

    subroutine update_bc(uwnd,vwnd,height,grid)

        implicit none

        type(grid2d), intent(in) :: grid
        real(kind=r_kind), dimension(:,:), intent(inout) :: uwnd
        real(kind=r_kind), dimension(:,:), intent(inout) :: vwnd
        real(kind=r_kind), dimension(:,:), intent(inout) :: height
        integer :: i, j

        height = -999.
        height(2:grid%nx+1,1)         = grid%height(:,grid%ny)
        height(2:grid%nx+1,grid%ny+2) = grid%height(:,1)
        height(1,2:grid%ny+1)         = grid%height(grid%nx,:)
        height(grid%nx+2,2:grid%ny+1) = grid%height(1,:)
        height(1,1)                   = grid%height(grid%nx,grid%ny)
        height(grid%nx+2,grid%ny+2)   = grid%height(1,1)
        height(1,grid%ny+2)           = grid%height(grid%nx,1)
        height(grid%nx+2,1)           = grid%height(1,grid%ny)

        do i = 1, grid%nx
            do j = 1, grid%ny
                height(i+1,j+1) = grid%height(i,j)
            end do 
        end do

        uwnd = -999.
        uwnd(2:grid%nx+1,1)         = grid%uwnd(:,grid%ny)
        uwnd(2:grid%nx+1,grid%ny+2) = grid%uwnd(:,1)
        uwnd(1,2:grid%ny+1)         = grid%uwnd(grid%nx,:)
        uwnd(grid%nx+2,2:grid%ny+1) = grid%uwnd(1,:)
        uwnd(1,1)                   = grid%uwnd(grid%nx,grid%ny)
        uwnd(grid%nx+2,grid%ny+2)   = grid%uwnd(1,1)
        uwnd(1,grid%ny+2)           = grid%uwnd(grid%nx,1)
        uwnd(grid%nx+2,1)           = grid%uwnd(1,grid%ny)

        do i = 1, grid%nx
            do j = 1, grid%ny
                uwnd(i+1,j+1) = grid%uwnd(i,j)
            end do 
        end do

        vwnd = -999.
        vwnd(2:grid%nx+1,1)         = grid%vwnd(:,grid%ny)
        vwnd(2:grid%nx+1,grid%ny+2) = grid%vwnd(:,1)
        vwnd(1,2:grid%ny+1)         = grid%vwnd(grid%nx,:)
        vwnd(grid%nx+2,2:grid%ny+1) = grid%vwnd(1,:)
        vwnd(1,1)                   = grid%vwnd(grid%nx,grid%ny)
        vwnd(grid%nx+2,grid%ny+2)   = grid%vwnd(1,1)
        vwnd(1,grid%ny+2)           = grid%vwnd(grid%nx,1)
        vwnd(grid%nx+2,1)           = grid%vwnd(1,grid%ny)

        do i = 1, grid%nx
            do j = 1, grid%ny
                vwnd(i+1,j+1) = grid%vwnd(i,j)
            end do 
        end do

        return

    end subroutine update_bc

end module physics_mpi