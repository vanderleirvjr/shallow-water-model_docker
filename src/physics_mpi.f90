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
    use mpi
     
    implicit none

    public :: update_state
    private

    contains

    subroutine update_state(grid,dt,mheight,fc,bc)

        implicit none

        type(grid2d), intent(inout)                            :: grid
        real(kind=r_kind), intent(in)                          :: dt
        real(kind=r_kind), intent(in)                          :: mheight
        real(kind=r_kind), intent(in)                          :: bc
        real(kind=r_kind), intent(in)                          :: fc
        real(kind=r_kind), dimension(:,:), pointer, contiguous :: guwnd, gvwnd, gheight ! global array
        real(kind=r_kind), dimension(:,:), pointer, contiguous :: luwnd, lvwnd, lheight ! local arrays
        real(kind=r_kind), dimension(:,:), allocatable         :: tuwnd, tvwnd, theight ! temporary arrays
        real(kind=r_kind)                                      :: term1, term2, term3, term4
        
        integer                                                :: i, j, l, iy !index variables
        
        integer                                                :: irank, ierror, isize ! mpi variables
        
        integer                                                :: sblock, rblock, pblock, sgrid ! mpi derived datatypes
        integer                                                :: blocklen, count, stride
        integer, dimension(9)                                  :: blocklengths, datatypes
        integer(KIND=MPI_ADDRESS_KIND)                         :: base ! memory address base
        integer(KIND=MPI_ADDRESS_KIND), dimension(9)           :: disp ! memory address displacement        

        call MPI_COMM_SIZE(MPI_COMM_WORLD,isize,ierror)
        call MPI_COMM_RANK(MPI_COMM_WORLD,irank,ierror)
       
        allocate(luwnd(grid%nx + 2 ,grid%ny/isize + 2 ))
        allocate(lvwnd(grid%nx + 2 ,grid%ny/isize + 2 ))
        allocate(lheight(grid%nx + 2 ,grid%ny/isize + 2 ))

        lheight = -999.9
        if ( irank == 0 ) then
            
            blocklen = 2 + grid%nx
            count = 2 + grid%ny/isize
            stride = grid%nx + 2        
            call MPI_TYPE_VECTOR(count,blocklen,stride,MPI_DOUBLE,sblock,ierror)
            call MPI_TYPE_COMMIT( sblock, ierror)

            allocate(guwnd(grid%nx + 2 ,grid%ny + 2 ))
            allocate(gvwnd(grid%nx + 2 ,grid%ny + 2 ))
            allocate(gheight(grid%nx + 2 ,grid%ny + 2 ))            
            
            call update_bc(guwnd,gvwnd,gheight,grid)

            do l = 0 , isize - 1
                iy = 1 + l*grid%ny/isize
                if ( l /= 0 ) then
                    
                    call MPI_SEND(guwnd(1,iy),1,sblock,l,1,MPI_COMM_WORLD,ierror)
                    call MPI_SEND(gvwnd(1,iy),1,sblock,l,1,MPI_COMM_WORLD,ierror)
                    call MPI_SEND(gheight(1,iy),1,sblock,l,1,MPI_COMM_WORLD,ierror)
                    
                else 
                    do i = 1, grid%nx + 2
                        do j = 1, 2 + grid%ny/isize 
                            luwnd(i,j) = guwnd(i,j)   
                            lvwnd(i,j) = gvwnd(i,j)   
                            lheight(i,j) = gheight(i,j)                            
                        end do
                    end do
                end if
            end do

        else
            
            blocklen = grid%nx + 2
            count = 2 + grid%ny/isize
            stride = grid%nx + 2
            
            call MPI_TYPE_VECTOR(count,blocklen,stride,MPI_DOUBLE,rblock,ierror)
            call MPI_TYPE_COMMIT( rblock, ierror)

            call MPI_RECV(luwnd(1,1),1,rblock,0,1,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierror)
            call MPI_RECV(lvwnd(1,1),1,rblock,0,1,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierror)
            call MPI_RECV(lheight(1,1),1,rblock,0,1,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierror)
            
        end if

        allocate(tuwnd(grid%nx + 2 ,grid%ny/isize + 2 ))
        allocate(tvwnd(grid%nx + 2 ,grid%ny/isize + 2 ))
        allocate(theight(grid%nx + 2 ,grid%ny/isize + 2 ))

        theight = lheight
        tuwnd = luwnd
        tvwnd = lvwnd

        do i = 2, grid%nx + 1
            do j = 2, 1 + grid%ny/isize
                term1 = luwnd(i,j) * ( luwnd(i+1,j) - 2*luwnd(i,j) + luwnd(i-1,j) ) / grid%dx * grid%dx
                term2 = lvwnd(i,j) * ( luwnd(i,j+1) - 2*luwnd(i,j) + luwnd(i,j-1) ) / grid%dy * grid%dy
                term3 = GRAV * ( lheight(i+1,j) - 2*lheight(i,j) + lheight(i-1,j) ) / grid%dx * grid%dx
                tuwnd(i,j) = luwnd(i,j) - dt * ( term1 + term2 + term3 - fc*lvwnd(i,j)) 
            end do
        end do

        do i = 2, grid%nx + 1
            do j = 2, 1 + grid%ny/isize
                term1 = luwnd(i,j) * ( lvwnd(i+1,j) - 2*lvwnd(i,j) + lvwnd(i-1,j) ) / grid%dx * grid%dx
                term2 = lvwnd(i,j) * ( lvwnd(i,j+1) - 2*lvwnd(i,j) + lvwnd(i,j-1) ) / grid%dy * grid%dy
                term3 = GRAV * ( lheight(i,j+1) - 2*lheight(i,j) + lheight(i,j-1) ) / grid%dy * grid%dy
                tvwnd(i,j) = lvwnd(i,j) - dt * ( term1 + term2 + term3 + fc*luwnd(i,j) ) 
            end do
        end do

        luwnd = tuwnd
        lvwnd = tvwnd

        deallocate(tuwnd)
        deallocate(tvwnd)        

        do i = 2, grid%nx + 1
            do j = 2, 1 + grid%ny/isize
                term1 = luwnd(i,j) * ( lheight(i+1,j) - 2*lheight(i,j) + lheight(i-1,j) ) / grid%dx * grid%dx 
                term2 = (mheight + lheight(i,j) ) * ( luwnd(i+1,j) - 2*luwnd(i,j) + luwnd(i-1,j) ) / grid%dx * grid%dx 
                term3 = lvwnd(i,j) * ( lheight(i,j+1) - 2*lheight(i,j) + lheight(i,j-1) ) / grid%dy * grid%dy
                term4 = (mheight + lheight(i,j) ) * ( lvwnd(i,j+1) -2*lvwnd(i,j) + lvwnd(i,j-1) ) / grid%dy * grid%dy
                theight(i,j) = lheight(i,j) - dt * ( term1 + term2 + term3 + term4 )                
            end do
        end do

        lheight = theight

        deallocate(theight)

        blocklen = grid%nx
        count = grid%ny/isize
        stride = grid%nx + 2
        call MPI_TYPE_VECTOR(count,blocklen,stride,MPI_DOUBLE,pblock,ierror)
        call MPI_TYPE_COMMIT( pblock, ierror)

        if ( irank /= 0 ) then

            call MPI_SEND(luwnd(2,2),1,pblock,0,1,MPI_COMM_WORLD,ierror)
            call MPI_SEND(lvwnd(2,2),1,pblock,0,2,MPI_COMM_WORLD,ierror)
            call MPI_SEND(lheight(2,2),1,pblock,0,3,MPI_COMM_WORLD,ierror)
            
        else
            do l = 0, isize - 1
                iy = 1 + l*grid%ny/isize
                if ( l == 0 ) then 
                    do i = 2, grid%nx + 1
                        do j = 2, 1 + grid%ny/isize
                            guwnd(i,j) = luwnd(i,j)
                            gvwnd(i,j) = lvwnd(i,j)
                            gheight(i,j) = lheight(i,j)                            
                        end do
                    end do
                else

                    blocklen = grid%nx
                    count = grid%ny/isize
                    stride = grid%nx + 2
                    call MPI_TYPE_VECTOR(count,blocklen,stride,MPI_DOUBLE,pblock,ierror)
                    call MPI_TYPE_COMMIT( pblock, ierror)
                    
                    call MPI_RECV(guwnd(2,iy + 1),1,pblock,l,1,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierror)
                    call MPI_RECV(gvwnd(2,iy + 1),1,pblock,l,2,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierror)
                    call MPI_RECV(gheight(2,iy + 1),1,pblock,l,3,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierror)
                end if
            end do

            do i = 1, grid%nx
                do j = 1, grid%ny
                    grid%height(i,j) = gheight(i+1,j+1)
                    grid%uwnd(i,j) = guwnd(i+1,j+1)
                    grid%vwnd(i,j) = gvwnd(i+1,j+1) 
                end do
            end do
            
            deallocate(guwnd)
            deallocate(gvwnd)
            deallocate(gheight)

        end if

        deallocate(luwnd)
        deallocate(lvwnd)
        deallocate(lheight)

        count = 9
            
        blocklengths(1:2) = 1            
        blocklengths(3) = grid%nx
        blocklengths(4) = grid%ny
        blocklengths(5:6) = 1            
        blocklengths(7:9) = grid%nx*grid%ny

        call MPI_GET_ADDRESS(grid, base, ierror)
        call MPI_GET_ADDRESS(grid%nx, disp(1), ierror)
        call MPI_GET_ADDRESS(grid%ny, disp(2), ierror)
        call MPI_GET_ADDRESS(grid%x, disp(3), ierror)
        call MPI_GET_ADDRESS(grid%y, disp(4), ierror) 
        call MPI_GET_ADDRESS(grid%dx, disp(5), ierror)
        call MPI_GET_ADDRESS(grid%dy, disp(6), ierror) 
        call MPI_GET_ADDRESS(grid%uwnd(1,1), disp(7), ierror)
        call MPI_GET_ADDRESS(grid%vwnd(1,1), disp(8), ierror)
        call MPI_GET_ADDRESS(grid%height(1,1), disp(9), ierror)

        disp(1) = disp(1) - base 
        disp(2) = disp(2) - base 
        disp(3) = disp(3) - base 
        disp(4) = disp(4) - base 
        disp(5) = disp(5) - base 
        disp(6) = disp(6) - base 
        disp(7) = disp(7) - base 
        disp(8) = disp(8) - base 
        disp(9) = disp(9) - base 
        
        datatypes(1:2) = MPI_INT            
        datatypes(3:9) = MPI_DOUBLE
        
        call MPI_TYPE_CREATE_STRUCT(count,blocklengths,disp,datatypes,sgrid,ierror)

        call MPI_TYPE_COMMIT(sgrid,ierror)

        call MPI_BCAST(grid, 1, sgrid, 0, MPI_COMM_WORLD, ierror)
 
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