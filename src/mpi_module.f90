module mpi_module

    use mpi
    use gridinfo, only: grid2d

    implicit none 

    public :: grid_mpi_type, partial_mpi_type, local_mpi_type, inner_mpi_type
    private
    
    contains

        function grid_mpi_type(grid) result(sgrid)

            implicit none

            type(grid2d), intent(in)                         :: grid
            integer                                          :: sgrid
            integer                                          :: count, ierror
            integer, dimension(9)                            :: blocklengths, datatypes
            integer(KIND=MPI_ADDRESS_KIND)                   :: base ! memory address base
            integer(KIND=MPI_ADDRESS_KIND), dimension(9)     :: disp ! memory address displacement     
                
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

            return 
        
        end function grid_mpi_type
        
        function partial_mpi_type(grid) result(sblock)

            implicit none 

            type(grid2d), intent(in)  :: grid
            integer                   :: sblock
            integer                   :: ierror, isize

            call MPI_COMM_SIZE(MPI_COMM_WORLD,isize,ierror)

            sblock = general_mpi_type(2 + grid%nx,2 + grid%ny/isize,grid%nx + 2)

            return

        end function partial_mpi_type

        function local_mpi_type(grid) result(rblock)

            implicit none 

            type(grid2d), intent(in)  :: grid
            integer                   :: rblock
            integer                   :: ierror, isize

            call MPI_COMM_SIZE(MPI_COMM_WORLD,isize,ierror)

            rblock = general_mpi_type(grid%nx + 2,2 + grid%ny/isize,grid%nx + 2)

            return

        end function local_mpi_type

        function inner_mpi_type(grid) result(pblock)

            implicit none 

            type(grid2d), intent(in)  :: grid
            integer                   :: pblock
            integer                   :: ierror, isize

            call MPI_COMM_SIZE(MPI_COMM_WORLD,isize,ierror)

            pblock = general_mpi_type(grid%nx,grid%ny/isize,grid%nx + 2)

            return

        end function inner_mpi_type

        function general_mpi_type(blocklen,count,stride) result(gblock)

            implicit none 

            integer, intent(in)       :: count, blocklen, stride
            integer                   :: gblock
            integer                   :: ierror, isize
            
            call MPI_TYPE_VECTOR(count,blocklen,stride,MPI_DOUBLE,gblock,ierror)
            call MPI_TYPE_COMMIT( gblock, ierror)

            return

        end function general_mpi_type


end module mpi_module