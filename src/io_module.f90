module io_module

    use, intrinsic :: iso_fortran_env, only: stderr => error_unit, stdout => output_unit
    use kinds, only: i_kind, r_kind
    use system_state, only: grid2d
    use netcdf

    implicit none

    public  :: read_namelist, write_output, namelist_options
    private

    type namelist_options
        
        integer(kind=i_kind) :: nx
        integer(kind=i_kind) :: ny
        real(kind=r_kind)    :: dx
        real(kind=r_kind)    :: dy

        real(kind=r_kind) :: dt
        real(kind=r_kind) :: run_time

        real(kind=r_kind) :: mheight = 2.
        real(kind=r_kind) :: bc = 0.
        real(kind=r_kind) :: fc = 0.

        real(kind=r_kind) :: history_interval
        character(len=100) :: output_file_name_prefix

    end type namelist_options
    
    contains
      
        subroutine read_namelist(opt)

            implicit none

            type(namelist_options), intent(out) :: opt
            integer(kind=i_kind) :: nx
            integer(kind=i_kind) :: ny
            real(kind=r_kind)    :: dx
            real(kind=r_kind)    :: dy
            real(kind=r_kind)    :: dt
            real(kind=r_kind)    :: run_time
            real(kind=r_kind)    :: history_interval
            integer(kind=i_kind) :: rc, fu
            character(len=100)   :: file_path = "namelist.input", output_file_name_prefix
            real(kind=r_kind) :: mheight
            real(kind=r_kind) :: bc
            real(kind=r_kind) :: fc

            namelist /gridinfo/ nx, ny, dx, dy
            namelist /integration/ dt, run_time
            namelist /parameters/ mheight, bc, fc
            namelist /io/ history_interval, output_file_name_prefix

            inquire (file=file_path, iostat=rc)

            if (rc /= 0) then
                write (stderr, '("Error: input file ", a, " does not exist")') file_path
                return
            end if

            open (newunit=fu, file=file_path, action='read', iostat=rc, status='old')
            read (nml=gridinfo, iostat=rc, unit=fu)
            if (rc /= 0) write (stderr, '("Error: Invalid namelist format")')
            
            read (nml=integration, iostat=rc, unit=fu)
            if (rc /= 0) write (stderr, '("Error: Invalid namelist format")')

            read (nml=parameters, iostat=rc, unit=fu)
            if (rc /= 0) write (stderr, '("Error: Invalid namelist format")')

            read (nml=io, iostat=rc, unit=fu)
            if (rc /= 0) write (stderr, '("Error: Invalid namelist format")')

            opt%nx = nx
            opt%ny = ny

            opt%dx = dx
            opt%dy = dy

            opt%run_time = run_time 
            opt%dt = dt

            opt%mheight = mheight
            opt%bc = bc
            opt%fc = fc

            opt%history_interval = history_interval
            opt%output_file_name_prefix = output_file_name_prefix

            return

        end subroutine read_namelist

        subroutine write_output(grid,opt,iteration)

            implicit none
   
            type(grid2d), intent(in)           :: grid
            type(namelist_options), intent(in) :: opt
            integer, intent(in)                :: iteration
            integer                            :: mili, seconds, minutes, hours
            integer                            :: ncid, uvarid, vvarid, hvarid, xvarid, yvarid, xdimid, ydimid
            character(len=200)                 :: file_name, xsec, xmin, xhour, xmili
            integer, dimension(:), allocatable :: dimids              

            mili = (iteration - 1)*opt%dt*1000
            seconds = 0
            minutes = 0
            hours = 0
            
            if ( mili > 999 ) then
                seconds = (mili - MOD(mili,1000))/1000
                mili = MOD(mili,1000)
                if ( seconds > 59 ) then
                    minutes = (seconds - MOD(seconds,60))/60
                    seconds = MOD(seconds,60)
                    if ( minutes > 59 ) then
                        hours = (minutes - MOD(minutes,60))/60
                        minutes = MOD(minutes,60)
                    end if
                end if
            end if

            if ( seconds < 10 ) then 
                write(xsec,'(A1,I1)') "0", seconds
            else 
                write(xsec,'(I2)') seconds
            end if

            if ( minutes < 10 ) then 
                write(xmin,'(A1,I1)') "0", minutes
            else 
                write(xmin,'(I2)') minutes
            end if

            if ( hours < 10 ) then 
                write(xhour,'(A2,I1)') "00", hours
            else if ( hours < 100 ) then 
                write(xhour,'(A1,I2)') "0", hours
            else 
                write(xhour,'(I3)') hours
            end if
    
            if ( mili < 10 ) then 
                write(xmili,'(A2,I1)') "00", mili
            else if ( mili < 100 ) then 
                write(xmili,'(A1,I2)') "0", mili
            else 
                write(xmili,'(I3)') mili
            end if

            write(file_name,"(A,'_',A3,':',A2,':',A2,'.',A3,A4)") trim(opt%output_file_name_prefix), xhour, xmin, &
            xsec, xmili, ".nc4"

            ! Create the netCDF file. The nf90_clobber parameter tells netCDF to
            ! overwrite this file, if it already exists.
            call check( nf90_create(file_name, NF90_CLOBBER, ncid) )

            ! Define the dimensions. NetCDF will hand back an ID for each. 
            call check( nf90_def_dim(ncid, "X", grid%nx, xdimid) )
            call check( nf90_def_dim(ncid, "Y", grid%ny, ydimid) )

            ! The dimids array is used to pass the IDs of the dimensions of
            ! the variables. Note that in fortran arrays are stored in
            ! column-major format.
            dimids =  (/ ydimid, xdimid /)

            ! Define the variable. The type of the variable in this case is
            call check( nf90_def_var(ncid, "Xc", NF90_DOUBLE, xdimid, xvarid))
            call check( nf90_put_att(ncid, xvarid, "description", "x coordinate"))
            call check( nf90_put_att(ncid, xvarid, "units", "m"))
            call check( nf90_def_var(ncid, "Yc", NF90_DOUBLE, ydimid, yvarid))
            call check( nf90_put_att(ncid, yvarid, "description", "y coordinate"))
            call check( nf90_put_att(ncid, yvarid, "units", "m"))
            call check( nf90_def_var(ncid, "uwnd", NF90_DOUBLE, dimids, uvarid))
            call check( nf90_put_att(ncid, uvarid, "description", "x-component of the wind"))
            call check( nf90_put_att(ncid, uvarid, "units", "m/s"))
            call check( nf90_def_var(ncid, "vwnd", NF90_DOUBLE, dimids, vvarid))
            call check( nf90_put_att(ncid, vvarid, "description", "y-component of the wind"))
            call check( nf90_put_att(ncid, vvarid, "units", "m/s"))
            call check( nf90_def_var(ncid, "height", NF90_DOUBLE, dimids, hvarid))
            call check( nf90_put_att(ncid, hvarid, "description", "wave height"))
            call check( nf90_put_att(ncid, hvarid, "units", "m"))

            ! End define mode. This tells netCDF we are done defining metadata.
            call check( nf90_enddef(ncid) )

            ! Write the intent data to the file. Although netCDF supports
            ! reading and writing subsets of data, in this case we write all the
            ! data in one operation.
            call check( nf90_put_var(ncid, xvarid, grid%x))
            call check( nf90_put_var(ncid, yvarid, grid%y))
            call check( nf90_put_var(ncid, uvarid, grid%uwnd))
            call check( nf90_put_var(ncid, vvarid, grid%vwnd))
            call check( nf90_put_var(ncid, hvarid, grid%height))

            call check( nf90_close(ncid) )

            return

        end subroutine write_output

        subroutine check(status)

            implicit none
      
            integer(kind=i_kind), intent(in) :: status
      
            if (status /= nf90_noerr) then
              print*, trim(nf90_strerror(status))
              stop "Stopped"
            end if
      
            return
      
        end subroutine check


end module io_module