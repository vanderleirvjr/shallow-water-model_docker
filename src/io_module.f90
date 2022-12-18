module io_module

    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    use kinds, only: i_kind, r_kind

    implicit none

    type namelist_options
        
        integer(kind=i_kind) :: nx
        integer(kind=i_kind) :: ny

        real(kind=r_kind) :: dt
        real(kind=r_kind) :: run_time

        real(kind=r_kind) :: time_step

    end type namelist_options
    
    contains
      
        subroutine read_namelist(opt)

            type(namelist_options), intent(out) :: opt
            integer(kind=i_kind) :: nx
            integer(kind=i_kind) :: ny
            real(kind=r_kind)    :: dt
            real(kind=r_kind)    :: run_time
            real(kind=r_kind)    :: time_step
            integer(kind=i_kind) :: rc, fu
            character(len=100)   :: file_path = "namelist.input"

            namelist /gridinfo/ nx, ny
            namelist /integration/ dt, run_time
            namelist /io/ time_step

            inquire (file=file_path, iostat=rc)

            if (rc /= 0) then
                write (stderr, '("Error: input file ", a, " does not exist")') file_path
                return
            end if

            open (newunit=fu, file=file_path, action='read', iostat=rc)
            read (nml=gridinfo, iostat=rc, unit=fu)
            if (rc /= 0) write (stderr, '("Error: Invalid namelist format")')
            
            read (nml=integration, iostat=rc, unit=fu)
            if (rc /= 0) write (stderr, '("Error: Invalid namelist format")')

            read (nml=io, iostat=rc, unit=fu)
            if (rc /= 0) write (stderr, '("Error: Invalid namelist format")')

            opt%nx = nx
            opt%ny = ny

            opt%run_time = run_time 
            opt%dt = dt

            opt%time_step = time_step

            return

        end subroutine read_namelist


end module io_module