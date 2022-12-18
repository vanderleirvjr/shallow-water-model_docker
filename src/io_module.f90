module io_module

    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    use kinds, only: i_kind, r_kind

    implicit none

    type namelist_options
        
        integer(kind=i_kind) :: nx
        integer(kind=i_kind) :: ny

    end type namelist_options
    
    contains
      
        subroutine read_namelist(opt)

            type(namelist_options), intent(out) :: opt
            integer(kind=i_kind) :: nx
            integer(kind=i_kind) :: ny
            integer(kind=i_kind) :: rc, fu
            character(len=100)   :: file_path = "namelist.input"

            namelist /dimensions/ nx, ny

            inquire (file=file_path, iostat=rc)

            if (rc /= 0) then
                write (stderr, '("Error: input file ", a, " does not exist")') file_path
                return
            end if

            open (newunit=fu, file=file_path, action='read', iostat=rc)
            read (nml=dimensions, iostat=rc, unit=fu)
            if (rc /= 0) write (stderr, '("Error: Invalid namelist format")')

            opt%nx = nx
            opt%ny = ny

            return

        end subroutine read_namelist


end module io_module