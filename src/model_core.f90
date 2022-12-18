module model_core

    use system_state, only: initialize_grid, initial_conditions
    use kinds, only: r_kind, i_kind 
    use gridinfo, only: grid2d
    use io_module, only: read_namelist, namelist_options
    use physics, only: update_height, update_wnd

    implicit none

    type(grid2d) :: grid

    contains

        subroutine initialize

            implicit none

            type(namelist_options) :: opt

            call read_namelist(opt)

            call initialize_grid(opt%nx,opt%ny,grid)

            call initial_conditions(grid)

            return

        end subroutine initialize

        subroutine run

            implicit none

            type(grid2d) :: new_grid

            call update_wnd
            call update_height

            grid = new_grid

            return

        end subroutine

        subroutine finalize

            implicit none

            return
        
        end subroutine finalize

end module model_core