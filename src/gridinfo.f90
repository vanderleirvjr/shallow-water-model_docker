module gridinfo

! This module contains the main types used to define the
! model grid and properties.

    use kinds, only: r_kind, i_kind 

    implicit none

    public :: grid2d
    private

    type grid2d

        integer(kind=i_kind)                       :: nx 
        integer(kind=i_kind)                       :: ny 
        real(kind=r_kind), pointer, dimension(:)   :: x
        real(kind=r_kind), pointer, dimension(:)   :: y
        real(kind=r_kind)                          :: dx 
        real(kind=r_kind)                          :: dy 

        real(kind=r_kind), pointer, dimension(:,:) :: uwnd
        real(kind=r_kind), pointer, dimension(:,:) :: vwnd
        real(kind=r_kind), pointer, dimension(:,:) :: height

    end type grid2d

end module gridinfo