module gridinfo

    use kinds, only: r_kind, i_kind 

    type grid2d

        integer(kind=i_kind)                       :: nx 
        integer(kind=i_kind)                       :: ny 
        real(kind=r_kind), pointer, dimension(:)   :: x
        real(kind=r_kind), pointer, dimension(:)   :: y

        real(kind=r_kind), pointer, dimension(:,:) :: uwnd
        real(kind=r_kind), pointer, dimension(:,:) :: vwnd
        real(kind=r_kind), pointer, dimension(:,:) :: height

    end type grid2d

end module gridinfo