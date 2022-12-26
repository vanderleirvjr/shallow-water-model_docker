module constants

    use kinds, only: i_kind, r_kind

    implicit none 

    real(kind=r_kind) :: PI=4.D0*datan(1.D0)
    real(kind=r_kind) :: GRAV=-9.81

end module constants