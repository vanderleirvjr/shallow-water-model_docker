module kinds

    implicit none

! Integer types    
    integer, parameter, public :: i_byte = selected_int_kind(1)        ! byte  integer
    integer, parameter, public  :: i_short = selected_int_kind(4)      ! short integer
    integer, parameter, public  :: i_long  = selected_int_kind(8)      ! long  integer
    integer, parameter, private :: llong_t = selected_int_kind(16)     ! llong integer
    integer, parameter, public  :: i_llong = max( llong_t, i_long )

    integer, parameter, private :: num_i_kinds = 4
    integer, parameter, dimension(num_i_kinds), private :: integer_types = (/ i_byte, i_short, i_long,  i_llong /) 

! Real types
    integer, parameter, public  :: r_single = selected_real_kind(6)  ! single precision
    integer, parameter, public  :: r_double = selected_real_kind(15) ! double precision
    integer, parameter, private :: quad_t   = selected_real_kind(20) ! quad precision
    integer, parameter, public  :: r_quad   = max( quad_t, r_double )

    integer, parameter, private :: num_r_kinds = 3
    integer, parameter, dimension(num_r_kinds), private :: real_kinds = (/ r_single, r_double, r_quad /) 

! Integer default types
    integer, parameter, private :: default_integer = 3  ! 1=byte, 
                                                        ! 2=short, 
                                                        ! 3=long, 
                                                        ! 4=llong
    integer, parameter, public  :: i_kind = integer_types(default_integer)

! real default types
    integer, parameter, private :: default_real = 2 

#ifdef _REAL8_
    default_real = 2  ! 2=double, 
#endif

#ifdef _REAL16_
    default_real = 3  ! 3=quad
#endif

    integer, parameter, public  :: r_kind = real_kinds(default_real)

end module kinds