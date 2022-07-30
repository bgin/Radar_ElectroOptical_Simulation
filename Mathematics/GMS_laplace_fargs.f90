

module laplace_fargs

!==================================================================!
! Module containing the Laplace integrand additional arguments 
! i.e. user_data.
! These arrays shall be used by the Laplace Transform integrands.
!==================================================================!
  
    use mod_kinds, only : sp, dp
    implicit none
    public

    ! The length of arrays is equal to the number of transformed points.
    
    complex(kind=sp), dimension(:), allocatable :: s_c4
    !dir$ attributes align : 64 :: s_c4
    complex(kind=dp), dimension(:), allocatable :: s_c8
    !dir$ attributes align : 64 :: s_c8



end module laplace_fargs
