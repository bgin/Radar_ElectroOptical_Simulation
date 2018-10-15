
module mod_vechelpers

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_vechelpers'
 !          
 !          Purpose:
 !                     Vector helper procedures 
 !                     
 !          History:
 !                        Date: 14-10-2017
 !                        Time: 13:11 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                    Bernard Gingold
 !                 
 !         
 !          
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,    only : int1, int4, dp
    use mod_vectypes
    implicit none
    
    public ::  v2f_cmpeq_xmm2r8_xmm2r8,   &
               v3f_cmpeq_ymm3r8_ymm3r8,   &
               v4f_cmpeq_ymm4r8_ymm4r8,   &
               v8f_cmpeq_zmm8r8_zmm8r8,   &
               v2f_cmpeq_xmm2r8_r8,       &
               v3f_cmpeq_ymm3r8_r8,       &
               v4f_cmpeq_ymm4r8_r8,       &
               v8f_cmpeq_zmm8r8_r8,       &
               v2f_cmpneq_xmm2r8_xmm2r8,  &
               v3f_cmnpeq_ymm3r8_ymm3r8,  &
               v4f_cmpneq_ymm4r8_ymm4r8,  &
               v8f_cmpneq_zmm8r8_zmm8r8,  &
               v2f_cmpneq_xmm2r8_r8,      &
               v3f_cmpneq_ymm3r8_r8,      &
               v4f_cmpneq_ymm4r8_r8,      &
               v8f_cmpneq_zmm8r8_r8,      &
               v2f_cmpgt_xmm2r8_xmm2r8,   &
               v3f_cmpgt_ymm3r8_ymm3r8,   &
               v4f_cmpgt_ymm4r8_ymm4r8,   &
               v8f_cmpgt_zmm8r8_zmm8r8,   &
               v2f_cmpgt_xmm2r8_r8,       &
               v3f_cmpgt_ymm3r8_r8,       &
               v4f_cmpgt_ymm4r8_r8,       &
               v8f_cmpgt_zmm8r8_r8,       &
               v2f_cmpge_xmm2r8_xmm2r8,   &
               v3f_cmpge_ymm3r8_ymm3r8,   &
               v4f_cmpge_ymm4r8_ymm4r8,   &
               v8f_cmpge_zmm8r8_zmm8r8,   &
               v2f_cmpge_xmm2r8_r8,       &
               v3f_cmpge_ymm3r8_r8,       &
               v4f_cmpge_ymm4r8_r8,       &
               v8f_cmpge_zmm8r8_r8,       &
               v2f_cmplt_xmm2r8_xmm2r8,   &
               v3f_cmplt_ymm3r8_ymm3r8,   &
               v4f_cmplt_ymm4r8_ymm4r8,   &
               v8f_cmplt_zmm8r8_zmm8r8,   &
               v2f_cmplt_xmm2r8_r8,       &
               v3f_cmplt_ymm3r8_r8,       &
               v4f_cmplt_ymm4r8_r8,       &
               v8f_cmplt_zmm8r8_r8,       &
               v2f_cmple_xmm2r8_xmm2r8,   &
               v3f_cmple_ymm3r8_ymm3r8,   &
               v4f_cmple_ymm4r8_ymm4r8,   &
               v8f_cmple_zmm8r8_zmm8r8,   &
               v2f_cmple_xmm2r8_r8,       &
               v3f_cmple_ymm3r8_r8,       &
               v4f_cmple_ymm4r8_r8,       &
               v8f_cmple_zmm8r8_r8,       &
               v2s_cmpeq_xmm2r8_xmm2r8,   &
               v3s_cmpeq_ymm3r8_ymm3r8,   &
               v4s_cmpeq_ymm4r8_ymm4r8,   &
               v8s_cmpeq_zmm8r8_zmm8r8,   &
               v2s_cmpeq_xmm2r8_r8,       &
               v3s_cmpeq_ymm3r8_r8,        &
               v4s_cmpeq_ymm4r8_r8,        &
               v8s_cmpeq_zmm8r8_r8,        &
               v2s_cmpneq_xmm2r8_xmm2r8,   &
               v3s_cmpneq_ymm3r8_ymm3r8,   &
               v4s_cmpneq_ymm4r8_ymm4r8,   &
               v8s_cmpneq_zmm8r8_zmm8r8,   &
               v2s_cmpneq_xmm2r8_r8,       &
               v3s_cmpneq_ymm3r8_r8,       &
               v4s_cmpneq_ymm4r8_r8,       &
               v8s_cmpneq_zmm8r8_r8,       &
               v2s_cmpgt_xmm2r8_xmm2r8,    &
               v3s_cmpgt_ymm3r8_ymm3r8,    &
               v4s_cmpgt_ymm4r8_ymm4r8,    &
               v8s_cmpgt_zmm8r8_zmm8r8,    &
               v2s_cmpgt_xmm2r8_r8,        &
               v3s_cmpgt_ymm3r8_r8,        &
               v4s_cmpgt_ymm4r8_r8,        &
               v8s_cmpgt_zmm8r8_r8,        &
               v2s_cmpge_xmm2r8_xmm2r8,    &
               v3s_cmpge_ymm3r8_ymm3r8,    &
               v4s_cmpge_ymm4r8_ymm4r8,    &
               v8s_cmpge_zmm8r8_zmm8r8,    &
               v2s_cmpge_xmm2r8_r8,        &
               v3s_cmpge_ymm3r8_r8,        &
               v4s_cmpge_ymm4r8_r8,        &
               v8s_cmpge_zmm8r8_r8,        &
               v2s_cmplt_xmm2r8_xmm2r8,    &
               v3s_cmplt_ymm3r8_ymm3r8,    &
               v4s_cmplt_ymm4r8_ymm4r8,    &
               v8s_cmplt_zmm8r8_zmm8r8,    &
               v2s_cmplt_xmm2r8_r8,        &
               v3s_cmplt_ymm3r8_r8,        &
               v4s_cmplt_ymm4r8_r8,        &
               v8s_cmplt_zmm8r8_r8,        &
               v2s_cmple_xmm2r8_xmm2r8,   &
               v3s_cmple_ymm3r8_ymm3r8,   &
               v4s_cmple_ymm4r8_ymm4r8,   &
               v8s_cmple_zmm8r8_zmm8r8,   &
               v2s_cmple_xmm2r8_r8,       &
               v3s_cmple_ymm3r8_r8,       &
               v4s_cmple_ymm4r8_r8,       &
               v8s_cmple_zmm8r8_r8
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version 
    integer(kind=int4), parameter, public :: MOD_VECHELPERS_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_VECHELPERS_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_VECHELPERS_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_VECHELPERS_FULLVER = 1000_int4*MOD_VECHELPERS_MAJOR+100_int4*MOD_VECHELPERS_MINOR+ &
                                                                      10_int4*MOD_VECHELPERS_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_VECHELPERS_CREATE_DATE = "14-10-2018 14:22 PM GMT+2 (SUN 14 OCT 2018 14:22 -00200)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_VECHELPERS_BUILD_DATE = "00-00-0000 00:00"
    
    ! Module author info
    character(*),       parameter, public :: MOD_VECHELPERS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_VECHELPERS_SYNOPSIS = " Vector helper procedures."
    
    !! Interfaces
    
    interface vf_cmpeq
       module procedure :: v2f_cmpeq_xmm2r8_xmm2r8,  &
                           v3f_cmpeq_ymm3r8_ymm3r8,  &
                           v4f_cmpeq_ymm4r8_ymm4r8,  &
                           v8f_cmpeq_zmm8r8_zmm8r8,  &
                           v2f_cmpeq_xmm2r8_r8,      &
                           v3f_cmpeq_ymm3r8_r8,      &
                           v4f_cmpeq_ymm4r8_r8,      &
                           v8f_cmpeq_zmm8r8_r8
    end interface vf_cmpeq
    
    interface vf_cmpneq
        module procedure :: v2f_cmpneq_xmm2r8_xmm2r8,  &
                            v3f_cmpneq_ymm3r8_ymm3r8,  &
                            v4f_cmpneq_ymm4r8_ymm4r8,  &
                            v8f_cmpneq_zmm8r8_zmm8r8,  &
                            v2f_cmpneq_xmm2r8_r8,      &
                            v3f_cmpneq_ymm3r8_r8,      &
                            v4f_cmpneq_ymm4r8_r8,      &
                            v8f_cmpneq_zmm8r8_r8
    end interface vf_cmpneq
    
    interface vf_cmpgt
         module procedure :: v2f_cmpgt_xmm2r8_xmm2r8,  &
                             v3f_cmpgt_ymm3r8_ymm3r8,  &
                             v4f_cmpgt_ymm4r8_ymm4r8,  &
                             v8f_cmpgt_zmm8r8_zmm8r8,  &
                             v2f_cmpgt_xmm2r8_r8,      &
                             v3f_cmpgt_ymm3r8_r8,      &
                             v4f_cmpgt_ymm4r8_r8,      &
                             v8f_cmpgt_zmm8r8_r8
    end interface vf_cmpgt
    
    interface vf_cmpge
         module procedure :: v2f_cmpge_xmm2r8_xmm2r8,  &
                             v3f_cmpge_ymm3r8_ymm3r8,  &
                             v4f_cmpge_ymm4r8_ymm4r8,  &
                             v8f_cmpge_zmm8r8_zmm8r8,  &
                             v2f_cmpge_xmm2r8_r8,      &
                             v3f_cmpge_ymm3r8_r8,      &
                             v4f_cmpge_ymm4r8_r8,      &
                             v8f_cmpge_zmm8r8_r8
    
    end interface vf_cmpge
    
    interface vf_cmplt
        module procedure ::  v2f_cmplt_xmm2r8_xmm2r8,  &
                             v3f_cmplt_ymm3r8_ymm3r8,  &
                             v4f_cmplt_ymm4r8_ymm4r8,  &
                             v8f_cmplt_zmm8r8_zmm8r8,  &
                             v2f_cmplt_xmm2r8_r8,      &
                             v3f_cmplt_ymm3r8_r8,      &
                             v4f_cmplt_ymm4r8_r8,      &
                             v8f_cmplt_zmm8r8_r8
    
    end interface vf_cmplt
    
    interface vf_cmple
         module procedure :: v2f_cmple_xmm2r8_xmm2r8,  &
                             v3f_cmple_ymm3r8_ymm3r8,  &
                             v4f_cmple_ymm4r8_ymm4r8,  &
                             v8f_cmple_zmm8r8_zmm8r8,  &
                             v2f_cmple_xmm2r8_r8,      &
                             v3f_cmple_ymm3r8_r8,      &
                             v4f_cmple_ymm4r8_r8,      &
                             v8f_cmple_zmm8r8_r8
    end interface vf_cmple
    
    interface vs_cmpeq
         module procedure :: v2s_cmpeq_xmm2r8_xmm2r8, &
                             v3s_cmpeq_ymm3r8_ymm3r8, &
                             v4s_cmpeq_ymm4r8_ymm4r8, &
                             v8s_cmpeq_zmm8r8_zmm8r8, &
                             v2s_cmpeq_xmm2r8_r8,     &
                             v3s_cmpeq_ymm3r8_r8,     &
                             v4s_cmpeq_ymm4r8_r8,     &
                             v8s_cmpeq_zmm8r8_r8      
    end interface vs_cmpeq
    
    interface vs_cmpneq
        module procedure ::  v2s_cmpneq_xmm2r8_xmm2r8, &
                             v3s_cmpneq_ymm3r8_ymm3r8, &
                             v4s_cmpneq_ymm4r8_ymm4r8, &
                             v8s_cmpneq_zmm8r8_zmm8r8, &
                             v2s_cmpneq_xmm2r8_r8,     &
                             v3s_cmpneq_ymm3r8_r8,     &
                             v4s_cmpneq_ymm4r8_r8,     &
                             v8s_cmpneq_zmm8r8_r8 
    
    end interface vs_cmpneq
    
    interface vs_cmpgt
        module procedure ::  v2s_cmpgt_xmm2r8_xmm2r8, &
                             v3s_cmpgt_ymm3r8_ymm3r8, &
                             v4s_cmpgt_ymm4r8_ymm4r8, &
                             v8s_cmpgt_zmm8r8_zmm8r8, &
                             v2s_cmpgt_xmm2r8_r8,     &
                             v3s_cmpgt_ymm3r8_r8,     &
                             v4s_cmpgt_ymm4r8_r8,     &
                             v8s_cmpgt_zmm8r8_r8 
    end interface vs_cmpgt
    
    interface vs_cmpge
        module procedure ::  v2s_cmpge_xmm2r8_xmm2r8, &
                             v3s_cmpge_ymm3r8_ymm3r8, &
                             v4s_cmpge_ymm4r8_ymm4r8, &
                             v8s_cmpge_zmm8r8_zmm8r8, &
                             v2s_cmpge_xmm2r8_r8,     &
                             v3s_cmpge_ymm3r8_r8,     &
                             v4s_cmpge_ymm4r8_r8,     &
                             v8s_cmpge_zmm8r8_r8 
    end interface vs_cmpge
    
    interface vs_cmplt
        module procedure ::  v2s_cmplt_xmm2r8_xmm2r8, &
                             v3s_cmplt_ymm3r8_ymm3r8, &
                             v4s_cmplt_ymm4r8_ymm4r8, &
                             v8s_cmplt_zmm8r8_zmm8r8, &
                             v2s_cmplt_xmm2r8_r8,     &
                             v3s_cmplt_ymm3r8_r8,     &
                             v4s_cmplt_ymm4r8_r8,     &
                             v8s_cmplt_zmm8r8_r8 
    
    end interface vs_cmplt
    
    interface vs_cmple
        module procedure ::  v2s_cmple_xmm2r8_xmm2r8, &
                             v3s_cmple_ymm3r8_ymm3r8, &
                             v4s_cmple_ymm4r8_ymm4r8, &
                             v8s_cmple_zmm8r8_zmm8r8, &
                             v2s_cmple_xmm2r8_r8,     &
                             v3s_cmple_ymm3r8_r8,     &
                             v4s_cmple_ymm4r8_r8,     &
                             v8s_cmple_zmm8r8_r8 
    end interface vs_cmple
    
    
    
    
    contains
    
    pure function v2f_cmpeq_xmm2r8_xmm2r8(x,y) result (bres)
            type(XMM2r8_t),     intent(in) :: x
            type(XMM2r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ....
            bres = x.v(0) == y.v(0) .and. x.v(1) == y.v(1)
    end function v2f_cmpeq_xm2r8_xmm2r8
    
    pure function v3f_cmpeq_ymm3r8_ymm3r8(x,y) result(bres)
            type(YMM3r8_t),     intent(in) :: x
            type(YMM3r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) == y.v(0) .and. x.v(1) == y.v(1) & 
                   .and. x.v(2) == y.v(2)
    end function v3f_cmpeq_ymm3r8_ymm3r8
    
    pure function v4f_cmpeq_ymm4r8_ymm4r8(x,y) result(bres)
            type(YMM4r8_t),     intent(in) :: x
            type(YMM4r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) == y.v(0) .and. &
                   x.v(1) == y.v(1) .and. &
                   x.v(2) == y.v(2) .and. &
                   x.v(3) == y.v(3)
    end function  v4f_cmpeq_ymm4r8_ymm4r8
    
    pure function v8f_cmpeq_zmm8r8_zmm8r8(x,y) result(bres)
            type(ZMM8r8_t),     intent(in) :: x
            type(ZMM8r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code...
            bres = x.v(0) == y.v(0) .and. &
                   x.v(1) == y.v(1) .and. &
                   x.v(2) == y.v(2) .and. &
                   x.v(3) == y.v(3) .and. &
                   x.v(4) == y.v(4) .and. &
                   x.v(5) == y.v(5) .and. &
                   x.v(6) == y.v(6) .and. &
                   x.v(7) == y.v(7)
    end function v8f_cmpeq_zmm8r8_zmm8r8
    
    pure function v2f_cmpeq_xmm2r8_r8(x,y) result(bres)
             type(XMM2r8_t),     intent(in) :: x
             real(kind=dp),      intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) == y .and. x.v(1) == y
    end function v2f_cmpeq_xmm2r8_r8
    
    pure function v3f_cmpeq_ymm3r8_r8(x,y) result(bres)
             type(YMM3r8_t),      intent(in) :: x
             real(kind=dp),       intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) == y .and. &
                    x.v(1) == y .and. &
                    x.v(2) == y
    end function v3f_cmpeq_ymm3r8_r8
    
    pure function v4f_cmpeq_ymm4r8_r8(x,y) result(bres)
            type(YMM4r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) == y .and. &
                   x.v(1) == y .and. &
                   x.v(2) == y .and. &
                   x.v(3) == y 
    end function v4f_cmpeq_ymm4r8_r8
    
    pure function v8f_cmpeq_zmm8r8_r8(x,y) result(bres)
            type(ZMM8r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) == y .and. &
                   x.v(1) == y .and. &
                   x.v(3) == y .and. &
                   x.v(4) == y .and. &
                   x.v(5) == y .and. &
                   x.v(6) == y .and. &
                   x.v(7) == y
    end function v8f_cmpeq_zmm8r8_r8
    
     pure function v2f_cmpneq_xmm2r8_xmm2r8(x,y) result (bres)
            type(XMM2r8_t),     intent(in) :: x
            type(XMM2r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ....
            bres = x.v(0) /= y.v(0) .and. x.v(1) /= y.v(1)
    end function v2f_cmpneq_xm2r8_xmm2r8
    
    pure function v3f_cmpneq_ymm3r8_ymm3r8(x,y) result(bres)
            type(YMM3r8_t),     intent(in) :: x
            type(YMM3r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) /= y.v(0) .and. x.v(1) /= y.v(1) & 
                   .and. x.v(2) /= y.v(2)
    end function v3f_cmpneq_ymm3r8_ymm3r8
    
    pure function v4f_cmpneq_ymm4r8_ymm4r8(x,y) result(bres)
            type(YMM4r8_t),     intent(in) :: x
            type(YMM4r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) /= y.v(0) .and. &
                   x.v(1) /= y.v(1) .and. &
                   x.v(2) /= y.v(2) .and. &
                   x.v(3) /= y.v(3)
    end function  v4f_cmpneq_ymm4r8_ymm4r8
    
    pure function v8f_cmpneq_zmm8r8_zmm8r8(x,y) result(bres)
            type(ZMM8r8_t),     intent(in) :: x
            type(ZMM8r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code...
            bres = x.v(0) /= y.v(0) .and. &
                   x.v(1) /= y.v(1) .and. &
                   x.v(2) /= y.v(2) .and. &
                   x.v(3) /= y.v(3) .and. &
                   x.v(4) /= y.v(4) .and. &
                   x.v(5) /= y.v(5) .and. &
                   x.v(6) /= y.v(6) .and. &
                   x.v(7) /= y.v(7)
    end function v8f_cmpneq_zmm8r8_zmm8r8
    
    pure function v2f_cmpneq_xmm2r8_r8(x,y) result(bres)
             type(XMM2r8_t),     intent(in) :: x
             real(kind=dp),      intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) /= y .and. x.v(1) /= y
    end function v2f_cmpneq_xmm2r8_r8
    
    pure function v3f_cmpneq_ymm3r8_r8(x,y) result(bres)
             type(YMM3r8_t),      intent(in) :: x
             real(kind=dp),       intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) /= y .and. &
                    x.v(1) /= y .and. &
                    x.v(2) /= y
    end function v3f_cmpneq_ymm3r8_r8
    
    pure function v4f_cmpneq_ymm4r8_r8(x,y) result(bres)
            type(YMM4r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) /= y .and. &
                   x.v(1) /= y .and. &
                   x.v(2) /= y .and. &
                   x.v(3) /= y 
    end function v4f_cmpneq_ymm4r8_r8
    
    pure function v8f_cmpneq_zmm8r8_r8(x,y) result(bres)
            type(ZMM8r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) /= y .and. &
                   x.v(1) /= y .and. &
                   x.v(3) /= y .and. &
                   x.v(4) /= y .and. &
                   x.v(5) /= y .and. &
                   x.v(6) /= y .and. &
                   x.v(7) /= y
    end function v8f_cmpneq_zmm8r8_r8
    
      pure function v2f_cmpgt_xmm2r8_xmm2r8(x,y) result (bres)
            type(XMM2r8_t),     intent(in) :: x
            type(XMM2r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ....
            bres = x.v(0) > y.v(0) .and. x.v(1) > y.v(1)
    end function v2f_cmpgt_xm2r8_xmm2r8
    
    pure function v3f_cmpgt_ymm3r8_ymm3r8(x,y) result(bres)
            type(YMM3r8_t),     intent(in) :: x
            type(YMM3r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) > y.v(0) .and. x.v(1) > y.v(1) & 
                   .and. x.v(2) > y.v(2)
    end function v3f_cmpgt_ymm3r8_ymm3r8
    
    pure function v4f_cmpgt_ymm4r8_ymm4r8(x,y) result(bres)
            type(YMM4r8_t),     intent(in) :: x
            type(YMM4r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) > y.v(0) .and. &
                   x.v(1) > y.v(1) .and. &
                   x.v(2) > y.v(2) .and. &
                   x.v(3) > y.v(3)
    end function  v4f_cmpgt_ymm4r8_ymm4r8
    
    pure function v8f_cmpgt_zmm8r8_zmm8r8(x,y) result(bres)
            type(ZMM8r8_t),     intent(in) :: x
            type(ZMM8r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code...
            bres = x.v(0) > y.v(0) .and. &
                   x.v(1) > y.v(1) .and. &
                   x.v(2) > y.v(2) .and. &
                   x.v(3) > y.v(3) .and. &
                   x.v(4) > y.v(4) .and. &
                   x.v(5) > y.v(5) .and. &
                   x.v(6) > y.v(6) .and. &
                   x.v(7) > y.v(7)
    end function v8f_cmpgt_zmm8r8_zmm8r8
    
    pure function v2f_cmpgt_xmm2r8_r8(x,y) result(bres)
             type(XMM2r8_t),     intent(in) :: x
             real(kind=dp),      intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) > y .and. x.v(1) > y
    end function v2f_cmpgt_xmm2r8_r8
    
    pure function v3f_cmpgt_ymm3r8_r8(x,y) result(bres)
             type(YMM3r8_t),      intent(in) :: x
             real(kind=dp),       intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) > y .and. &
                    x.v(1) > y .and. &
                    x.v(2) > y
    end function v3f_cmpgt_ymm3r8_r8
    
    pure function v4f_cmpgt_ymm4r8_r8(x,y) result(bres)
            type(YMM4r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) > y .and. &
                   x.v(1) > y .and. &
                   x.v(2) > y .and. &
                   x.v(3) > y 
    end function v4f_cmpgt_ymm4r8_r8
    
    pure function v8f_cmpgt_zmm8r8_r8(x,y) result(bres)
            type(ZMM8r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) > y .and. &
                   x.v(1) > y .and. &
                   x.v(3) > y .and. &
                   x.v(4) > y .and. &
                   x.v(5) > y .and. &
                   x.v(6) > y .and. &
                   x.v(7) > y
    end function v8f_cmpgt_zmm8r8_r8
    
    pure function v2f_cmplt_xmm2r8_xmm2r8(x,y) result (bres)
            type(XMM2r8_t),     intent(in) :: x
            type(XMM2r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ....
            bres = x.v(0) < y.v(0) .and. x.v(1) < y.v(1)
    end function v2f_cmplt_xm2r8_xmm2r8
    
    pure function v3f_cmplt_ymm3r8_ymm3r8(x,y) result(bres)
            type(YMM3r8_t),     intent(in) :: x
            type(YMM3r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) < y.v(0) .and. &
                   x.v(1) < y.v(1) .and. & 
                   x.v(2) < y.v(2)
    end function v3f_cmplt_ymm3r8_ymm3r8
    
    pure function v4f_cmplt_ymm4r8_ymm4r8(x,y) result(bres)
            type(YMM4r8_t),     intent(in) :: x
            type(YMM4r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) < y.v(0) .and. &
                   x.v(1) < y.v(1) .and. &
                   x.v(2) < y.v(2) .and. &
                   x.v(3) < y.v(3)
    end function  v4f_cmplt_ymm4r8_ymm4r8
    
    pure function v8f_cmplt_zmm8r8_zmm8r8(x,y) result(bres)
            type(ZMM8r8_t),     intent(in) :: x
            type(ZMM8r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code...
            bres = x.v(0) < y.v(0) .and. &
                   x.v(1) < y.v(1) .and. &
                   x.v(2) < y.v(2) .and. &
                   x.v(3) < y.v(3) .and. &
                   x.v(4) < y.v(4) .and. &
                   x.v(5) < y.v(5) .and. &
                   x.v(6) < y.v(6) .and. &
                   x.v(7) < y.v(7)
    end function v8f_cmplt_zmm8r8_zmm8r8
    
    pure function v2f_cmplt_xmm2r8_r8(x,y) result(bres)
             type(XMM2r8_t),     intent(in) :: x
             real(kind=dp),      intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) < y .and. x.v(1) == y
    end function v2f_cmplt_xmm2r8_r8
    
    pure function v3f_cmplt_ymm3r8_r8(x,y) result(bres)
             type(YMM3r8_t),      intent(in) :: x
             real(kind=dp),       intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) < y .and. &
                    x.v(1) < y .and. &
                    x.v(2) < y
    end function v3f_cmplt_ymm3r8_r8
    
    pure function v4f_cmplt_ymm4r8_r8(x,y) result(bres)
            type(YMM4r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) < y .and. &
                   x.v(1) < y .and. &
                   x.v(2) < y .and. &
                   x.v(3) < y 
    end function v4f_cmplt_ymm4r8_r8
    
    pure function v8f_cmplt_zmm8r8_r8(x,y) result(bres)
            type(ZMM8r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) < y .and. &
                   x.v(1) < y .and. &
                   x.v(3) < y .and. &
                   x.v(4) < y .and. &
                   x.v(5) < y .and. &
                   x.v(6) < y .and. &
                   x.v(7) < y
    end function v8f_cmplt_zmm8r8_r8
    
    pure function v2f_cmpge_xmm2r8_xmm2r8(x,y) result (bres)
            type(XMM2r8_t),     intent(in) :: x
            type(XMM2r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ....
            bres = x.v(0) >= y.v(0) .and. x.v(1) >= y.v(1)
    end function v2f_cmpge_xm2r8_xmm2r8
    
    pure function v3f_cmpge_ymm3r8_ymm3r8(x,y) result(bres)
            type(YMM3r8_t),     intent(in) :: x
            type(YMM3r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) >= y.v(0) .and.  &
                   x.v(1) >= y.v(1) .and.  & 
                   x.v(2) >= y.v(2)
    end function v3f_cmpge_ymm3r8_ymm3r8
    
    pure function v4f_cmpge_ymm4r8_ymm4r8(x,y) result(bres)
            type(YMM4r8_t),     intent(in) :: x
            type(YMM4r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) >= y.v(0) .and. &
                   x.v(1) >= y.v(1) .and. &
                   x.v(2) >= y.v(2) .and. &
                   x.v(3) >= y.v(3)
    end function  v4f_cmpge_ymm4r8_ymm4r8
    
    pure function v8f_cmpge_zmm8r8_zmm8r8(x,y) result(bres)
            type(ZMM8r8_t),     intent(in) :: x
            type(ZMM8r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code...
            bres = x.v(0) >= y.v(0) .and. &
                   x.v(1) >= y.v(1) .and. &
                   x.v(2) >= y.v(2) .and. &
                   x.v(3) >= y.v(3) .and. &
                   x.v(4) >= y.v(4) .and. &
                   x.v(5) >= y.v(5) .and. &
                   x.v(6) >= y.v(6) .and. &
                   x.v(7) >= y.v(7)
    end function v8f_cmpge_zmm8r8_zmm8r8
    
    pure function v2f_cmpge_xmm2r8_r8(x,y) result(bres)
             type(XMM2r8_t),     intent(in) :: x
             real(kind=dp),      intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) >= y .and. x.v(1) >= y
    end function v2f_cmpge_xmm2r8_r8
    
    pure function v3f_cmpge_ymm3r8_r8(x,y) result(bres)
             type(YMM3r8_t),      intent(in) :: x
             real(kind=dp),       intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) >= y .and. &
                    x.v(1) >= y .and. &
                    x.v(2) >= y
    end function v3f_cmpge_ymm3r8_r8
    
    pure function v4f_cmpge_ymm4r8_r8(x,y) result(bres)
            type(YMM4r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) >= y .and. &
                   x.v(1) >= y .and. &
                   x.v(2) >= y .and. &
                   x.v(3) >= y 
    end function v4f_cmpge_ymm4r8_r8
    
    pure function v8f_cmpge_zmm8r8_r8(x,y) result(bres)
            type(ZMM8r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) >= y .and. &
                   x.v(1) >= y .and. &
                   x.v(3) >= y .and. &
                   x.v(4) >= y .and. &
                   x.v(5) >= y .and. &
                   x.v(6) >= y .and. &
                   x.v(7) >= y
    end function v8f_cmpge_zmm8r8_r8
    
     pure function v2f_cmple_xmm2r8_xmm2r8(x,y) result (bres)
            type(XMM2r8_t),     intent(in) :: x
            type(XMM2r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ....
            bres = x.v(0) <= y.v(0) .and. x.v(1) <= y.v(1)
    end function v2f_cmple_xm2r8_xmm2r8
    
    pure function v3f_cmple_ymm3r8_ymm3r8(x,y) result(bres)
            type(YMM3r8_t),     intent(in) :: x
            type(YMM3r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) <= y.v(0) .and. x.v(1) <= y.v(1) & 
                   .and. x.v(2) <= y.v(2)
    end function v3f_cmple_ymm3r8_ymm3r8
    
    pure function v4f_cmple_ymm4r8_ymm4r8(x,y) result(bres)
            type(YMM4r8_t),     intent(in) :: x
            type(YMM4r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) <= y.v(0) .and. &
                   x.v(1) <= y.v(1) .and. &
                   x.v(2) <= y.v(2) .and. &
                   x.v(3) <= y.v(3)
    end function  v4f_cmple_ymm4r8_ymm4r8
    
    pure function v8f_cmple_zmm8r8_zmm8r8(x,y) result(bres)
            type(ZMM8r8_t),     intent(in) :: x
            type(ZMM8r8_t),     intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code...
            bres = x.v(0) <= y.v(0) .and. &
                   x.v(1) <= y.v(1) .and. &
                   x.v(2) <= y.v(2) .and. &
                   x.v(3) <= y.v(3) .and. &
                   x.v(4) <= y.v(4) .and. &
                   x.v(5) <= y.v(5) .and. &
                   x.v(6) <= y.v(6) .and. &
                   x.v(7) <= y.v(7)
    end function v8f_cmple_zmm8r8_zmm8r8
    
    pure function v2f_cmple_xmm2r8_r8(x,y) result(bres)
             type(XMM2r8_t),     intent(in) :: x
             real(kind=dp),      intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) <= y .and. x.v(1) <= y
    end function v2f_cmple_xmm2r8_r8
    
    pure function v3f_cmple_ymm3r8_r8(x,y) result(bres)
             type(YMM3r8_t),      intent(in) :: x
             real(kind=dp),       intent(in) :: y
             ! Locals
             logical(kind=int4) :: bres
             ! Exec code ....
             bres = x.v(0) <= y .and. &
                    x.v(1) <= y .and. &
                    x.v(2) <= y
    end function v3f_cmple_ymm3r8_r8
    
    pure function v4f_cmple_ymm4r8_r8(x,y) result(bres)
            type(YMM4r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) <= y .and. &
                   x.v(1) <= y .and. &
                   x.v(2) <= y .and. &
                   x.v(3) <= y 
    end function v4f_cmple_ymm4r8_r8
    
    pure function v8f_cmple_zmm8r8_r8(x,y) result(bres)
            type(ZMM8r8_t),        intent(in) :: x
            real(kind=dp),         intent(in) :: y
            ! Locals
            logical(kind=int4) :: bres
            ! Exec code ...
            bres = x.v(0) <= y .and. &
                   x.v(1) <= y .and. &
                   x.v(3) <= y .and. &
                   x.v(4) <= y .and. &
                   x.v(5) <= y .and. &
                   x.v(6) <= y .and. &
                   x.v(7) <= y
    end function v8f_cmple_zmm8r8_r8
    
    subroutine v2s_cmpeq_xmm2r8_xmm2r8(mask,x,y)
             type(Mask2_t),   intent(out) :: mask
             type(XMM2r8_t),  intent(in)  :: x
             type(XMM2r8_t),  intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) == y.v(0)
             mask.m(1) = x.v(1) == y.v(1)
    end subroutine
    
    subroutine v3s_cmpeq_ymm3r8_ymm3r8(mask,x,y)
             type(Mask3_t),    intent(out) :: mask
             type(YMM3r8_t),   intent(in)  :: x
             type(YMM3r8_t),   intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) == y.v(0)
             mask.m(1) = x.v(1) == y.v(1)
             mask.m(2) = x.v(2) == y.v(2)
    end subroutine v3s_cmpeq_ymm3r8_ymm3r8
    
    subroutine v4s_cmpeq_ymm4r8_ymm4r8(mask,x,y)
             type(Mask4_t),    intent(out) :: mask
             type(YMM4r8_t),   intent(in)  :: x
             type(YMM4r8_t),   intent(in)  :: y
             ! Exec code....
             mask.m(0) = x.v(0) == y.v(0)
             mask.m(1) = x.v(1) == y.v(1)
             mask.m(2) = x.v(2) == y.v(2)
             mask.m(3) = x.v(3) == y.v(3)
    end subroutine v4s_cmpeq_ymm4r8_ymm4r8
    
    subroutine v8s_cmpeq_zmm8r8_zmm8r8(mask,x,y)
            type(Mask8_t),     intent(out)  :: mask
            type(ZMM8r8_t),    intent(in)   :: x
            type(ZMM8r8_t),    intent(in)   :: y
            ! Exec code ....
            mask.m(0) = x.v(0) == y.v(0)
            mask.m(1) = x.v(1) == y.v(1)
            mask.m(2) = x.v(2) == y.v(2)
            mask.m(3) = x.v(3) == y.v(3)
            mask.m(4) = x.v(4) == y.v(4)
            mask.m(5) = x.v(5) == y.v(5)
            mask.m(6) = x.v(6) == y.v(6)
            mask.m(7) = x.v(7) == y.v(7)
    end subroutine v8s_zmm8r8_zmm8r8
    
     subroutine v2s_cmpneq_xmm2r8_xmm2r8(mask,x,y)
             type(Mask2_t),   intent(out) :: mask
             type(XMM2r8_t),  intent(in)  :: x
             type(XMM2r8_t),  intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) /= y.v(0)
             mask.m(1) = x.v(1) /= y.v(1)
    end subroutine  v2s_cmpneq_xmm2r8_xmm2r8
    
    subroutine v3s_cmpneq_ymm3r8_ymm3r8(mask,x,y)
             type(Mask3_t),    intent(out) :: mask
             type(YMM3r8_t),   intent(in)  :: x
             type(YMM3r8_t),   intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) /= y.v(0)
             mask.m(1) = x.v(1) /= y.v(1)
             mask.m(2) = x.v(2) /= y.v(2)
    end subroutine v3s_cmpneq_ymm3r8_ymm3r8
    
    subroutine v4s_cmpneq_ymm4r8_ymm4r8(mask,x,y)
             type(Mask4_t),    intent(out) :: mask
             type(YMM4r8_t),   intent(in)  :: x
             type(YMM4r8_t),   intent(in)  :: y
             ! Exec code....
             mask.m(0) = x.v(0) /= y.v(0)
             mask.m(1) = x.v(1) /= y.v(1)
             mask.m(2) = x.v(2) /= y.v(2)
             mask.m(3) = x.v(3) /= y.v(3)
    end subroutine v4s_cmpneq_ymm4r8_ymm4r8
    
    subroutine v8s_cmpneq_zmm8r8_zmm8r8(mask,x,y)
            type(Mask8_t),     intent(out)  :: mask
            type(ZMM8r8_t),    intent(in)   :: x
            type(ZMM8r8_t),    intent(in)   :: y
            ! Exec code ....
            mask.m(0) = x.v(0) /= y.v(0)
            mask.m(1) = x.v(1) /= y.v(1)
            mask.m(2) = x.v(2) /= y.v(2)
            mask.m(3) = x.v(3) /= y.v(3)
            mask.m(4) = x.v(4) /= y.v(4)
            mask.m(5) = x.v(5) /= y.v(5)
            mask.m(6) = x.v(6) /= y.v(6)
            mask.m(7) = x.v(7) /= y.v(7)
    end subroutine v8s_cmpneq_zmm8r8_zmm8r8
    
       subroutine v2s_cmpgt_xmm2r8_xmm2r8(mask,x,y)
             type(Mask2_t),   intent(out) :: mask
             type(XMM2r8_t),  intent(in)  :: x
             type(XMM2r8_t),  intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) > y.v(0)
             mask.m(1) = x.v(1) > y.v(1)
    end subroutine  v2s_cmpgt_xmm2r8_xmm2r8
    
    subroutine v3s_cmpgt_ymm3r8_ymm3r8(mask,x,y)
             type(Mask3_t),    intent(out) :: mask
             type(YMM3r8_t),   intent(in)  :: x
             type(YMM3r8_t),   intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) > y.v(0)
             mask.m(1) = x.v(1) > y.v(1)
             mask.m(2) = x.v(2) > y.v(2)
    end subroutine v3s_cmpgt_ymm3r8_ymm3r8
    
    subroutine v4s_cmpgt_ymm4r8_ymm4r8(mask,x,y)
             type(Mask4_t),    intent(out) :: mask
             type(YMM4r8_t),   intent(in)  :: x
             type(YMM4r8_t),   intent(in)  :: y
             ! Exec code....
             mask.m(0) = x.v(0) > y.v(0)
             mask.m(1) = x.v(1) > y.v(1)
             mask.m(2) = x.v(2) > y.v(2)
             mask.m(3) = x.v(3) > y.v(3)
    end subroutine v4s_cmpgt_ymm4r8_ymm4r8
    
    subroutine v8s_cmpgt_zmm8r8_zmm8r8(mask,x,y)
            type(Mask8_t),     intent(out)  :: mask
            type(ZMM8r8_t),    intent(in)   :: x
            type(ZMM8r8_t),    intent(in)   :: y
            ! Exec code ....
            mask.m(0) = x.v(0) > y.v(0)
            mask.m(1) = x.v(1) > y.v(1)
            mask.m(2) = x.v(2) > y.v(2)
            mask.m(3) = x.v(3) > y.v(3)
            mask.m(4) = x.v(4) > y.v(4)
            mask.m(5) = x.v(5) > y.v(5)
            mask.m(6) = x.v(6) > y.v(6)
            mask.m(7) = x.v(7) > y.v(7)
    end subroutine v8scmpgt_zmm8r8_zmm8r8
    
     subroutine v2s_cmpge_xmm2r8_xmm2r8(mask,x,y)
             type(Mask2_t),   intent(out) :: mask
             type(XMM2r8_t),  intent(in)  :: x
             type(XMM2r8_t),  intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) >= y.v(0)
             mask.m(1) = x.v(1) >= y.v(1)
    end subroutine  v2s_cmpge_xmm2r8_xmm2r8
    
    subroutine v3s_cmpge_ymm3r8_ymm3r8(mask,x,y)
             type(Mask3_t),    intent(out) :: mask
             type(YMM3r8_t),   intent(in)  :: x
             type(YMM3r8_t),   intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) >= y.v(0)
             mask.m(1) = x.v(1) >= y.v(1)
             mask.m(2) = x.v(2) >= y.v(2)
    end subroutine v3s_cmpge_ymm3r8_ymm3r8
    
    subroutine v4s_cmpge_ymm4r8_ymm4r8(mask,x,y)
             type(Mask4_t),    intent(out) :: mask
             type(YMM4r8_t),   intent(in)  :: x
             type(YMM4r8_t),   intent(in)  :: y
             ! Exec code....
             mask.m(0) = x.v(0) >= y.v(0)
             mask.m(1) = x.v(1) >= y.v(1)
             mask.m(2) = x.v(2) >= y.v(2)
             mask.m(3) = x.v(3) >= y.v(3)
    end subroutine v4s_cmpge_ymm4r8_ymm4r8
    
    subroutine v8s_cmpge_zmm8r8_zmm8r8(mask,x,y)
            type(Mask8_t),     intent(out)  :: mask
            type(ZMM8r8_t),    intent(in)   :: x
            type(ZMM8r8_t),    intent(in)   :: y
            ! Exec code ....
            mask.m(0) = x.v(0) >= y.v(0)
            mask.m(1) = x.v(1) >= y.v(1)
            mask.m(2) = x.v(2) >= y.v(2)
            mask.m(3) = x.v(3) >= y.v(3)
            mask.m(4) = x.v(4) >= y.v(4)
            mask.m(5) = x.v(5) >= y.v(5)
            mask.m(6) = x.v(6) >= y.v(6)
            mask.m(7) = x.v(7) >= y.v(7)
    end subroutine v8s_cmpge_zmm8r8_zmm8r8
    
            subroutine v2s_cmplt_xmm2r8_xmm2r8(mask,x,y)
             type(Mask2_t),   intent(out) :: mask
             type(XMM2r8_t),  intent(in)  :: x
             type(XMM2r8_t),  intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) < y.v(0)
             mask.m(1) = x.v(1) < y.v(1)
    end subroutine v2s_cmplt_xmm2r8_xmm2r8
    
    subroutine v3s_cmplt_ymm3r8_ymm3r8(mask,x,y)
             type(Mask3_t),    intent(out) :: mask
             type(YMM3r8_t),   intent(in)  :: x
             type(YMM3r8_t),   intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) < y.v(0)
             mask.m(1) = x.v(1) < y.v(1)
             mask.m(2) = x.v(2) < y.v(2)
    end subroutine v3s_cmplt_ymm3r8_ymm3r8
    
    subroutine v4s_cmplt_ymm4r8_ymm4r8(mask,x,y)
             type(Mask4_t),    intent(out) :: mask
             type(YMM4r8_t),   intent(in)  :: x
             type(YMM4r8_t),   intent(in)  :: y
             ! Exec code....
             mask.m(0) = x.v(0) < y.v(0)
             mask.m(1) = x.v(1) < y.v(1)
             mask.m(2) = x.v(2) < y.v(2)
             mask.m(3) = x.v(3) < y.v(3)
    end subroutine v4s_cmplt_ymm4r8_ymm4r8
    
    subroutine v8s_cmplt_zmm8r8_zmm8r8(mask,x,y)
            type(Mask8_t),     intent(out)  :: mask
            type(ZMM8r8_t),    intent(in)   :: x
            type(ZMM8r8_t),    intent(in)   :: y
            ! Exec code ....
            mask.m(0) = x.v(0) < y.v(0)
            mask.m(1) = x.v(1) < y.v(1)
            mask.m(2) = x.v(2) < y.v(2)
            mask.m(3) = x.v(3) < y.v(3)
            mask.m(4) = x.v(4) < y.v(4)
            mask.m(5) = x.v(5) < y.v(5)
            mask.m(6) = x.v(6) < y.v(6)
            mask.m(7) = x.v(7) < y.v(7)
    end subroutine v8s_cmplt_zmm8r8_zmm8r8
    
     subroutine v2s_cmple_xmm2r8_xmm2r8(mask,x,y)
             type(Mask2_t),   intent(out) :: mask
             type(XMM2r8_t),  intent(in)  :: x
             type(XMM2r8_t),  intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) <= y.v(0)
             mask.m(1) = x.v(1) <= y.v(1)
    end subroutine v2s_cmple_xmm2r8_xmm2r8
    
    subroutine v3s_cmple_ymm3r8_ymm3r8(mask,x,y)
             type(Mask3_t),    intent(out) :: mask
             type(YMM3r8_t),   intent(in)  :: x
             type(YMM3r8_t),   intent(in)  :: y
             ! Exec code ...
             mask.m(0) = x.v(0) <= y.v(0)
             mask.m(1) = x.v(1) <= y.v(1)
             mask.m(2) = x.v(2) <= y.v(2)
    end subroutine v3s_cmple_ymm3r8_ymm3r8
    
    subroutine v4s_cmple_ymm4r8_ymm4r8(mask,x,y)
             type(Mask4_t),    intent(out) :: mask
             type(YMM4r8_t),   intent(in)  :: x
             type(YMM4r8_t),   intent(in)  :: y
             ! Exec code....
             mask.m(0) = x.v(0) <= y.v(0)
             mask.m(1) = x.v(1) <= y.v(1)
             mask.m(2) = x.v(2) <= y.v(2)
             mask.m(3) = x.v(3) <= y.v(3)
    end subroutine v4s_cmple_ymm4r8_ymm4r8
    
    subroutine v8s_cmple_zmm8r8_zmm8r8(mask,x,y)
            type(Mask8_t),     intent(out)  :: mask
            type(ZMM8r8_t),    intent(in)   :: x
            type(ZMM8r8_t),    intent(in)   :: y
            ! Exec code ....
            mask.m(0) = x.v(0) <= y.v(0)
            mask.m(1) = x.v(1) <= y.v(1)
            mask.m(2) = x.v(2) <= y.v(2)
            mask.m(3) = x.v(3) <= y.v(3)
            mask.m(4) = x.v(4) <= y.v(4)
            mask.m(5) = x.v(5) <= y.v(5)
            mask.m(6) = x.v(6) <= y.v(6)
            mask.m(7) = x.v(7) <= y.v(7)
    end subroutine v8s_cmple_zmm8r8_zmm8r8
    
    subroutine v2s_cmpeq_xmm2r8_r8(mask,x,y)
            type(Mask2_t),      intent(out) :: mask
            type(XMM2r8_t),     intent(in)  :: x
            real(kind=dp),      intent(in)  :: y
            ! Exec code ....
            mask.m(0) = x.v(0) == y
            mask.m(1) = v.x(1) == y
    end subroutine v2s_cmpeq_xmm2r8_r8
    
    subroutine v3s_cmpeq_ymm3r8_r8(mask,x,y)
            type(Mask3_t),       intent(out) :: mask
            type(YMM3r8_t),      intent(in)  :: x 
            real(kind=dp),       intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) == y
            mask.m(1) = x.v(1) == y
            mask.m(2) = x.v(2) == y
    end subroutine v2s_cmpeq_ymm3r8_r8
    
    subroutine v4s_cmpeq_ymm4r8_r8(mask,x,y)
            type(Mask4_t),        intent(out) :: mask
            type(YMM4r8_t),       intent(in)  :: x
            real(kind=dp),        intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) == y
            mask.m(1) = x.v(1) == y
            mask.m(2) = x.v(2) == y
            mask.m(3) = x.v(3) == y
    end subroutine v4s_cmpeq_ymm4r8_r8
    
    subroutine v8s_cmpeq_zmm8r8_r8(mask,x,y)
           type(Mask8_t),        intent(out) :: mask
           type(ZMM8r8_t),       intent(in)  :: x
           real(kind=dp),        intent(in)  :: y
           ! Exec code ...
           mask.m(0) = x.v(0) == y
           mask.m(1) = x.v(1) == y
           mask.m(2) = x.v(2) == y
           mask.m(3) = x.v(3) == y
           mask.m(4) = x.v(4) == y
           mask.m(5) = x.v(5) == y
           mask.m(6) = x.v(6) == y
           mask.m(7) = x.v(7) == y
    end subroutine v8s_cmpeq_zmm8r8_r8
    
     subroutine v2s_cmpneq_xmm2r8_r8(mask,x,y)
            type(Mask2_t),      intent(out) :: mask
            type(XMM2r8_t),     intent(in)  :: x
            real(kind=dp),      intent(in)  :: y
            ! Exec code ....
            mask.m(0) = x.v(0) /= y
            mask.m(1) = v.x(1) /= y
    end subroutine v2s_cmpneq_xmm2r8_r8
    
    subroutine v3s_cmpneq_ymm3r8_r8(mask,x,y)
            type(Mask3_t),       intent(out) :: mask
            type(YMM3r8_t),      intent(in)  :: x 
            real(kind=dp),       intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) /= y
            mask.m(1) = x.v(1) /= y
            mask.m(2) = x.v(2) /= y
    end subroutine v2s_cmpneq_ymm3r8_r8
    
    subroutine v4s_cmpneq_ymm4r8_r8(mask,x,y)
            type(Mask4_t),        intent(out) :: mask
            type(YMM4r8_t),       intent(in)  :: x
            real(kind=dp),        intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) /= y
            mask.m(1) = x.v(1) /= y
            mask.m(2) = x.v(2) /= y
            mask.m(3) = x.v(3) /= y
    end subroutine v4s_cmpneq_ymm4r8_r8
    
    subroutine v8s_cmpneq_zmm8r8_r8(mask,x,y)
           type(Mask8_t),        intent(out) :: mask
           type(ZMM8r8_t),       intent(in)  :: x
           real(kind=dp),        intent(in)  :: y
           ! Exec code ...
           mask.m(0) = x.v(0) /= y
           mask.m(1) = x.v(1) /= y
           mask.m(2) = x.v(2) /= y
           mask.m(3) = x.v(3) /= y
           mask.m(4) = x.v(4) /= y
           mask.m(5) = x.v(5) /= y
           mask.m(6) = x.v(6) /= y
           mask.m(7) = x.v(7) /= y
    end subroutine v8s_cmpneq_zmm8r8_r8
    
     subroutine v2s_cmpgt_xmm2r8_r8(mask,x,y)
            type(Mask2_t),      intent(out) :: mask
            type(XMM2r8_t),     intent(in)  :: x
            real(kind=dp),      intent(in)  :: y
            ! Exec code ....
            mask.m(0) = x.v(0) > y
            mask.m(1) = v.x(1) > y
    end subroutine v2s_cmpgt_xmm2r8_r8
    
    subroutine v3s_cmpgt_ymm3r8_r8(mask,x,y)
            type(Mask3_t),       intent(out) :: mask
            type(YMM3r8_t),      intent(in)  :: x 
            real(kind=dp),       intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) > y
            mask.m(1) = x.v(1) > y
            mask.m(2) = x.v(2) > y
    end subroutine v2s_cmpgt_ymm3r8_r8
    
    subroutine v4s_cmpgt_ymm4r8_r8(mask,x,y)
            type(Mask4_t),        intent(out) :: mask
            type(YMM4r8_t),       intent(in)  :: x
            real(kind=dp),        intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) > y
            mask.m(1) = x.v(1) > y
            mask.m(2) = x.v(2) > y
            mask.m(3) = x.v(3) > y
    end subroutine v4s_cmpgt_ymm4r8_r8
    
    subroutine v8s_cmpgt_zmm8r8_r8(mask,x,y)
           type(Mask8_t),        intent(out) :: mask
           type(ZMM8r8_t),       intent(in)  :: x
           real(kind=dp),        intent(in)  :: y
           ! Exec code ...
           mask.m(0) = x.v(0) > y
           mask.m(1) = x.v(1) > y
           mask.m(2) = x.v(2) > y
           mask.m(3) = x.v(3) > y
           mask.m(4) = x.v(4) > y
           mask.m(5) = x.v(5) > y
           mask.m(6) = x.v(6) > y
           mask.m(7) = x.v(7) > y
    end subroutine v8s_cmpgt_zmm8r8_r8
    
     subroutine v2s_cmpge_xmm2r8_r8(mask,x,y)
            type(Mask2_t),      intent(out) :: mask
            type(XMM2r8_t),     intent(in)  :: x
            real(kind=dp),      intent(in)  :: y
            ! Exec code ....
            mask.m(0) = x.v(0) >= y
            mask.m(1) = v.x(1) >= y
    end subroutine v2s_cmpge_xmm2r8_r8
    
    subroutine v3s_cmpge_ymm3r8_r8(mask,x,y)
            type(Mask3_t),       intent(out) :: mask
            type(YMM3r8_t),      intent(in)  :: x 
            real(kind=dp),       intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) >= y
            mask.m(1) = x.v(1) >= y
            mask.m(2) = x.v(2) >= y
    end subroutine v2s_cmpge_ymm3r8_r8
    
    subroutine v4s_cmpge_ymm4r8_r8(mask,x,y)
            type(Mask4_t),        intent(out) :: mask
            type(YMM4r8_t),       intent(in)  :: x
            real(kind=dp),        intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) >= y
            mask.m(1) = x.v(1) >= y
            mask.m(2) = x.v(2) >= y
            mask.m(3) = x.v(3) >= y
    end subroutine v4s_cmpge_ymm4r8_r8
    
    subroutine v8s_cmpge_zmm8r8_r8(mask,x,y)
           type(Mask8_t),        intent(out) :: mask
           type(ZMM8r8_t),       intent(in)  :: x
           real(kind=dp),        intent(in)  :: y
           ! Exec code ...
           mask.m(0) = x.v(0) >= y
           mask.m(1) = x.v(1) >= y
           mask.m(2) = x.v(2) >= y
           mask.m(3) = x.v(3) >= y
           mask.m(4) = x.v(4) >= y
           mask.m(5) = x.v(5) >= y
           mask.m(6) = x.v(6) >= y
           mask.m(7) = x.v(7) >= y
    end subroutine v8s_cmpge_zmm8r8_r8
    
    subroutine v2s_cmplt_xmm2r8_r8(mask,x,y)
            type(Mask2_t),      intent(out) :: mask
            type(XMM2r8_t),     intent(in)  :: x
            real(kind=dp),      intent(in)  :: y
            ! Exec code ....
            mask.m(0) = x.v(0) < y
            mask.m(1) = v.x(1) < y
    end subroutine v2s_cmplt_xmm2r8_r8
    
    subroutine v3s_cmplt_ymm3r8_r8(mask,x,y)
            type(Mask3_t),       intent(out) :: mask
            type(YMM3r8_t),      intent(in)  :: x 
            real(kind=dp),       intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) < y
            mask.m(1) = x.v(1) < y
            mask.m(2) = x.v(2) < y
    end subroutine v2s_cmplt_ymm3r8_r8
    
    subroutine v4s_cmplt_ymm4r8_r8(mask,x,y)
            type(Mask4_t),        intent(out) :: mask
            type(YMM4r8_t),       intent(in)  :: x
            real(kind=dp),        intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) < y
            mask.m(1) = x.v(1) < y
            mask.m(2) = x.v(2) < y
            mask.m(3) = x.v(3) < y
    end subroutine v4s_cmplt_ymm4r8_r8
    
    subroutine v8s_cmplt_zmm8r8_r8(mask,x,y)
           type(Mask8_t),        intent(out) :: mask
           type(ZMM8r8_t),       intent(in)  :: x
           real(kind=dp),        intent(in)  :: y
           ! Exec code ...
           mask.m(0) = x.v(0) < y
           mask.m(1) = x.v(1) < y
           mask.m(2) = x.v(2) < y
           mask.m(3) = x.v(3) < y
           mask.m(4) = x.v(4) < y
           mask.m(5) = x.v(5) < y
           mask.m(6) = x.v(6) < y
           mask.m(7) = x.v(7) < y
    end subroutine v8s_cmplt_zmm8r8_r8
    
     subroutine v2s_cmple_xmm2r8_r8(mask,x,y)
            type(Mask2_t),      intent(out) :: mask
            type(XMM2r8_t),     intent(in)  :: x
            real(kind=dp),      intent(in)  :: y
            ! Exec code ....
            mask.m(0) = x.v(0) <= y
            mask.m(1) = v.x(1) <= y
    end subroutine v2s_cmple_xmm2r8_r8
    
    subroutine v3s_cmple_ymm3r8_r8(mask,x,y)
            type(Mask3_t),       intent(out) :: mask
            type(YMM3r8_t),      intent(in)  :: x 
            real(kind=dp),       intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) <= y
            mask.m(1) = x.v(1) <= y
            mask.m(2) = x.v(2) <= y
    end subroutine v2s_cmple_ymm3r8_r8
    
    subroutine v4s_cmple_ymm4r8_r8(mask,x,y)
            type(Mask4_t),        intent(out) :: mask
            type(YMM4r8_t),       intent(in)  :: x
            real(kind=dp),        intent(in)  :: y
            ! Exec code ...
            mask.m(0) = x.v(0) <= y
            mask.m(1) = x.v(1) <= y
            mask.m(2) = x.v(2) <= y
            mask.m(3) = x.v(3) <= y
    end subroutine v4s_cmple_ymm4r8_r8
    
    subroutine v8s_cmple_zmm8r8_r8(mask,x,y)
           type(Mask8_t),        intent(out) :: mask
           type(ZMM8r8_t),       intent(in)  :: x
           real(kind=dp),        intent(in)  :: y
           ! Exec code ...
           mask.m(0) = x.v(0) <= y
           mask.m(1) = x.v(1) <= y
           mask.m(2) = x.v(2) <= y
           mask.m(3) = x.v(3) <= y
           mask.m(4) = x.v(4) <= y
           mask.m(5) = x.v(5) <= y
           mask.m(6) = x.v(6) <= y
           mask.m(7) = x.v(7) <= y
    end subroutine v8s_cmple_zmm8r8_r8
    
    pure function v4_simd_negate(x) result(neg_ymm)
         type(YMM4r8_t),  intent(in) :: x
         type(YMM4r8_t) :: neg_ymm
         ! Exec code ...
         neg_ymm.v = v4_n0.v - x.v
    end function 
    
    pure function v8_simd_negate(x) result(neg_zmm)
         type(ZMM8r8_t),  intent(in) :: x
         type(ZMM8r8_t)  :: neg_zmm
         ! Exec code ..
         neg_zmm.v = v8_n0 - x.v
    end function
    
    
    
    

end module mod_vechelpers