
module mod_kinds 

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_kinds'
 !          
 !          Purpose:
 !                      Primitive integral and floating-point data types.
 !                      
 !                       
 !          History:
 !                        Date: 08-10-2018
 !                        Time: 11:18 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !          
 !                 
 !          References:
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
     
    use ISO_FORTRAN_ENV, only : INT8,INT16,INT32,INT64
    use intrinsic :: IEEE_ARITHMETIC
    implicit none
    
    ! Integral intrinsic primitives
    
    integer(4), parameter, public :: int1 = INT8
    
    integer(4), parameter, public :: int2 = INT16
    
    integer(4), parameter, public :: int4 = INT32
    
    integer(4), parameter, public :: int8 = INT64
    
    ! Floating-point intrinsic primitives
    
    integer(4), parameter, public :: sp32 = selected_real_kind(6,37)
    
    integer(4), parameter, public :: dp64 = selected_real_kind(15,307)
    
    integer(4), parameter, public :: ep128 = selected_real_kind(33,4931)
    
    


end module mod_kinds