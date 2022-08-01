

module hankel_transform




 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'hankel_transform'
 !          
 !          Purpose:
 !                      This module contains an implementation of the Hankel
 !                      Transform with the help of Quadpack QAGE integrators.
 !          History:
 !                        Date: 01-08-2022
 !                        Time: 08:34 GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 
 !
 !          Author:  
 !                      Bernard Gingold
 !          
 !                 
 !          References:
 !         
 !                          https://mathworld.wolfram.com/HankelTransform.html
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds, only : i4, dp
    implicit none
    public

    !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4),   parameter :: HANKEL_TRANSFORM_MAJOR = 1
     ! Minor version
     integer(kind=i4),   parameter :: HANKEL_TRANSFORM_MINOR = 0
     ! Micro version
     integer(kind=i4),   parameter :: HANKEL_TRANSFORM_MICRO = 1
     ! Full version
     integer(kind=i4),   parameter :: HANKEL_TRANSFORM_FULLVER =   &
            1000*HANKEL_TRANSFORM_MAJOR+100*HANKEL_TRANSFORM_MINOR+10*HANKEL_TRANSFORM_MICRO
     ! Module creation date
     character(*),        parameter :: HANKEL_TRANSFORM_DATE        = "01-08-2022 08:34 +00200 (MON 01 AUG 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: HANKEL_TRANSFORM_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: HANKEL_TRANSFORM_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: HANKEL_TRANSFORM_SYNOPSIS    = "HANKEL-Transform computed by QUADPACK QAGE integrators."

     type, public :: h_qage_params
           ! QAGE return info
        real(kind=dp),    dimension(:), allocatable :: re_abserr
        integer(kind=i4), dimension(:), allocatable :: re_neval
        integer(kind=i4), dimension(:), allocatable :: re_ier
        integer(kind=i4), dimension(:), allocatable :: re_last
        !dir$ attributes align : 64 :: re_abserr
        !dir$ attributes align : 64 :: re_neval
        !dir$ attributes align : 64 :: re_ier
        !dir$ attributes align : 64 :: re_last
        integer(kind=i4) :: npts ! Number of transform points.
        logical(kind=i4) :: isalloc
     end type h_qage_params


     abstract interface

        real(kind=dp) function func(r)
             import :: dp
             implicit none
             real(kind=dp),   intent(in) :: r
  
        end function func

     
     end interface

     real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp

     contains

     
     subroutine init_h_qage_params(params,npts)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: init_h_qage_params
         type(h_qage_params),   intent(in) :: params
         integer(kind=i4),      intent(in) :: npts
         ! Exec code
         if(params.isalloc .or. &
              npts <= 2)   return
         allocate(params.re_abserr(npts))
         allocate(params.re_neval(npts))
         allocate(params.re_ier(npts))
         allocate(params.re_last(npts))
         params.isalloc = .true.
     end subroutine init_h_qage_params

     subroutine free_h_qage_params(params)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: free_qage_params
         type(qage_params),   intent(in) :: params
         ! Exec code
         if(.not.params.isalloc) return
         deallocate(params.re_abserr)
         deallocate(params.re_neval)
         deallocate(params.re_ier)         
         deallocate(params.re_last)
     end subroutine free_h_qage_params


#if 0

   Integrands arguments are as follows
   x = rcos(theta) 
   y = rsin(theta)
   r = sqrt(x^2+y^2)
   u = qcos(phi)
   v = qsin(phi)
   q = sqrt(u^2+v^2)
   Integral = 2pi Integral |0-inf f(r)J0(2piqr)rdr
   
   pure function func(r) result(y)
        use hankel_fargs
        real(kind=dp), intent(in) :: r
        real(kind=dp) :: y
        real(kind=dp) :: t
        t = !... result of f(r)
        y = t*bessel_jn(0,twopi*r*q)
   end function func
#endif


       subroutine hankelt_dqagi_omp(f,bound,inf,epsabs,  &
                                     epsrel,params,lenw,limit,    &
                                     work,iwork,output)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: hankel_dqagi_omp
              use quadpack, only : dqagi
              use omp_lib
              implicit none
              procedure(func)                                   :: f
              real(kind=dp),                      intent(in)    :: bound
              integer(kind=i4),                   intent(in)    :: inf
              real(kind=dp),                      intent(in)    :: epsabs
              real(kind=dp),                      intent(in)    :: epsrel
              type(h_qage_params),                intent(inout) :: params
              integer(kind=i4),                   intent(in)    :: lenw
              integer(kind=i4),                   intent(in)    :: limit
              real(kind=dp),    dimension(lenw),  intent(inout) :: work
              integer(kind=i4), dimension(limit), intent(inout) :: iwork
           
              real(kind=dp), dimension(:),     intent(out)   :: output
              ! Locals
            
              real(kind=dp),    automatic :: result
              integer(kind=i4), automatic :: i
              ! Exec code ....
              result = 0.0_dp
             
           
!$omp parallel do schedule(runtime) default(none) if(params.npts >= 8) &
!$omp private(i,iwork,work,result)                               &
!$omp shared(npts,f,bound,inf,epsabs,epsrel,re_abserr)  &
!$omp shared(re_neval,re_ier,limit,lenw,re_last,output,twopi)                     
                     do i=1, params.npts
                        call dqagi(f,bound,inf,epsabs,epsrel,result,         &
                                   params.re_abserr(i),params.re_neval(i),  &
                                   params.re_ier(i),limit,lenw,             &
                                   params.re_last(i),iwork,work)
                         output(i) = twopi*result
                     end do
!$omp end parallel do
                 
          end subroutine hankelt_dqagi_omp


          subroutine hankelt_dqage_omp(f,a,b,epsabs,epsrel,key, &
                                        limit,params,alist,blist,rlist,  &
                                        elist,iord,output)
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: hankelt_dqage_omp
                   use omp_lib
                   use quadpack, only : dqage
                   implicit none
                   procedure(func)                                    :: f

                   real(kind=dp),                      intent(in)     :: a
                   real(kind=dp),                      intent(in)     :: b
                   real(kind=dp),                      intent(in)     :: epsabs
                   real(kind=dp),                      intent(in)     :: epsrel
                   integer(kind=i4),                   intent(in)     :: key
                   integer(kind=i4),                   intent(in)     :: limit
                   type(h_qage_params),                intent(inout)  :: params
                   real(kind=dp),    dimension(limit), intent(out)    :: alist
                   real(kind=dp),    dimension(limit), intent(out)    :: blist
                   real(kind=dp),    dimension(limit), intent(out)    :: elist
                   real(kind=dp),    dimension(limit), intent(out)    :: rlist
                   integer(kind=i4), dimension(limit), intent(out)    :: iord
            
                   real(kind=dp), dimension(:),     intent(out)    :: output
                   ! Locals
                   
                   real(kind=dp),    automatic :: result
                  
                   integer(kind=i4), automatic :: i
                   ! Exec code ....
                   result = 0.0_dp
                  
                 
!$omp parallel do default(none) schedule(runtime) if(params.npts>=8)    &
!$omp private(i,result,alist,blist,rlist,elist,iord)                     &
!$omp shared(npts,f,a,b,epsabs,epsrel,key,limit)                &
!$omp shared(re_abserr,re_neval,re_ier,re_last,output,twopi)            
                          
                            do i=1, params.npts
                                call dqage(f,a,b,epsabs,epsrel,key,limit,           &
                                           result,params.re_abserr(i),params.re_neval(i),  &
                                           params.re_ier(i),alist,blist,rlist,elist,   &
                                           iord,re_last(i))
                    
                                output(i) = twopi*result
                             end do
!$omp end parallel do
                      
                        
          end subroutine hankelt_dqage_omp
          

          
          subroutine hankelt_dqagp_omp(f,a,b,npts2,points,epsabs,epsrel, &
                                        params,leniw,lenw,iwork,work,output)
                                      
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: hankelt_dqagp_omp
                   use omp_lib
                   use quadpack, only : dqagp
                   implicit none
                   procedure(func)                                 :: f
                 
                   real(kind=dp),                      intent(in)     :: a
                   real(kind=dp),                      intent(in)     :: b
                   integer(kind=i4),                   intent(in)     :: npts2
                   real(kind=dp),   dimension(npts2),  intent(in)     :: points
                   real(kind=dp),                      intent(in)     :: epsabs
                   real(kind=dp),                      intent(in)     :: epsrel
                   type(h_qage_params),                intent(inout)  :: params
                   integer(kind=i4),                   intent(in)     :: leniw
                   integer(kind=i4),                   intent(in)     :: lenw
                   integer(kind=i4), dimension(leniw), intent(inout)  :: iwork
                   real(kind=dp),    dimension(lenw),  intent(inout)  :: work
                  
                   real(kind=dp), dimension(:),     intent(out)    :: output
                   ! Locals
                   
                   real(kind=dp),    automatic :: result
                   
                   integer(kind=i4), automatic :: i
                   ! Exec code ....
                   result = 0.0_dp
                   
                 
!$omp parallel do default(none) schedule(runtime) if(params.npts>=8)    &
!$omp private(i,result,iwork,work)                                       &
!$omp shared(npts2,npts,f,a,b,epsabs,epsrel,lenw,leniw,points)       &
!$omp shared(re_abserr,re_neval,re_ier,re_last,output,twopi)          
                      
                            do i=1, params.npts
                                call dqagp(f,a,b,npts2,points,epsabs,epsrel,        &
                                           result,params.re_abserr(i),params.re_neval(i),  &
                                           params.re_ier(i),leniw,lenw,re_last(i),iwork   &
                                           work)
                                
                                output(i) = twopi*result
                             end do
!$omp end parallel do
                      
                        
          end subroutine hankelt_dqagp_omp

                
            !==================================!
            !     Single-threaded versions     !
            !==================================!

          subroutine hankelt_dqagi(f,bound,inf,epsabs,  &
                                    epsrel,params,lenw,limit,    &
                                    work,iwork,output)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: hankelt_dqagi
              use quadpack, only : dqagi
             
              implicit none
              procedure(func)                                :: f
              
              real(kind=dp),                      intent(in)    :: bound
              integer(kind=i4),                   intent(in)    :: inf
              real(kind=dp),                      intent(in)    :: epsabs
              real(kind=dp),                      intent(in)    :: epsrel
              type(h_qage_params),                intent(inout) :: params
              integer(kind=i4),                   intent(in)    :: lenw
              integer(kind=i4),                   intent(in)    :: limit
              real(kind=dp),    dimension(lenw),  intent(inout) :: work
              integer(kind=i4), dimension(limit), intent(inout) :: iwork
             
              real(kind=dp), dimension(:),     intent(out)   :: output
              ! Locals
             
              real(kind=dp),    automatic :: result
            
              integer(kind=i4), automatic :: i
              ! Exec code ....
              result = 0.0_dp
            
         
                    
                     do i=1, params.npts
                        call dqagi(f,bound,inf,epsabs,epsrel,result,         &
                                   params.re_abserr(i),params.re_neval(i),  &
                                   params.re_ier(i),limit,lenw,             &
                                   params.re_last(i),iwork,work)
                       
                        output(i) = twopi*result
                     end do

               
          end subroutine hankelt_dqagi


          subroutine hankelt_dqage(f,a,b,epsabs,epsrel,key, &
                                        limit,params,alist,blist,rlist,  &
                                        elist,iord,output)
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: hankelt_dqage
              
                   use quadpack, only : dqage
                   implicit none
                   procedure(func)                                 :: f
                 
                   real(kind=dp),                      intent(in)     :: a
                   real(kind=dp),                      intent(in)     :: b
                   real(kind=dp),                      intent(in)     :: epsabs
                   real(kind=dp),                      intent(in)     :: epsrel
                   integer(kind=i4),                   intent(in)     :: key
                   integer(kind=i4),                   intent(in)     :: limit
                   type(h_qage_params),                intent(inout)  :: params
                   real(kind=dp),    dimension(limit), intent(out)    :: alist
                   real(kind=dp),    dimension(limit), intent(out)    :: blist
                   real(kind=dp),    dimension(limit), intent(out)    :: elist
                   real(kind=dp),    dimension(limit), intent(out)    :: rlist
                   integer(kind=i4), dimension(limit), intent(out)    :: iord
                  
                   real(kind=dp), dimension(:),     intent(out)    :: output
                   ! Locals
                   
                   real(kind=dp),    automatic :: result
                  
                   integer(kind=i4), automatic :: i
                   ! Exec code ....
                   result = 0.0_dp
                  
               
                        
                            do i=1, params.npts
                                call dqage(f,a,b,epsabs,epsrel,key,limit,           &
                                           result,params.re_abserr(i),params.re_neval(i),  &
                                           params.re_ier(i),alist,blist,rlist,elist,   &
                                           iord,re_last(i))
                               
                                output(i) = twopi*result
                             end do

                     
                        
          end subroutine hankelt_dqage
          

          
          subroutine hankelt_dqagp(f,a,b,npts2,points,epsabs,epsrel, &
                                    params,leniw,lenw,iwork,work,output)
                                      
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: hankelt_dqagp
                  
                   use quadpack, only : dqagp
                   implicit none
                   procedure(func)                                 :: f
                  
                   real(kind=dp),                      intent(in)     :: a
                   real(kind=dp),                      intent(in)     :: b
                   integer(kind=i4),                   intent(in)     :: npts2
                   real(kind=dp),   dimension(npts2),  intent(in)     :: points
                   real(kind=dp),                      intent(in)     :: epsabs
                   real(kind=dp),                      intent(in)     :: epsrel
                   type(h_qage_params),                intent(inout)  :: params
                   integer(kind=i4),                   intent(in)     :: leniw
                   integer(kind=i4),                   intent(in)     :: lenw
                   integer(kind=i4), dimension(leniw), intent(inout)  :: iwork
                   real(kind=dp),    dimension(lenw),  intent(inout)  :: work
                 
                   real(kind=dp), dimension(:),     intent(out)    :: output
                   ! Locals
                 
                   real(kind=dp),    automatic :: result
               
                   integer(kind=i4), automatic :: i
                   ! Exec code ....
                   result = 0.0_dp
                 
                  
                         
                            do i=1, params.npts
                                call dqagp(f,a,b,npts2,points,epsabs,epsrel,        &
                                           result,params.re_abserr(i),params.re_neval(i),  &
                                           params.re_ier(i),leniw,lenw,re_last(i),iwork   &
                                           work)
                               
                                output(i) = twopi*result
                             end do

                     
                        
            end subroutine hankelt_dqagp


end module hankel_transform
