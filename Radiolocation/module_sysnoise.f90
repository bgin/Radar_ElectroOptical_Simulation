
module mod_sysnoise

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_sysnoise'
 !          
 !          Purpose:
 !                   Radar system noise temperature in units of Kelvin (K)
 !                   Sky noise temperature in units of Kelvin (K).
 !                     
 !          History:
 !                        Date: 18-10-2017
 !                        Time: 16:37 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                  Bernard Gingold
 !                 
 !          References:
 !         
 !                      Doppler Radar and Weather Observations
 !                      Richard L. Dvorak, Dusan S. Zrnic
 !                      
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    
 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    implicit none
    private
    use module_kinds
    public :: compute_sys_noise
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter :: MOD_SYSNOISE_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter :: MOD_SYSNOISE_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter :: MOD_SYSNOISE_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter :: MOD_SYSNOISE_FULLVER = 1000*MOD_SYSNOISE_MAJOR+100*MOD_SYSNOISE_MINOR+ &
                                                       10*MOD_SYSNOISE_MICRO
    ! Module/file creation date
    character(*),  parameter :: MOD_SYSNOISE_CREATE_DATE = "18-10-2017 16:37 +00200 (WED 18 OCT 2017 GMT+2)"
    
    ! Module build date (should be set after successful build date/time)
    character(*),  parameter :: MOD_SYSNOISE_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter :: MOD_SYSNOISE_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter :: MOD_SYSNOISE_DESCRIPT = " Radar system noise temperature."
    
    contains
    
    subroutine compute_sys_noise(Tsy,Ts,Lr,Ll,L1,Tr,Tl,T1,TempR,nx)
          implicit none
          real(R64P), dimension(nx), intent(out) :: Tsy
!DIR$     ASSUME_ALIGNED Tsy:32
          real(R64P), dimension(nx), intent(in)  :: Ts,Lr,Ll,L1,Tr, &
                                                    Tl,T1,TempR
!DIR$     ASSUME_ALIGNED Ts:32
!DIR$     ASSUME_ALIGNED Lr:32
!DIR$     ASSUME_ALIGNED Ll:32
!DIR$     ASSUME_ALIGNED L1:32
!DIR$     ASSUME_ALIGNED Tr:32
!DIR$     ASSUME_ALIGNED Tl:32
!DIR$     ASSUME_ALIGNED T1:32
!DIR$     ASSUME_ALIGNED TempR:32
          integer(I32P) :: nx
          ! Locals
          real(R64P) :: inv1,inv2,inv3,inv4, &
                        term1,term2,term3,term4
          integer(I32P) :: i
          ! Start of executable statements
          do i = 1, nx
              inv1  = (Lr(i)*Ll(i)*L1(i))**-1._R64P
              term1 = Ts(i)*inv1
              inv2  = (Ll(i)*L1(i))**-1._R64P
              term2 = TempR*inv2
              inv3  = 1._R64P-(Lr(i)**-1._R64P)
              term2 = term2*inv3
              inv4 = 1._R64P-(Ll(i)**-1._R64P)
              term3 = Tl(i)*(L1(i)**-1._R64P)*inv4
              term4 = T1(i)*(1._R64P-(L1(i)**-1.R64P)+TempR
              Tsy(i) = term1+term2+term3+term4
            end do
    end subroutine
    
    pure function compute_sky_noise(Ts,alpha,nur,Tg) result(snoise)
          implicit none
          real(R64P), intent(in) :: Ts,alpha,nur,Tg
          ! Locals
          real(R64P) :: term1,term2,term3,snoise
          ! Start of executable statements
          term1 = Ts*(1._R64P-alpha)
          term2 = alpha*(1._R64P-nur)*Tg
          term3 = Ts*alpha*nur
          snoise = ter1+term2+term3
    end function

end module mod_sysnoise 