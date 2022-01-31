

module quadrature_rules



 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'quadrature_rules'
 !          
 !          Purpose:
 !                      This module contains a 5 Quadrature rules.
 !                      1) Clenshaw-Curtis
 !                      2) Fejer
 !                      3) Gauss
 !                      4) Newton-Cotes
 !                      5) Romberg
 !                      
 !          History:
 !                        Date: 31-01-2022
 !                        Time: 08:57 GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Modified and adapted by Bernard Gingold
 !                      Original authors: https://github.com/giacrossi/FORbID
 !          
 !                 
 !          References:
 !         
 !                        Adapted from FORbID project by removal of Object-Oriented design
 !                        and slightly simplifying the overall design.
 !                        Added Intel Fortran Compiler directives and OpenMP pragmas.
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds, only : i4, dp
     use omp_lib
     implicit none

     real(kind=dp), parameter, private :: PI = 3.1415926535897932384626_dp

     
     abstract interface

        ! User supplied 1D integrand
        real(kind=dp) function func(x,y,z)
             import :: dp
             real(kind=dp),  intent(in)           :: x
             real(kind=dp),  intent(in), optional :: y
             real(kind=dp),  intent(in), optional :: z
        end function func

        ! User supplied 2D integrand
        !real(kind=dp) function f_2D(x,y)
        !     import :: dp
        !     real(kind=dp),  intent(in) :: x
        !     real(kind=dp),  intent(in) :: y
        !end function f_2D

        ! User supplied 3D integrand   
        !real(kind=dp) function f_3D(x,y,z)
        !     import :: dp
        !     real(kind=dp),  intent(in) :: x
        !     real(kind=dp),  intent(in) :: y
        !     real(kind=dp),  intent(in) :: z
        !end function f_3D
        
     end interface


     type, public :: Clenshaw_Curtis
        sequence
        real(kind=dp), allocatable, dimension(:) :: w ! weights array
        !dir$ attributes align : 64 :: w
        real(kind=dp), allocatable, dimension(:) :: x ! nodes x array
        !dir$ attributes align : 64 :: x
        real(kind=dp), allocatable, dimension(:) :: y ! nodes y array
        !dir$ attributes align : 64 :: y
        real(kind=dp), allocatable, dimension(:) :: z ! nodes z array
        !dir$ attributes align : 64 :: z
        integer(kind=i4)                         :: n ! number of quadrature points
     end type Clenshaw_Curtis


     type, public :: Fejer
        sequence
        real(kind=dp), allocatable, dimension(:) :: w ! weights array
        !dir$ attributes align : 64 :: w
        real(kind=dp), allocatable, dimension(:) :: x ! nodes x array
        !dir$ attributes align : 64 :: x
        real(kind=dp), allocatable, dimension(:) :: y ! nodes y array
        !dir$ attributes align : 64 :: y
        real(kind=dp), allocatable, dimension(:) :: z ! nodes z array
        !dir$ attributes align : 64 :: z
        integer(kind=i4)                         :: n ! number of quadrature points
     end type Fejer


     type, public :: Gauss
        sequence
        real(kind=dp), allocatable, dimension(:) :: w ! weights array
        !dir$ attributes align : 64 :: w
        real(kind=dp), allocatable, dimension(:) :: x ! nodes x array
        !dir$ attributes align : 64 :: x
        real(kind=dp), allocatable, dimension(:) :: y ! nodes y array
        !dir$ attributes align : 64 :: y
        real(kind=dp), allocatable, dimension(:) :: z ! nodes z array
        !dir$ attributes align : 64 :: z
        character(3)                             :: idx ! Quadrature index: KRO for Kronrod, LEG for legendre, CHE for chebyshev
        integer(kind=i4)                         :: n ! number of quadrature points
      end type Gauss


     type, public :: Newton_Cotes
        sequence
        real(kind=dp), allocatable, dimension(:) :: w ! weights array
        !dir$ attributes align : 64 :: w
        real(kind=dp)                            :: k ! coefficient of integration
        integer(kind=i4)                         :: n ! number of quadrature points
     end type Newton_Cotes


     type, public :: Romberg
        sequence
        real(kind=dp)                            :: tol ! tolerance
        integer(kind=i4)                         :: n ! number of quadrature points
     end type Romberg


     contains

     ! Initialize Clenshaw-Curtis derived type
     elemental subroutine init_clenshaw_curtis(this,n,d)
           !dir$ attributes optimize : 3
           !dir$ attributes code_align : 32 :: init_clenshaw_curtis
           type(Clenshaw_Curtis),      intent(inout) :: this
           integer(kind=i4),           intent(in)    :: n
           integer(kind=i4),           intent(in)    :: d
           ! Locals
           real(kind=dp),    automatic :: sum   ! hot temporary for reduction
           real(kind=dp),    automatic :: t0,t1,carg,t2,t3
           integer(kind=i4), automatic :: i,j,n2
           ! Exec code ...
           this.n=n
           t0=0.0_dp
           t1=0.0_dp
           carg=0.0_dp
           t2=0.0_dp
           t3=0.0_dp
           if (allocated(this.w)) then
               deallocate(this.w)
               allocate(this.w(1:n))
           end if
           if (allocated(this.x)) then
               deallocate(this.x)
               allocate(this.x(1:n))
           end if
           n2=(n-1)/2
           if(mod(n,2)==0) then
              !dir$ assume_aligned this.x(1):64
              !dir$ assume_aligned this.w(1):64
              do i=2,n-1
                 carg=(i-1.0_dp)/(n-1.0_dp)*PI
                 this.x(i)=cos(carg)
                 sum=0.0_dp
                 !$omp simd reduction(+:sum) linear(j:1) 
                 do j=1,n2
                    t0=2.0_dp/(4.0_dp*j*j-1.0_dp)
                    sum=sum+t0*cos(2.0_dp*j*carg*PI)
                 end do
                 this.w(i)=1.0_dp-sum
              end do
              t1=1.0_dp/(n-1.0_dp)**2.0_dp
              this.w(1)=t1
              this.w(n)=t1
           else
              t2=(n-1.0_dp)*0.5_dp
              t3=1.0_dp/(4.0_dp*t2**2-1.0_dp)
              !dir$ assume_aligned this.x(1):64
              !dir$ assume_aligned this.w(1):64
              do i=2,n-1
                 carg=(i-1.0_dp)/(n-1.0_dp)*PI
                 this.x(i)=cos(carg)
                 sum=0.0_dp
                 !$omp simd reduction(+:sum) linear(j:1) 
                 do j=1,((n-1)-1)/2
                    t0=2.0_dp/(4.0_dp*j*j-1.0_dp)
                    sum=sum+t0*cos(2.0_dp*j*carg*PI)
                 end do
                 sum=sum+t3*cos(2.0_dp*(t2*carg*PI))
                 this.w(i)=1.0_dp-sum
              end do
              this.w(1)=1.0_dp/(n*(n-2.0_dp))
              this.w(n)=1.0_dp/(n*(n-2.0_dp))
           end if
           this.x(1)=1.0_dp
           this.x(n)= -1.0_dp
           select case(d)
           case(2)
              if (allocated(this.y)) then
                  deallocate(this.y)
                  allocate(this.y(1:n))
                  this.y=this.x
              end if
           case(3)
              if (allocated(this.y)) then
                  deallocate(this.y)
                  allocate(this.y(1:n))
                  this.y=this.x
              end if
              if (allocated(this.z)) then
                  deallocate(this.z)
                  allocate(this.z(1:n))
                  this.z=this.x
              end if
            end select
     end subroutine init_clenshaw_curtis
     
     ! Clenshaw-Curtis quadrature 1D
     function clenshaw_curtis_Q1D(this,f,a,b) result(quad)
       !dir$ attributes forceinline :: clenshaw_curtis_Q1D
       !dir$ attributes optimize : 3
       !dir$ attributes code_align : 32 :: clenshaw_curtis_Q1D
        type(Clenshaw_Curtis),     intent(in) :: this
        procedure(func)                       :: f
        real(kind=dp),             intent(in) :: a
        real(kind=dp),             intent(in) :: b
        real(kind=dp) :: quad
        !Locals
        real(kind=dp),    automatic :: t0,t1,wi,xi
        integer(kind=i4), automatic :: i
        ! Exec code ...
        t0=0.0_dp
        t1=0.0_dp
        wi=0.0_dp
        xi=0.0_dp
        t0=(b-a)*0.5_dp
        t1=(a+b)*0.5_dp
        quad=0.0_dp
        !dir$ assume_aligned this.x(1):64
        !dir$ assume_aligned this.w(1):64
        !$omp simd reduction(+:quad) linear(i:1)
        do i=1,this.n
           xi=this.x(i)
           wi=this.w(i)
           quad=quad+wi*f(xi*t0+t1)
        end do
        quad=quad*t0
     end function clenshaw_curtis_Q1D

     ! Clenshaw-Curtis quadrature 2D
     function clenshaw_curtis_Q2D(this,f,a,b,c,d) result(quad)
       !dir$ attributes forceinline :: clenshaw_curtis_Q2D
       !dir$ attributes optimize : 3
       !dir$ attributes code_align : 32 :: clenshaw_curtis_Q2D
        type(Clenshaw_Curtis),     intent(in) :: this
        procedure(func)                       :: f
        real(kind=dp),             intent(in) :: a
        real(kind=dp),             intent(in) :: b
        real(kind=dp),             intent(in) :: c
        real(kind=dp),             intent(in) :: d
        real(kind=dp)  :: quad
        real(kind=dp),    automatic :: wi,wj,xi,yj,t0,t1,t2,t3,fyj
        integer(kind=i4), automatic :: i,j
        ! Exec code ....
        wi=0.0_dp;wj=0.0_dp;
        xi=0.0_dp;yj=0.0_dp
        quad=0.0_dp
        t0=(b-a)*0.5_dp
        t1=(a+b)*0.5_dp
        t2=(d-c)*0.5_dp
        t3=(d+c)*0.5_dp
        !dir$ assume_aligned this.x(1):64
        !dir$ assume_aligned this.w(1):64
        !dir$ assume_aligned this.y(1):64
        do j=1,this.n
           wj=this.w(j)
           yj=this.y(j)
           fyj=f(yj*t2+t3)
           !$omp simd reduction(+:quad) linear(i:1)
           do i=1,this.n
              wi=this.w(i)
              xi=this.x(i)
              quad=quad+wi*wj*f(xi*t0+t1)*fyj
           end do
        end do
        quad=quad*t0*t2                   
     end function clenshaw_curtis_Q2D    
    
     
      ! Clenshaw-Curtis quadrature 3D
     function clenshaw_curtis_Q3D(this,f,a,b,c,d,g,h) result(quad)
       !dir$ attributes inline :: clenshaw_curtis_Q3D
       !dir$ attributes optimize : 3
       !dir$ attributes code_align : 32 :: clenshaw_curtis_Q3D
        type(Clenshaw_Curtis),     intent(in) :: this
        procedure(func)                       :: f
        real(kind=dp),             intent(in) :: a
        real(kind=dp),             intent(in) :: b
        real(kind=dp),             intent(in) :: c
        real(kind=dp),             intent(in) :: d
        real(kind=dp),             intent(in) :: g
        real(kind=dp),             intent(in) :: h
        real(kind=dp)  :: quad
        ! Locals
        real(kind=dp),  automatic :: wi,wj,xi,yj,zk, &
                                     t0,t1,t2,t3,t4, &
                                     t5,fyj,fzk
        integer(kind=i4), automatic :: k,j,i
        ! Exec code ....
        t0=(b-a)*0.5_dp;t1=(a+b)*0.5_dp
        t2=(d-c)*0.5_dp;t3=(d+c)*0.5_dp
        t4=(h-g)*0.5_dp;t5=(g+h)*0.5_dp
        w=0.0_dp;wj=0.0_dp;xi=0.0_dp
        yj=0.0_dp;zk=0.0_dp
        quad=0.0_dp
        !dir$ assume_aligned this.x(1):64
        !dir$ assume_aligned this.w(1):64
        !dir$ assume_aligned this.y(1):64
        !dir$ assume_aligned this.z(1):64
        do k=1,this.n
           zk=this.z(k)
           fzk=f(zk*t4+t5)
           do j=1,this.n
              wj=this.w(j)
              yj=this.y(j)
              fyj=f(yj*t2+t3)
              !$omp simd reduction(+:quad) linear(i:1)
              do i=1,this.n
                 wi=this.w(i)
                 xi=this.x(i)
                 quad=quad+wi*wj*wj*f(xi*t0+t1)*fyj*fzk
              end do
           end do
        end do  
        quad=quad*t0*t2*t4;
     end function clenshaw_curtis_Q3D                   
          
      
     ! Gauss Quadrature initialization routine
     elemental subroutine init_gauss(this,n,q,d)
           !dir$ attributes optimize : 3
           !dir$ attributes code_align : 32 :: init_gauss
           type(Gauss),          intent(inout) :: this
           integer(kind=i4),     intent(in)    :: n
           character(*),         intent(in)    :: q
           integer(kind=i4),     intent(in)    :: d
           ! Locals
           real(kind=dp),    automatic :: pin
           integer(kind=i4), automatic :: i
           !Exec code ...
           this.n=n
           this.q=q
           select case(this%q)
           case('LEG')
              if (allocated(this%w)) deallocate(this%w); allocate(this%w(1:n))
              if (allocated(this%x)) deallocate(this%x); allocate(this%x(1:n))
              select case(n)
              case(1)
                 this%x(1) =  0._dp;                   this%w(1) = 2._dp
              case(2)
                 this%x(1) = -0.5773502691896257_dp;   this%w(1) = 1._dp
                 this%x(2) =  0.5773502691896257_dp;   this%w(2) = 1._dp
              case(3)
                 this%x(1) =  0._dp;                   this%w(1) = 0.8888888888888889_dp
                 this%x(2) = -0.7745966692414834_dp;   this%w(2) = 0.5555555555555556_dp
                 this%x(3) =  0.7745966692414834_dp;   this%w(3) = 0.5555555555555556_dp
              case(4)
                 this%x(1) = -0.3399810435848563_dp;   this%w(1) = 0.6521451548625461_dp
                 this%x(2) =  0.3399810435848563_dp;   this%w(2) = 0.6521451548625461_dp
                 this%x(3) = -0.8611363115940526_dp;   this%w(3) = 0.3478548451374538_dp
                 this%x(4) =  0.8611363115940526_dp;   this%w(4) = 0.3478548451374538_dp
              case(5)
                 this%x(1) =  0._dp;                   this%w(1) = 0.5688888888888889_dp
                 this%x(2) = -0.5384693101056831_dp;   this%w(2) = 0.4786286704993665_dp
                 this%x(3) =  0.5384693101056831_dp;   this%w(3) = 0.4786286704993665_dp
                 this%x(4) = -0.9061798459386640_dp;   this%w(4) = 0.2369268850561891_dp
                 this%x(5) =  0.9061798459386640_dp;   this%w(5) = 0.2369268850561891_dp
              case(6)
                 this%x(1) = -0.2386191860831969_dp;   this%w(1) = 0.4679139345726910_dp
                 this%x(2) =  0.2386191860831969_dp;   this%w(2) = 0.4679139345726910_dp
                 this%x(3) = -0.6612093864662645_dp;   this%w(3) = 0.3607615730481386_dp
                 this%x(4) =  0.6612093864662645_dp;   this%w(4) = 0.3607615730481386_dp
                 this%x(5) = -0.9324695142031521_dp;   this%w(5) = 0.1713244923791704_dp
                 this%x(6) =  0.9324695142031521_dp;   this%w(6) = 0.1713244923791704_dp
              case(7)
                 this%x(1) =  0._dp;                   this%w(1) = 0.4179591836734694_dp
                 this%x(2) = -0.4058451513773972_dp;   this%w(2) = 0.3818300505051189_dp
                 this%x(3) =  0.4058451513773972_dp;   this%w(3) = 0.3818300505051189_dp
                 this%x(4) = -0.7415311855993945_dp;   this%w(4) = 0.2797053914892766_dp
                 this%x(5) =  0.7415311855993945_dp;   this%w(5) = 0.2797053914892766_dp
                 this%x(6) = -0.9491079123427585_dp;   this%w(6) = 0.1294849661688697_dp
                 this%x(7) =  0.9491079123427585_dp;   this%w(7) = 0.1294849661688697_dp
              case(8)
                 this%x(1) = -0.1834346424956498_dp;   this%w(1) = 0.3626837833783620_dp
                 this%x(2) =  0.1834346424956498_dp;   this%w(2) = 0.3626837833783620_dp
                 this%x(3) = -0.5255324099163290_dp;   this%w(3) = 0.3137066458778873_dp
                 this%x(4) =  0.5255324099163290_dp;   this%w(4) = 0.3137066458778873_dp
                 this%x(5) = -0.7966664774136267_dp;   this%w(5) = 0.2223810344533745_dp
                 this%x(6) =  0.7966664774136267_dp;   this%w(6) = 0.2223810344533745_dp
                 this%x(7) = -0.9602898564975363_dp;   this%w(7) = 0.1012285362903763_dp
                 this%x(8) =  0.9602898564975363_dp;   this%w(8) = 0.1012285362903763_dp
              case(9)
                 this%x(1) =  0._dp;                   this%w(1) = 0.3302393550012598_dp
                 this%x(2) = -0.3242534234038089_dp;   this%w(2) = 0.3123470770400029_dp
                 this%x(3) =  0.3242534234038089_dp;   this%w(3) = 0.3123470770400029_dp
                 this%x(4) = -0.6133714327005904_dp;   this%w(4) = 0.2606106964029354_dp
                 this%x(5) =  0.6133714327005904_dp;   this%w(5) = 0.2606106964029354_dp
                 this%x(6) = -0.8360311073266358_dp;   this%w(6) = 0.1806481606948574_dp
                 this%x(7) =  0.8360311073266358_dp;   this%w(7) = 0.1806481606948574_dp
                 this%x(8) = -0.9681602395076261_dp;   this%w(8) = 0.0812743883615744_dp
                 this%x(9) =  0.9681602395076261_dp;   this%w(9) = 0.0812743883615744_dp
              case(10)
                 this%x(1 ) = -0.1488743389816312_dp;  this%w(1 ) = 0.2955242247147529_dp
                 this%x(2 ) =  0.1488743389816312_dp;  this%w(2 ) = 0.2955242247147529_dp
                 this%x(3 ) = -0.4333953941292472_dp;  this%w(3 ) = 0.2692667193099963_dp
                 this%x(4 ) =  0.4333953941292472_dp;  this%w(4 ) = 0.2692667193099963_dp
                 this%x(5 ) = -0.6794095682990244_dp;  this%w(5 ) = 0.2190863625159820_dp
                 this%x(6 ) =  0.6794095682990244_dp;  this%w(6 ) = 0.2190863625159820_dp
                 this%x(7 ) = -0.8650633666889845_dp;  this%w(7 ) = 0.1494513491505806_dp
                 this%x(8 ) =  0.8650633666889845_dp;  this%w(8 ) = 0.1494513491505806_dp
                 this%x(9 ) = -0.9739065285171717_dp;  this%w(9 ) = 0.0666713443086881_dp
                 this%x(10) =  0.9739065285171717_dp;  this%w(10) = 0.0666713443086881_dp
              case(11)
                 this%x(1 ) =  0._dp;                  this%w(1 ) = 0.2729250867779006_dp
                 this%x(2 ) = -0.2695431559523450_dp;  this%w(2 ) = 0.2628045445102467_dp
                 this%x(3 ) =  0.2695431559523450_dp;  this%w(3 ) = 0.2628045445102467_dp
                 this%x(4 ) = -0.5190961292068118_dp;  this%w(4 ) = 0.2331937645919905_dp
                 this%x(5 ) =  0.5190961292068118_dp;  this%w(5 ) = 0.2331937645919905_dp
                 this%x(6 ) = -0.7301520055740494_dp;  this%w(6 ) = 0.1862902109277343_dp
                 this%x(7 ) =  0.7301520055740494_dp;  this%w(7 ) = 0.1862902109277343_dp
                 this%x(8 ) = -0.8870625997680953_dp;  this%w(8 ) = 0.1255803694649046_dp
                 this%x(9 ) =  0.8870625997680953_dp;  this%w(9 ) = 0.1255803694649046_dp
                 this%x(10) = -0.9782286581460570_dp;  this%w(10) = 0.0556685671161737_dp
                 this%x(11) =  0.9782286581460570_dp;  this%w(11) = 0.0556685671161737_dp
               case(12)
                 this%x(1 ) = -0.1252334085114689_dp;  this%w(1 ) = 0.2491470458134028_dp
                 this%x(2 ) =  0.1252334085114689_dp;  this%w(2 ) = 0.2491470458134028_dp
                 this%x(3 ) = -0.3678314989981802_dp;  this%w(3 ) = 0.2334925365383548_dp
                 this%x(4 ) =  0.3678314989981802_dp;  this%w(4 ) = 0.2334925365383548_dp
                 this%x(5 ) = -0.5873179542866175_dp;  this%w(5 ) = 0.2031674267230659_dp
                 this%x(6 ) =  0.5873179542866175_dp;  this%w(6 ) = 0.2031674267230659_dp
                 this%x(7 ) = -0.7699026741943047_dp;  this%w(7 ) = 0.1600783285433462_dp
                 this%x(8 ) =  0.7699026741943047_dp;  this%w(8 ) = 0.1600783285433462_dp
                 this%x(9 ) = -0.9041172563704749_dp;  this%w(9 ) = 0.1069393259953184_dp
                 this%x(10) =  0.9041172563704749_dp;  this%w(10) = 0.1069393259953184_dp
                 this%x(11) = -0.9815606342467192_dp;  this%w(11) = 0.0471753363865118_dp
                 this%x(12) =  0.9815606342467192_dp;  this%w(12) = 0.0471753363865118_dp
               case(13)
                 this%x(1 ) =  0.0000000000000000_dp;  this%w(1 ) = 0.2325515532308739_dp
                 this%x(2 ) = -0.2304583159551348_dp;  this%w(2 ) = 0.2262831802628972_dp
                 this%x(3 ) =  0.2304583159551348_dp;  this%w(3 ) = 0.2262831802628972_dp
                 this%x(4 ) = -0.4484927510364469_dp;  this%w(4 ) = 0.2078160475368885_dp
                 this%x(5 ) =  0.4484927510364469_dp;  this%w(5 ) = 0.2078160475368885_dp
                 this%x(6 ) = -0.6423493394403402_dp;  this%w(6 ) = 0.1781459807619457_dp
                 this%x(7 ) =  0.6423493394403402_dp;  this%w(7 ) = 0.1781459807619457_dp
                 this%x(8 ) = -0.8015780907333099_dp;  this%w(8 ) = 0.1388735102197872_dp
                 this%x(9 ) =  0.8015780907333099_dp;  this%w(9 ) = 0.1388735102197872_dp
                 this%x(10) = -0.9175983992229779_dp;  this%w(10) = 0.0921214998377285_dp
                 this%x(11) =  0.9175983992229779_dp;  this%w(11) = 0.0921214998377285_dp
                 this%x(12) = -0.9841830547185881_dp;  this%w(12) = 0.0404840047653159_dp
                 this%x(13) =  0.9841830547185881_dp;  this%w(13) = 0.0404840047653159_dp
             case(14)
                 this%x(1 ) = -0.1080549487073437_dp;  this%w(1 ) = 0.2152638534631578_dp
                 this%x(2 ) =  0.1080549487073437_dp;  this%w(2 ) = 0.2152638534631578_dp
                 this%x(3 ) = -0.3191123689278897_dp;  this%w(3 ) = 0.2051984637212956_dp
                 this%x(4 ) =  0.3191123689278897_dp;  this%w(4 ) = 0.2051984637212956_dp
                 this%x(5 ) = -0.5152486363581541_dp;  this%w(5 ) = 0.1855383974779378_dp
                 this%x(6 ) =  0.5152486363581541_dp;  this%w(6 ) = 0.1855383974779378_dp
                 this%x(7 ) = -0.6872929048116855_dp;  this%w(7 ) = 0.1572031671581935_dp
                 this%x(8 ) =  0.6872929048116855_dp;  this%w(8 ) = 0.1572031671581935_dp
                 this%x(9 ) = -0.8272013150697650_dp;  this%w(9 ) = 0.1215185706879032_dp
                 this%x(10) =  0.8272013150697650_dp;  this%w(10) = 0.1215185706879032_dp
                 this%x(11) = -0.9284348836635735_dp;  this%w(11) = 0.0801580871597602_dp
                 this%x(12) =  0.9284348836635735_dp;  this%w(12) = 0.0801580871597602_dp
                 this%x(13) = -0.9862838086968123_dp;  this%w(13) = 0.0351194603317519_dp
                 this%x(14) =  0.9862838086968123_dp;  this%w(14) = 0.0351194603317519_dp
             case(15)
                 this%x(1 ) =  0.0000000000000000_dp;  this%w(1 ) = 0.2025782419255613_dp
                 this%x(2 ) = -0.2011940939974345_dp;  this%w(2 ) = 0.1984314853271116_dp
                 this%x(3 ) =  0.2011940939974345_dp;  this%w(3 ) = 0.1984314853271116_dp
                 this%x(4 ) = -0.3941513470775634_dp;  this%w(4 ) = 0.1861610000155622_dp
                 this%x(5 ) =  0.3941513470775634_dp;  this%w(5 ) = 0.1861610000155622_dp
                 this%x(6 ) = -0.5709721726085388_dp;  this%w(6 ) = 0.1662692058169939_dp
                 this%x(7 ) =  0.5709721726085388_dp;  this%w(7 ) = 0.1662692058169939_dp
                 this%x(8 ) = -0.7244177313601701_dp;  this%w(8 ) = 0.1395706779261543_dp
                 this%x(9 ) =  0.7244177313601701_dp;  this%w(9 ) = 0.1395706779261543_dp
                 this%x(10) = -0.8482065834104272_dp;  this%w(10) = 0.1071592204671719_dp
                 this%x(11) =  0.8482065834104272_dp;  this%w(11) = 0.1071592204671719_dp
                 this%x(12) = -0.9372733924007060_dp;  this%w(12) = 0.0703660474881081_dp
                 this%x(13) =  0.9372733924007060_dp;  this%w(13) = 0.0703660474881081_dp
                 this%x(14) = -0.9879925180204854_dp;  this%w(14) = 0.0307532419961173_dp
                 this%x(15) =  0.9879925180204854_dp;  this%w(15) = 0.0307532419961173_dp
             case(16)
                 this%x(1 ) = -0.0950125098376374_dp;  this%w(1 ) = 0.1894506104550685_dp
                 this%x(2 ) =  0.0950125098376374_dp;  this%w(2 ) = 0.1894506104550685_dp
                 this%x(3 ) = -0.2816035507792589_dp;  this%w(3 ) = 0.1826034150449236_dp
                 this%x(4 ) =  0.2816035507792589_dp;  this%w(4 ) = 0.1826034150449236_dp
                 this%x(5 ) = -0.4580167776572274_dp;  this%w(5 ) = 0.1691565193950025_dp
                 this%x(6 ) =  0.4580167776572274_dp;  this%w(6 ) = 0.1691565193950025_dp
                 this%x(7 ) = -0.6178762444026438_dp;  this%w(7 ) = 0.1495959888165767_dp
                 this%x(8 ) =  0.6178762444026438_dp;  this%w(8 ) = 0.1495959888165767_dp
                 this%x(9 ) = -0.7554044083550030_dp;  this%w(9 ) = 0.1246289712555339_dp
                 this%x(10) =  0.7554044083550030_dp;  this%w(10) = 0.1246289712555339_dp
                 this%x(11) = -0.8656312023878318_dp;  this%w(11) = 0.0951585116824928_dp
                 this%x(12) =  0.8656312023878318_dp;  this%w(12) = 0.0951585116824928_dp
                 this%x(13) = -0.9445750230732326_dp;  this%w(13) = 0.0622535239386479_dp
                 this%x(14) =  0.9445750230732326_dp;  this%w(14) = 0.0622535239386479_dp
                 this%x(15) = -0.9894009349916499_dp;  this%w(15) = 0.0271524594117541_dp
                 this%x(16) =  0.9894009349916499_dp;  this%w(16) = 0.0271524594117541_dp
              case(17)
                 this%x(1 ) =  0.0000000000000000_dp;  this%w(1 ) = 0.1794464703562065_dp
                 this%x(2 ) = -0.1784841814958479_dp;  this%w(2 ) = 0.1765627053669926_dp
                 this%x(3 ) =  0.1784841814958479_dp;  this%w(3 ) = 0.1765627053669926_dp
                 this%x(4 ) = -0.3512317634538763_dp;  this%w(4 ) = 0.1680041021564500_dp
                 this%x(5 ) =  0.3512317634538763_dp;  this%w(5 ) = 0.1680041021564500_dp
                 this%x(6 ) = -0.5126905370864769_dp;  this%w(6 ) = 0.1540457610768103_dp
                 this%x(7 ) =  0.5126905370864769_dp;  this%w(7 ) = 0.1540457610768103_dp
                 this%x(8 ) = -0.6576711592166907_dp;  this%w(8 ) = 0.1351363684685255_dp
                 this%x(9 ) =  0.6576711592166907_dp;  this%w(9 ) = 0.1351363684685255_dp
                 this%x(10) = -0.7815140038968014_dp;  this%w(10) = 0.1118838471934040_dp
                 this%x(11) =  0.7815140038968014_dp;  this%w(11) = 0.1118838471934040_dp
                 this%x(12) = -0.8802391537269859_dp;  this%w(12) = 0.0850361483171792_dp
                 this%x(13) =  0.8802391537269859_dp;  this%w(13) = 0.0850361483171792_dp
                 this%x(14) = -0.9506755217687678_dp;  this%w(14) = 0.0554595293739872_dp
                 this%x(15) =  0.9506755217687678_dp;  this%w(15) = 0.0554595293739872_dp
                 this%x(16) = -0.9905754753144174_dp;  this%w(16) = 0.0241483028685479_dp
                 this%x(17) =  0.9905754753144174_dp;  this%w(17) = 0.0241483028685479_dp
              endselect
            case('KRO')
            if (allocated(this%w)) deallocate(this%w); allocate(this%w(1:2*n+1))
            if (allocated(this%x)) deallocate(this%x); allocate(this%x(1:2*n+1))
            select case(n)
             case(1)
              this%x(1) =  0._dp;                   this%w(1) = 0.8888888888888889_dp
              this%x(2) = -0.7745966692414834_dp;   this%w(2) = 0.5555555555555556_dp
              this%x(3) =  0.7745966692414834_dp;   this%w(3) = 0.5555555555555556_dp
        case(2)
           this%x(3) =  0._dp;                   this%w(3) = 0.6222222222222222_dp
           this%x(1) = -0.5773502691896257_dp;   this%w(1) = 0.4909090909090909_dp
           this%x(2) =  0.5773502691896257_dp;   this%w(2) = 0.4909090909090909_dp
           this%x(4) = -0.9258200997725515_dp;   this%w(4) = 0.1979797979797979_dp
           this%x(5) =  0.9258200997725515_dp;   this%w(5) = 0.1979797979797979_dp
        case(3)
           this%x(1) =  0._dp;                   this%w(1) = 0.4509165386584741_dp
           this%x(2) = -0.4342437493468026_dp;   this%w(2) = 0.4013974147759622_dp
           this%x(3) =  0.4342437493468026_dp;   this%w(3) = 0.4013974147759622_dp
           this%x(4) = -0.7745966692414834_dp;   this%w(4) = 0.2684880898683334_dp
           this%x(5) =  0.7745966692414834_dp;   this%w(5) = 0.2684880898683334_dp
           this%x(6) = -0.9604912687080203_dp;   this%w(6) = 0.1046562260264673_dp
           this%x(7) =  0.9604912687080203_dp;   this%w(7) = 0.1046562260264673_dp
        case(4)
           this%x(1) =  0._dp;                   this%w(1) = 0.3464429818901364_dp
           this%x(2) = -0.3399810435848563_dp;   this%w(2) = 0.3269491896014516_dp
           this%x(3) =  0.3399810435848563_dp;   this%w(3) = 0.3269491896014516_dp
           this%x(4) = -0.6402862174963000_dp;   this%w(4) = 0.2667983404522844_dp
           this%x(5) =  0.6402862174963000_dp;   this%w(5) = 0.2667983404522844_dp
           this%x(6) = -0.8611363115940526_dp;   this%w(6) = 0.1700536053357227_dp
           this%x(7) =  0.8611363115940526_dp;   this%w(7) = 0.1700536053357227_dp
           this%x(8) = -0.9765602507375731_dp;   this%w(8) = 0.0629773736654730_dp
           this%x(9) = -0.9765602507375731_dp;   this%w(9) = 0.0629773736654730_dp
        case(5)
           this%x(1 ) =  0._dp;                  this%w(1 ) = 0.2829874178574912_dp
           this%x(2 ) = -0.2796304131617832_dp;  this%w(2 ) = 0.2728498019125589_dp
           this%x(3 ) =  0.2796304131617832_dp;  this%w(3 ) = 0.2728498019125589_dp
           this%x(4 ) = -0.5384693101056831_dp;  this%w(4 ) = 0.2410403392286476_dp
           this%x(5 ) =  0.5384693101056831_dp;  this%w(5 ) = 0.2410403392286476_dp
           this%x(6 ) = -0.7541667265708492_dp;  this%w(6 ) = 0.1868007965564926_dp
           this%x(7 ) =  0.7541667265708492_dp;  this%w(7 ) = 0.1868007965564926_dp
           this%x(8 ) = -0.9061798459386640_dp;  this%w(8 ) = 0.1152333166224734_dp
           this%x(9 ) =  0.9061798459386640_dp;  this%w(9 ) = 0.1152333166224734_dp
           this%x(10) = -0.9840853600948425_dp;  this%w(10) = 0.0425820367510818_dp
           this%x(11) =  0.9840853600948425_dp;  this%w(11) = 0.0425820367510818_dp
        case(6)
           this%x(1 ) =  0._dp;                  this%w(1 ) = 0.2410725801734648_dp
           this%x(2 ) = -0.2386191860831969_dp;  this%w(2 ) = 0.2337708641169944_dp
           this%x(3 ) =  0.2386191860831969_dp;  this%w(3 ) = 0.2337708641169944_dp
           this%x(4 ) = -0.4631182124753046_dp;  this%w(4 ) = 0.2132096522719622_dp
           this%x(5 ) =  0.4631182124753046_dp;  this%w(5 ) = 0.2132096522719622_dp
           this%x(6 ) = -0.6612093864662645_dp;  this%w(6 ) = 0.1810719943231376_dp
           this%x(7 ) =  0.6612093864662645_dp;  this%w(7 ) = 0.1810719943231376_dp
           this%x(8 ) = -0.8213733408650279_dp;  this%w(8 ) = 0.1373206046344469_dp
           this%x(9 ) =  0.8213733408650279_dp;  this%w(9 ) = 0.1373206046344469_dp
           this%x(10) = -0.9324695142031521_dp;  this%w(10) = 0.0836944404469066_dp
           this%x(11) =  0.9324695142031521_dp;  this%w(11) = 0.0836944404469066_dp
           this%x(12) = -0.9887032026126789_dp;  this%w(12) = 0.0303961541198198_dp
           this%x(13) =  0.9887032026126789_dp;  this%w(13) = 0.0303961541198198_dp
       case(7)
           this%x(1 ) =  0._dp;                  this%w(1 ) = 0.2094821410847278_dp
           this%x(2 ) = -0.2077849550078985_dp;  this%w(2 ) = 0.2044329400752989_dp
           this%x(3 ) =  0.2077849550078985_dp;  this%w(3 ) = 0.2044329400752989_dp
           this%x(4 ) = -0.4058451513773972_dp;  this%w(4 ) = 0.1903505780647854_dp
           this%x(5 ) =  0.4058451513773972_dp;  this%w(5 ) = 0.1903505780647854_dp
           this%x(6 ) = -0.5860872354676911_dp;  this%w(6 ) = 0.1690047266392679_dp
           this%x(7 ) =  0.5860872354676911_dp;  this%w(7 ) = 0.1690047266392679_dp
           this%x(8 ) = -0.7415311855993945_dp;  this%w(8 ) = 0.1406532597155259_dp
           this%x(9 ) =  0.7415311855993945_dp;  this%w(9 ) = 0.1406532597155259_dp
           this%x(10) = -0.8648644233597691_dp;  this%w(10) = 0.1047900103222502_dp
           this%x(11) =  0.8648644233597691_dp;  this%w(11) = 0.1047900103222502_dp
           this%x(12) = -0.9491079123427585_dp;  this%w(12) = 0.0630920926299786_dp
           this%x(13) =  0.9491079123427585_dp;  this%w(13) = 0.0630920926299786_dp
           this%x(14) = -0.9914553711208126_dp;  this%w(14) = 0.0229353220105292_dp
           this%x(15) =  0.9914553711208126_dp;  this%w(15) = 0.0229353220105292_dp
      case(8)
           this%x(1 ) =  0._dp;                  this%w(1 ) = 0.1844464057446916_dp
           this%x(2 ) = -0.1834346424956498_dp;  this%w(2 ) = 0.1814000250680346_dp
           this%x(3 ) =  0.1834346424956498_dp;  this%w(3 ) = 0.1814000250680346_dp
           this%x(4 ) = -0.3607010979281320_dp;  this%w(4 ) = 0.1720706085552113_dp
           this%x(5 ) =  0.3607010979281320_dp;  this%w(5 ) = 0.1720706085552113_dp
           this%x(6 ) = -0.5255324099163290_dp;  this%w(6 ) = 0.1566526061681884_dp
           this%x(7 ) =  0.5255324099163290_dp;  this%w(7 ) = 0.1566526061681884_dp
           this%x(8 ) = -0.6723540709451587_dp;  this%w(8 ) = 0.1362631092551722_dp
           this%x(9 ) =  0.6723540709451587_dp;  this%w(9 ) = 0.1362631092551722_dp
           this%x(10) = -0.7966664774136267_dp;  this%w(10) = 0.1116463708268396_dp
           this%x(11) =  0.7966664774136267_dp;  this%w(11) = 0.1116463708268396_dp
           this%x(12) = -0.8941209068474564_dp;  this%w(12) = 0.0824822989313583_dp
           this%x(13) =  0.8941209068474564_dp;  this%w(13) = 0.0824822989313583_dp
           this%x(14) = -0.9602898564975363_dp;  this%w(14) = 0.0494393950021393_dp
           this%x(15) =  0.9602898564975363_dp;  this%w(15) = 0.0494393950021393_dp
           this%x(16) = -0.9933798758817162_dp;  this%w(16) = 0.0178223833207104_dp
           this%x(17) =  0.9933798758817162_dp;  this%w(17) = 0.0178223833207104_dp
      case(9)
           this%x(1 ) =  0._dp;                  this%w(1 ) = 0.1648960128283494_dp
           this%x(2 ) = -0.1642235636149868_dp;  this%w(2 ) = 0.1628628274401151_dp
           this%x(3 ) =  0.1642235636149868_dp;  this%w(3 ) = 0.1628628274401151_dp
           this%x(4 ) = -0.3242534234038089_dp;  this%w(4 ) = 0.1564135277884839_dp
           this%x(5 ) =  0.3242534234038089_dp;  this%w(5 ) = 0.1564135277884839_dp
           this%x(6 ) = -0.4754624791124599_dp;  this%w(6 ) = 0.1452395883843662_dp
           this%x(7 ) =  0.4754624791124599_dp;  this%w(7 ) = 0.1452395883843662_dp
           this%x(8 ) = -0.6133714327005904_dp;  this%w(8 ) = 0.1300014068553412_dp
           this%x(9 ) =  0.6133714327005904_dp;  this%w(9 ) = 0.1300014068553412_dp
           this%x(10) = -0.7344867651839338_dp;  this%w(10) = 0.1117891346844183_dp
           this%x(11) =  0.7344867651839338_dp;  this%w(11) = 0.1117891346844183_dp
             this%x(12) = -0.8360311073266358_dp;  this%w(12) = 0.0907906816887264_dp
             this%x(13) =  0.8360311073266358_dp;  this%w(13) = 0.0907906816887264_dp
             this%x(14) = -0.9149635072496779_dp;  this%w(14) = 0.0665181559402741_dp
             this%x(15) =  0.9149635072496779_dp;  this%w(15) = 0.0665181559402741_dp
             this%x(16) = -0.9681602395076261_dp;  this%w(16) = 0.0396318951602613_dp
             this%x(17) =  0.9681602395076261_dp;  this%w(17) = 0.0396318951602613_dp
             this%x(18) = -0.9946781606773402_dp;  this%w(18) = 0.0143047756438389_dp
             this%x(19) =  0.9946781606773402_dp;  this%w(19) = 0.0143047756438389_dp
         case(10)
              this%x(1 ) =  0._dp;                  this%w(1 ) = 0.1494455540029169_dp
             this%x(2 ) = -0.1488743389816312_dp;  this%w(2 ) = 0.1477391049013385_dp
             this%x(3 ) =  0.1488743389816312_dp;  this%w(3 ) = 0.1477391049013385_dp
             this%x(4 ) = -0.2943928627014602_dp;  this%w(4 ) = 0.1427759385770601_dp
             this%x(5 ) =  0.2943928627014602_dp;  this%w(5 ) = 0.1427759385770601_dp
             this%x(6 ) = -0.4333953941292472_dp;  this%w(6 ) = 0.1347092173114733_dp
             this%x(7 ) =  0.4333953941292472_dp;  this%w(7 ) = 0.1347092173114733_dp
             this%x(8 ) = -0.5627571346686047_dp;  this%w(8 ) = 0.1234919762620658_dp
             this%x(9 ) =  0.5627571346686047_dp;  this%w(9 ) = 0.1234919762620658_dp
             this%x(10) = -0.6794095682990244_dp;  this%w(10) = 0.1093871588022976_dp
             this%x(11) =  0.6794095682990244_dp;  this%w(11) = 0.1093871588022976_dp
             this%x(12) = -0.7808177265864169_dp;  this%w(12) = 0.0931254545836976_dp
             this%x(13) =  0.7808177265864169_dp;  this%w(13) = 0.0931254545836976_dp
             this%x(14) = -0.8650633666889845_dp;  this%w(14) = 0.0750396748109199_dp
             this%x(15) =  0.8650633666889845_dp;  this%w(15) = 0.0750396748109199_dp
             this%x(16) = -0.9301574913557082_dp;  this%w(16) = 0.0547558965743520_dp
             this%x(17) =  0.9301574913557082_dp;  this%w(17) = 0.0547558965743520_dp
             this%x(18) = -0.9739065285171717_dp;  this%w(18) = 0.0325581623079647_dp
             this%x(19) =  0.9739065285171717_dp;  this%w(19) = 0.0325581623079647_dp
             this%x(20) = -0.9956571630258081_dp;  this%w(20) = 0.0116946388673719_dp
             this%x(21) =  0.9956571630258081_dp;  this%w(21) = 0.0116946388673719_dp
         case(11)
             this%x(1 ) =  0._dp;                  this%w(1 ) =  0.1365777947111183_dp
             this%x(2 ) = -0.1361130007993618_dp;  this%w(2 ) =  0.1351935727998845_dp
             this%x(3 ) =  0.1361130007993618_dp;  this%w(3 ) =  0.1351935727998845_dp
             this%x(4 ) = -0.2695431559523450_dp;  this%w(4 ) =  0.1312806842298056_dp
             this%x(5 ) =  0.2695431559523450_dp;  this%w(5 ) =  0.1312806842298056_dp
             this%x(6 ) = -0.3979441409523776_dp;  this%w(6 ) =  0.1251587991003195_dp
             this%x(7 ) =  0.3979441409523776_dp;  this%w(7 ) =  0.1251587991003195_dp
             this%x(8 ) = -0.5190961292068118_dp;  this%w(8 ) =  0.1167395024610473_dp
             this%x(9 ) =  0.5190961292068118_dp;  this%w(9 ) =  0.1167395024610473_dp
              this%x(10) = -0.6305995201619651_dp;  this%w(10) =  0.1058720744813894_dp
             this%x(11) =  0.6305995201619651_dp;  this%w(11) =  0.1058720744813894_dp
             this%x(12) = -0.7301520055740494_dp;  this%w(12) =  0.0929530985969008_dp
             this%x(13) =  0.7301520055740494_dp;  this%w(13) =  0.0929530985969008_dp
             this%x(14) = -0.8160574566562209_dp;  this%w(14) =  0.0786645719322273_dp
             this%x(15) =  0.8160574566562209_dp;  this%w(15) =  0.0786645719322273_dp
             this%x(16) = -0.8870625997680953_dp;  this%w(16) =  0.0630974247503749_dp
             this%x(17) =  0.8870625997680953_dp;  this%w(17) =  0.0630974247503749_dp
             this%x(18) = -0.9416771085780680_dp;  this%w(18) =  0.0458293785644264_dp
             this%x(19) =  0.9416771085780680_dp;  this%w(19) =  0.0458293785644264_dp
             this%x(20) = -0.9782286581460570_dp;  this%w(20) =  0.0271565546821043_dp
             this%x(21) =  0.9782286581460570_dp;  this%w(21) =  0.0271565546821043_dp
             this%x(22) = -0.9963696138895426_dp;  this%w(22) =  0.0097654410459608_dp
             this%x(23) =  0.9963696138895426_dp;  this%w(23) =  0.0097654410459608_dp
        endselect
        case('CHE')
          if (allocated(this%w)) deallocate(this%w); allocate(this%w(1:n))
          if (allocated(this%x)) deallocate(this%x); allocate(this%x(1:n))
          pin=PI/real(n,kind=dp)
          !dir$ assume_aligned this.x(1):64
          !dir$ assume_aligned this.w(1):64
           !$omp simd linear(i:1)
          do i=1,n
              this%x(i) = cos((2._dp*i - 1._dp)/(2._dp*n)*PI)
              this%w(i) = pin * sqrt(1._dp - (this%x(i))**2._dp)
          enddo
          select case(d)
          case(2)
             if (allocated(this%y)) deallocate(this%y); allocate(this%y(1:n)); this%y = this%x
          case(3)
             if (allocated(this%y)) deallocate(this%y); allocate(this%y(1:n)); this%y = this%x
             if (allocated(this%z)) deallocate(this%z); allocate(this%z(1:n)); this%z = this%x
          endselect
        endselect
     end subroutine init_gauss
     
     
     
     
     










end module quadrature_rules
