

module eos_radiometry


   !============================================================
   ! This module is based on: 'The Infrared & Electro-Optical
   ! Systems Handbook' tom 1.
   ! Many of the equations was implemented from the above 
   ! quoted source.
   !============================================================

   use mod_kinds, only : i4,sp,dp
   public
   implicit none

   ! Constants
   
   real(kind=dp), parameter :: c      = 299792458.0_dp    ! speed of light in vacuum
   real(kind=dp), parameter :: h      = 6.62607015e-34_dp ! Planck const
   real(kind=dp), parameter :: k      = 1.380649e−23_dp   ! Boltzmann const
   real(kind=dp), parameter :: c1     = 6.283185307179586476925286766559_dp*c*c*h ! first radiation constant
   real(kind=dp), parameter :: c2     = h*c/k                                     ! second radiation constant
   !Maxima for Different Planck Functions
   real(kind=dp), dimension(2), parameter :: photon_freq = [1.593624260_dp,0.6476_dp] ! x-max,R-max
   real(kind=dp), dimension(2), parameter :: power_freq  = [2.821439372_dp,1.4214_dp] ! as above
   real(kind=dp), dimension(2), parameter :: photon_gam  = [3.920690395_dp,4.7796_dp] ! as above
   real(kind=dp), dimension(2), parameter :: power_gam   = [4.96511423_dp, 21.2036_dp]! as above
   real(kind=dp), dimension(2), parameter :: pow_contrast= [5.96940917_dp,115.9359_dp]! as above 

   ! Interstellar-Extinction integrand variable
   real(kind=dp) :: Rd,b,l,rad ! 'r' can not be zero

   interface radiant_exitance
       module procedure :: radiant_exitance_r4
       module procedure :: radiant_exitance_r8
   end interface radiant_exitance

   interface bbody_radiance
       module procedure :: bbody_radiance_r4
       module procedure :: bbody_radiance_r8
   end interface bbody_radiance


   contains


   
#if 0

  Table 1.1 Symbols, Nomenclature, and Units
Symbol Description Units
B, Rotational constant, cm- 1
C, Constant -
c, Speed of light in vacuum, m S-1
Cl, First radiation constant, Wcm 2
C2, Second radiation constant, cmK
E, Incidance [irradiance], Wm- 2
h, Planck's constant, Js
I, Radiance intensity, W sr- 1
Ie, Moment of inertia ,gcm 2
i, Running index, 1, 2, 3, .., -
K,L,M,N Atomic shell designations -
k, Boltzmann constant ,J K- 1
k, Force constant, J m- 2
k, Radian wave number, m- 1
L, Radiance [sterance] W m- 2 sr- 1
l, Orbital quantum number -
M, Radiant exitance [emittance], Wm- 2
m, Integers 1, 2, 3, ... -
m, Magnetic quantum number -
m, Reduced mass, g
n, Principal quantum number -
P, Pressure P
P,II> Flux J s-1, W
PO, Legendre polynomial -
Q, Quantity Various
q, Volume density ofQ Various, m- 3
R, Distance m
R, General radiometric variable Various
RH, Rydberg constant for hydrogen cm- 1
RO, Radial wave function -
r, Axial distance m
S, Line strength -
T, Temperature K
U, Energy J
u, Energy density J m- 3
x, Dimensionless frequency = c2/'AT -
y, General spectral variable Various
a, Absorptivity -
aO, Absorption coefficient, cm- 1
/l'A, /lv Bandwidth fJ.m, cm- 1
E,Emissivity -
Variable of integration -
eo, Azimuthal wave function -
e, Angle r, deg
Wavelength ,fJ.m
v ,Frequency, Hz
v ,Wave number, cm- 1
'!I" 3.14159... -
p Reflectivity -
(J Stephan-Boltzmann constant, W m- 2 K- 4
,. Transmissi vi ty -
11>0 Angular wave function -
<I> Angle r, deg
il Solid angle sr
il' Projected solid angle sr
Superscripts:
BB Blackbody
Subscripts:
b Bidirectional
h Hemispheric
q Photonic
s Specular
u Energetic
v Visible
'A Wavelength
00 Indicating a property with infinite
number of passes

Table 1.2 Radiometric Terms
Name Symbol Definition Expression Units Alternates
Quantity Q,U Radiant Joule [J]
energy
Flux <I>,P Time rate of aQ Watt [W]
quantity at
Flux density Flux per unit a<l> Wm- 2
normal area aA
Exitance M[W] Exitent flux a<l> Wm- 2 Areance
density aA Radiant emittance
Incidance E[m Incident flux a<l> Wm- 2 Areance
density aA Irradiance
Intensity l[J] Flux per a<l> W sr- 2 Pointance
solid angle afi
Sterance L [N] Flux density a 2<1> W m- 2 sr- 1 Radiance
per solid coseaAafi
angle
Sterisent Sterance per a 3 p W m- 3 sr- 1 Path radiance
unit coseaAafial
pathlength
Fluence exposure Incidance aQ Jm- 2
times time aA
Radiosity Emitted and a 2 p W m- 2 sr- 1 Radiance
reflected coseaAafi Sterance
radiance

#endif


     pure elemental function radiant_exitance_r4(gamma,T) result(Mgamm)
        !dir$ optimize:3
        !dir$ attributes forceinline :: radiant_exitance_r4
        !dir$ attributes code_align : 32 :: radiant_exitance_r4
        !dir$ attributes vector:processor(skylake_avx512) :: radiant_exitance_r4
        real(kind=sp),  intent(in) :: gamma !wavelength
        real(kind=sp),  intent(in) :: T     !temperature
        real(kind=sp) :: Mgamm
        ! Locals
        real(kind=sp), automatic :: gamm5,x
        ! Exec code ....
        gamm5 = gamma*gamma*gamma*gamma*gamma
        x     = real(c2,kind=sp)/gamma*T
        Mgamm = real(c1,kind=sp)/(gamm5*exp(x)-1.0_sp)
     end function radiant_exitance_r4


     pure elemental function radiant_exitance_r8(gamma,T) result(Mgamm)
        !dir$ optimize:3
        !dir$ attributes forceinline :: radiant_exitance_r8
        !dir$ attributes code_align : 32 :: radiant_exitance_r8
        !dir$ attributes vector:processor(skylake_avx512) :: radiant_exitance_r8
        real(kind=dp),  intent(in) :: gamma !wavelength
        real(kind=dp),  intent(in) :: T     !temperature
        real(kind=dp) :: Mgamm
        ! Locals
        real(kind=dp), automatic :: gamm5,x
        ! Exec code ....
        gamm5 = gamma*gamma*gamma*gamma*gamma
        x     = c2/gamma*T
        Mgamm = c1/(gamm5*exp(x)-1.0_dp)
     end function radiant_exitance_r8


     pure elemental function bbody_radiance_r4(gamma,T) result(Lgamm)
        !dir$ optimize:3
        !dir$ attributes forceinline :: bbody_radiance_r4
        !dir$ attributes code_align : 32 :: bbody_radiance_r4
        !dir$ attributes vector:processor(skylake_avx512) :: bbody_radiance_r4
        real(kind=sp),  intent(in) :: gamma !wavelength
        real(kind=sp),  intent(in) :: T     !temperature
        real(kind=sp) :: Lgamm
        ! Locals
        real(kind=sp), automatic :: num,gamm5,x
        ! Exec code ....
        gamm5 = gamma*gamma*gamma*gamma*gamma
        num   = 2.0_sp*real(c*c,kind=sp)*h
        x     = c2/gamma*T
        Lgamm = num/(gamm5*exp(x)-1.0_sp)
     end function bbody_radiance_r4


     pure elemental function bbody_radiance_r8(gamma,T) result(Lgamm)
        !dir$ optimize:3
        !dir$ attributes forceinline :: bbody_radiance_r8
        !dir$ attributes code_align : 32 :: bbody_radiance_r8
        !dir$ attributes vector:processor(skylake_avx512) :: bbody_radiance_r8
        real(kind=dp),  intent(in) :: gamma !wavelength
        real(kind=dp),  intent(in) :: T     !temperature
        real(kind=dp) :: Lgamm
        ! Locals
        real(kind=dp), automatic :: num,gamm5,x
        ! Exec code ....
        gamm5 = gamma*gamma*gamma*gamma*gamma
        num   = 2.0_dp*c*c*h
        x     = c2/gamma*T
        Lgamm = num/(gamm5*exp(x)-1.0_dp)
     end function bbody_radiance_r8


     pure elemental function contrast_analytic_r4(Ry,T,x) result(dT)
        !dir$ optimize:3
        !dir$ attributes forceinline :: contrast_analytic_r4
        !dir$ attributes code_align : 32 :: contrast_analytic_r4
        !dir$ attributes vector:processor(skylake_avx512) :: contrast_analytic_r4
        real(kind=sp),  intent(in) :: Ry  !spectral radiometric function, radiance, radiant emittance
        real(kind=sp),  intent(in) :: T   ! Temperature
        real(kind=sp),  intent(in) :: x   ! dimensionless frequency
        ! Locals
        real(kind=sp), automatic :: num,den,expx
        ! Exec code ....
        expx = exp(x)
        num = x*expx
        den = T*expx-1.0_sp
        dT  = (num/den)*Ry
     end function contrast_analytic_r4


     pure elemental function contrast_analytic_r8(Ry,T,x) result(dT)
        !dir$ optimize:3
        !dir$ attributes forceinline :: contrast_analytic_r8
        !dir$ attributes code_align : 32 :: contrast_analytic_r8
        !dir$ attributes vector:processor(skylake_avx512) :: contrast_analytic_r8
        real(kind=dp),  intent(in) :: Ry  !spectral radiometric function, radiance, radiant emittance
        real(kind=dp),  intent(in) :: T   ! Temperature
        real(kind=dp),  intent(in) :: x   ! dimensionless frequency
        ! Locals
        real(kind=dp), automatic :: num,den,expx
        ! Exec code ....
        expx = exp(x)
        num = x*expx
        den = T*expx-1.0_dp
        dT  = (num/den)*Ry
     end function contrast_analytic_r8


     subroutine contrast_num_r4(Ry,T,dRydT,n,omp)
 
        !dir$ optimize:3
        !dir$ attributes forceinline :: contrast_num_r4
        !dir$ attributes code_align : 32 :: contrast_num_r4
        use data_fdiff
        implicit none
        real(kind=sp),   dimension(n), intent(in) :: Ry !any spectral radiometric function, radiance, radiant emittance
        real(kind=sp),   dimension(n), intent(in) :: T  !Temperature as an independent variable
        real(kind=sp),   dimension(n), intent(out):: dRydT !Resulting numerical derivative
        integer(kind=i4),              intent(in) :: n
        logical(kind=i4),              intent(in) :: omp !use OpenMP
        ! Exec code ....
        if(omp) then
           data_fdiff_omp(Ry,T,dRydT,n)
        else
           data_fdiff(Ry,T,dRyDt,n)
        end if
     end subroutine contrast_num_r4


     subroutine contrast_num_r8(Ry,T,dRydT,n,omp)
 
        !dir$ optimize:3
        !dir$ attributes forceinline :: contrast_num_r8
        !dir$ attributes code_align : 32 :: contrast_num_r8
        use data_fdiff
        implicit none
        real(kind=dp),   dimension(n), intent(in) :: Ry !any spectral radiometric function, radiance, radiant emittance
        real(kind=dp),   dimension(n), intent(in) :: T  !Temperature as an independent variable
        real(kind=dp),   dimension(n), intent(out):: dRydT !Resulting numerical derivative
        integer(kind=i4),              intent(in) :: n
        logical(kind=i4),              intent(in) :: omp !use OpenMP
        ! Exec code ....
        if(omp) then
           data_fdiff_omp(Ry,T,dRydT,n)
        else
           data_fdiff(Ry,T,dRyDt,n)
        end if
     end subroutine contrast_num_r8


     subroutine relative_contrast_r4(x,T,dx,dT,n,omp)

        !dir$ optimize:3
        !dir$ attributes forceinline :: relative_contrast_r4
        !dir$ attributes code_align : 32 :: relative_contrast_r4
        use data_fdiff
        implicit none
        real(kind=sp),                 intent(in) :: x
        real(kind=sp),   dimension(n), intent(in) :: T  !Temperature
        real(kind=sp),   dimension(n), intent(in) :: dx !independent variable
        real(kind=sp),   dimension(n), intent(out):: dT !Resulting numerical derivative
        integer(kind=i4),              intent(in) :: n
        logical(kind=i4),              intent(in) :: omp !use OpenMP
        ! Locals
        real(kind=sp), automatic :: expx,num,den,rat
        expx = exp(x)
        num  = x*exp(x)
        den  = expx-1.0_sp
        rat  = num/den
        if(omp) then
           data_fdiff_omp(T,dx,dT,n)
        else
           data_fdiff(T,dx,dT,n)
        end if
        !dir$ assume_aligned dT:64
        dT(:) = dT(:)*rat
     end subroutine relative_contrast_r4


     subroutine relative_contrast_r8(x,T,dx,dT,n,omp)

        !dir$ optimize:3
        !dir$ attributes forceinline :: relative_contrast_r8
        !dir$ attributes code_align : 32 :: relative_contrast_r8
        use data_fdiff
        implicit none
        real(kind=dp),                 intent(in) :: x
        real(kind=dp),   dimension(n), intent(in) :: T  !Temperature
        real(kind=dp),   dimension(n), intent(in) :: dx !independent variable
        real(kind=dp),   dimension(n), intent(out):: dT !Resulting numerical derivative
        integer(kind=i4),              intent(in) :: n
        logical(kind=i4),              intent(in) :: omp !use OpenMP
        ! Locals
        real(kind=dp), automatic :: expx,num,den,rat
        expx = exp(x)
        num  = x*exp(x)
        den  = expx-1.0_dp
        rat  = num/den
        if(omp) then
           data_fdiff_omp(T,dx,dT,n)
        else
           data_fdiff(T,dx,dT,n)
        end if
        !dir$ assume_aligned dT:64
        dT(:) = dT(:)*rat
     end subroutine relative_contrast_r8
   
     

     ! Planck function in terms of dimensionless frequency 'x'
     pure elemental function M_x_r4(x,T) result(Mx)
        !dir$ optimize:3
        !dir$ attributes forceinline :: M_x_r4
        !dir$ attributes code_align : 32 :: M_x_r4
        real(kind=sp),   intent(in) :: x  ! dimensionless frequency
        real(kind=sp),   intent(in) :: T  ! Temperature
        real(kind=sp) :: Mx
        real(kind=sp), automatic :: T4,x3,ratio,lt
        T4 = T*T*T*T
        x3 = x*x*x
        ratio = real(c1,kind=sp)*(T4/real(c2,kind=sp))
        lt    = x3/(exp(x)-1.0_sp)
        Mx    = ratio*lt
     end function M_x_r4

     pure elemental function M_x_r8(x,T) result(Mx)
        !dir$ optimize:3
        !dir$ attributes forceinline :: M_x_r8
        !dir$ attributes code_align : 32 :: M_x_r8
        real(kind=dp),   intent(in) :: x  ! dimensionless frequency
        real(kind=dp),   intent(in) :: T  ! Temperature
        real(kind=dp) :: Mx
        real(kind=dp), automatic :: T4,x3,ratio,lt
        T4 = T*T*T*T
        x3 = x*x*x
        ratio = c1*(T4/c2)
        lt    = x3/(exp(x)-1.0_dp)
        Mx    = ratio*lt
     end function M_x_r8


     ! The planck function integrand
     pure function planck_f(x) result(y)
        real(kind=dp),  intent(in) :: x
        real(kind=dp) :: y
        real(kind=dp), automatic :: x3
        x3 = x*x*x
        y  = x3/(exp(x)-1.0_dp)
     end function planck_f

     ! Compute Planck function integral
     subroutine planckf_quadrature(T,x,a,b,key,limit,lenw,ier,result) 
         use quadpack, only : dqag
         real(kind=dp),    intent(in) :: T
         real(kind=dp),    intent(in) :: x
         real(kind=dp),    intent(in) :: a
         real(kind=dp),    intent(in) :: b
         integer(kind=i4), intent(in) :: key
         integer(kind=i4), intent(in) :: limit
         integer(kind=i4), intent(in) :: lenw
         integer(kind=i4), intent(out):: ier
         real(kind=dp),    intent(out):: result
         real(kind=dp),    intent(out):: abserr
         real(kind=dp),    dimension(lenw)  :: work
         !dir$ attributes align : 64 :: work
         integer(kind=i4), dimension(limit) :: iwork
         !dir$ attributes align : 64 :: iwork
         real(kind=dp), automatic :: epsabs,epsrel
         real(kind=dp), automatic :: c4,T4,fact
         integer(kind=i4), automatic :: neval,last
         T4   = T*T*T*T
         c4   = c2*c2*c2*c2
         fact = (c1/c4)*T4
         call dqag(planck_f,x,a,b,epsabs,epsrel,key,result,  &
                   abserr,neval,ier,limit,lenw,last,iwork,work)
         result = fact*result
     end subroutine  planckf_quadrature


     ! Unpolarized light transmission
     pure elemental function theta_transmit_r4(theta,rho) result(value)
         
        !dir$ optimize:3
        !dir$ attributes forceinline :: theta_transmit_r4
        !dir$ attributes code_align : 32 :: theta_transmit_r4
        real(kind=sp),  intent(in) :: theta
        real(kind=sp),  intent(in) :: rho
        real(kind=sp) :: value
        real(kind=sp), automatic :: num,den,t0,sqr
        t0    = (1.0_sp-rho)
        sqr   = t0*t0
        num   = sqr*rho
        den   = 1.0_sp-rho*rho*theta*theta
        value = num/den
     end function theta_transmit_r4


     pure elemental function theta_transmit_r8(theta,rho) result(value)
         
        !dir$ optimize:3
        !dir$ attributes forceinline :: theta_transmit_r8
        !dir$ attributes code_align : 32 :: theta_transmit_r8
        real(kind=dp),  intent(in) :: theta
        real(kind=dp),  intent(in) :: rho
        real(kind=dp) :: value
        real(kind=dp), automatic :: num,den,t0,sqr
        t0    = (1.0_dp-rho)
        sqr   = t0*t0
        num   = sqr*rho
        den   = 1.0_dp-rho*rho*theta*theta
        value = num/den
     end function theta_transmit_r8


     ! Simple radiation geometry.
     
     pure elemental function dfluxp_r4(L,dA1,tht1,dA2,tht2,R) result(dPhi)
        !dir$ optimize:3
        !dir$ attributes forceinline :: dfluxp_r4
        !dir$ attributes code_align : 32 :: dfluxp_r4
        real(kind=sp),  intent(in) :: L
        real(kind=sp),  intent(in) :: dA1
        real(kind=sp),  intent(in) :: tht1
        real(kind=sp),  intent(in) :: dA2
        real(kind=sp),  intent(in) :: tht2
        real(kind=sp),  intent(in) :: R
        real(kind=sp) :: dPhi
        real(kind=sp), automatic :: ctht1,ctht2,R2,t0,t1
        R2 = R*R
        t0 = L*dA1
        ctht1 = cos(tht1)
        t1 = t0*ctht1
        ctht2 = cos(tht2)
        dPhi = (t1*dA2*ctht2)/R2
     end function dfluxp_r4


     pure elemental function dfluxp_r8(L,dA1,tht1,dA2,tht2,R) result(dPhi)
        !dir$ optimize:3
        !dir$ attributes forceinline :: dfluxp_r8
        !dir$ attributes code_align : 32 :: dfluxp_r8
        real(kind=dp),  intent(in) :: L
        real(kind=dp),  intent(in) :: dA1
        real(kind=dp),  intent(in) :: tht1
        real(kind=dp),  intent(in) :: dA2
        real(kind=dp),  intent(in) :: tht2
        real(kind=dp),  intent(in) :: R
        real(kind=dp) :: dPhi
        real(kind=dp), automatic :: ctht1,ctht2,R2,t0,t1
        R2 = R*R
        t0 = L*dA1
        ctht1 = cos(tht1)
        t1 = t0*ctht1
        ctht2 = cos(tht2)
        dPhi = (t1*dA2*ctht2)/R2
     end function dfluxp_r8

#if 0
Ba = vertical scale height = 0.1 kpc
ha = in plane scale height = 4.0 kpc
AD = 0.07[2.2 J.Lm/A (J.Lm)]2 mag kpc- 1
R = distance to point from sun, kpc
Ro = distance of the sun from the galactic center = 8.5 kpc
b = galactic latitude
l = galactic longitude.

#endif

     pure function ie_integrand(x) result(y)
          
           real(kind=dp), intent(in) :: x
           real(kind=dp), parameter  :: Ba  = 0.1_dp
           real(kind=dp), parameter  :: ha  = 4.0_dp
           real(kind=dp), parameter  :: Ad  = 0.07_dp
           real(kind=dp), parameter  :: Rd0 = 8.5_dp
           real(kind=dp), automatic  :: t0,t1,t2,t3,Rd2,rad2
           t0 = (r*sin(abs(b)))/Ba
           t1 = cos(l)
           Rd2 = Rd*Rd
           rad2= rad*rad
           t2  = Rd2+rad2-2.0_dp*rad*Rd0*t1
           t3  = (sqrt(t2)-Rd0)/ha
           y   = t3-t0
     end function ie_integrand

     ! Daylight direct sunlight emmision (curve fitted)
     pure function E_sunlight(m_a) result(E)

        !dir$ optimize:3
        !dir$ attributes forceinline :: E_sunlight
        !dir$ attributes code_align : 32 :: E_sunlight
        real(kind=sp), intent(in) :: m_a !number of air masses
        real(kind=sp) :: E
        E = 1089.5_sp/m_a*exp(-0.2819_sp*m_a)
     end function E_sunlight


      subroutine interstellar_extinct(a,b,key,limit,lenw,ier,A_R,abserr)
           
            real(kind=dp),    intent(in) :: a
            real(kind=dp),    intent(in) :: b
            integer(kind=i4), intent(in) :: key
            integer(kind=i4), intent(in) :: limit
            integer(kind=i4), intent(in) :: lenw
            integer(kind=i4), intent(out) :: ier
            real(kind=dp),    intent(out) :: A_R
            real(kind=dp),    intent(out):: abserr
            real(kind=dp),    dimension(lenw)  :: work
            !dir$ attributes align : 64 :: work
            integer(kind=i4), dimension(limit) :: iwork
            !dir$ attributes align : 64 :: iwork
            real(kind=dp), automatic :: epsabs,epsrel
            integer(kind=i4), automatic :: neval,last
            call dqag(ie_integrand,a,b,epsabs,epsrel,key,A_R,  &
                      abserr,neval,ier,limit,lenw,last,iwork,work)
      end subroutine interstellar_extinct


end module eos_radiometry
