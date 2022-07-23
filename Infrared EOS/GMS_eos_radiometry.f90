

module eos_radiometry


   !============================================================
   ! This module is based on: 'The Infrared & Electro-Optical
   ! Systems Handbook' tom 1.
   ! Many of the equations will be implemented from the above 
   ! quoted source.
   !============================================================

   use mod_kinds, only : i4,sp,dp
   public
   implicit none

   ! Constants
   
   real(kind=sp), parameter :: c      = 299792458.0_dp    ! speed of light in vacuum
   real(kind=dp), parameter :: h      = 6.62607015e-34_dp ! Planck const
   real(kind=dp), parameter :: k      = 1.380649e−23_dp   ! Boltzmann const
   real(kind=dp), parameter :: c1     = 6.283185307179586476925286766559_dp*c*c*h ! first radiation constant
   real(kind=sp), parameter :: c2     = h*c/k                                     ! second radiation constant


   interface radiant_exitance
       module procedure :: radiant_exitance_r4
       module procedure :: radiant_exitance_r8
   end interface radiant_exitance


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











end module eos_radiometry
