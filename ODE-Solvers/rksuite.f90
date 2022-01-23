!module rksuite_90_prec
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
!integer(i4), parameter :: wp = selected_real_kind(10,50)

!end module rksuite_90_prec

module rksuite_90
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
!use rksuite_90_prec, only:wp
use mod_kinds, only : i4,dp
implicit none

private

public :: wp, setup, range_integrate, step_integrate, interpolate, &
          global_error, statistics, reset_t_end, collect_garbage,  &
          set_stop_on_fatal, get_saved_fatal
!starta!
public :: rk_comm_real_1d
type rk_comm_real_1d

private

real(kind=dp) :: t, t_old, t_start, t_end, dir                       !indep!
real(kind=dp) :: h, h_old, h_start, h_average                        !indep!
real(kind=dp) :: tol
integer(i4) :: f_count, full_f_count, step_count, bad_step_count
logical :: at_t_start, at_t_end


real(kind=dp), dimension(:), pointer :: thresh, weights, ymax        !shp-dep!

real(kind=dp), dimension(:), pointer :: scratch, y, yp, y_new        !dep!
real(kind=dp), dimension(:), pointer :: y_old, yp_old, v0, v1        !dep!
real(kind=dp), dimension(:), pointer :: err_estimates, v2, v3        !dep!
real(kind=dp), dimension(:), pointer ::  vtemp                       !dep!
real(kind=dp), dimension(:,:), pointer :: stages                     !dep!
!dir$ attributes align : 64 :: thresh,weights,ymax
!dir$ attributes align : 64 :: y_old,yp_old,v0,v1
!dir$ attributes align : 64 :: err_estimates,v2,v3
!dir$ attributes align : 64 :: vtemp
!dir$ attributes align : 64 :: stages
real(kind=dp) :: a(13,13), b(13), c(13), bhat(13), r(11,6), e(7)
integer(i4) :: ptr(13), no_of_stages, rk_method, intrp_degree
logical :: intrp_able, intrp_needs_stages

real(kind=dp) :: toosml, cost, safety, expon, stability_radius, tan_angle, &
    rs, rs1, rs2, rs3, rs4
integer(i4) :: order, last_stage, max_stiff_iters, no_of_ge_steps
logical :: fsal

real(kind=dp) :: ge_max_contrib
real(kind=dp) :: t_ge_max_contrib                                    !indep!
integer(i4) :: ge_f_count
real(kind=dp), dimension(:), pointer :: ge_assess                    !shp-dep!

real(kind=dp), dimension(:), pointer :: ge_y, ge_yp, ge_y_new        !dep!
real(kind=dp), dimension(:), pointer :: ge_err_estimates             !dep!
real(kind=dp), dimension(:,:), pointer :: ge_stages                  !dep!
!dir$ attributes align : 64 :: ge_assess
!dir$ attributes align : 64 :: ge_y,ge_yp,ge_y_new
!dir$ attributes align : 64 :: ge_err_estimates
!dir$ attributes align : 64 :: ge_stages
logical :: erason, erasfl

real(kind=dp) :: mcheps, dwarf, round_off, sqrrmc, cubrmc, sqtiny
integer(i4) :: outch

logical :: print_message, use_range

character(len=80) :: rec(10)

real(kind=dp) :: tlast, range_t_end                                  !indep!

real(kind=dp), dimension(:), pointer :: xstage, ytemp                !dep!
real(kind=dp), dimension(:,:), pointer :: p                          !dep!
!dir$ attributes align : 64 :: xstage,ytemp
!dir$ attributes align : 64 :: p

integer(i4) :: stiff_bad_step_count, hit_t_end_count
real(kind=dp) :: errold
logical :: chkeff, phase2

integer(i4), dimension(7) :: save_states

logical :: stop_on_fatal, saved_fatal_err

end type rk_comm_real_1d
!enda!
interface setup
   module procedure setup_r1
end interface
interface range_integrate
   module procedure range_integrate_r1
end interface
interface step_integrate
   module procedure step_integrate_r1
end interface
interface statistics
   module procedure statistics_r1
end interface
interface global_error
   module procedure global_error_r1
end interface
interface reset_t_end
   module procedure reset_t_end_r1
end interface
interface interpolate
   module procedure interpolate_r1
end interface
interface set_stop_on_fatal
   module procedure set_stop_on_fatal_r1
end interface
interface get_saved_fatal
   module procedure get_saved_fatal_r1
end interface
interface collect_garbage
   module procedure collect_garbage_r1
end interface

contains
!startb!
subroutine machine_const(round_off,sqrrmc,cubrmc,sqtiny,outch)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
real(kind=dp), intent(out) :: round_off, sqrrmc, cubrmc, sqtiny
integer(i4), intent(out) :: outch
!
real(kind=dp) :: dummy
real(kind=dp), parameter :: third=1.0_dp/3.0_dp, ten=10.0_dp
!
outch = 6
!
round_off = ten*epsilon(dummy)
sqrrmc = sqrt(epsilon(dummy))
cubrmc = epsilon(dummy)**third
sqtiny = sqrt(tiny(dummy))
!
end subroutine machine_const

subroutine method_const(rk_method, a, b, c, bhat, r, e, ptr, no_of_stages, &
                        intrp_degree, intrp_able, intrp_needs_stages, &
                        cost, safety, expon, stability_radius, &
                        tan_angle, rs, rs1, rs2, rs3, rs4, order, last_stage, &
                        max_stiff_iters, no_of_ge_steps, fsal, cdiff)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
integer(i4), intent(in) :: rk_method
real(kind=dp), intent(out) :: a(13,13), b(13), c(13), bhat(13), r(11,6), e(7)
integer(i4), intent(out) :: ptr(13), no_of_stages, intrp_degree
logical, intent(out) :: intrp_able, intrp_needs_stages

real(kind=dp), intent(out) :: cost, safety, expon, stability_radius, &
    tan_angle, rs, rs1, rs2, rs3, rs4, cdiff
integer(i4), intent(out) :: order, last_stage, max_stiff_iters, no_of_ge_steps
logical, intent(out) :: fsal
!
integer(i4) :: i
real(kind=dp), parameter :: fivepc=0.05_dp,  one=1.0_dp, two=2.0_dp, &
   fifty=50.0_dp
!
select case (rk_method)
case(1)
!
!  METHD = 1.
!    This pair is from "A 3(2) Pair of Runge-Kutta Formulas" by P. Bogacki
!    and L.F. Shampine, Appl. Math. Lett., 2, pp. 321-325, 1989.  The authors
!    are grateful to P. Bogacki for his assistance in implementing the pair.
!
   no_of_stages = 4; fsal = .true.; order = 2
   tan_angle = 8.9_dp; stability_radius = 2.3_dp
   safety = 0.8_dp; intrp_able = .true.; intrp_degree = 3
   intrp_needs_stages = .false.; no_of_ge_steps = 3
!
   ptr(1:4) = (/ 0, 1, 2, 3 /)
!
   a(2,1) = 0.5_dp
   a(3,1) = 0.0_dp
   a(3,2) = 0.75_dp
   a(4,1) = 0.222222222222222222222222222222_dp
   a(4,2) = 0.333333333333333333333333333333_dp
   a(4,3) = 0.444444444444444444444444444444_dp
!
!  The coefficients BHAT refer to the formula used to advance the
!  integration, here the one of order 3.  The coefficients B refer
!  to the other formula, here the one of order 2. For this pair, BHAT
!  is not needed since FSAL = .TRUE.
!
   b(1) = 0.291666666666666666666666666667_dp
   b(2) = 0.25_dp
   b(3) = 0.333333333333333333333333333333_dp
   b(4) = 0.125_dp
!
   c(1) = 0.0_dp
   c(2) = 0.5_dp
   c(3) = 0.75_dp
   c(4) = 1.0_dp
!
case (2)
!
!  METHD = 2
!    This pair is from "An Efficient Runge-Kutta (4,5) Pair" by P. Bogacki
!    and L.F. Shampine, Rept. 89-20, Math. Dept., Southern Methodist
!    University, Dallas, Texas, USA, 1989.  The authors are grateful to
!    P. Bogacki for his assistance in implementing the pair.  Shampine and
!    Bogacki subsequently modified the formula to enhance the reliability of
!    the pair.  The original fourth order formula is used in an estimate of
!    the local error.  If the step fails, the computation is broken off.  If
!    the step is acceptable, the first evaluation of the next step is done,
!    i.e., the pair is implemented as FSAL and the local error of the step
!    is again estimated with a fourth order formula using the additional data.
!    The step must succeed with both estimators to be accepted.  When the
!    second estimate is formed, it is used for the subsequent adjustment of
!    the step size because it is of higher quality.  The two fourth order
!    formulas are well matched to leading order, and only exceptionally do
!    the estimators disagree -- problems with discontinuous coefficients are
!    handled more reliably by using two estimators as is global error
!    estimation.
!
   no_of_stages = 8; fsal = .true.; order = 4
   tan_angle = 5.2_dp; stability_radius = 3.9_dp
   safety = 0.8_dp; intrp_able = .true.
   intrp_needs_stages = .true.; intrp_degree = 6
   no_of_ge_steps = 2
!
   ptr(1:8) = (/ 0, 1, 2, 3, 4, 5, 6, 7 /)
!
   a(2,1) = 0.166666666666666666666666666667_dp !1.0_dp/6.0_dp
   a(3,1) = 0.074074074074074074074074074074_dp !2.0_dp/27.0_dp
   a(3,2) = 0.148148148148148148148148148148_dp !4.0_dp/27.0_dp
   a(4,1) = 0.133381924198250728862973760933_dp !183.0_dp/1372.0_dp
   a(4,2) = −0.472303206997084548104956268222_dp !-162.0_dp/343.0_dp
   a(4,3) = 1053.0_dp/1372.0_dp
   a(5,1) = 68.0_dp/297.0_dp
   a(5,2) = -4.0_dp/11.0_dp
   a(5,3) = 42.0_dp/143.0_dp
   a(5,4) = 1960.0_dp/3861.0_dp
   a(6,1) = 597.0_dp/22528.0_dp
   a(6,2) = 81.0_dp/352.0_dp
   a(6,3) = 63099.0_dp/585728.0_dp
   a(6,4) = 58653.0_dp/366080.0_dp
   a(6,5) = 4617.0_dp/20480.0_dp
   a(7,1) = 174197.0_dp/959244.0_dp
   a(7,2) = -30942.0_dp/79937.0_dp
   a(7,3) = 8152137.0_dp/19744439.0_dp
   a(7,4) = 666106.0_dp/1039181.0_dp
   a(7,5) = -29421.0_dp/29068.0_dp
   a(7,6) = 482048.0_dp/414219.0_dp
   a(8,1) = 587.0_dp/8064.0_dp
   a(8,2) = 0.0_dp
   a(8,3) = 4440339.0_dp/15491840.0_dp
   a(8,4) = 24353.0_dp/124800.0_dp
   a(8,5) = 387.0_dp/44800.0_dp
   a(8,6) = 2152.0_dp/5985.0_dp
   a(8,7) = 7267.0_dp/94080.0_dp
!
!  The coefficients B refer to the formula of order 4.
!
   b(1) = 2479.0_dp/34992.0_dp
   b(2) = 0.0_dp
   b(3) = 123.0_dp/416.0_dp
   b(4) = 612941.0_dp/3411720.0_dp
   b(5) = 43.0_dp/1440.0_dp
   b(6) = 2272.0_dp/6561.0_dp
   b(7) = 79937.0_dp/1113912.0_dp
   b(8) = 3293.0_dp/556956.0_dp
!
!  The coefficients E refer to an estimate of the local error based on
!  the first formula of order 4.  It is the difference of the fifth order
!  result, here located in A(8,:), and the fourth order result.  By
!  construction both E(2) and E(7) are zero.
!
   e(1) = -3.0_dp/1280.0_dp
   e(2) = 0.0_dp
   e(3) = 6561.0_dp/632320.0_dp
   e(4) = -343.0_dp/20800.0_dp
   e(5) = 243.0_dp/12800.0_dp
   e(6) = -1.0_dp/95.0_dp
   e(7) = 0.0_dp
!
   c(1) = 0.0_dp
   c(2) = 1.0_dp/6.0_dp
   c(3) = 2.0_dp/9.0_dp
   c(4) = 3.0_dp/7.0_dp
   c(5) = 2.0_dp/3.0_dp
   c(6) = 3.0_dp/4.0_dp
   c(7) = 1.0_dp
   c(8) = 1.0_dp
!
!  To do interpolation with this pair, some extra stages have to be computed.
!  The following additional A and C coefficients are for this purpose.
!  In addition there is an array R that plays a role for interpolation
!  analogous to that of BHAT for the basic step.
!
   c(9) = 1.0_dp/2.0_dp
   c(10) = 5.0_dp/6.0_dp
   c(11) = 1.0_dp/9.0_dp
!
   a(9,1) = 455.0_dp/6144.0_dp
   a(10,1) = -837888343715.0_dp/13176988637184.0_dp
   a(11,1) = 98719073263.0_dp/1551965184000.0_dp
   a(9,2) = 0.0_dp
   a(10,2) = 30409415.0_dp/52955362.0_dp
   a(11,2) = 1307.0_dp/123552.0_dp
   a(9,3) = 10256301.0_dp/35409920.0_dp
   a(10,3) = -48321525963.0_dp/759168069632.0_dp
   a(11,3) = 4632066559387.0_dp/70181753241600.0_dp
   a(9,4) = 2307361.0_dp/17971200.0_dp
   a(10,4) = 8530738453321.0_dp/197654829557760.0_dp
   a(11,4) = 7828594302389.0_dp/382182512025600.0_dp
   a(9,5) = -387.0_dp/102400.0_dp
   a(10,5) = 1361640523001.0_dp/1626788720640.0_dp
   a(11,5) = 40763687.0_dp/11070259200.0_dp
   a(9,6) = 73.0_dp/5130.0_dp
   a(10,6) = -13143060689.0_dp/38604458898.0_dp
   a(11,6) = 34872732407.0_dp/224610586200.0_dp
   a(9,7) = -7267.0_dp/215040.0_dp
   a(10,7) = 18700221969.0_dp/379584034816.0_dp
   a(11,7) = -2561897.0_dp/30105600.0_dp
   a(9,8) = 1.0_dp/32.0_dp
   a(10,8) = -5831595.0_dp/847285792.0_dp
   a(11,8) = 1.0_dp/10.0_dp
   a(10,9) = -5183640.0_dp/26477681.0_dp
   a(11,9) = -1.0_dp/10.0_dp
   a(11,10) = -1403317093.0_dp/11371610250.0_dp
!
   r(1:11,1) = 0.0_dp; r(2,1:6) = 0.0_dp
   r(1,6) = -12134338393.0_dp/1050809760.0_dp
   r(1,5) = -1620741229.0_dp/50038560.0_dp
   r(1,4) = -2048058893.0_dp/59875200.0_dp
   r(1,3) = -87098480009.0_dp/5254048800.0_dp
   r(1,2) = -11513270273.0_dp/3502699200.0_dp
!
   r(3,6) = -33197340367.0_dp/1218433216.0_dp
   r(3,5) = -539868024987.0_dp/6092166080.0_dp
   r(3,4) = -39991188681.0_dp/374902528.0_dp
   r(3,3) = -69509738227.0_dp/1218433216.0_dp
   r(3,2) = -29327744613.0_dp/2436866432.0_dp
!
   r(4,6) = -284800997201.0_dp/19905339168.0_dp
   r(4,5) = -7896875450471.0_dp/165877826400.0_dp
   r(4,4) = -333945812879.0_dp/5671036800.0_dp
   r(4,3) = -16209923456237.0_dp/497633479200.0_dp
   r(4,2) = -2382590741699.0_dp/331755652800.0_dp
!
   r(5,6) = -540919.0_dp/741312.0_dp
   r(5,5) = -103626067.0_dp/43243200.0_dp
   r(5,4) = -633779.0_dp/211200.0_dp
   r(5,3) = -32406787.0_dp/18532800.0_dp
   r(5,2) = -36591193.0_dp/86486400.0_dp
!
   r(6,6) = 7157998304.0_dp/374350977.0_dp
   r(6,5) = 30405842464.0_dp/623918295.0_dp
   r(6,4) = 183022264.0_dp/5332635.0_dp
   r(6,3) = -3357024032.0_dp/1871754885.0_dp
   r(6,2) = -611586736.0_dp/89131185.0_dp
!
   r(7,6) = -138073.0_dp/9408.0_dp
   r(7,5) = -719433.0_dp/15680.0_dp
   r(7,4) = -1620541.0_dp/31360.0_dp
   r(7,3) = -385151.0_dp/15680.0_dp
   r(7,2) = -65403.0_dp/15680.0_dp
!
   r(8,6) = 1245.0_dp/64.0_dp
   r(8,5) = 3991.0_dp/64.0_dp
   r(8,4) = 4715.0_dp/64.0_dp
   r(8,3) = 2501.0_dp/64.0_dp
   r(8,2) = 149.0_dp/16.0_dp
   r(8,1) = 1.0_dp
!
   r(9,6) = 55.0_dp/3.0_dp
   r(9,5) = 71.0_dp
   r(9,4) = 103.0_dp
   r(9,3) = 199.0_dp/3.0_dp
   r(9,2) = 16.0d0
!
   r(10,6) = -1774004627.0_dp/75810735.0_dp
   r(10,5) = -1774004627.0_dp/25270245.0_dp
   r(10,4) = -26477681.0_dp/359975.0_dp
   r(10,3) = -11411880511.0_dp/379053675.0_dp
   r(10,2) = -423642896.0_dp/126351225.0_dp
!
   r(11,6) = 35.0_dp
   r(11,5) = 105.0_dp
   r(11,4) = 117.0_dp
   r(11,3) = 59.0_dp
   r(11,2) = 12.0_dp
!
case (3)
!
!  METHD = 3
!    This pair is from "High Order Embedded Runge-Kutta Formulae" by P.J.
!    Prince and J.R. Dormand, J. Comp. Appl. Math.,7, pp. 67-75, 1981.  The
!    authors are grateful to P. Prince and J. Dormand for their assistance in
!    implementing the pair.
!
   no_of_stages = 13; fsal = .false.; order = 7
   tan_angle = 11.0_dp; stability_radius = 5.2_dp
   safety = 0.8_dp; intrp_able = .false.
   intrp_needs_stages = .false.; intrp_degree = 0
   no_of_ge_steps = 2
!
   ptr(1:13) = (/ 0, 1, 2, 1, 3, 2, 4, 5, 6, 7, 8, 9, 1 /)
!
   a(2,1) = 5.55555555555555555555555555556e-2_dp
   a(3,1) = 2.08333333333333333333333333333e-2_dp
   a(3,2) = 6.25e-2_dp
   a(4,1) = 3.125e-2_dp
   a(4,2) = 0.0_dp
   a(4,3) = 9.375e-2_dp
   a(5,1) = 3.125e-1_dp
   a(5,2) = 0.0_dp
   a(5,3) = -1.171875_dp
   a(5,4) = 1.171875_dp
   a(6,1) = 3.75e-2_dp
   a(6,2) = 0.0_dp
   a(6,3) = 0.0_dp
   a(6,4) = 1.875e-1_dp
   a(6,5) = 1.5e-1_dp
   a(7,1) = 4.79101371111111111111111111111e-2_dp
   a(7,2) = 0.0_dp
   a(7,3) = 0.0_dp
   a(7,4) = 1.12248712777777777777777777778e-1_dp
   a(7,5) = -2.55056737777777777777777777778e-2_dp
   a(7,6) = 1.28468238888888888888888888889e-2_dp
   a(8,1) = 1.6917989787292281181431107136e-2_dp
   a(8,2) = 0.0_dp
   a(8,3) = 0.0_dp
   a(8,4) = 3.87848278486043169526545744159e-1_dp
   a(8,5) = 3.59773698515003278967008896348e-2_dp
   a(8,6) = 1.96970214215666060156715256072e-1_dp
   a(8,7) = -1.72713852340501838761392997002e-1_dp
   a(9,1) = 6.90957533591923006485645489846e-2_dp
   a(9,2) = 0.0_dp
   a(9,3) = 0.0_dp
   a(9,4) = -6.34247976728854151882807874972e-1_dp
   a(9,5) = -1.61197575224604080366876923982e-1_dp
   a(9,6) = 1.38650309458825255419866950133e-1_dp
   a(9,7) = 9.4092861403575626972423968413e-1_dp
   a(9,8) = 2.11636326481943981855372117132e-1_dp
   a(10,1) = 1.83556996839045385489806023537e-1_dp
   a(10,2) = 0.0_dp
   a(10,3) = 0.0_dp
   a(10,4) = -2.46876808431559245274431575997_dp
   a(10,5) = -2.91286887816300456388002572804e-1_dp
   a(10,6) = -2.6473020233117375688439799466e-2_dp
   a(10,7) = 2.84783876419280044916451825422_dp
   a(10,8) = 2.81387331469849792539403641827e-1_dp
   a(10,9) = 1.23744899863314657627030212664e-1_dp
   a(11,1) = -1.21542481739588805916051052503_dp
   a(11,2) = 0.0_dp
   a(11,3) = 0.0_dp
   a(11,4) = 1.66726086659457724322804132886e1_dp
   a(11,5) = 9.15741828416817960595718650451e-1_dp
   a(11,6) = -6.05660580435747094755450554309_dp
   a(11,7) = -1.60035735941561781118417064101e1_dp
   a(11,8) = 1.4849303086297662557545391898e1_dp
   a(11,9) = -1.33715757352898493182930413962e1_dp
   a(11,10) = 5.13418264817963793317325361166_dp
   a(12,1) = 2.58860916438264283815730932232e-1_dp
   a(12,2) = 0.0_dp
   a(12,3) = 0.0_dp
   a(12,4) = -4.77448578548920511231011750971_dp
   a(12,5) = -4.3509301377703250944070041181e-1_dp
   a(12,6) = -3.04948333207224150956051286631_dp
   a(12,7) = 5.57792003993609911742367663447_dp
   a(12,8) = 6.15583158986104009733868912669_dp
   a(12,9) = -5.06210458673693837007740643391_dp
   a(12,10) = 2.19392617318067906127491429047_dp
   a(12,11) = 1.34627998659334941535726237887e-1_dp
   a(13,1) = 8.22427599626507477963168204773e-1_dp
   a(13,2) = 0.0_dp
   a(13,3) = 0.0_dp
   a(13,4) = -1.16586732572776642839765530355e1_dp
   a(13,5) = -7.57622116690936195881116154088e-1_dp
   a(13,6) = 7.13973588159581527978269282765e-1_dp
   a(13,7) = 1.20757749868900567395661704486e1_dp
   a(13,8) = -2.12765911392040265639082085897_dp
   a(13,9) = 1.99016620704895541832807169835_dp
   a(13,10) = -2.34286471544040292660294691857e-1_dp
   a(13,11) = 1.7589857770794226507310510589e-1_dp
   a(13,12) = 0.0_dp
!
!  The coefficients BHAT refer to the formula used to advance the
!  integration, here the one of order 8.  The coefficients B refer
!  to the other formula, here the one of order 7.
!
   bhat(1) = 4.17474911415302462220859284685e-2_dp
   bhat(2) = 0.0_dp
   bhat(3) = 0.0_dp
   bhat(4) = 0.0_dp
   bhat(5) = 0.0_dp
   bhat(6) = -5.54523286112393089615218946547e-2_dp
   bhat(7) = 2.39312807201180097046747354249e-1_dp
   bhat(8) = 7.0351066940344302305804641089e-1_dp
   bhat(9) = -7.59759613814460929884487677085e-1_dp
   bhat(10) = 6.60563030922286341461378594838e-1_dp
   bhat(11) = 1.58187482510123335529614838601e-1_dp
   bhat(12) = -2.38109538752862804471863555306e-1_dp
   bhat(13) = 2.5e-1_dp
!
   b(1) = 2.9553213676353496981964883112e-2_dp
   b(2) = 0.0_dp
   b(3) = 0.0_dp
   b(4) = 0.0_dp
   b(5) = 0.0_dp
   b(6) = -8.28606276487797039766805612689e-1_dp
   b(7) = 3.11240900051118327929913751627e-1_dp
   b(8) = 2.46734519059988698196468570407_dp
   b(9) = -2.54694165184190873912738007542_dp
   b(10) = 1.44354858367677524030187495069_dp
   b(11) = 7.94155958811272872713019541622e-2_dp
   b(12) = 4.44444444444444444444444444445e-2_dp
   b(13) = 0.0_dp
!
   c(1) = 0.0_dp
   c(2) = 5.55555555555555555555555555556e-2_dp
   c(3) = 8.33333333333333333333333333334e-2_dp
   c(4) = 1.25e-1_dp
   c(5) = 3.125e-1_dp
   c(6) = 3.75e-1_dp
   c(7) = 1.475e-1_dp
   c(8) = 4.65e-1_dp
   c(9) = 5.64865451382259575398358501426e-1_dp
   c(10) = 6.5e-1_dp
   c(11) = 9.24656277640504446745013574318e-1_dp
   c(12) = 1.0_dp
   c(13) = c(12)
!
end select
!
!  The definitions of all pairs come here for the calculation of
!  LAST_STAGE - the position of the last evaluated stage in a method
!  RS1, RS2, RS3, RS4 - minimum and maximum rations used is step selection
!  COST - the cost of a step
!  MAX_STIFF_ITERS - the number of iterations permitted in stiffness detection
!     There are at most Q = 3 function calls per iteration. MAX_STIFF_ITERS
!     is determined so that  Q*MAX_STIFF_ITERS <= 5% of the cost of
!     50 steps and 1 <= MAX_STIFF_ITERS <= 8. This limits the number of
!     function calls in each diagnosis of stiffness to 24.
!  EXPON - an exponent for use in step slection
!  CDIFF - a coefficent used in determining the minimum permissible step
!
last_stage = ptr(no_of_stages)
if (fsal) then
   cost = real(no_of_stages-1,kind=wp)
else
   cost = real(no_of_stages,kind=wp)
end if
!
max_stiff_iters = min(8,max(1,int(fivepc*cost*fifty)))
!
expon = one/(order+one)
!
!     In calculating CDIFF it is assumed that there will be a non-zero
!     difference |C(I) - C(J)| less than one. If C(I) = C(J) for any I not
!     equal to J, they should be made precisely equal by assignment.
!
cdiff = one
do i = 1, no_of_stages - 1
   cdiff = min( cdiff, minval( &
  abs((c(i)-c(i+1:no_of_stages))), &
       mask=(c(i)-c(i+1:no_of_stages)/=0)) )
end do
!
rs = two; rs1 = one/rs; rs2 = rs**2
rs3 = rs*rs2; rs4 = one/rs3
!
end subroutine method_const
!endb!
!startc!
subroutine setup_r1(comm,t_start,y_start,t_end,tolerance,thresholds, &
                    method,task,error_assess,h_start,message)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
real(kind=dp), intent(in) :: t_end, t_start                          !indep!
real(kind=dp), intent(in) :: tolerance
real(kind=dp), dimension(:), intent(in) :: y_start                   !dep!
real(kind=dp), dimension(:), intent(in) :: thresholds                !shp-dep!
type(rk_comm_real_1d) :: comm
real(kind=dp), intent(in), optional :: h_start                       !indep!
logical, intent(in), optional :: error_assess, message
character(len=*), intent(in), optional :: task, method
!
character(len=*), parameter :: srname="SETUP"
!
real(kind=dp) :: hmin                                                !indep!
real(kind=dp) :: cdiff
integer(i4) :: ier, nrec, tr_dim_of_stages
logical :: legalt
character(len=1) :: task1, method1
!
integer(i4), parameter :: not_ready=-1, fatal=911, just_fine=1
real(kind=dp), parameter :: zero=0.0_dp, pt01=0.01_dp, fivepc=0.05_dp, &
   third=1.0_dp/3.0_dp, one=1.0_dp, two=2.0_dp, ten=10.0_dp, fifty=50.0_dp
!
ier = just_fine; nrec = 0
!
!  Clear previous state of the suite.
!
call setup_global_stuff
nullify(comm%thresh, comm%err_estimates, comm%weights, comm%y_old, &
   comm%scratch, &
   comm%y, comm%yp, comm%y_new, comm%yp_old, comm%stages, comm%ge_y, &
   comm%ge_yp, comm%ge_err_estimates, comm%ge_assess, comm%ge_y_new, &
   comm%ge_stages, comm%v0, comm%v1, comm%v2, comm%v3, comm%vtemp, &
   comm%xstage, comm%ytemp, comm%p)
!
!  Fetch output channel and machine constants;
!
call machine_const(comm%round_off,comm%sqrrmc,comm%cubrmc,comm%sqtiny, &
                   comm%outch)
!
body: do
!
!  Check for valid shape
   if (size(shape(y_start))>0) then
      if (any(shape(y_start)==0)) then
         ier = fatal; nrec = 2; write (comm%rec,"(a)") &
" ** An extent of Y_START has zero length. This is not permitted."
         exit body
      end if
   end if
!
!  Check and process non-trivial optional arguments
   if (present(task)) then
      task1 = task(1:1); comm%use_range = task1 == "R" .or. task1 == "r"
      legalt = comm%use_range  .or.  task1 == "S" .or. task1 == "s"
      if (.not.legalt) then
         ier = fatal; nrec = 2; write (comm%rec,"(a,a,a/a)") &
" ** You have set the first character of TASK to be '",TASK1,"'.", &
" ** It must be one of 'R','r','S' or 's'."
         exit body
      end if
   end if
   if (present(method)) then
      method1 = method(1:1)
      select case (method1)
      case("L","l"); comm%rk_method = 1
      case("M","m"); comm%rk_method = 2
      case("H","h"); comm%rk_method = 3
      case default
         ier = fatal; nrec = 2; write (comm%rec,"(a,a,a/a)") &
" ** You have set the first character of METHOD to be '",METHOD1,"'.", &
" ** It must be one of 'L','l','M','m','H' or 'h'."
         exit body
      end select
   end if
   if (present(message)) comm%print_message = message
!
! Check consistency of array arguments
!
   if (any(shape(y_start)/=shape(thresholds))) then
      ier = fatal; nrec = 1; write (comm%rec,"(a)") &
" ** The shapes of Y_START and THRESHOLDS are not consistent."
      exit body
   end if
!
! Check and process compulsory arguments
   if (t_start == t_end) then
      ier = fatal; nrec = 1; write (comm%rec,"(a,e13.5,a)") &
" ** You have set T_START = T_END = ",T_START,"."
      exit body
   else
      comm%t_end = t_end; comm%t_start = t_start
      comm%t_old = t_start; comm%t = t_start
      comm%dir = sign(one,t_end-t_start)
   end if
   if ((tolerance > pt01) .or. (tolerance < comm%round_off)) then
      ier = fatal; nrec = 2; write (comm%rec,"(a,e13.5,a/a,e13.5,a)") &
" ** You have set TOLERANCE = ",tolerance," which is not permitted. The", &
" ** range of permitted values is (",comm%round_off,",0.01)."
      exit body
   else
      comm%tol = tolerance
   end if
   if (minval(thresholds) < comm%sqtiny) then                        !spec-ar!
      ier = fatal; nrec = 2; write (comm%rec,"(a/a,e13.5,a)") &
" ** You have set a component of THRESHOLDS to be less than the permitted", &
" ** minimum,",comm%sqtiny,"."
      exit body
   end if
!
!  Set formula definitions and characteristics
   call method_const(comm%rk_method, comm%a, comm%b, comm%c, comm%bhat, &
        comm%r, comm%e, comm%ptr, comm%no_of_stages, comm%intrp_degree, &
        comm%intrp_able, comm%intrp_needs_stages,  comm%cost, &
        comm%safety, comm%expon, comm%stability_radius, comm%tan_angle, &
        comm%rs, comm%rs1, comm%rs2, comm%rs3, comm%rs4, comm%order, &
        comm%last_stage, comm%max_stiff_iters, comm%no_of_ge_steps, comm%fsal,&
        cdiff)
!
   tr_dim_of_stages = maxval(comm%ptr(2:comm%no_of_stages))
   comm%toosml = comm%round_off/cdiff
!
!  In STEP_INTEGRATE the first step taken will have magnitude H.  If
!  H_START = ABS(H_START) is not equal to zero, H = H_START.  If H_START is
!  equal to zero, the code is to find an on-scale initial step size H.  To
!  start this process, H is set here to an upper bound on the first step
!  size that reflects the scale of the independent variable.
!  RANGE_INTEGRATE has some additional information, namely the first output
!  point, that is used to refine this bound in STEP_INTEGRATE when called
!  from RANGE_INTEGRATE.  If H_START is not zero, but it is either too big
!  or too small, the input H_START is ignored and H_START is set to zero to
!  activate the automatic determination of an on-scale initial step size.
!  
   hmin = max(comm%sqtiny,comm%toosml*max(abs(t_start),abs(t_end)))
   if (abs(t_end-t_start) < hmin) then
      ier = fatal; nrec = 4; write (comm%rec,"(a/a/a,e13.5,a/a,e13.5,a)") &
" ** You have set values for T_END and T_START that are not clearly", &
" ** distinguishable for the method and the precision of the computer", &
" ** being used. ABS(T_END-T_START) is ",ABS(T_END-T_START)," but should be", &
" **  at least ",hmin,"."
     exit body
   end if
   if (present(h_start)) comm%h_start = abs(h_start)
   if (comm%h_start > abs(t_end-t_start) .or. comm%h_start < hmin) &
      comm%h_start = zero
   if (comm%h_start == zero) then
      comm%h = max(abs(t_end-t_start)/comm%rs3,hmin)
   else
      comm%h = comm%h_start
   end if
!
!  Allocate a number of arrays using pointers.
!
   allocate(comm%thresh(size(y_start,1)), &                          !alloc!
            comm%err_estimates(size(y_start,1)), &                   !alloc!
            comm%weights(size(y_start,1)), &                         !alloc!
            comm%y_old(size(y_start,1)), &                           !alloc!
            comm%scratch(size(y_start,1)), &                         !alloc!
            comm%y(size(y_start,1)), &                               !alloc!
            comm%yp(size(y_start,1)), &                              !alloc!
            comm%stages(size(y_start,1),tr_dim_of_stages), &         !alloc!
            comm%ymax(size(y_start,1)),stat=ier)                     !alloc!
   if (ier /= 0) then
      ier = fatal; nrec = 1 ; write (comm%rec,"(a)") &
" ** Not enough storage available to create workspace required internally."
      exit body
   else
      comm%y = y_start; comm%ymax = abs(y_start) 
      comm%thresh = thresholds;
      comm%y_new => comm%scratch; comm%yp_old => comm%scratch
      comm%v0 => comm%err_estimates; comm%vtemp => comm%scratch
      comm%v1 => comm%stages(:,1); comm%v2 => comm%stages(:,2)
      comm%v3 => comm%stages(:,3)
   end if
!
!  Pre-allocate storage for interpolation if the TASK = `R' was specified. 
!
   if (comm%use_range) then
      if (comm%intrp_able) then
         if (comm%rk_method==1) then
            comm%p => comm%stages(:,1:2)
         else if (comm%rk_method==2) then
            allocate(comm%p(size(y_start,1),5), &                    !alloc!
                     comm%ytemp(size(y_start,1)), &                  !alloc!
                     comm%xstage(size(y_start,1)),stat=ier)          !alloc!
            if (ier /= 0) then
               ier = fatal; nrec = 1 ; write (comm%rec,"(a)") &
" ** Not enough storage available to create workspace required internally."
               exit body
            end if
         end if
      end if
   end if
!
!  Initialise state and allocate storage for global error assessment
!
   comm%t_ge_max_contrib = t_start
   if (present(error_assess)) comm%erason = error_assess
   if (comm%erason) then
!
!  Storage is required for the stages of a secondary integration. The
!  stages of the primary intergration can only be overwritten in the
!  cases where there is no interpolant or the interpolant does not
!  require information about the stages (e.g. METHOD 'H' and METHOD 'L',
!  respectively).
      if (.not.comm%intrp_needs_stages) then
         comm%ge_stages => comm%stages
      else
         allocate(comm%ge_stages(size(y_start,1),tr_dim_of_stages),stat=ier) !alloc!
         if (ier /= 0) then
            ier = fatal; nrec = 1 ; write (comm%rec,"(a)") &
" ** Not enough storage available to create workspace required internally."
            exit body
         end if
      end if
      allocate(comm%ge_y(size(y_start,1)), &                         !alloc!
               comm%ge_yp(size(y_start,1)), &                        !alloc!
               comm%ge_err_estimates(size(y_start,1)), &             !alloc!
               comm%ge_assess(size(y_start,1)), &                    !alloc!
               comm%ge_y_new(size(y_start,1)),stat=ier)              !alloc!
      if (ier /= 0) then
         ier = fatal; nrec = 1 ; write (comm%rec,"(a)") &
" ** Not enough storage available to create workspace required internally."
         exit body
      else
         comm%ge_assess = 0.0_dp; comm%ge_y = y_start
      end if
   end if
   exit body
end do body
!
call rkmsg_r1(ier,srname,nrec,comm)
!
contains

subroutine setup_global_stuff
!
comm%h_start=0.0_dp; comm%h_old=0.0_dp
comm%f_count=0; comm%full_f_count=0; comm%step_count=0; comm%bad_step_count=0
comm%at_t_start=.true.;  comm%at_t_end=.false.
comm%rk_method=2; 
comm%ge_max_contrib=0.0_dp; comm%ge_f_count=0
comm%erason=.false.;  comm%erasfl=.false.
comm%print_message=.true.;  comm%use_range=.true.
comm%stiff_bad_step_count=0;  comm%hit_t_end_count=0
comm%errold=0.0_dp;  comm%h_average=0.0_dp
comm%chkeff=.false.;  comm%phase2=.true.
comm%save_states(1:7)=not_ready
comm%stop_on_fatal=.true.;  comm%saved_fatal_err=.false.
!
end subroutine setup_global_stuff

end subroutine setup_r1


subroutine collect_garbage_r1(comm)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
! Modified by I.Gladwell (Aug 2002)
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
type(rk_comm_real_1d) :: comm
!
if (associated(comm%thresh)) then
   deallocate(comm%thresh); nullify(comm%thresh)
end if
if (associated(comm%y)) then
   deallocate(comm%y); nullify(comm%y)
end if
if (associated(comm%yp)) then
   deallocate(comm%yp); nullify(comm%yp)
end if
if (associated(comm%ymax)) then
   deallocate(comm%ymax); nullify(comm%ymax)
end if
if (associated(comm%scratch)) then
   deallocate(comm%scratch); nullify(comm%scratch)
   nullify(comm%y_new); nullify(comm%yp_old); nullify(comm%vtemp)
end if
if (associated(comm%weights)) then
   deallocate(comm%weights); nullify(comm%weights)
end if
if (associated(comm%ytemp)) then
   deallocate(comm%ytemp); nullify(comm%ytemp)
end if
if (associated(comm%y_old)) then
   deallocate(comm%y_old); nullify(comm%y_old)
end if
if (associated(comm%err_estimates)) then
   deallocate(comm%err_estimates); nullify(comm%err_estimates)
   nullify(comm%v0)
end if
if (associated(comm%p,comm%stages(:,1:2))) then
   nullify(comm%p)
end if
if (associated(comm%ge_stages,comm%stages)) then
   deallocate(comm%stages); nullify(comm%stages); nullify(comm%ge_stages);
   nullify(comm%v1,comm%v2,comm%v3)
else if (associated(comm%ge_stages)) then
   deallocate(comm%ge_stages); nullify(comm%ge_stages)
end if
if (associated(comm%ge_y_new)) then
   deallocate(comm%ge_y_new); nullify(comm%ge_y_new)
end if
if (associated(comm%ge_assess)) then
   deallocate(comm%ge_assess); nullify(comm%ge_assess)
end if
if (associated(comm%ge_err_estimates)) then
   deallocate(comm%ge_err_estimates); nullify(comm%ge_err_estimates)
end if
if (associated(comm%ge_yp)) then
   deallocate(comm%ge_yp); nullify(comm%ge_yp)
end if
if (associated(comm%ge_y)) then
   deallocate(comm%ge_y); nullify(comm%ge_y)
end if
if (associated(comm%xstage)) then
   deallocate(comm%xstage); nullify(comm%xstage)
end if
if (associated(comm%p)) then
   deallocate(comm%p); nullify(comm%p)
end if
if (associated(comm%stages)) then
  deallocate(comm%stages); nullify(comm%stages)
end if
!
end subroutine collect_garbage_r1

recursive subroutine range_integrate_r1(comm,f,t_want,t_got,y_got,yderiv_got, &
                                        flag)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
real(kind=dp), intent(in) :: t_want                                  !indep!
real(kind=dp), intent(out) :: t_got                                  !indep!
real(kind=dp), dimension(:), intent(out) :: y_got, yderiv_got        !dep!
integer(i4), intent(out), optional :: flag
type(rk_comm_real_1d), intent(inout) :: comm
!
interface 
   function f(t,y)
      use rksuite_90_prec, only:wp
      real(kind=dp), intent(in) :: t                                 !indep!
      real(kind=dp), dimension(:), intent(in) :: y                   !dep!
      real(kind=dp), dimension(size(y,1)) :: f                       !dep!
   end function f
end interface
!
character(len=*), parameter :: srname="RANGE_INTEGRATE"
!
real(kind=dp) :: hmin, t_now                                         !indep!
integer(i4) :: step_flag, ier, nrec, state
logical :: goback, baderr
!
integer(i4), parameter :: not_ready=-1, usable=-2, fatal=911, catastrophe=912, &
   just_fine=1
logical, parameter :: tell=.false., ask=.true.
real(kind=dp), parameter :: zero=0.0_dp
!
ier = just_fine; nrec = 0
goback = .false.; baderr = .false.
body: do
!
!  Is it permissible to call RANGE_INTEGRATE?
!
   state = get_saved_state_r1("SETUP",comm%save_states)
   if (state==fatal) then
      ier = catastrophe; nrec = 1; write (comm%rec,"(a)") &
" ** A catastrophic error has already been detected elsewhere."
      exit body
   end if
   if (state==not_ready) then
      ier = fatal; nrec = 1; write (comm%rec,"(a)") &
" ** You have not called SETUP, so you cannot use RANGE_INTEGRATE."
      exit body
   end if
   if (.not.comm%use_range) then
      ier = fatal; nrec = 2; write (comm%rec,"(a/a)") &
" ** You have called RANGE_INTEGRATE after you specified in SETUP that you",&
" ** were going to use STEP_INTEGRATE. This is not permitted."
      exit body
   end if
   state = get_saved_state_r1(srname,comm%save_states)
   if (state==5 .or. state==6) then
      ier = fatal; nrec = 1; write (comm%rec,"(a/a)") &
" ** This routine has already returned with a hard failure. You must call",&
" ** SETUP to start another problem."
      exit body
   end if
   state = usable
   call set_saved_state_r1(srname,state,comm)
!
   if (comm%at_t_start) then
!
!  First call.
!
!  A value of T_END is specified in SETUP. When INTRP_ABLE = .FALSE., as with
!  METHOD = 'H', output is obtained at the specified T_WANT by resetting T_END
!  to T_WANT.  At this point, before the integration gets started, this can
!  be done with a simple assignment.  Later it is done with a call to 
!  RESET_T_END. The original T_END is saved in RANGE_T_END.
!
      comm%range_t_end = comm%t_end
      if (.not.comm%intrp_able) comm%t_end = t_want
!
!  The last T_GOT returned is in the variable TLAST. T records how far the 
!  integration has advanced towards the specified T_END.  When output is 
!  obtained by interpolation, the integration goes past the T_GOT returned 
!  (T is closer to the specified T_END than T_GOT).
      comm%tlast = comm%t_start; t_got = comm%t_start
!
!  If the code is to find an on-scale initial step size H, a bound was placed
!  on H in SETUP.  Here the first output point is used to refine this bound.
      if (comm%h_start==zero) then
         comm%h = min(abs(comm%h),abs(t_want-comm%t_start))
         hmin = max(comm%sqtiny,comm%toosml* &
                    max(abs(comm%t_start),abs(comm%t_end)))
         comm%h = max(comm%h,hmin)
      end if
!
   else
!
!  Subsequent call.
!
      if (comm%tlast==comm%range_t_end) then
         ier = fatal; nrec = 3; write (comm%rec,"(a/a/a)") &
" ** You have called RANGE_INTEGRATE after reaching T_END. (Your last call",&
" ** to RANGE_INTEGRATE  resulted in T_GOT = T_END.)  To start a new",&
" ** problem, you will need to call SETUP."
         exit body
      end if
!
   end if
!
!  Check for valid T_WANT.
!
   if (comm%dir*(t_want-comm%tlast)<=zero) then
      ier = fatal; nrec = 3; write (comm%rec,"(a/a/a)") &
" ** You have made a call to RANGE_INTEGRATE with a T_WANT that does not lie",&
" ** between the previous value of T_GOT (T_START on the first call) and",&
" ** T_END. This is not permitted. Check your program carefully."
      exit body
   end if
   if (comm%dir*(t_want-comm%range_t_end)>zero) then
      hmin = max(comm%sqtiny,comm%toosml* &
             max(abs(t_want),abs(comm%range_t_end)))
      if (abs(t_want-comm%range_t_end)<hmin) then
         ier = fatal; nrec = 4; write (comm%rec,"(a/a/a/a)") &
" ** You have made a call to RANGE_INTEGRATE with a T_WANT that does not lie",&
" ** between the previous value of T_GOT (T_START on the first call) and",&
" ** T_END. This is not permitted. T_WANT is very close to T_END, so you may",&
" ** have meant to set it to be T_END exactly.  Check your program carefully."
      else
         ier = fatal; nrec = 3; write (comm%rec,"(a/a/a/a)") &
" ** You have made a call to RANGE_INTEGRATE with a T_WANT that does not lie",&
" ** between the previous value of T_GOT (T_START on the first call) and",&
" ** T_END. This is not permitted. Check your program carefully."
      end if
      exit body
   end if
   if (.not.comm%intrp_able) then
      hmin = max(comm%sqtiny,comm%toosml*max(abs(comm%tlast),abs(t_want)))
      if (abs(t_want-comm%tlast)<hmin) then
         ier = fatal; nrec = 4; write (comm%rec,"(a/a/a/a,e13.5,a)") &
" ** You have made a call to RANGE_INTEGRATE with a T_WANT that is not",&
" ** sufficiently different from the last value of T_GOT (T_START on the",&
" ** first call). When using METHOD = 'H', it must differ by at least ",&
" ** ",HMIN,"."
         exit body
      end if
!
!  We have a valid T_WANT. There is no interpolation with this METHOD and
!  therefore we step to T_WANT exactly by resetting T_END with a call to 
!  RESET_T_END. On the first step this matter is handled differently as
!  explained above.
!
      if (.not.comm%at_t_start) then
         call reset_t_end(comm,t_want)
         baderr = get_saved_fatal_r1(comm)
         if (baderr) exit body
      end if
   end if
!
!  Process output, decide whether to take another step.
!
   proceed: do
!
      if (comm%intrp_able) then
!
!  Interpolation is possible with this METHOD.  The integration has
!  already reached T. If this is past T_WANT, GOBACK is set .TRUE. and
!  the answers are obtained by interpolation.
!
         goback = comm%dir*(comm%t-t_want) >= zero
         if (goback) then
            call interpolate(comm,f,t_want,y_got,yderiv_got)
            baderr = get_saved_fatal_r1(comm)
            if (baderr) exit body
            t_got = t_want
         end if
      else
!
!  Interpolation is not possible with this METHOD, so output is obtained
!  by integrating to T_WANT = T_END.  Both Y_GOT and YDERIV_GOT are then 
!  already loaded with the solution at T_WANT by STEP_INTEGRATE.
!
         goback = comm%t == t_want
         if (goback) t_got = t_want
      end if
!
!  If done, go to the exit point.
      if (goback) exit body
!
!  Take a step with STEP_INTEGRATE in the direction of T_END.  On exit, the
!  solution is advanced to T_NOW.  The approximate solution at T_NOW is
!  available in Y_GOT.  If output is obtained by stepping to the end (T_NOW
!  = T_WANT = T_END), Y_GOT can be returned directly.  If output is
!  obtained by interpolation, the subroutine INTERPOLATE that does this uses
!  the values in COMM for its computations and places the approximate solution
!  at T_WANT in the arrays Y_GOT,YDERIV_GOT for return to the calling
!  program. T_NOW is output from STEP_INTEGRATE and is actually a copy of T
!  from inside COMM.

      call step_integrate(comm,f,t_now,y_got,yderiv_got,step_flag)
      ier = step_flag 
!  
!  A successful step by STEP_INTEGRATE is indicated by step_flag= 1.
!
      select case(step_flag)
         case(1); cycle proceed
         case(2); nrec = 4; write (comm%rec,"(a/a/a/a)") &
" ** The last message was produced on a call to STEP_INTEGRATE from",&
" ** RANGE_INTEGRATE. In RANGE_INTAGRATE the appropriate action is to",&
" ** change to METHOD = 'M', or, if insufficient memory is available,",&
" ** to METHOD = 'L'. "
         case(3:6); nrec = 2; write (comm%rec,"(a)") &
" ** The last message was produced on a call to STEP_INTEGRATE from",&
" ** RANGE_INTEGRATE."
         case default; baderr = .true.
      end select
      t_got = comm%t; exit body
   end do proceed
!
end do body
!
if (baderr) then
   ier = fatal; nrec = 3; write (comm%rec,"(a/a/a)") &
" ** An internal call by RANGE_INTEGRATE to a subroutine resulted in an",&
" ** error that should not happen. Check your program carefully for array",&
" ** sizes, correct number of arguments, type mismatches ... ."
end if
!
comm%tlast = t_got
!
!  All exits are done here after a call to RKMSG_R1 to report
!  what happened
!
call rkmsg_r1(ier,srname,nrec,comm,flag)
!
end subroutine range_integrate_r1

recursive subroutine step_integrate_r1(comm,f,t_now,y_now,yderiv_now,flag)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
real(kind=dp), intent(out) :: t_now                                  !indep!
integer(i4), intent(out), optional :: flag
type(rk_comm_real_1d), intent(inout) :: comm
real(kind=dp), dimension(:), intent(out) :: y_now, yderiv_now        !dep!
interface
   function f(t,y)
      use rksuite_90_prec, only:wp
      real(kind=dp), intent(in) :: t                                 !indep!
      real(kind=dp), dimension(:), intent(in) :: y                   !dep!
      real(kind=dp), dimension(size(y,1)) :: f                       !dep!
   end function f
end interface
!
character(len=*), parameter :: srname="STEP_INTEGRATE"
!
real(kind=dp) :: hmin, htry                                          !indep!
real(kind=dp) :: alpha, beta, err, tau, t1, t2, ypnorm, extra_wk
integer(i4) :: ier, nrec, state
logical :: failed, phase1, phase3, toomch, sure_stiff
!
integer(i4), parameter :: not_ready=-1, usable=-2, fatal=911, catastrophe=912, &
    max_f_count=5000, just_fine=1
logical, parameter :: tell=.false., ask=.true.
real(kind=dp),parameter :: zero=0.0_dp, pt1=0.1_dp, pt9=0.9_dp, one=1.0_dp, &
   two=2.0_dp, hundrd=100.0_dp
!
ier = just_fine; nrec = 0
!
!  Is it permissible to call STEP_INTEGRATE?
!
body: do
   state = get_saved_state_r1("SETUP",comm%save_states)
   if (state==fatal) then
      ier = catastrophe; nrec = 1; write (comm%rec,"(a)") &
" ** A catastrophic error has already been detected elsewhere."
      exit body
   end if
   if (state==not_ready) then
      ier = fatal; nrec = 1; write (comm%rec,"(a)") &
" ** You have not called SETUP, so you cannot use STEP_INTEGRATE."
     exit body
   end if
   if (comm%use_range) then
      if (get_saved_state_r1("RANGE_INTEGRATE",comm%save_states)/=usable) then
         ier = fatal; nrec = 2; write (comm%rec,"(a/a)") &
" ** You have called STEP_INTEGRATE after you specified in SETUP that you", &
" ** were going to use RANGE_INTEGRATE. This is not permitted."
         comm%use_range = .false.
         exit body
      end if
   end if
   state = get_saved_state_r1(srname,comm%save_states)
   if (state==5 .or. state==6) then
      ier = fatal; nrec = 3; write (comm%rec,"(a/a/a)") &
" ** STEP_INTEGRATE has already returned with a flag value of 5 or 6. You",&
" ** cannot continue integrating this problem. You must call SETUP to start ",&
" ** another problem."
      exit body
   end if
!
   if (comm%at_t_start) then
!
      comm%yp = f(comm%t,comm%y); comm%f_count = comm%f_count + 1
      if (comm%erason) comm%ge_yp = comm%yp
!
!  The weights for the control of the error depend on the size of the
!  solution at the beginning and at the end of the step. On the first
!  step we do not have all this information. Whilst determining the
!  initial step size we initialize each component of WEIGHTS to the
!  larger of the corresponding component of both abs(Y) and the threshold.
!
      comm%weights = max(abs(comm%y),comm%thresh)
!  
!  If H_START is equal to zero, the code is to find an on-scale initial
!  step size H.  STEP_INTEGRATE has an elaborate scheme of three phases for
!  finding such an H, and some preparations are made earlier.  In SETUP an
!  upper bound is placed on H that reflects the scale of the independent
!  variable.  RANGE_INTEGRATE, when used, refines this bound using the
!  first output point.  Here in STEP_INTEGRATE PHASE1 applies a rule of
!  thumb based on the error control, the order of the the formula, and the
!  size of the initial slope to get a crude approximation to an on-scale H.
!  PHASE2 may reduce H in the course of taking the first step.  PHASE3
!  repeatedly adjusts H and retakes the first step until H is on scale.
!  
!  A guess for the magnitude of the first step size H can be provided to SETUP
!  as H_START.  If it is too big or too small, it is ignored and the automatic
!  determination of an on-scale initial step size is activated.  If it is
!  acceptable, H is set to H_START in SETUP.  Even when H is supplied to
!  STEP_INTEGRATE, PHASE3 of the scheme for finding an on-scale initial step 
!  size is made active so that the code can deal with a bad guess.
!
      phase1 = comm%h_start == zero; comm%phase2 = phase1; phase3 = .true.
      if (phase1) then
         comm%h = abs(comm%h)
         ypnorm = max(zero, &
             maxval(abs(comm%yp)/comm%weights,mask=comm%y/=zero))    !spec-ar1!
         tau = comm%tol**comm%expon
         if (comm%h*ypnorm > tau) comm%h = tau/ypnorm
         hmin = max(comm%sqtiny,comm%toosml* &
                                max(abs(comm%t_start),abs(comm%t_end)))
         comm%h = comm%dir*max(comm%h,hmin)
         phase1 = .false.
      end if
!
   else
!
! Continuation call
!
      if (comm%at_t_end) then
         ier = fatal; nrec = 3; write (comm%rec,"(a,e13.5,a/a/a)") &
" ** You have already reached T_END ( = ",comm%t_end, "). To integrate",&
" ** furhter with the same problem you must call the routine RESET_T_END",&
" ** with a new value of T_END."
         exit body
      end if
   end if
!
!  Begin computation of a step here.
!
   failed = .false.
!
   take_step: do
!
      comm%h = sign(abs(comm%h),comm%dir)
!
!  Reduce the step size if necessary so that the code will not step
!  past T_END.  "Look ahead" to prevent unnecessarily small step sizes.
!
      comm%at_t_end = comm%dir*((comm%t+comm%h)-comm%t_end) >= zero
      if (comm%at_t_end) then
         comm%h = comm%t_end - comm%t
      else if (comm%dir*((comm%t+two*comm%h)-comm%t_end) >= zero) then
         comm%h = (comm%t_end-comm%t)/two
      end if
!
!  When the integrator is at T and attempts a step of H, the function
!  defining the differential equations will be evaluated at a number of
!  arguments between T and T+H.  If H is too small, these arguments cannot
!  be clearly distinguished in the precision available.
!
      hmin = max(comm%sqtiny,comm%toosml*max(abs(comm%t),abs(comm%t+comm%h)))
      if (abs(comm%h)<hmin) then
         ier = 5; nrec = 3; write (comm%rec,"(a/a,e13.5,a,e13.5,a/a)") &
" ** In order to satisfy your error requirements STEP_INTEGRATE would have",&
" ** to use a step size of ",comm%H," at T_NOW = ",comm%T," This is too",&
" ** small for the machine precision."
         exit body
      end if
!
!  Monitor the impact of output on the efficiency of the integration.
!
      if (comm%chkeff) then
         comm%hit_t_end_count = comm%hit_t_end_count + 1
         if (comm%hit_t_end_count>=100 .and. &
             comm%hit_t_end_count>=comm%step_count/3) then
            ier = 2; nrec = 5; write (comm%rec,"(a/a/a/a/a)") &
" ** More than 100 output points have been obtained by integrating to T_END.",&
" ** They have been sufficiently close to one another that the efficiency",&
" ** of the integration has been degraded. It would probably be (much) more",&
" ** efficient to obtain output by interpolating with INTERPOLATE (after",&
" ** changing to METHOD='M' if you are using METHOD = 'H')."
            comm%hit_t_end_count = 0; exit body
         end if
      end if
!
!  Check for stiffness and for too much work.  Stiffness can be
!  checked only after a successful step.
!
      if (.not.failed) then
!
!  Check for too much work.
         toomch = comm%f_count > max_f_count
         if (toomch) then
            ier = 3; nrec = 3; write (comm%rec,"(a,i6,a/a/a)") &
" ** Approximately ",max_f_count," function evaluations have been used to",&
" ** compute the solution since the integration started or since this", &
" ** message was last printed."
!
!  After this warning message, F_COUNT is reset to permit the integration
!  to continue.  The total number of function evaluations in the primary
!  integration is FULL_F_COUNT + F_COUNT
!
            comm%full_f_count = comm%full_f_count + comm%f_count
            comm%f_count = 0
         end if
!
!  Check for stiffness.  If stiffness is detected, the message about too
!  much work is augmented inside STIFF to explain that it is due to
!  stiffness.
!
         call stiff_r1(comm,f,toomch,sure_stiff)
         if (sure_stiff) then
!
!  Predict how much extra work will be needed to reach TND.
            extra_wk = (comm%cost*abs((comm%t_end-comm%t)/comm%h_average)) / &
                     real(comm%full_f_count+comm%f_count,kind=wp)
            ier = 4; nrec = nrec + 4 
            write (comm%rec(nrec-3:nrec),"(a/a,e13.5,a/a/a)") &
" ** Your problem has been diagnosed as stiff.  If the  situation persists,",&
" ** it will cost roughly ",extra_wk," times as much to reach T_END as it", &
" ** has cost to reach T_NOW. You should probably change to a code intended",&
" ** for stiff problems."
         end if
         if (ier/=just_fine) exit body
      end if
!
!  Take a step.  Whilst finding an on-scale H (PHASE2 = .TRUE.), the input
!  value of H might be reduced (repeatedly), but it will not be reduced
!  below HMIN.  The local error is estimated, a weight vector is formed,
!  and a weighted maximum norm, ERR, of the local error is returned.
!  The presence of the optional argument PHASE2 in the call to STEP 
!  indicates that this is the primary integration.
!
!  H is used by both STEP_INTEGRATE and STEP. Since it may be changed inside 
!  STEP, a local copy is made.
!
      htry = comm%h
      call step_r1(comm,f,comm%t,comm%y,comm%yp,comm%stages,comm%tol,htry, &
                   comm%y_new,comm%err_estimates,err,hmin,comm%phase2)
      comm%h = htry
!
!  Compare the norm of the local error to the tolerance.
!
      if (err > comm%tol) then
!
!  Failed step.  Reduce the step size and try again.
!
!  First step:  Terminate PHASE3 of the search for an on-scale step size.
!               The step size is not on scale, so ERR may not be accurate;
!               reduce H by a fixed factor.  Failed attempts to take the
!               first step are not counted.
!  Later step:  Use ERR to compute an "optimal" reduction of H.  More than
!               one failure indicates a difficulty with the problem and an
!               ERR that may not be accurate, so reduce H by a fixed factor.
!
         if (comm%at_t_start) then
            phase3 = .false.; alpha = comm%rs1
         else
            comm%bad_step_count = comm%bad_step_count + 1
            comm%stiff_bad_step_count = comm%stiff_bad_step_count + 1
            if (failed) then
               alpha = comm%rs1
            else
               alpha = comm%safety*(comm%tol/err)**comm%expon
               alpha = max(alpha,comm%rs1)
            end if
         end if
         comm%h = alpha*comm%h; failed = .true.; cycle take_step
      end if
!
!  Successful step.
!
!  Predict a step size appropriate for the next step.  After the first
!  step the prediction can be refined using an idea of H.A. Watts that
!  takes account of how well the prediction worked on the previous step.
!
      beta = (err/comm%tol)**comm%expon
      if (.not.comm%at_t_start) then
         t1 = (err**comm%expon)/comm%h
         t2 = (comm%errold**comm%expon)/comm%h_old
         if (t1<t2*hundrd .and. t2<t1*hundrd) beta = beta*(t1/t2)
      end if
      alpha = comm%rs3
      if (comm%safety < beta*alpha) alpha = comm%safety/beta
!
!  On the first step a search is made for an on-scale step size.  PHASE2
!  of the scheme comes to an end here because a step size has been found
!  that is both successful and has a credible local error estimate. Except
!  in the special case that the first step is also the last, the step is
!  repeated in PHASE3 as long as an increase greater than RS2 appears
!  possible.  An increase as big as RS3 is permitted.  A step failure
!  terminates PHASE3.
!
      if (comm%at_t_start) then
         comm%phase2 = .false.
         phase3 = phase3 .and. .not. comm%at_t_end .and. (alpha > comm%rs2)
         if (phase3) then
            comm%h = alpha*comm%h; cycle take_step
         end if
      end if
!
!  After getting on scale, step size changes are more restricted.
!
      alpha = min(alpha,comm%rs)
      if (failed) alpha = min(alpha,one)
      alpha = max(alpha,comm%rs1)
      comm%h_old = comm%h; comm%h = alpha*comm%h
!
!  For the diagnosis of stiffness, an average accepted step size, H_AVERAGE,
!  must be computed.
!
      if (comm%at_t_start) then
         comm%h_average = comm%h_old
      else
         comm%h_average = pt9*comm%h_average + pt1*comm%h_old
      end if
!
      comm%at_t_start = .false.; comm%errold = err; comm%t_old = comm%t
!
!  Take care that T is set to precisely T_END when the end of the
!  integration is reached.
!
      if (comm%at_t_end) then
         comm%t = comm%t_end
      else
         comm%t = comm%t + comm%h_old
      end if
!
!  Increment counter on accepted steps.  Note that successful steps
!  that are repeated whilst getting on scale are not counted.
!
      comm%step_count = comm%step_count + 1
!
!  Advance the current solution and its derivative. Note that the previous
!  derivative will overwrite Y_NEW (see pointer assignments in SETUP).
!
      comm%y_old = comm%y; comm%y = comm%y_new
      comm%yp_old = comm%yp
!
      if (comm%fsal) then
!
!  When FSAL = .TRUE., YP is the last stage of the step.
!
        comm%yp = comm%stages(:,comm%last_stage) 
      else
!
!  Call F to evaluate YP.
!
         comm%yp = f(comm%t,comm%y); comm%f_count = comm%f_count + 1
      end if
!
!  If global error assessment is desired, advance the secondary
!  integration from TOLD to T.
!
      if (comm%erason) then
         call truerr_r1(comm,f,ier)
         if (ier==6) then
!
!  The global error estimating procedure has broken down. Treat it as a
!  failed step. The solution and derivative are reset to their values at
!  the beginning of the step since the last valid error assessment refers
!  to them.
!
            comm%step_count = comm%step_count - 1; comm%erasfl = .true.
            comm%at_t_end = .false.
            comm%t = comm%t_old; comm%h = comm%h_old
            comm%y = comm%y_old; comm%yp = comm%yp_old
            if (comm%step_count > 0) then
               nrec = 2; write (comm%rec,"(a/a,e13.5/a)") &
" ** The global error assessment may not be reliable for T past ",&
" ** T_NOW = ",comm%t,". The integration is being terminated."
               exit body
            else
               nrec = 2; write (comm%rec,"(a/a)") &
" ** The global error assessment algorithm failed at the start of the ",&
" ** integration.  The integration is being terminated."
               exit body
            end if
         end if
      end if
      exit take_step
   end do take_step
   exit body
end do body
!
!  Exit point for STEP_INTEGRATE
!  Set the output variables and flag that interpolation is permitted
!
if (ier < fatal) then
   t_now = comm%t; comm%at_t_end = t_now == comm%t_end
   comm%chkeff = comm%at_t_end;
   y_now = comm%y; yderiv_now = comm%yp
   comm%ymax = max(abs(comm%y),comm%ymax)
   if (ier==just_fine) then
      state = usable; call set_saved_state_r1("INTERPOLATE",state,comm)
   end if
end if
!
!  Call RKMSG_R1 to report what happened and set FLAG.
!
call rkmsg_r1(ier,srname,nrec,comm,flag)
!
end subroutine step_integrate_r1


subroutine truerr_r1(comm,f,ier)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
type(rk_comm_real_1d), intent(inout) :: comm
integer(i4), intent(inout) :: ier
!
interface
   function f(t,y)
      use rksuite_90_prec, only:wp
      real(kind=dp), intent(in) :: t                                 !indep!
      real(kind=dp), dimension(:), intent(in) :: y                   !dep!
      real(kind=dp), dimension(size(y,1)) :: f                       !dep!
   end function f
end interface
!
real(kind=dp) :: hmin, hsec                                          !indep!
real(kind=dp) :: diff, errmax, mxerlc, tsec, ge_err, ge_test1, ge_test2
integer(i4) :: istep, level
!
integer(i4), parameter :: just_fine=1
real(kind=dp), parameter :: pt1=0.1_dp, ten=10.0_dp
real(kind=dp), dimension(:,:), pointer :: ge_stages                  !dep!
real(kind=dp), dimension(:), pointer :: ge_y, ge_yp, ge_y_new        !dep!
real(kind=dp), dimension(:), pointer :: ge_err_estimates, y          !dep!
real(kind=dp), dimension(:), pointer :: ge_assess, weights           !shp-dep!
!
ge_stages => comm%ge_stages
ge_y => comm%ge_y
ge_yp => comm%ge_yp
ge_y_new => comm%ge_y_new
ge_err_estimates => comm%ge_err_estimates
ge_assess => comm%ge_assess
y => comm%y
weights => comm%weights
!
tsec = comm%t - comm%h_old
hsec = comm%h_old/real(comm%no_of_ge_steps,kind=wp)
hmin = max(comm%sqtiny,comm%toosml*max(abs(tsec),abs(comm%t)))
body: do
   if (abs(hsec)<hmin) then
      ier = 6; exit body
   end if
   ge_test1 = comm%tol/real(comm%no_of_ge_steps,kind=wp)
   ge_test2 = comm%tol/ten; level = 0
!
!  The subroutine STEP is used to take a step.
!
!  Perform secondary integration.
!
   do istep = 1, comm%no_of_ge_steps
!
!  Take a step.
      call step_r1(comm,f,tsec,ge_y,ge_yp,ge_stages,ge_test1,hsec,ge_y_new, &
         ge_err_estimates,ge_err)
!
!  The primary integration is using a step size of H_OLD and the
!  secondary integration is using the smaller step size 
!      HSEC = H_OLD/(NO_OF_GE_STEPS).
!  If steps of this size were taken from the same starting point and the
!  asymptotic behavior were evident, the smaller step size would result in
!  a local error that is considerably smaller, namely by a factor of
!  1/(NO_OF_GE_STEPSSEC**(ORDER+1)).  If the two approximate solutions are
!  close and TOL is neither too large nor too small, this should be
!  approximately true.  The step size is chosen in the primary integration
!  so that the local error ERR is no larger than TOL.  The local error,
!  GE_ERR, of the secondary integration is compared to TOL in an attempt to
!  diagnose a secondary integration that is not rather more accurate than
!  the primary integration.
!
      if (ge_err>=ge_test1) then
         level = 2
      else if (ge_err>ge_test2) then
         level = level + 1
      end if
      if (level>=2) then
         ier = 6; exit body
      end if
!
!  Advance TSEC and the dependent variables GE_Y and GE_YP.
!
      tsec = comm%t - real(comm%no_of_ge_steps-istep,kind=wp)*hsec
      ge_y = ge_y_new
!
      if (comm%fsal) then
!
!  When FSAL = .TRUE., the derivative GE_YP is the last stage of the step.
!
         ge_yp = ge_stages(:,comm%last_stage)
      else
!
!  Call F to evaluate GE_YP.
!
         ge_yp = f(tsec,ge_y); comm%ge_f_count = comm%ge_f_count + 1
      end if
!
   end do
!
!  Update the maximum error seen, GE_MAX_CONTRIB, and its location, 
!  T_GE_MAX_CONTRIB. Use local variables ERRMAX and MXERLC.
!
   errmax = comm%ge_max_contrib; mxerlc = comm%t_ge_max_contrib; 
   diff = maxval(abs(ge_y-y)/weights)                                !spec-ar!
   if (diff>errmax) then
      errmax = diff; mxerlc = comm%t
   end if
!
!  If the global error is greater than 0.1, the solutions have diverged so
!  far that comparing them may not provide a reliable estimate of the global
!  error. The test is made before GE_ASSESS and GE_MAX_CONTRIB,
!  T_GE_MAX_CONTRIB are updated so that on a failure, they refer to the
!  last reliable results.
!
   if (errmax>pt1) then
      ier = 6
   else
      comm%ge_max_contrib = errmax; comm%t_ge_max_contrib = mxerlc; 
      ge_assess = ge_assess + (abs(ge_y-y)/weights)**2 
      ier = just_fine
   end if
   exit body
!
end do body
!
end subroutine truerr_r1

subroutine step_r1(comm,f,tnow,y,yp,stages,tol,htry,y_new,    &
                     errest,err,hmin,phase_2)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
type(rk_comm_real_1d), intent(inout), target :: comm
real(kind=dp), intent(out) :: err
real(kind=dp), intent(inout) :: htry                                 !indep!
real(kind=dp), intent(in) :: tnow                                    !indep!
real(kind=dp), intent(in) :: tol
real(kind=dp), intent(in), optional :: hmin                          !indep!
logical, intent(inout), optional :: phase_2
!
real(kind=dp), dimension(:), intent(in) :: y, yp                     !dep!
real(kind=dp), dimension(:), intent(out) ::  errest, y_new           !dep!
real(kind=dp), dimension(:,:), intent(out) :: stages                 !dep!
!
interface
   function f(t,y)
      use rksuite_90_prec, only:wp
      real(kind=dp), intent(in) :: t                                 !indep!
      real(kind=dp), dimension(:), intent(in) :: y                   !dep!
      real(kind=dp), dimension(size(y,1)) :: f                       !dep!
   end function f
end interface
!
real(kind=dp) :: tstg                                                !indep!
integer(i4) :: i, j
logical :: cutbak, main
!
intrinsic         abs, max, sign
!
real(kind=dp), dimension(:), pointer :: weights, thresh              !shp-dep!
real(kind=dp), dimension(:,:), pointer :: a                          !real!
real(kind=dp), dimension(:), pointer :: b, bhat, c                   !real!
integer(i4), dimension(:), pointer :: ptr                                !integer(i4)!
!
real(kind=dp), parameter :: zero=0.0_dp, half=0.5_dp, one=1.0_dp
!
!  ERREST is used for working storage in this computation.
!
weights => comm%weights
thresh => comm%thresh
a => comm%a
b => comm%b
bhat => comm%bhat
c => comm%c
ptr => comm%ptr
!
main = present(hmin) .and. present(phase_2)
attempt_step: do
!
   if (main) then
      if (comm%phase2) weights = max(thresh,abs(y))
   end if
!
   do i = 2, comm%no_of_stages
      errest = a(i,1)*yp
      do j = 2, i - 1
         if (a(i,j)/=zero) errest = errest + a(i,j)*stages(:,ptr(j))
      end do
      y_new = y + htry*errest
!
!  METHOD = 'M' is special in that an estimate of the local error can be
!  formed before the step is completed.  If the step is a failure,
!  return immediately.  Otherwise, complete the step and compute a more
!  accurate error estimate.
!
      if (comm%rk_method==2 .and. i==7) then
         call stepb
         if (err>tol) return
      end if
!
      tstg = tnow + c(i)*htry
      if (main .and. comm%at_t_end .and. c(i)==one) tstg = comm%t_end
      stages(:,ptr(i)) = f(tstg,y_new)
!
!  Increment the counter for the number of function evaluations
!  depending on whether the primary or secondary integration is taking
!  place.
!
      if (main) then
         comm%f_count = comm%f_count + 1
      else
         comm%ge_f_count = comm%ge_f_count + 1
      end if
!
!  When PHASE2 is .TRUE. we are in the second phase of the automatic
!  selection of the initial step size.  The results of the first three
!  stages are monitored in the subroutine STEPA for evidence that H is
!  too large -- instability and/or an unreliable estimate of the error
!  of the step is then possible.  When the subroutine believes H to be
!  too large, it returns CUTBAK = .TRUE. and a suitably reduced H for
!  another try.
!
      if (main) then
         if (phase_2) then
            if (i<=3 .and. abs(htry)>hmin) then
               call stepa(stages(:,ptr(i)),htry,cutbak)
               if (cutbak) then
                  comm%at_t_end = .false.
!
!  Make sure that STEPA does not reduce the step size below the
!  minimum. If it does, reset H to HMIN and deactivate PHASE2.
!
                  if (abs(htry)<=hmin) then
                     htry = sign(hmin,htry); comm%phase2 = .false.
                  end if
                  cycle attempt_step
               end if
            end if
         end if
      end if
!
   end do
!
!  Some formulas are constructed so that the last stage represents
!  the result of the step (FSAL=.TRUE.), hence if the step is acceptable,
!  it will be the first stage for the next step. When FSAL=.FALSE., we
!  have to complete the computation of the step.
!
   if (.not.comm%fsal) then
      errest = bhat(1)*yp
      do i = 2, comm%no_of_stages
         if (bhat(i)/=zero) errest = errest + bhat(i)*stages(:,ptr(i))
      end do
      y_new = y + htry*errest
   end if
!
!  Form an estimate of the error in the lower order formula by comparing
!  it to the higher order formula of the pair. ERREST has been used
!  as working storage above.  The higher order approximation has been
!  formed as Y_NEW = Y + HTRY*ERREST where ERREST is a linear
!  combination of the stages of the formula. The lower order result also
!  has the form Y plus HTRY times a different linear combination of
!  the stages. Hence, this different linear combination of stages for
!  the lower order formula can just be subtracted from the combination
!  stored in ERREST to produce the errors. The result is then
!  multiplied by HTRY to obtain the error estimate.
!
   if (b(1)/=zero) errest = errest - b(1)*yp
   do i = 2, comm%no_of_stages
      if (b(i)/=zero) errest = errest - b(i)*stages(:,ptr(i))
   end do
   errest = htry*errest
!
!  The error in a solution component is measured relative to a weight
!  that is the larger of a threshold and the size of the solution over
!  the step.  Using the magnitude of a solution component at both ends
!  of the step in the definition of "size" increases the robustness of
!  the test. When global error estimation is specified, the weight
!  vector WEIGHTS is defined by the primary integration and is then
!  used in the secondary integration.
!
   if (main) weights = max(half*(abs(y)+abs(y_new)),thresh)
!
   err = maxval(abs(errest/weights))                                 !spec-ar!
!
   exit attempt_step
!
end do attempt_step
!
contains
!
   subroutine stepa(ypnew,htry,cutbak)
!
   real(kind=dp), intent(inout) :: htry                              !indep!
   real(kind=dp), dimension(:), intent(in) :: ypnew                  !dep!
   logical, intent(out) :: cutbak
!
   real(kind=dp) :: argdif, fdiff, scl, tdiff, twt, ynrm, ystgnm
!
!  Update the weights to account for the current intermediate solution
!  approximation Y_NEW.  Compute the sizes of Y and Y_NEW in the
!  new norm.  The size of the Lipschitz constant is assessed by a difference
!  in the arguments Y, Y_NEW and a difference in the function evaluated
!  at these arguments.
!
   weights = max(weights,abs(y_new))
   ynrm = maxval(abs(y)/weights)                                     !spec-ar!
   ystgnm = maxval(abs(y_new)/weights)                               !spec-ar!
   argdif = maxval(abs(y_new-y)/weights)                             !spec-ar!
   fdiff = maxval(abs(ypnew-yp)/weights)                             !spec-ar!
!
!  The transformation of the equation to autonomous form is done
!  implicitly.  The difference of the arguments must take into account
!  the difference between the values of the independent variable T and
!  TSTG. The difference of the corresponding component of the function
!  is zero because of the way the standard transformation is done.
!
   tdiff = tstg - tnow
   twt = abs(comm%t_end-tnow)
   ynrm = max(ynrm,abs(tnow)/twt)
   ystgnm = max(ystgnm,abs(tstg)/twt)
   argdif = max(argdif,abs(tdiff)/twt)
!
!  The ratio FDIFF/ARGDIF is a lower bound for, and an approximation to,
!  a Lipschitz constant L for the differential equation written in 
!  autonomous form.  First we must ask if the difference ARGDIF is 
!  significant in the precision available.  If it appears to be, we insist
!  that abs(HTRY)*L be less than an approximate radius, STABILITY_RADIUS,
!  of the stability region of the method.  This is more stringent than 
!  necessary for stability, possibly a lot more stringent, but the aim is
!  to get an HTRY small enough that the error estimate for the step is
!  credible.  The reduction is required to be at least as much as the step
!  control parameter RS1. It is necessary to limit the reduction of HTRY
!  at any one time because we may be misled in the size of the reduction
!  that is appropriate due to nonlinearity of the differential equation
!  and to inaccurate weights caused by HTRY much too large.  The reduction
!  is not permitted to be more than the step control parameter RS4.
!
   cutbak = .false.
   if (argdif > comm%round_off*max(ynrm,ystgnm)) then
      if ((abs(htry)*fdiff) > (comm%stability_radius*argdif)) then
         scl = max(comm%rs4,min(comm%rs1, &
                   (comm%stability_radius*argdif)/(abs(htry)*fdiff)))
         htry = scl*htry; cutbak = .true.
      end if
   end if
!
   end subroutine stepa
!
   subroutine stepb 
!
   real(kind=dp), dimension(:), pointer :: e                         !real!
!
   e => comm%e
!
   if (main) then
      err = maxval( abs( e(1)*yp + e(3)*stages(:,ptr(3)) + &         !spec-ar!
                         e(4)*stages(:,ptr(4)) + e(5)*stages(:,ptr(5)) + &
                         e(6)*stages(:,ptr(6)) ) /  &
                    max ( half*(abs(y)+abs(y_new)), thresh ) )
   else
      err = maxval( abs( e(1)*yp + e(3)*stages(:,ptr(3)) + &         !spec-ar!
                         e(4)*stages(:,ptr(4)) + e(5)*stages(:,ptr(5)) + &
                         e(6)*stages(:,ptr(6)) ) / weights ) 
   end if
!
   err = abs(comm%h)*err
!
   end subroutine stepb
!
end subroutine step_r1


subroutine stiff_r1(comm,f,toomch,sure_stiff)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
type(rk_comm_real_1d), intent(inout), target :: comm
logical, intent(in) :: toomch
logical, intent(out) :: sure_stiff
!
interface 
   function f(t,y)
      use rksuite_90_prec, only:wp
      real(kind=dp), intent(in) :: t                                 !indep!
      real(kind=dp), dimension(:), intent(in) :: y                   !dep!
      real(kind=dp), dimension(size(y,1)) :: f                       !dep!
   end function f
end interface
!
logical :: maybe_stiff, lots_of_fails
!
real(kind=dp) :: alpha1, alpha2, beta1, beta2                        !dep!
real(kind=dp) :: rold, v1v0, v2v0, v2v1, v3v1, v3v2                  !dep!
real(kind=dp) :: dist, res2, scale, v0nrm, v3nrm, ynrm, rho, v0v0, v1v1, &
   v2v2, v3v3, yy, det1, det2
integer(i4) :: ntry
complex(kind=wp), dimension(2) :: root_pair, prv_root_pair
integer(i4), parameter :: bigr=1, smlr=2
real(kind=dp), dimension(:), pointer :: v0, v1, v2, v3, y, y_old     !dep!
real(kind=dp), dimension(:), pointer :: weights, thresh              !shp-dep!
!
integer(i4), parameter :: max_f_count=5000
real(kind=dp),parameter :: zero=0.0_dp, pt001=0.001_dp, pt9=0.9_dp, &
   fifth=0.2_dp, half=0.5_dp, one=1.0_dp, two=2.0_dp, five=5.0_dp, &
   large=1.0e+10_dp
!
v0 => comm%v0
v1 => comm%v1
v2 => comm%v2
v3 => comm%v3
weights => comm%weights
thresh => comm%thresh
y => comm%y
y_old => comm%y_old
!
sure_stiff = .false.
lots_of_fails = .false.
!
if (mod(comm%step_count-10,40)==0) then
   lots_of_fails = comm%stiff_bad_step_count >= 10
   comm%stiff_bad_step_count = 0
end if
!
!  If either too much work has been done or there are lots of failed steps,
!  test for stiffness.
!
maybe_stiff = toomch .or. lots_of_fails
if (maybe_stiff) then
!
!  Regenerate weight vector
!
   weights = max(half*(abs(y)+abs(y_old)),thresh)
   maybe_stiff = fifth <  abs(comm%h/comm%h_average) .and. &
                          abs(comm%h/comm%h_average) < five 
   if (maybe_stiff) then
!
!  The average step size is used to predict the cost in function evaluations
!  of finishing the integration to T_END.  If this cost is no more than 
!  MAX_F_COUNT, the problem is declared not stiff. If the step size is 
!  being restricted on grounds of stability, it will stay close to H_AVERAGE.
!  The prediction will then be good, but the cost is too low to consider
!  the problem stiff.  If the step size is not close to H_AVERAGE, the
!  problem is not stiff.  Either way there is no point to testing for a step
!  size restriction due to stability.
!
      maybe_stiff  = comm%cost*abs((comm%t_end-comm%t)/comm%h_average) > &
                     real(max_f_count,kind=wp)
      if (maybe_stiff) then
!
!  There have been many step failures or a lot of work has been done.  Now 
!  we must determine if this is due to the stability characteristics of the
!  formula.  This is done by calculating the dominant eigenvalues of the
!  local Jacobian and then testing whether H_AVERAGE corresponds to being
!  on the boundary of the stability region.
!  The size of Y provides scale information needed to approximate
!  the Jacobian by differences.
!
         v0v0 = wt_inner_prod(v0,v0)
         yy = wt_inner_prod(y,y)
         ynrm = sqrt(yy)
         scale = ynrm*comm%sqrrmc
         if (scale==zero) then
!
!  Degenerate case.  Y is (almost) the zero vector so the scale is not 
!  defined.  The input vector V0 is the difference between Y and a 
!  lower order approximation to the solution that is within the error 
!  tolerance.  When Y vanishes, V0 is itself an acceptable approximate
!  solution, so we take SCALE from it, if this is possible.
!
            scale = v0v0*comm%sqrrmc
            maybe_stiff = scale > zero
         end if
      end if
   end if
end if
!
if (.not. maybe_stiff) return
!
if (v0v0==zero) then
!
!  Degenerate case.  V0 is (almost) the zero vector so cannot
!  be used to define a direction for an increment to Y.  Try a
!  "random" direction.
!
   v0 = one;   v0v0 = wt_inner_prod(v0,v0)
end if
!
v0nrm = sqrt(v0v0)
v0 = v0/v0nrm; v0v0 = one
!
!  Use a nonlinear power method to estimate the two dominant eigenvalues.
!  V0 is often very rich in the two associated eigenvectors.  For this 
!  reason the computation is organized with the expectation that a minimal 
!  number of iterations will suffice.  Indeed, it is necessary to recognize 
!  a kind of degeneracy when there is a dominant eigenvalue.  The function
!  DOMINANT_EIGENVALUE does this.  In the first try, NTRY = 1, a Rayleigh 
!  quotient for such an eigenvalue is initialized as ROLD.  After each 
!  iteration, DOMINANT_EIGENVALUE computes a new Rayleigh quotient and
!  tests whether the two approximations agree to one tenth of one per cent
!  and the eigenvalue, eigenvector pair satisfy a stringent test on the 
!  residual.
!
ntry = 1
do
!
   v1 = approx_jacobian(f,v0,v0v0)
   v1v1 = wt_inner_prod(v1,v1)
!
!  The quantity SQRT(V1V1/V0V0) is a lower bound for the product of H_AVERAGE
!  and a Lipschitz constant.  If it should be LARGE, stiffness is not
!  restricting the step size to the stability region.  The principle is
!  clear enough, but the real reason for this test is to recognize an
!  extremely inaccurate computation of V1V1 due to finite precision
!  arithmetic in certain degenerate circumstances.
!
   if (sqrt(v1v1) > large*sqrt(v0v0)) return
!
   v1v0 = wt_inner_prod(v1,v0)
   if (ntry==1) then
      rold = v1v0/v0v0
!
!  This is the first Rayleigh quotient approximating the product of H_AVERAGE
!  and a dominant eigenvalue.  If it should be very small, the
!  problem is not stiff.  It is important to test for this possibility so
!  as to prevent underflow and degeneracies in the subsequent iteration.
!
      if (abs(rold) < comm%cubrmc) return
   else
!
      if (dominant_eigenvalue(v1v1,v1v0,v0v0)) exit
   end if
!
   v2 = approx_jacobian(f,v1,v1v1)
   v2v2 = wt_inner_prod(v2,v2)
   v2v0 = wt_inner_prod(v2,v0)
   v2v1 = wt_inner_prod(v2,v1)
   if (dominant_eigenvalue(v2v2,v2v1,v1v1)) exit
!
!  Fit a quadratic in the eigenvalue to the three successive iterates
!  V0,V1,V2 of the power method to get a first approximation to
!  a pair of eigenvalues.  A test made earlier in DOMINANT_EIGENVALUE
!  implies that the quantity DET1 here will not be too small.
!
   det1 = v0v0*v1v1 - v1v0*rev_wt_inner_prod(v1v0)
   alpha1 = (-v0v0*v2v1 + rev_wt_inner_prod(v1v0)*v2v0)/det1
   beta1 = (v1v0*v2v1 - v1v1*v2v0)/det1
!
!  Iterate again to get V3, test again for degeneracy, and then fit a
!  quadratic to V1,V2,V3 to get a second approximation to a pair
!  of eigenvalues.
!
   v3 = approx_jacobian(f,v2,v2v2)
   v3v3 = wt_inner_prod(v3,v3)
   v3v1 = wt_inner_prod(v3,v1)
   v3v2 = wt_inner_prod(v3,v2)
   if (dominant_eigenvalue(v3v3,v3v2,v2v2)) exit
!
   det2 = v1v1*v2v2 - v2v1*rev_wt_inner_prod(v2v1)
   alpha2 = (-v1v1*v3v2 + rev_wt_inner_prod(v2v1)*v3v1)/det2
   beta2 = (v2v1*v3v2 - v2v2*v3v1)/det2
!
!  First test the residual of the quadratic fit to see if we might
!  have determined a pair of eigenvalues.
!
   res2 = abs( v3v3 + rev_wt_inner_prod(alpha2)*v3v2 + &
               rev_wt_inner_prod(beta2)*v3v1 + &
               alpha2*rev_wt_inner_prod(v3v2) + &
               alpha2*rev_wt_inner_prod(alpha2)*v2v2 + &
               alpha2*rev_wt_inner_prod(beta2)*v2v1 + &
               beta2*rev_wt_inner_prod(v3v1) + &
               beta2*rev_wt_inner_prod(alpha2)*rev_wt_inner_prod(v2v1) + &
               beta2*rev_wt_inner_prod(beta2)*v1v1 )
   if (res2 <= abs(v3v3)*pt001**2) then
!
!  Calculate the two approximate pairs of eigenvalues.
!
      prv_root_pair(1:2) = quadratic_roots(alpha1,beta1)
      root_pair(1:2) = quadratic_roots(alpha2,beta2)
!
!  The test for convergence is done on the larger root of the second
!  approximation.  It is complicated by the fact that one pair of roots 
!  might be real and the other complex.  First calculate the spectral 
!  radius RHO of HAVG*J as the magnitude of ROOT1.  Then see if one of 
!  the roots R1,R2 is within one per cent of ROOT1.  A subdominant root 
!  may be very poorly approximated if its magnitude is much smaller than 
!  RHO -- this does not matter in our use of these eigenvalues.
!
      rho = abs(prv_root_pair(bigr))
      dist = min( abs(root_pair(bigr) - prv_root_pair(bigr)), &
                  abs(root_pair(bigr) - prv_root_pair(smlr)) )
      if (dist <= pt001*rho) exit
   end if
!
!  Do not have convergence yet.  Because the iterations are cheap, and
!  because the convergence criterion is stringent, we are willing to try
!  a few iterations.
!
   ntry = ntry + 1
   if (ntry > comm%max_stiff_iters) return
   v3nrm = sqrt(v3v3)
   v0 = v3/v3nrm
   v0v0 = one
!
end do
!
!  We now have the dominant eigenvalues.  Decide if the average step
!  size is being restricted on grounds of stability.  Check the real
!  parts of the eigenvalues.  First see if the dominant eigenvalue is
!  in the left half plane -- there won't be a stability restriction
!  unless it is. If there is another eigenvalue of comparable magnitude
!  with a positive real part, the problem is not stiff. If the dominant
!  eigenvalue is too close to the imaginary axis, we cannot diagnose
!  stiffness.
!
if ( real(root_pair(bigr)) < zero) then
   if ( .not. ( abs(root_pair(smlr)) >= pt9*rho .and. &
                real(root_pair(smlr)) > zero ) ) then
      if ( abs(aimag(root_pair(bigr))) <= &
           abs(real(root_pair(bigr)))*comm%tan_angle) then
!
!  If the average step size corresponds to being well within the
!  stability region, the step size is not being restricted because
!  of stability.
!
         sure_stiff = rho >= pt9*comm%stability_radius
      end if
   end if
end if
!
contains

function approx_jacobian(f,v,vdotv)
!
real(kind=dp), intent(in) :: vdotv
real(kind=dp), dimension(:), intent(in) :: v                         !dep!
real(kind=dp), dimension(size(v,1)) :: approx_jacobian               !dep!
!
interface
   function f(t,y)
      use rksuite_90_prec, only:wp
      real(kind=dp), intent(in) :: t                                 !indep!
      real(kind=dp), dimension(:), intent(in) :: y                   !dep!
      real(kind=dp), dimension(size(y,1)) :: f                       !dep!
   end function f
end interface
!
real(kind=dp) :: temp1
!
!  Scale V so that it can be used as an increment to Y
!  for an accurate difference approximation to the Jacobian.
!
temp1 = scale/sqrt(vdotv)
comm%vtemp = y + temp1*v
!
approx_jacobian = f(comm%t,comm%vtemp)
comm%f_count = comm%f_count + 1
!
!  Form the difference approximation.  At the same time undo
!  the scaling of V and introduce the factor of H_AVERAGE.
!
approx_jacobian = &
    (comm%h_average/temp1)*(approx_jacobian-comm%yp)
!
end function approx_jacobian


function quadratic_roots(alpha,beta)
!
real(kind=dp), intent(in) :: alpha, beta                             !dep!
complex(kind=wp), dimension(2) :: quadratic_roots
!
complex(kind=wp) :: temp, sqdisc, r1, r2
!
!  For types other than real/complex, this procedure must be constructed
!  such that temp and sqdisc are evaluated as compelx quantities
!
temp = alpha/two; sqdisc = sqrt(temp**2 - beta)
!
! Do we have double root?
!
if (sqdisc==zero) then
   quadratic_roots = (/ -temp, -temp /)
!
! Distinct roots
!
else
   r1 = -temp + sqdisc; r2 = -temp + sqdisc
   if (abs(r1) > abs(r2)) then
      quadratic_roots = (/ r1, r2 /)
   else
      quadratic_roots = (/ r2, r1 /)
   end if
end if
!
end function quadratic_roots

function dominant_eigenvalue(v1v1,v1v0,v0v0)
!
real(kind=dp), intent(in) :: v0v0, v1v1
real(kind=dp), intent(in) :: v1v0                                    !dep!
logical :: dominant_eigenvalue
!
real(kind=dp) :: ratio                                               !dep!
real(kind=dp) :: res, det
logical :: big
!
ratio = v1v0/v0v0; rho = abs(ratio)
det = v0v0*v1v1 - v1v0*rev_wt_inner_prod(v1v0); res = abs(det/v0v0)
!
big = det == zero .or. &
                  (res<=abs(v1v1)*pt001**2 .and. abs(ratio-rold)<=pt001*rho)
!
if (big) then
   root_pair(bigr) = cmplx(ratio)
   root_pair(smlr) = cmplx(zero)
end if
!
rold = ratio
dominant_eigenvalue = big
!
end function dominant_eigenvalue

function wt_inner_prod(vec_1,vec_2)
!
real(kind=dp), dimension(:), intent(in) :: vec_1, vec_2              !dep!
real(kind=dp) :: wt_inner_prod                                       !dep!
!
!
wt_inner_prod = sum ( (vec_1/weights) * (vec_2/weights) )            !spec-ar!
!
end function wt_inner_prod

function rev_wt_inner_prod(value)
!
real(kind=dp), intent(in) :: value                                   !dep!
real(kind=dp) :: rev_wt_inner_prod                                   !dep!
!
! given result of inner product value = v1.v0
! must return the reverse, ie v0.v1
!
! for real variables the value is the same
! for complex need to conjugate
!
rev_wt_inner_prod = value                                            !spec-line!
!
end function rev_wt_inner_prod

end subroutine stiff_r1

subroutine statistics_r1(comm,total_f_calls,step_cost,waste,num_succ_steps,&
                         h_next,y_maxvals)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
type(rk_comm_real_1d), intent(inout) :: comm
real(kind=dp), optional, intent(out) :: h_next                       !indep!
real(kind=dp), optional, intent(out) :: waste
real(kind=dp), dimension(:), optional, intent(out) :: y_maxvals      !shp-dep!
integer(i4), optional, intent(out) :: step_cost, num_succ_steps, total_f_calls
!
character(len=*), parameter :: srname="STATISTICS"
!
integer(i4) :: ier, nrec, state
!
integer(i4), parameter :: not_ready=-1, not_reusable=-3, fatal=911, &
   catastrophe=912, just_fine=1
logical, parameter :: ask=.true.
real(kind=dp), parameter :: zero=0.0_dp
!
ier = just_fine; nrec = 0
!
body: do
!
!  Is it permissible to call STATISTICS?
!
   state = get_saved_state_r1(srname,comm%save_states)
   if (state==fatal) then
      ier = catastrophe; nrec = 1; write (comm%rec,"(a)") &
" ** A catastrophic error has already been detected elsewhere."
      exit body
   end if
   if (state==not_reusable) then
      ier = fatal; nrec = 2; write (comm%rec,"(a/a)") &
" ** You have already made a call to STATISTICS after a hard failure was ", &
" ** reported from the integrator. You cannot call STATISTICS again."
      exit body
   end if
   state = get_saved_state_r1("STEP_INTEGRATE",comm%save_states)
   if (state==not_ready) then
      ier = fatal; nrec = 1
      if (comm%use_range) then
         write (comm%rec,"(a)") &
" ** You have not called RANGE_INTEGRATE, so you cannot use STATISTICS."
      else
         write (comm%rec,"(a)") &
" ** You have not called STEP_INTEGRATE, so you cannot use STATISTICS."
      end if
      exit body
   end if
   if (present(y_maxvals)) then
      if (any(shape(y_maxvals) /= shape(comm%y))) then
         ier = fatal; nrec = 2; write (comm%rec,"(a,i6,a/a,i6,a)") &
" ** The shape of Y_MAXVALS is not consistent with the shape of the", &
" ** dependent variables."
         exit body
      end if
   end if
!
!  Set flag so that the routine can only be called once after a hard 
!  failure from the integrator.
!
   if (state==5 .or. state==6) ier = not_reusable
!
   if (present(total_f_calls)) then
      total_f_calls = comm%full_f_count + comm%f_count
!      if (comm%erason) total_f_calls = total_f_calls + comm%ge_f_count
   end if
   if (present(step_cost)) step_cost = comm%cost
   if (present(num_succ_steps)) num_succ_steps = comm%step_count
   if (present(waste)) then
      if (comm%step_count<=1) then
         waste = zero
      else
         waste = real(comm%bad_step_count,kind=wp) / &
                 real(comm%bad_step_count+comm%step_count,kind=wp)
      end if
   end if
   if (present(h_next)) h_next = comm%h
   if (present(y_maxvals)) y_maxvals = comm%ymax
   exit body
end do body
!
call rkmsg_r1(ier,srname,nrec,comm)
!
end subroutine statistics_r1

subroutine global_error_r1(comm,rms_error,max_error,t_max_error)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
type(rk_comm_real_1d), intent(inout) :: comm
real(kind=dp), optional, intent(out) :: max_error
real(kind=dp), optional, intent(out) :: t_max_error                  !indep!
real(kind=dp), dimension(:), optional, intent(out) :: rms_error      !shp-dep!
!
character(len=*), parameter :: srname="GLOBAL_ERROR"
!
integer(i4) :: ier, nrec, state
!
intrinsic         sqrt
!
integer(i4), parameter :: not_ready=-1, not_reusable=-3, fatal=911, &
   catastrophe=912, just_fine=1
logical, parameter :: ask=.true.
!
ier = just_fine; nrec = 0
!
body: do 
!
!  Is it permissible to call GLOBAL_ERROR?
!
   state = get_saved_state_r1(srname,comm%save_states)
   if (state==fatal) then
      ier = catastrophe; nrec = 1; write (comm%rec,"(a)") &
" ** A catastrophic error has already been detected elsewhere."
      exit body
   end if
   if (state==not_reusable) then
      ier = fatal; nrec = 2; write (comm%rec,"(a/a)") &
" ** You have already made a call to GLOBAL_ERROR after a hard failure was", &
" ** reported from the integrator. You cannot call GLOBAL_ERROR again."
      exit body
   end if
   state = get_saved_state_r1("STEP_INTEGRATE",comm%save_states)
   if (state==not_ready) then
      ier = fatal; nrec = 1
      if (comm%use_range) then
         write (comm%rec,"(a)") &
" ** You have not yet called RANGE_INTEGRATE, so you cannot call GLOBAL_ERROR."
      else
         write (comm%rec,"(a)") &
" ** You have not yet called STEP_INTEGRATE, so you cannot call GLOBAL_ERROR."
      end if
      exit body
   end if
!
!  Set flag so that the routine can only be called once after a hard 
!  failure from the integrator.
!
   if (state==5 .or. state==6) ier = not_reusable
!
!  Check that ERROR_ASSESS was set properly for error assessment in SETUP.
!
   if (.not.comm%erason) then
      ier = fatal; nrec = 3; write (comm%rec,"(a/a/a)") &
" ** No error assessment is available since you did not ask for it in your",&
" ** call to the routine SETUP. Check your program carefully."
      exit body
   end if
!
! Check size of RMS_ERROR
!
   if (present(rms_error)) then
      if (any(shape(rms_error) /= shape(comm%y))) then
         ier = fatal; nrec = 2; write (comm%rec,"(a,a)") &
" ** The shape of RMS_ERROR is not consistent with the shape of the", &
" ** dependent variables."
         exit body
      end if
   end if
!
!  Check to see if the integrator has not actually taken a step.
!
   if (comm%step_count==0) then
      ier = fatal; nrec = 2; write (comm%rec,"(a/a)") &
" ** The integrator has not actually taken any successful steps. You cannot",&
" ** call GLOBAL_ERROR in this circumstance. Check your program carefully."
       exit body
   end if
!
!  Compute RMS error and set output variables.
!
   if (present(max_error)) max_error = comm%ge_max_contrib
   if (present(t_max_error)) t_max_error = comm%t_ge_max_contrib
   if (present(rms_error)) rms_error = &
      sqrt(comm%ge_assess/real(comm%step_count,kind=wp))
!
   exit body
end do body
!
call rkmsg_r1(ier,srname,nrec,comm)
!
end subroutine global_error_r1

subroutine reset_t_end_r1(comm,t_end_new)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
real(kind=dp), intent(in) :: t_end_new                               !indep!
type(rk_comm_real_1d), intent(inout) :: comm
!
character(len=*), parameter :: srname="RESET_T_END"
!
real(kind=dp) :: hmin, tdiff                                         !indep!
integer(i4) ::           ier, nrec, state
!
integer(i4), parameter :: not_ready=-1, usable=-2, fatal=911, catastrophe=912, &
   just_fine=1
logical, parameter :: ask=.true.
real(kind=dp), parameter :: zero=0.0_dp
!
ier = just_fine; nrec = 0
!
!  Is it permissible to call RESET_T_END?
!
body: do
!
   state = get_saved_state_r1("STEP_INTEGRATE",comm%save_states)
   if (state==fatal) then
      ier = catastrophe; nrec = 1; write (comm%rec,"(a)") &
" ** A catastrophic error has already been detected elsewhere."
      exit body
   end if
   if (comm%use_range) then
      if (get_saved_state_r1("RANGE_INTEGRATE",comm%save_states)/=usable) then
         ier = fatal; nrec = 2; write (comm%rec,"(a/a)") &
" ** You have called RESET_T_END after you specified to SETUP that you were",&
" ** going to use RANGE_INTEGRATE. This is not permitted."
         exit body
      end if
   end if
   if (state==not_ready) then
      ier = fatal; nrec = 1; write (comm%rec,"(a)") &
" ** You have not called STEP_INTEGRATE, so you cannot use RESET_T_END."
      exit body
   end if
   if (state==5 .or. state==6) then
      ier = fatal; nrec = 2; write (comm%rec,"(a,i1,a/a)") &
" ** STEP_INTEGRATE has returned with FLAG =  ",STATE," You cannot call",&
" ** RESET_T_END inthis circumstance."
      exit body
   end if
!
!  Check value of T_END_NEW
!
   if (comm%dir>zero .and. t_end_new<=comm%t) then
      ier = fatal; nrec = 3; write (comm%rec,"(a/a,e13.5/a,e13.5,a)") &
" ** Integration is proceeding in the positive direction. The current value",&
" ** for the independent variable is ",comm%T," and you have set T_END_NEW =",&
" ** ",T_END_NEW,".  T_END_NEW must be greater than T."
      exit body
   else if (comm%dir<zero .and. t_end_new>=comm%t) then
      ier = fatal; nrec = 3; write (comm%rec,"(a/a,e13.5/a,e13.5,a)") &
" ** Integration is proceeding in the negative direction. The current value",&
" ** for the independent variable is ",comm%T," and you have set T_END_NEW =",&
" ** ",T_END_NEW,".  T_END_NEW must be less than T."
      exit body
   else
      hmin = max(comm%sqtiny,comm%toosml*max(abs(comm%t),abs(t_end_new)))
      tdiff = abs(t_end_new-comm%t)
      if (tdiff<hmin) then
         ier = fatal; nrec = 4 
         write (comm%rec,"(a,e13.5,a/a,e13.5,a/a/a,e13.5,a)")&
" ** The current value of the independent variable T is ",comm%T,". The",&
" ** T_END_NEW you supplied has ABS(T_END_NEW-T) = ",TDIFF,". For the METHOD",&
" ** and the precision of the computer being used, this difference must be",&
" ** at least ",HMIN,"."
         exit body
      end if
   end if
!
   comm%t_end = t_end_new; comm%at_t_end = .false.
!
   exit body
end do body
!
call rkmsg_r1(ier,srname,nrec,comm)
!
end subroutine reset_t_end_r1

subroutine interpolate_r1(comm,f,t_want,y_want,yderiv_want)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
real(kind=dp), intent(in) :: t_want                                  !indep!
type(rk_comm_real_1d), intent(inout), target :: comm
real(kind=dp), dimension(:), intent(out), optional :: y_want         !dep!
real(kind=dp), dimension(:), intent(out), optional :: yderiv_want    !dep!
!
interface
   function f(t,y)
      use rksuite_90_prec, only:wp
      real(kind=dp), intent(in) :: t                                 !indep!
      real(kind=dp), dimension(:), intent(in) :: y                   !dep!
      real(kind=dp), dimension(size(y,1)) :: f                       !dep!
   end function f
end interface
!
character(len=*),parameter :: srname="INTERPOLATE"
integer(i4) :: ier, jer, nrec, state, npcls
logical :: intrp_initialised
!
integer(i4), parameter :: not_ready=-1, usable=-2, fatal=911, catastrophe=912, &
   just_fine=1
logical, parameter :: ask=.true.
!
ier = just_fine; nrec = 0
!
body: do
!
!  Is it permissible to call INTERPOLATE?
!
   state = get_saved_state_r1("STEP_INTEGRATE",comm%save_states)
   if (state==fatal) then
      ier = catastrophe; nrec = 1; write (comm%rec,"(a)") &
" ** A catastrophic error has already been detected elsewhere."
      exit body
   end if
   if (comm%use_range) then
      if (get_saved_state_r1("RANGE_INTEGRATE",comm%save_states)/=usable) then
         ier = fatal; nrec = 2; write (comm%rec,"(a/a)") &
" ** You have called INTERPOLATE after you specified to SETUP that you were",&
" ** going to use RANGE_INTEGRATE. This is not permitted."
         exit body
      end if
   end if
   if (state==not_ready) then
      ier = fatal; nrec = 1; write (comm%rec,"(a)") &
" ** You have not called STEP_INTEGRATE, so you cannot use INTERPOLATE."
      exit body
   end if
   if (state > just_fine) then
      ier = fatal; nrec = 2; write (comm%rec,"(a/a)") &
" ** STEP_INTEGRATE has returned with a flag value greater than 1. You", &
" ** cannot call INTERPOLATE in this circumstance."
      exit body
   end if
!
!  Check sizes of arrays
!
   if (present(y_want)) then
      if (any(shape(y_want)/=shape(comm%y))) then
         ier = fatal; nrec = 3; write (comm%rec,"(a,i6,a/a,i6,a/a)") &
" ** The shape of the array Y_WANT is not consistent with the shape of the ", &
" ** dependent variables."
         exit body
      end if
   end if
   if (present(yderiv_want)) then
      if (any(shape(yderiv_want)/=shape(comm%y))) then
         ier = fatal; nrec = 3; write (comm%rec,"(a,i6,a/a,i6,a/a)") &
" ** The shape of the array YDERIV_WANT is not consistent with the shape of", &
" ** the dependent variables."
         exit body
      end if
   end if
!
!  Check METHOD is ok to interpolate with
!
   if (comm%rk_method==3) then
      ier = fatal; nrec = 5; write (comm%rec,"(a/a/a/a/a)") &
" ** You have been using STEP_INTEGRATE with METHOD = 'H' to integrate your",&
" ** equations. You have just called INTERPOLATE, but interpolation is not",&
" ** available for this METHOD. Either use METHOD = 'M', for which",&
" ** interpolation is available, or use RESET_T_END to make STEP_INTEGRATE",&
" ** step exactly to the points where you want output."
      exit body
   end if
!
!  Get some workspace -
!     can overwrite STAGES in METHOD 'L' since they're not requird for the
!     interpolant
!
   select case(comm%rk_method)
   case(1)
      if (.not. associated(comm%p)) comm%p => comm%stages(:,1:2)
      npcls = 2
      if (.not. associated(comm%ytemp)) comm%p => comm%stages(:,1:3)
   case(2)
      jer = 0
      if (.not.associated(comm%xstage)) then
         allocate(comm%xstage(size(comm%y,1)),stat=jer)              !alloc!
      end if
      if (.not.associated(comm%ytemp)) then
         allocate(comm%ytemp(size(comm%y,1)),stat=jer)               !alloc!
      end if
      if (.not.associated(comm%p)) then
         allocate(comm%p(size(comm%y,1),5),stat=jer)                 !alloc!
      end if
      npcls = 5
      if (jer /= 0) then
         ier = fatal; nrec = 1 ; write (comm%rec,"(a)") &
" ** Not enough storage available to create workspace required internally."
         exit body
      end if
   end select
!
!  Check data to see if interpolant has already been calculated for this
!  step
!
   intrp_initialised = get_saved_state_r1(srname,comm%save_states) /= usable
!
!  Some initialization must be done before interpolation is possible.
!
   if (.not.intrp_initialised) call form_intrp(f,comm%p)
!
!  The actual evaluation of the interpolating polynomial and/or its first
!  derivative is done in EVALUATE_INTRP.
!
   call evaluate_intrp(comm%p,y_want,yderiv_want)
   exit body
!
end do body
!
call rkmsg_r1(ier,srname,nrec,comm)
!
contains
!
subroutine form_intrp(f,p)
!
real(kind=dp), intent(out), dimension(:,:) :: p                      !dep!
!
interface
   function f(t,y)
      use rksuite_90_prec, only:wp
      real(kind=dp), intent(in) :: t                                 !indep!
      real(kind=dp), dimension(:), intent(in) :: y                   !dep!
      real(kind=dp), dimension(size(y,1)) :: f                       !dep!
   end function f
end interface
!
real(kind=dp), dimension(:,:), pointer :: r                          !real!
real(kind=dp), dimension(:,:), pointer :: stages                     !dep!
real(kind=dp), dimension(:), pointer :: y, yp, y_old, yp_old         !dep!
real(kind=dp), dimension(:), pointer :: xstage                       !dep!
!
stages => comm%stages
r => comm%r
y => comm%y
yp => comm%yp
y_old => comm%y_old
yp_old => comm%yp_old
xstage => comm%xstage
!
select case(comm%rk_method)
case(1)
!
!  METHOD = 'L'.  Use the cubic Hermite interpolant that is fully
!  specified by the values and slopes at the two ends of the step.
!
   p(:,2) = y - y_old
   p(:,1) = comm%h_old*yp - p(:,2)
   p(:,2) = p(:,1) - (p(:,2)-comm%h_old*yp_old)
   p(:,1) = p(:,1) + p(:,2)
!
case(2)
!
!  METHOD = 'M'.
!
   if (.not.intrp_initialised) call extra_stages(f,comm%ytemp,comm%xstage)
!
!  Form the coefficients of the interpolating polynomial in its shifted
!  and scaled form.  The transformation from the form in which the
!  polynomial is derived can be somewhat ill-conditioned.  The terms 
!  are grouped so as to minimize the errors of the transformation.
!
!  Coefficient of SIGMA**6
   p(:,5) = r(5,6)*stages(:,4) + &
                     ((r(10,6)*xstage+r(8,6)*yp)+ &
                     (r(7,6)*stages(:,6)+r(6,6)*stages(:,5))) + &
                     ((r(4,6)*stages(:,3)+r(9,6)*stages(:,7))+ &
                     (r(3,6)*stages(:,2)+r(11,6)*stages(:,1))+ &
                     r(1,6)*yp_old)
!
!  Coefficient of SIGMA**5
   p(:,4) = (r(10,5)*xstage+r(9,5)*stages(:,7)) + &
                     ((r(7,5)*stages(:,6)+r(6,5)*stages(:,5))+ &
                     r(5,5)*stages(:,4)) + ((r(4,5)*stages(:,3)+ &
                     r(8,5)*yp)+(r(3,5)*stages(:,2)+r(11,5)* &
                     stages(:,1))+r(1,5)*yp_old)
!
!  Coefficient of SIGMA**4
   p(:,3) = ((r(4,4)*stages(:,3)+r(8,4)*yp)+ &
                     (r(7,4)*stages(:,6)+r(6,4)*stages(:,5))+ &
                     r(5,4)*stages(:,4)) + ((r(10,4)*xstage+ &
                     r(9,4)*stages(:,7))+(r(3,4)*stages(:,2)+ &
                     r(11,4)*stages(:,1))+r(1,4)*yp_old)
!
!  Coefficient of SIGMA**3
   p(:,2) = r(5,3)*stages(:,4) + r(6,3)*stages(:,5) + &
                     ((r(3,3)*stages(:,2)+r(9,3)*stages(:,7))+ &
                     (r(10,3)*xstage+r(8,3)*yp)+r(1,3)* &
                     yp_old)+((r(4,3)*stages(:,3)+r(11,3)* &
                     stages(:,1))+r(7,3)*stages(:,6))
!
!  Coefficient of SIGMA**2
   p(:,1) = r(5,2)*stages(:,4) + ((r(6,2)*stages(:,5)+ &
                   r(8,2)*yp)+r(1,2)*yp_old) + &
                   ((r(3,2)*stages(:,2)+r(9,2)*stages(:,7))+ &
                   r(10,2)*xstage) + ((r(4,2)*stages(:,3)+ &
                   r(11,2)*stages(:,1))+r(7,2)*stages(:,6))
!
!  Scale all the coefficients by the step size.
   p(:,:) = comm%h_old*p(:,:)
!
end select
!
end subroutine form_intrp


subroutine evaluate_intrp(p,y_want,yderiv_want)
!
real(kind=dp), dimension(:), optional, intent(out) :: y_want         !dep!
real(kind=dp), dimension(:), optional, intent(out) :: yderiv_want    !dep!
real(kind=dp), dimension(:,:), intent(in) :: p                       !dep!
!
real :: sigma
integer(i4) :: i
!
sigma = (t_want-comm%t)/comm%h_old
!
if (present(y_want)) then
   y_want = p(:,comm%intrp_degree-1)*sigma
   do i = comm%intrp_degree - 2, 1, -1
      y_want = (y_want+p(:,i))*sigma
   end do
   y_want = (y_want+comm%h_old*comm%yp)*sigma + comm%y
end if
!
if (present(yderiv_want)) then
   yderiv_want = comm%intrp_degree*p(:,comm%intrp_degree-1)*sigma
   do i = comm%intrp_degree - 1, 2, -1
      yderiv_want = (yderiv_want+i*p(:,i-1))*sigma
   end do
   yderiv_want = (yderiv_want+comm%h_old*comm%yp)/comm%h_old
end if
!
end subroutine evaluate_intrp


subroutine extra_stages(f,ytemp,xstage)
!
real(kind=dp), dimension(:), intent(out) :: ytemp, xstage            !dep!
!
interface 
   function f(t,y)
      use rksuite_90_prec, only:wp
      real(kind=dp), intent(in) :: t                                 !indep!
      real(kind=dp), dimension(:), intent(in) :: y                   !dep!
      real(kind=dp), dimension(size(y,1)) :: f                       !dep!
   end function f
end interface
!
real(kind=dp), dimension(:,:), pointer :: stages                     !dep!
real(kind=dp), dimension(:), pointer :: yp, y_old, yp_old            !dep!
!
real(kind=dp), dimension(:,:), pointer :: a                          !real!
real(kind=dp), dimension(:), pointer :: c                            !real!
real(kind=dp), pointer :: h_old, t_old                               !indep!
!
integer(i4) :: i, j
!
a => comm%a
stages => comm%stages
c => comm%c
yp => comm%yp
y_old => comm%y_old
yp_old => comm%yp_old
h_old => comm%h_old
t_old => comm%t_old
!
!  Compute the extra stages needed for interpolation using the facts that
!       1. Stage 1 is YP_OLD.
!       2. Stage i (i>1) is stored in STAGES(...,i-1).
!       3. This pair is FSAL, i.e. STAGES(...,7)=YP, which frees
!          up STAGES(...,7) for use by stage 9.
!       4. XSTAGE is used for stage 10.
!       5. The coefficient of stage 2 in the interpolant is always 0, so
!          STAGES(...,1) is used for stage 11.
!
do i = 9, 11
   do j = 1, i-1
      select case (j)
         case(1); ytemp = a(i,1)*yp_old
! could have used matmul here but that prevents increasing rank of dep-var
         case(2:7);ytemp = ytemp + a(i,j)*stages(:,j-1)
         case(8); ytemp = ytemp + a(i,j)*yp
         case(9); ytemp = ytemp + a(i,j)*stages(:,7)
         case(10); ytemp = ytemp + a(i,j)*xstage
      end select
   end do
   ytemp = y_old + h_old*ytemp
   select case(i)
      case(9)
         stages(:,7) = f(t_old+c(i)*h_old,ytemp)
      case(10)
         xstage = f(t_old+c(i)*h_old,ytemp)
      case(11)
         stages(:,1) = f(t_old+c(i)*h_old,ytemp)
   end select
   comm%f_count = comm%f_count + 1
end do
!
end subroutine extra_stages

end subroutine interpolate_r1

subroutine rkmsg_r1(ier,srname,nrec,comm,flag)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
integer(i4), intent(in) :: ier, nrec
integer(i4), intent(out), optional :: flag
character(len=*), intent(in) :: srname
type(rk_comm_real_1d), intent(inout) :: comm
!
logical :: ok, on, range_call
!
integer(i4), parameter :: fatal=911, catastrophe=912, just_fine=1
logical, parameter :: tell=.false.
!
!  Check where the call came from - if it is an indirect call from 
!  RANGE_INTEGRATE the run is not STOPped.
!
range_call = (srname=="RESET_T_END" .or. srname=="STEP_INTEGRATE" .or. &
          srname=="INTERPOLATE") .and. comm%use_range
!
!  Check if can continue with integrator.
!
ok = (srname=="STEP_INTEGRATE" .or. srname=="RANGE_INTEGRATE") .and. &
     (ier==2 .or. ier==3 .or. ier==4)
!
!  Check if program termination has been overridden.
!
on = get_stop_on_fatal_r1(comm)
!
if ((comm%print_message.and.ier>just_fine) .or. ier>=fatal) then
   write (comm%outch,"(/a)") " **"
   write (comm%outch,"(a)") comm%rec(1:nrec)
   if (ier>=fatal) then
      write (comm%outch,"(a/a,a,a/a/)") &
" **",&
" ** Catastrophic error detected in ", srname, ".",&
" **"
      if ((.not.range_call.and.on.and.ier==fatal) .or. ier==catastrophe) then
         write (comm%outch,"(a/a/a)") &
" **",&
" ** Execution of your program is being terminated.",&
" **"
         stop
      end if
   else 
      if (ok) then
         write (comm%outch,"(a/a,a,a,i2,a/a/a)")  &
" **", &
" ** Warning from routine ", srname, " with flag set ",ier, ".",&
" ** You can continue integrating this problem.",&
" **"
      else
         write (comm%outch,"(a/a,a,a,i2,a/a/a)")  &
" **", &
" ** Warning from routine ", srname, " with flag set ",ier, ".", &
" ** You cannot continue integrating this problem.", &
" **"
      end if
      if (.not.present(flag)) then
         write (comm%outch,"(a/a/a)") &
" **",&
" ** Execution of your program is being terminated.",&
" **"
         stop
      end if
   end if
end if
!
if (present(flag)) flag = ier
comm%rec(nrec+1:10) = " "
!
!  Save the status of the routine associated with SRNAME
!
call set_saved_state_r1(srname,ier,comm)
!
!  Indicate that a catastrophic error has been detected
!
!call set_saved_fatal_r1(comm,ier >= catastrophe)
!
end subroutine rkmsg_r1

subroutine set_saved_state_r1(srname,state,comm)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
integer(i4), intent(in) :: state
type(rk_comm_real_1d), intent(inout) :: comm
character(len=*), intent(in) :: srname
!
integer(i4) :: name
!
integer(i4), parameter :: fatal=911
!
select case (srname)
   case("SETUP"); name = 1
   case("RANGE_INTEGRATE"); name = 2
   case("STATISTICS"); name = 3
   case("GLOBAL_ERROR"); name = 4
   case("STEP_INTEGRATE"); name = 5
   case("INTERPOLATE"); name= 6
   case("RESET_T_END"); name = 7
   case default; name = 0
end select
!
comm%save_states(name) = state
comm%saved_fatal_err = state >= fatal
!
end subroutine set_saved_state_r1

function get_saved_state_r1(srname,save_states)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
integer(i4), dimension(7), intent(inout) :: save_states
character(len=*), intent(in) :: srname
integer(i4) :: get_saved_state_r1
!
integer(i4) :: name
!
integer(i4), parameter :: fatal=911
!
select case (srname)
   case("SETUP"); name = 1
   case("RANGE_INTEGRATE"); name = 2
   case("STATISTICS"); name = 3
   case("GLOBAL_ERROR"); name = 4
   case("STEP_INTEGRATE"); name = 5
   case("INTERPOLATE"); name= 6
   case("RESET_T_END"); name = 7
   case default; name = 0
end select
!
!  Check for status of given routine but check for any fatal errors first
!
if (any(save_states(1:7)==fatal)) then
   get_saved_state_r1 = fatal
else
   get_saved_state_r1 = save_states(name)
end if
!
end function get_saved_state_r1

function get_saved_fatal_r1(comm)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
type(rk_comm_real_1d), intent(in) :: comm
logical :: get_saved_fatal_r1
!
get_saved_fatal_r1 = comm%saved_fatal_err
!
end function get_saved_fatal_r1

subroutine set_stop_on_fatal_r1(comm,action)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
type(rk_comm_real_1d), intent(inout) :: comm
logical, intent(in) :: action
!
comm%stop_on_fatal = action
!
end subroutine set_stop_on_fatal_r1

function get_stop_on_fatal_r1(comm)
!
! Part of rksuite_90 v1.0 (Aug 1994)
!         software for initial value problems in ODEs
!
! Authors: R.W. Brankin (NAG Ltd., Oxford, England)
!          I. Gladwell  (Math Dept., SMU, Dallas, TX, USA)
!          see main doc for contact details
!
type(rk_comm_real_1d), intent(in) :: comm
logical get_stop_on_fatal_r1
!
get_stop_on_fatal_r1 = comm%stop_on_fatal
!
end function get_stop_on_fatal_r1

!endc!
end module rksuite_90
