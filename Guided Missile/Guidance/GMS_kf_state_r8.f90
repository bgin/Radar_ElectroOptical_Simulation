
module kf_state_r8
!** filterMod contains variables used in various Kalman filter and smoother software.
!** It is only necessary to set variables that are actually used in the filter code.
!** Author: B. Gibbs, 12/2009
!==============================================================================!
!** @@@@Modified by Bernard Gingold 22-05-2022 09:25 AM, beniekg@gmail.com
!==============================================================================!
!**************************************
! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an “as is” basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.
!**************************************
      use mod_kinds, only : i4,dp

      !** contains global variables used in defining Kalman filter, UD and SRIF
      implicit none
      integer(i4) :: nstate        !total number of states in model (set by model)
      integer(i4) :: ncore         !number of core states in model
      integer(i4) :: nMarkov       !number of markov states in model
      integer(i4) :: nbias         !number of bias states in model

      integer(i4) :: nest          !total number of estimated states
      integer(i4) :: nConsider     !total number of consider parameters
      integer(i4) :: nUnadj        !total number of unadjusted-analyze parameters
      integer(i4) :: ncoreE        !number of estimated core states
      integer(i4) :: nMarkE        !number of estimated Markov states
      integer(i4) :: nbiasE        !number of estimated bias states
      integer(i4) :: nDynE         !number of estimated core+markov states

      integer(i4) :: nmeas         !total number of vector meas in input file
      integer(i4) :: method_udtime = 1!method used for ud time update: 1=wgs, 2=rank-1, 3=square phi*u*d*u^T*phi^T  ###
      integer(i4) :: KFmethod = 2   !Kalman filter (covariance) measurement update method: 1=short, 2=Biermans Joseph, 3=full Joseph ###

      integer(i4),parameter :: mmaxi = 16  !maximum number of allowed scalar measurements at one time point (16), originally was mmaxi = 10

      integer(i4),dimension(:),allocatable :: mapxe!mapping from estimated states (nest) to total model states
      integer(i4),dimension(:),allocatable :: mapxu!mapping from unadjusted analyze parameter (nUnadj) to total model states
      integer(i4),dimension(:),allocatable :: mapxei!mapping from total model states (nstate) to estimated states
      integer(i4),dimension(:),allocatable :: mapxui!mapping from total model states (nstate) to estimated states
      !dir$ attributes align : 64 :: mapxe
      !dir$ attributes align : 64 :: mapxu
      !dir$ attributes align : 64 :: mapxei
      !dir$ attributes align : 64 :: mapxui

!      character(80) setupFile   !name of file containing data for structure xstate
!      character(80) measFile    !name of file containing measurement data
      character(20),allocatable :: xnames(:) !names of total states as set by model routines

      logical(i4) :: sparse =.true.  !true = use sparse multiplies  !###
      logical(i4) :: debugp =.true.  !generates debug print
      logical(i4) :: first(3) =.true.!initializes filters
      logical(i4) :: runFilt(3) =.true. !denotes whether Kalman covariance, U-D and SRIF filters are to be run

      real(kind=dp) :: tepoch           !epoch time of x0 and p0
      real(kind=dp) :: tend             !end time for filter processing
      real(kind=dp), dimension(3) :: rsum          !residual sum-of-squares for 3 filters

      real(kind=dp), dimension(:),  allocatable   :: xfu
      real(kind=dp), dimension(:),  allocatable   :: ud
      real(kind=dp), dimension(:),  allocatable   :: xfk
      real(kind=dp), dimension(:,:),allocatable   :: pf
      real(kind=dp), dimension(:),  allocatable   :: xfs
      real(kind=dp), dimension(:),  allocatable   :: xfa
      real(kind=dp), dimension(:,:),allocatable   :: pfa
      real(kind=dp), dimension(:),  allocatable   :: rsrif
      real(kind=dp), dimension(:,:),allocatable   :: dxsum
      real(kind=dp), dimension(:,:),allocatable   :: sigsum
      !dir$ attributes align : 64 :: xfu
      !dir$ attributes align : 64 :: ud
      !dir$ attributes align : 64 :: xfk
      !dir$ attributes align : 64 :: pf
      !dir$ attributes align : 64 :: xfs
      !dir$ attributes align : 64 :: xfa
      !dir$ attributes align : 64 :: pfa
      !dir$ attributes align : 64 :: rsrif
      !dir$ attributes align : 64 :: dxsum
      !dir$ attributes align : 64 :: sigsum
!      real(kind=dp) rnoise(mmaxi,mmaxi)           !meas noise covariance
#if 1
      type, public :: states
        integer(i4)   :: status      !0=ignore, 1=unadjusted analyze, 2=consider, 3=estimate
        integer(i4)   :: xtype       !1=core state, 2=1 or 2 order Markov, 3=bias
        character(20) :: xname    !name of state that must match name in hard-coded model
        real(kind=dp) :: x0             !a priori estimate at epoch time
        real(kind=dp) :: sig0           !1-sigma uncertainty of x0 at epoch time
        real(kind=dp) :: qs             !PSD of driving white noise
        real(kind=dp) :: const(2)       !known constants associated with this state in dynamic or measurement model
      end type states
      type(states),dimension(:), allocatable :: xstate
      !dir$ attributes align : 64 :: xstate

      type, public ::  meass
        integer(i4)   :: mtype       !meas type (must match types in model)
        integer(i4)   :: m           !number of scalar meas included in this meas
        character(20) :: mname    !name of meas for output (may include sensor name)
        real(kind=dp) :: t              !meas time
        real(kind=dp) :: y(mmaxi)        !up to mmax scalar meas values
        real(kind=dp) :: sigr(mmaxi)     !1-sigma meas noise for mmax scalar meas
      end type
!      type (meass),allocatable :: meas(:)
#endif
#if 0
      type, public :: states_soa
          character(20) :: xname    !name of state that must match name in hard-coded model
          integer(i4)   :: status      !0=ignore, 1=unadjusted analyze, 2=consider, 3=estimate
          integer(i4)   :: xtype       !1=core state, 2=1 or 2 order Markov, 3=bias
          real(kind=dp), dimension(:),   allocatable :: x0    !a priori estimate at epoch time
          real(kind=dp), dimension(:),   allocatable :: sig0  !1-sigma uncertainty of x0 at epoch time
          real(kind=dp), dimension(:),   allocatable :: qs    !PSD of driving white noise
          real(kind=dp), dimension(:,:), allocatable :: const !known constants associated with this state in dynamic or measurement model
          !dir$ attributes align : 64 :: x0
          !dir$ attributes align : 64 :: sig0
          !dir$ attributes align : 64 :: qs
          !dir$ attributes align : 64 :: const
      end type states_soa

      type, public :: measures_soa
          character(20) :: mname       !name of meas for output (may include sensor name)
          integer(i4)   :: mtype       !meas type (must match types in model)
          integer(i4)   :: m           !number of scalar meas included in this meas
          real(kind=dp), dimension(:),   allocatable :: t    ! measures time
          real(kind=dp), dimension(:,:), allocatable :: y    ! mmax scalar values per single measure
          real(kind=dp), dimension(:,:), allocatable :: sigr ! !1-sigma meas noise for mmax scalar measures
          !dir$ attributes align : 64 :: t
          !dir$ attributes align : 64 :: y
          !dir$ attributes align : 64 :: sigr
      end type measures_soa

#endif
end module kf_state_r8
