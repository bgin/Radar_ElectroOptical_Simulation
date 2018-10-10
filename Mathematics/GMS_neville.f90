

#include "Config.fpp"

module mod_neville

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_neville'
 !          
 !          Purpose:
 !                      Modern object oriented reimplementation of
 !                      David Kahaner subroutine 'DIFF'
 !          History:
 !                        Date: 18-07-2017
 !                        Time: 14:04 GMT+2
 !                        Modification: 
 !                        Date: 09-10-2018
 !                        Time: 16:31 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                 Original implementor: David Kahaner, NBS (GAITHERSBURG)
 !          Modification:
 !                        Bernard Gingold 
 !          1) Packaging subroutines into ADT.
 !          2) Switching to double precision arithmentics
 !          3) Removing (when possible) excessive usage of goto's
 !          
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !=================================================================================
 !                  Original Description                                          !
 !                                                                                !
 !          NUMERICAL DIFFERENTIATION OF USER DEFINED FUNCTION                    !
 !                                                                                !
 !                  DAVID KAHANER, NBS (GAITHERSBURG)                             !
 !                                                                                !
 !  THE PROCEDURE DIFFERENTIATE CALCULATES THE FIRST, SECOND OR                   !
 !  THIRD ORDER DERIVATIVE OF A FUNCTION BY USING NEVILLE'S PROCESS TO            !
 !  EXTRAPOLATE FROM A SEQUENCE OF SIMPLE POLYNOMIAL APPROXIMATIONS BASED ON      !
 !  INTERPOLATING POINTS DISTRIBUTED SYMMETRICALLY ABOUT X0 (OR LYING ONLY ON     !
 !  ONE SIDE OF X0 SHOULD THIS BE NECESSARY).  IF THE SPECIFIED TOLERANCE IS      !
 !  NON-ZERO THEN THE PROCEDURE ATTEMPTS TO SATISFY THIS ABSOLUTE OR RELATIVE     !
 !  ACCURACY REQUIREMENT, WHILE IF IT IS UNSUCCESSFUL OR IF THE TOLERANCE IS      !
 !  SET TO ZERO THEN THE RESULT HAVING THE MINIMUM ACHIEVABLE ESTIMATED ERROR     !
 !  IS RETURNED INSTEAD.                                                          !
 !                                                                                !
 !  INPUT PARAMETERS:                                                             !
 !  IORD = 1, 2 OR 3 SPECIFIES THAT THE FIRST, SECOND OR THIRD ORDER              !
 !  DERIVATIVE,RESPECTIVELY, IS REQUIRED.                                         !
 !  X0 IS THE POINT AT WHICH THE DERIVATIVE OF THE FUNCTION IS TO BE CALCULATED.  !
 !  XMIN, XMAX RESTRICT THE INTERPOLATING POINTS TO LIE IN [XMIN, XMAX], WHICH    !
 !  SHOULD BE THE LARGEST INTERVAL INCLUDING X0 IN WHICH THE FUNCTION IS          !
 !  CALCULABLE AND CONTINUOUS.                                                    !
 !  F, A REAL PROCEDURE SUPPLIED BY THE USER, MUST YIELD THE VALUE OF THE         !
 !  FUNCTION AT X FOR ANY X IN [XMIN, XMAX] WHEN CALLED BY F(X).                  !
 !  EPS DENOTES THE TOLERANCE, EITHER ABSOLUTE OR RELATIVE.  EPS=0 SPECIFIES THAT !
 !  THE ERROR IS TO BE MINIMISED, WHILE EPS>0 OR EPS<0 SPECIFIES THAT THE         !
 !  ABSOLUTE OR RELATIVE ERROR, RESPECTIVELY, MUST NOT EXCEED ABS(EPS) IF         !
 !  POSSIBLE.  THE ACCURACY REQUIREMENT SHOULD NOT BE MADE STRICTER THAN          !
 !  NECESSARY, SINCE THE AMOUNT OF COMPUTATION TENDS TO INCREASE AS               !
 !  THE MAGNITUDE OF EPS DECREASES, AND IS PARTICULARLY HIGH WHEN EPS=0.          !
 !  ACC DENOTES THAT THE ABSOLUTE (ACC>0) OR RELATIVE (ACC<0) ERRORS IN THE       !
 !  COMPUTED VALUES OF THE FUNCTION ARE MOST UNLIKELY TO EXCEED ABS(ACC), WHICH   !
 !  SHOULD BE AS SMALL AS POSSIBLE.  IF THE USER CANNOT ESTIMATE ACC WITH         !
 !  COMPLETE CONFIDENCE, THEN IT SHOULD BE SET TO ZERO.                           !
 !                                                                                !
 !  OUTPUT PARAMETERS:                                                            !
 !  DERIV IS THE CALCULATED VALUE OF THE DERIVATIVE.                              !
 !  ERROR IS AN ESTIMATED UPPER BOUND ON THE MAGNITUDE OF THE ABSOLUTE ERROR IN   !
 !  THE CALCULATED RESULT.  IT SHOULD ALWAYS BE EXAMINED, SINCE IN EXTREME CASE   !
 !  MAY INDICATE THAT THERE ARE NO CORRECT SIGNIFICANT DIGITS IN THE VALUE        !
 !  RETURNED FOR DERIVATIVE.                                                      !
 !  IFAIL WILL HAVE ONE OF THE FOLLOWING VALUES ON EXIT:                          !
 !  0   THE PROCEDURE WAS SUCCESSFUL.                                             !
 !  1   THE ESTIMATED ERROR IN THE RESULT EXCEEDS THE (NON-ZERO) REQUESTED        !
 !         ERROR, BUT THE MOST ACCURATE RESULT POSSIBLE HAS BEEN RETURNED.        !
 !  2   INPUT DATA INCORRECT (DERIVATIVE AND ERROR WILL BE UNDEFINED).            !
 !  3   THE INTERVAL [XMIN, XMAX] IS TOO SMALL (DERIVATIVE AND ERROR WILL BE      !
 !         UNDEFINED);                                                            !
 ! ================================================================================
 !                       
 !==================================================================================85   


 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

    implicit none
    use mod_kinds, only : int1,int4,dp
    use ifcore
    
    
    
!DIR$ IF .NOT. DEFINED (GMS_NEVILLE_HANDLE_IEEE_EXCEPTIONS)
    !DIR$ DEFINE GMS_NEVILLE_HANDLE_IEEE_EXCEPTION = 1
!DIR$ ENDIF

!DIR$ IF .NOT. DEFINED (GMS_NEVILLE_USE_STRUCT_PADDING)
    !DIR$ DEFINE GMS_NEVILLE_USE_STRUCT_PADDING = 1
!DIR$ ENDIF
    
!DIR$ IF .NOT. DEFINED (GMS_NEVILLE_ADD_PADDING_TO_VECTORIZE_REMAINDER)
    !DIR$ DEFINE GMS_NEVILLE_ADD_PADDING_TO_VECTORIZE_REMAINDER = 0
!DIR$ ENDIF
    

    
    type , public :: NevilleDeriv_t
        
        SEQUENCE
        
        real(kind=dp)      :: m_x0
     
        real(kind=dp)      :: m_xmin
       
        real(kind=dp)      :: m_xmax
        
        real(kind=dp)      :: m_eps
        
        real(kind=dp)      :: m_acc
        
        real(kind=dp)      :: m_deriv
        
        real(kind=dp)      :: m_error
        
        procedure(func), pointer :: m_pfunc => NULL()
        
        integer(kind=int4)   :: m_iord
!DIR$ IF (GMS_NEVILLE_USE_STRUCT_PADDING .EQ. 1)
         PAD_TO(0,4)
!DIR$ ENDIF       
        integer(kind=int4)   :: m_ifail
!DIR$ IF (GMS_NEVILLE_USE_STRUCT_PADDING .EQ. 1)       
        PAD_TO(1,4)
!DIR$ ENDIF       
        logical(kind=int4)       :: m_isbuilt
!DIR$ IF (GMS_NEVILLE_USE_STRUCT_PADDING .EQ. 1)       
       PAD_TO(2,4)
!DIR$ ENDIF       
!DIR$ IF (GMS_NEVILLE_USE_STRUCT_PADDING .EQ. 1)
       PAD_TO(3,40)
!DIR$ ENDIF
        
        contains
    
        procedure, pass(this), public :: construct
        
        procedure, pass(this), public :: destroy
        
        procedure, pass(this), public :: differentiate ! Neville extrapolation process (originally named DIFF)
        
        procedure, pass(this), public :: faccur
        
       
        
    end type NevilleDeriv_t
        
    abstract interface
        function func(x0) result(value)
          implicit none
          real(kind=dp), intent(in) :: x0
          ! Locals
          real(kind=dp)             :: value
        end function
    end interface
    
    contains
    
    !==========================================================================80
    !  subroutine: 'construct'
    !               Class NevilleDeriv_t constructor
    !==========================================================================80
    subroutine construct(this,iord,x0,xmin,xmax,f,eps,acc)
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
                       
          class(NevilleDeriv_t),      intent(inout) :: this
          integer(kind=int4),         intent(in)    :: iord
          real(kind=dp),            intent(in)    :: x0,xmin, &
                                                       xmax
          procedure(func),            intent(in)    :: f
          real(kind=dp),            intent(in)    :: eps,acc
          ! Locals
          logical(kind=int4)                        :: b1=.false.,b2=.false., &
                                                       b3=.false.,b4=.false., & 
                                                       b5=.false.
          ! Start of executable statements
          ! Error checking of inputs moved here!!
          b1 = iord < 1
          b2 = iord >  3
          b3 = xmax <= xmin
          b4 = x0   > xmax
          b5 = x0   < xmin
          if(b1 .or. b2 .or. b3 .or. &
             b4 .or. b5 )  then
             return
          end if  
          
          this%m_iord = iord
          this%m_x0   = x0
          this%m_xmin = xmin
          this%m_xmax = xmax
          this%m_pfunc => f
          this%m_eps  = eps
          this%m_acc  = acc
          this%m_deriv = TINY(1.0_dp)   ! Unlikely value of derivative
          this%m_error = TINY(1.0_dp)
          this%m_ifail = -1
          if(ASSOCIATED(this%m_pfunc)) then
              this%m_isbuilt = .true.  ! prevent calling disassociated procedure pointer
          else
              this%m_isbuilt = .false.
          end if
    end subroutine
    
    !==========================================================================80
    !  subroutine: 'destroy'
    !               Class NevilleDeriv_t destructor
    !==========================================================================80
    subroutine destroy(this)
         
          class(NevilleDeriv_t), intent(inout) :: this
          ! Start of execuatble statements
          if(ASSOCIATED(this%m_pfunc)) then
              NULLIFY(this%m_pfunc)
          end if
          if(.NOT. ASSOCIATED(this%m_pfunc)) then
              this%m_isbuilt = .false.
          end if
    end subroutine
    !==========================================================================80
    ! subroutine: 'differentiate'
    !              Runs Neville extrapolation process.
    ! Author Description:
    !     THE PROCEDURE DIFFERENTIATE CALCULATES THE FIRST, SECOND OR
    !     THIRD ORDER DERIVATIVE OF A FUNCTION BY USING NEVILLE'S PROCESS TO
    !     EXTRAPOLATE FROM A SEQUENCE OF SIMPLE POLYNOMIAL APPROXIMATIONS BASED ON
    !     INTERPOLATING POINTS DISTRIBUTED SYMMETRICALLY ABOUT X0 (OR LYING ONLY ON
    !     ONE SIDE OF X0 SHOULD THIS BE NECESSARY).  IF THE SPECIFIED TOLERANCE IS
    !     NON-ZERO THEN THE PROCEDURE ATTEMPTS TO SATISFY THIS ABSOLUTE OR RELATIVE
    !     ACCURACY REQUIREMENT, WHILE IF IT IS UNSUCCESSFUL OR IF THE TOLERANCE IS
    !     SET TO ZERO THEN THE RESULT HAVING THE MINIMUM ACHIEVABLE ESTIMATED ERROR
    !     IS RETURNED INSTEAD.
    !==========================================================================80
    subroutine differentiate(this,fp_flags)
!DIR$ IF (GMS_NEVILLE_HANDLE_IEEE_EXCEPTIONS .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          use mod_constants, only   : v1over3
          class(NevilleDeriv_t),            intent(inout) :: this
          logical(kind=int4), dimension(5), intent(inout) :: fp_flags
          ! Locals
!DIR$ ATTRUBUTES ALIGN : 64 :: d,denom,e,minerr          
          real(kind=dp),   dimension(10)   :: d,denom,e,   &
                                                minerr
!DIR$ ATTRIBUTES ALIGN : 64 :: maxf
          real(kind=dp),   dimension(0:10)   :: maxf
!DIR$ ATTRIBUTES ALIGN : 64 :: sav
          real(kind=dp),   dimension(0:13)   :: sav
!DIR$ ATTRIBUTES ALIGN : 64 :: storef
          real(kind=dp),   dimension(-45:45) :: storef
!DIR$ ATTRIBUTES ALIGN : 8 :: beta,beta4,h,h0,h1,h2,newh1,   &
                              newh2,heval,hprev,baseh,hacc1, &
                              nhacc1,nhacc2,minh,maxh,       &
                              maxh1,maxh2,tderiv0,f0,twof0,  &
                              f1,f2,f3,f4,fmax,maxfun,pmaxf, &
                              df1,deltaf,pdelta,z,zpower,    &
                              c0f0,c1,c2,c3,dnew,dprev,re,   &
                              te,newerr,temerr,newacc,pacc1, &
                              pacc2,facc1,facc2,acc0,acc1,   &
                              acc2,relacc,twoinf,twosup,s,   &
                              factor,pad0,pad1,pad2,pad3,    &
                              pad4,pad5,pad6
          real(kind=dp)  :: beta,beta4,h,h0,h1,h2,newh1,   &
                              newh2,heval,hprev,baseh,hacc1, &
                              hacc2,nhacc1,nhacc2,minh,maxh, &
                              maxh1,maxh2,tderiv0,f0,twof0,  &
                              f1,f2,f3,f4,fmax,maxfun,pmaxf, &
                              df1,deltaf,pdelta,z,zpower,    &
                              c0f0,c1,c2,c3,dnew,dprev,re,   &
                              te,newerr,temerr,newacc,pacc1, &
                              pacc2,facc1,facc2,acc0,acc1,   &
                              acc2,relacc,twoinf,twosup,s,   &
                              factor,pad0,pad1,pad2,pad3,    &
                              pad4,pad5,pad6
         
          !real(kind=dp),   parameter :: THIRD = 0.333333333333333_dp64
!DIR$ ATTRUBUTES ALIGN : 4 :: eta,inf,i,j,k,n,nmax,method,signh,fcount,init
          integer(kind=int4)              :: eta,  inf,&
                                             i,j,k,n,   &
                                             nmax,      &
                                             method,    &
                                             signh,     &
                                             fcount,    &
                                             init
         
!DIR$ ATTRIBUTES ALIGN : 64 :: ignore                                            
          logical(kind=int4), dimension(10)    :: ignore
!DIR$ ATTRIBUTES ALIGN : 4 :: contin,saved
          logical(kind=int4)                   :: contin,    &
                                                  saved
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of object build state
          if(this%m_isbuilt .EQ. .false.) then
              this%m_ifail = 4  ! Changed from original
              return            ! ifail == 4 means: disassciated
          end if                ! procedure pointer
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
    !   ETA IS THE MINIMUM NUMBER OF SIGNIFICANT BINARY DIGITS (APART FROM THE
    !   SIGN DIGIT) USED TO REPRESENT THE MANTISSA OF REAL NUMBERS. IT SHOULD
    !   BE DEVREASED BY ONE IF THE COMPUTER TRUNCATES RATHER THAN ROUNDS.
    !   INF, SUP ARE THE LARGEST POSSIBLE POSITIVE INTEGERS SUBJECT TO
    !   2**(-INF), -2**(-INF), 2**SUP, AND -2**SUP ALL BEING REPRESENTABLE REAL
    !   NUMBERS    
          eta = digits(1.0_dp) - 1
          inf = -minexponent(1.0_dp) - 2
          sup = maxexponent(1.0_dp) - 1
          twoinf = 2.0_dp**(-inf)
          twosup = 2.0_dp**sup
          factor = 2**(DBLE((inf+sup))*v1over3)
          if(factor .LT. 256.0_dp) factor = 256.0_dp
          maxh1 = this%m_xmax - this%m_x0
          signh = 1
          if(this%m_x0-this%m_xmin .LE. maxh1) then
              maxh2 = this%m_x0-this%m_xmin
          else
              maxh2 = maxh1
              maxh1 = this%m_x0-this%m_xmin
              signh = -1
          end if
          relacc = 2.0_dp**(1.0_dp-eta)
          maxh1  = (1.0_dp-relacc)*maxh1
          maxh2  = (1.0_dp-relacc)*maxh2
          s = 128.0_dp*twoinf
          if(DABS(this%m_x0) .GT. (128.0_dp*twoinf*2.0_dp**eta)) then
              s = DABS(this%m_x0)*2.0_dp**(-eta)
          end if
          if(maxh1 .LT. s) then
              ! Interval too small
              this%m_ifail = 3
              return
          end if
          if(acc0 .LT. 0.0_dp) then
              if(-acc0 .GT. relacc) relacc = -acc0
              acc0 = 0.0_dp
          end if
          !   DETERMINE THE SMALLEST SPACING AT WHICH THE CALCULATED
          !   FUNCTION VALUES ARE UNEQUAL NEAR X0.
          f0 = this%m_pfunc(this%m_x0)
          twof0 = f0+f0
          if(DABS(this%m_x0) .GT. twoinf*2.0_dp**eta) then
              h = DABS(this%m_x0)*2.0_dp**(-eta)
              z = 2.0_dp
          else
              h = twoinf
              z = 64.0_dp
          end if
          df1 = this%m_pfunc(this%m_x0+signh*h) - f0
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
          do
              if(df1 .NE. 0.0_dp .OR. z*h .GT. maxh1) exit
              h = z*h
              df1 = this%m_pfunc(this%m_x0+signh*h)-f0
              if(z .NE. 2.0_dp) then
                  if(df1 .NE. 0.0_dp) then
                      h = h/z
                      z = 2.0_dp
                      df1 = 0.0_dp
                  else
                      if(z*h .GT. maxh1) z = 2.0_dp
                  end if
              end if
          end do
          if(df1 .EQ. 0.0_dp) then
              !    CONSTANT FUNCTION
              this%m_deriv = 0.0_dp
              this%m_error = 0.0_dp
              this%m_ifail = 0
              return
          end if
          if(h .GT. maxh1/128.0_dp) then
              !   MINIMUM H TOO LARGE 
              ifail = 3
              return
          end if
          h = 8.0_dp*h
          h1 = signh*h
          h0 = h1
          h2 = -h1
          minh = 2.0_dp**(-MIN(inf,sup)/this%m_iord)
          if(minh .LT. h) minh = h
          if(this%m_iord .EQ. 1) s = 8.0_dp
          if(this%m_iord .EQ. 2) s = 9.0_dp*DSQRT(3.0_dp)
          if(this%m_iord .EQ. 3) s = 27.0_dp
          if(minh .GT. maxh1/s) then
              this%m_ifail = 3
              return
          end if
          if(minh.GT.maxh2/s .OR. maxh2.LT.128.0_dp*twoinf) then
              method = 1
          else
              method = 2
          end if
          ! METHOD 1 USES 1-SIDED FORMULAE, AND METHOD 2 SYMMETRIC.
          ! NOW ESTIMATE ACCURACY OF CALCULATED FUNCTION VALUES.  
          if(method.NE.2 .OR. this%m_iord.EQ.2 ) then
              if(this%m_x0 .NE. 0.0_dp) then
                  call this%faccur(0.0_dp,-h1,acc0,twoinf,f0,f1)
              else
                  acc = 0.0_dp
              end if
          end if
          if(DABS(h1) .GT. twosup/128.0_dp) then
              hacc1 = twosup
          else
              hacc1 = 128.0_dp*h1
          end if
          if(DABS(hacc1)*0.25_dp .LT. minh) then
              hacc1 - 4.0_dp*signh*minh
          else if(DABS(hacc1) .GT. maxh1) then
              hacc1 = signh*maxh1
          end if
          f1 = this%m_pfunc(this%m_x0+hacc1)
          call this%faccur(hacc1,h1,acc1,twoinf,f0,f1)
          if(method .EQ. 2) then
              hacc2 = -hacc1
              if(DABS(hacc2) .GT. maxh2) hacc2 = -signh*maxh2
              f1 = this%m_pfunc(this%m_x0+hacc2)
              call this%faccur(hacc2,h2,acc2,twoinf,f0,f1)
          end if
          nmax = 8
          if(eta .GT. 36) nmax = 10
          n = -1
          fcount = 0
          this%m_deriv = 0.0_dp
          this%m_error = twosup
          init = 3
          contin = .true.
          do
              n = n+1
              if(.NOT. contin) exit
              if(init .EQ. 3) then
                  !  CALCULATE COEFFICIENTS FOR DIFFERENTIATION FORMULAE
                  !   AND NEVILLE EXTRAPOLATION ALGORITHM 
                  if(this%m_iord .EQ. 1) then
                      beta = 2.0_dp
                  else if(method .EQ. 2) then
                      beta = DSQRT(2.0_dp)
                  else
                      beta = DSQRT(3.0_dp)
                  end if
                  beta4 = beta**4
                  z = beta
                  if(method .EQ. 2) z = z**2
                  zpower = 1._R64P
                  do k = 1, nmax
                      zpower = z*zpower
                      denom(k) = zpower-1._dp
                  end do
                  if(method.EQ.2 .AND. this%m_iord.EQ.1) then
                      e(1) = 5.0_dp
                      e(2) = 6.3_dp
                      do i = 3, nmax
                          e(i) = 6.81_dp
                      end do
                  else if((method.NE.2 .AND. this%m_iord.EQ.1) .OR. &
                          (method.EQ.2 .AND. this%m_iord.EQ.2)) then
                           e(1) = 10._dp
                           e(2) = 16._dp
                           e(3) = 20.36_dp
                           e(4) = 23._dp
                           e(5) = 24.46_dp
                           do i = 6, nmax
                               e(i) = 26._dp
                           end do
                           if(method.EQ.2 .AND. this%m_iord.EQ.2) then
                              
                               e = 2.0_dp * e
                           end if
                  else if(method.NE.2 .AND. this%m_iord.EQ.2) then
                          e(1) = 17.78_dp
                          e(2) = 30.06_dp
                          e(3) = 39.66_dp
                          e(4) = 46.16_dp
                          e(5) = 50.26_dp
                          do i = 6, nmax
                              e(6) = 55.0_dp
                          end do
                  else if(method.EQ.2 .AND. this%m_iord.EQ.3) then
                          e(1) = 25.97_dp
                          e(2) = 41.22_dp
                          e(3) = 50.95_dp
                          e(4) = 56.4_dp
                          e(5) = 59.3_dp
                          do i = 6, nmax
                              e(i) = 62.0_dp
                          end do
                  else
                          e(1) = 24.5_dp
                          e(2) = 40.4_dp
                          e(3) = 52.78_dp
                          e(4) = 61.2_dp
                          e(5) = 66.55_dp
                          do i = 6, nmax
                              e(i) = 73.0_dp
                          end do
                          c0f0 = -twof0/(3.0_dp*beta)
                          c1 = 3.0_dp/(3.0_dp*beta-1.0_dp)
                          c2 = -1.0_dp/(3.0_dp*(beta-1.0_dp))
                          c3 = 1.0_dp/(3.0_dp*beta*(5.0_dp-2.0_dp*beta))
                  end if
              end if
              if(init .GE. 2) then
                  !  INITIALIZATION OF STEPLENGTHS, ACCURACY AND OTHER 
                  !   PARAMETERS
                  heval = signh*minh
                  h = heval
                  baseh = heval
                  maxh = maxh2
                  if(method .EQ. 1) maxh = maxh1
                  do k = 1, nmax
                      minerr(k) = twosup
                      ignore(k) = .false.
                  end do
                  if(method .EQ. 1) newacc = acc1
                  if(method .EQ. -1) newacc = acc2
                  if(method .EQ. 2) newacc = (acc1+acc2)*0.5_dp
                  if(newacc .LT. acc) newacc = acc
                  if((method.NE.2 .OR. this%m_iord.EQ.2) .AND. &
                     newacc.LT.acc0 ) newacc = acc0
                  if(method .NE. -1) then
                      facc1 = acc1
                      mhacc1 = hacc1
                      newh1 = h1
                  end if
                  if(method .NE. 1) then
                      facc2 = acc2
                      mhacc2 = hacc2
                      newh2 = h2
                  else
                      facc2 = 0.0_dp
                      nhacc2 = 0.0_dp
                  end if
                  init = 1
                  j = 0
                  saved = .false.
              end if
              !  CALCULATE NEW OR INITIAL FUNCTION VALUES
              if(init.EQ.1 .AND. (n.EQ.0 .OR. this%m_iord.EQ.1) .AND. &
                 .NOT.(method.EQ.2 .AND. fcount.GT.45)) then
                  if(method .EQ. 2) then
                      fcount = fcount+1
                      f1 = this%m_pfunc(this%m_x0+heval)
                      storef(fcount) = f1
                      f2 = this%m_pfunc(this%m_x0-heval)
                      storef(-fcount) = f2
                  else
                      j = j+1
                      if(j .LE. fcount) then
                          f1 = storef(j*method)
                      else
                          f1 = this%m_pfunc(this%m_x0+heval)
                      end if
                  end if
              else
                  f1 = this%m_pfunc(this%m_x0+heval)
                  if(method .EQ. 2) f2 = this%m_pfunc(this%m_x0-heval)
              end if
              if(n .EQ. 0) then
                  if(method.EQ.2 .AND. this%m_iord.EQ.3) then
                      pdelta = f1-f2
                      pmaxf = (DABS(f1)+DABS(f2))*0.5_dp
                      heval = beta*heval
                      f1 = this%m_pfunc(this%m_x0+heval)
                      f2 = this%m_pfunc(this%m_x0-heval)
                      deltaf = f1-f2
                      maxfun = (DABS(f1)+DABS(f2))*0.5_dp
                      heval = beta*heval
                      f1 = this%m_pfunc(this%m_x0+heval)
                      f2 = this%m_pfunc(this%m_x0-heval)
                  else if(method.NE.2 .AND. this%m_iord.GE.2) then
                      if(this%m_iord .EQ. 2) then
                          f3 = f1
                      else
                          f4 = f1
                          heval = beta*heval
                          f3 = this%m_pfunc(this%m_x0+heval)
                      end if
                      heval = beta*heval
                      f2 = this%m_pfunc(this%m_x0+heval)
                      heval = beta*heval
                      f1 = this%m_pfunc(this%m_x0+heval)
                  end if
              end if
              !     EVALUATE A NEW APPROXIMATION DNEW TO THE DERIVATIVE
              if(n .GT. nmax) then
                  n = nmax
                  do i = 1, n
                      maxf(i-1) = maxf(i)
                  end do
              end if
              if(method .EQ. 2) then
                  maxf(n) = (DABS(f1)+DABS(f2))*0.5_dp
                  if(this%m_iord .EQ. 1) then
                      dnew = (f1-f2)*0.5_dp
                  else if(this%m_iord .EQ. 2) then
                      dnew = f1+f2-twof0
                  else
                      dnew = -pdelta
                      pdelta = deltaf
                      deltaf = f1-f2
                      dnew = dnew+0.5_dp*deltaf
                      if(maxf(n) .LT. pmaxf) maxf(n) = pmaxf
                      pmaxf = maxfun
                      maxfun = (DABS(f1)+DABS(f2))*0.5_dp
                  end if
              else
                   maxf(n) = DABS(f1)
                   if(this%m_iord .EQ. 1) then
                       dnew = f1-f0
                   else if(this%m_iord .EQ. 2) then
                       dnew = (twof0-3.0_dp*f3+f1)*v1over3
                       if(maxf(n).LT.DABS(f3)) maxf(n) = DABS(f3)
                       f3 = f2
                       f2 = f1
                   else
                       dnew = c3*f1+c2*f2+c1*f4+c0f0
                       if(maxf(n).LT.DABS(f2)) maxf(n) = DABS(f2)
                       if(maxf(n).LT.DABS(f4)) maxf(n) = DABS(f4)
                       f4 = f3
                       f3 = f2
                       f2 = f1
                   end if
              end if
              if(DABS(h) .GT. 1.0_dp) then
                  dnew = dnew/h**this%m_iord
              else
                  if(128._R64P*DABS(dnew).GT.twosup*DABS(h)**this%m_iord) then
                      dnew = twosup/128.0_dp
                  else
                      dnew = dnew/h**this%m_iord
                  end if
              end if
              if(init .EQ. 0) then
                  !   UPDATE ESTIMATED ACCURACY OF FUNCTION VALUES
                  newacc = acc
                  if((method.NE.2 .OR. this%m_iord.EQ.2) .AND. &
                      newacc.LT.acc0) newacc = acc0
                  if(method.NE.-1 .AND. DABS(nhacc1).LE.1.125_dp*DABS(heval)/beta4) then
                      nhacc1 = heval
                      pacc1 = facc1
                      call this%faccur(mhacc1,newh1,facc1,twoinf.f0,f1)
                      if(facc1.LT.pacc1) facc1 = (3.0_dp*facc1+pacc1)*0.25_dp
                  end if
                  if(method.NE.1 .AND. DABS(nhacc2).LE.1.125_dp*DABS(heval)/beta4) then
                      if(method .EQ. 2) then
                          f1 = f2
                          nhacc2 = -heval
                      else
                          nhacc2 = heval
                      end if
                      pacc2 = facc2
                      call this%faccur(nhacc2,newh2,facc2,twoinf,f0,f1)
                      if(facc2 .LT. pacc2) facc2 = 3._R64P*facc2+pacc2*0.25_dp
                  end if
                  if(method.EQ.1 .AND. newacc.LT.facc1) newacc = facc1
                  if(method.EQ.-1 .AND. newaccLT.facc2) newacc = facc2
                  if(method.EQ.2 .AND. newacc.LT.(facc1+facc2)*0.5_dp)
                       newacc = (facc1+facc2)*0.5_dp
              end if
              !   EVALUATE SUCCESSIVE ELEMENTS OF THE CURRENT ROW IN THE NEVILLE
             !   ARRAY, ESTIMATING AND EXAMINING THE TRUNCATION AND ROUNDING
             !   ERRORS IN EACH
              contin = n.LT.nmax
              hprev = DABS(h)
              fmax = maxf(n)
              if((method.NE.2 .OR. this%m_iord.EQ.2) .AND. &
                  fmax.LT.DABS(f0)) fmax = DABS(f0)
              do k = 1, n
                  dprev = d(k)
                  d(k) = dnew
                  dnew = dprev+(dprev-dnew)/denom(k)
                  te = DABS(dnew-d(k))
                  if(fmax .LT. maxf(n-k)) fmax = maxf(n-k)
                  hprev = hprev/beta
                  if(newacc .GE. relacc*fmax) then
                      re = newacc*e(k)
                  else
                      re = relacc*fmax*e(k)
                  end if
                  if(re .NE. 0.0_dp) then
                      if(hprev .GT. 1._R64P) then
                          re = re/hprev**this%m_iord
                      else if(2.0_dp*re.GT.twosup*hprev**this%m_iord) then
                          re = twosup*0.5_dp
                      else
                          re = re/hprev**this%m_iord
                      end if
                  end if
                  newerr = te+re
                  if(te .GT. re) newerr = 1.25_R64P*newerr
                  if(.NOT. ignore(k)) then
                      if((init.EQ.0 .OR. (k.EQ.2 .AND. .NOT.ignore(1))) &
                                    .AND. newerr.LT.this%m_error) then
                           this%m_deriv = d(k)
                           this%m_error = newerror
                      end if
                      if(init.EQ.1 .AND. n.EQ.1) then
                          tderiv = d(1)
                          temerr = newerr
                      end if
                      if(minerr(k).LT.twosup*0.25_dp) then
                          s =4._R64P*minerr(k)
                      else
                          s = twosup
                      end if
                      if(te.GT.re .OR. newerr.GT.s) then
                          ignore(k) = .true.
                      else
                          contin = .true.
                      end if
                      if(newerr.LT.minerr(k)) minerr(k) = newerr
                      if(init.EQ.1 .AND. n.EQ.2 .AND. k.EQ.1 &
                          .AND. .NOT.ignore(1)) then
                          if(newerr.LT.temerr) then
                              tderiv = d(1)
                              temerr = newerr
                          end if
                          if(temerr.LT.this%m_error) then
                              this%m_deriv = tderiv
                              this%m_error = temerr
                          end if
                      end if
                  end if
              end do
              if(n.LT.nmax) d(n+1) = dnew
                  if(this%m_eps.LT.0.0_dp) then
                      s = DABS(this%m_eps*this%m_deriv)
                  else
                      s = this%m_eps
                  end if
               if(this%m_error.LE.s) then
                   contin = .false.
               else if(init.EQ.1 .AND. (n.EQ.2.OR.ignore(1))) then
                   if((ignore(1) .OR. ignore(2)) .AND. saved) then
                       saved = .false.
                       n = 2
                       h = beta*sav(0)
                       heval = beta*sav(1)
                       maxf(0) = sav(2)
                       maxf(1) = sav(3)
                       maxf(2) = sav(4)
                       d(1) = sav(5)
                       d(2) = sav(6)
                       d(3) = sav(7)
                       minerr(1) = sav(8)
                       minerr(2) = sav(9)
                       if(method.EQ.2 .AND. this%m_iord.EQ.3) then
                          pdelta = sav(10)
                          deltaf = sav(11)
                          pmaxf  = sav(12)
                          maxfun = sav(13)
                       else if(method.NE.2 .AND. this%m_iord.GE.2) then
                           f2 = sav(10)
                           f3 = sav(11)
                           if(this%m_iord.EQ.3) f4 = sav(12)
                       end if
                       init = 0
                       ignore(1) = .false.
                       ignore(2) = .false.
                   else if(.NOT. (ignore(1).OR.ignore(2)) .AND. n.EQ.2 &
                            .AND. beta4*factor*DABS(heval).LE.maxh) then
                       !  SAVE ALL CURRENT VALUES IN CASE OF RETURN TO
                       !  CURRENT POINT
                          saved = .true.
                          sav(0) = h
                          sav(1) = heval
                          sav(2) = maxf(0)
                          sav(3) = maxf(1)
                          sav(4) = maxf(2)
                          sav(5) = d(1)
                          sav(6) = d(2)
                          sav(7) = d(3)
                          sav(8) = minerr(1)
                          sav(9) = minerr(2)
                          if(method.EQ.2 .AND. this%m_iord.EQ.3) then
                              sav(10) = pdelta
                              sav(11) = deltaf
                              sav(12) = pmaxf
                              sav(13) = maxfun
                          else if(method.NE.2 .AND. this%m_iord.GE.2) then
                              sav(10) = f2
                              sav(11) = f3
                              if(this%m_iord.EQ.3) sav(12) = f4
                          end if
                          h = factor*baseh
                          heval = h
                          baseh = h
                          n = -1
                   else
                          init = 0
                          h = beta*h
                          heval = beta*heval
                   end if
               else if(contin .AND. beta*DABS(heval).LE.maxh) then
                   h = beta*h
                   heval = beta*heval
               else if(method.NE.1) then
                   contin = .true.
                   if(method.EQ.2) then
                       init = 3
                       method = -1
                       if(this%m_iord.NE.2) then
                           if(this%m_x0.NE.0.0_dp) then
                               call this%faccur(0.0_dp,-h0,acc0,twoinf,f0,f1)
                           else
                               acc0 = 0.0_dp
                           end if
                       end if
                   else
                       init = 2
                       method = 1
                   end if
                   n = -1
                   signh = -signh
               else
                   contin = .false.
               end if
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  this%m_ifail = 5
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " differentiate: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF          
          if(this%m_eps.LT.0.0_dp) then
              s = DABS(this%m_eps*this%m_deriv)
          else
              s = this%m_eps
          end if
          this%m_ifail = 0
          if(this%m_eps.NE.0.0_dp .AND. this%m_error.GT.s) this%m_ifail = 1
          
    end subroutine                   
                   
    subroutine faccur(this,h0,h1,facc,twoinf,f0,f1)
          implicit none
          class(NevilleDeriv_t), intent(in)    :: this
          real(R64P),            intent(in)    :: h0,h1, &
                                                  facc,  &
                                                  twoinf,&
                                                  f0,f1
          ! Locals
          real(R64P), parameter :: TWOQUART = 0.125_dp
          real(R64P), parameter :: FORQUART = 0.0625_dp
          real(R64P), parameter :: ONEDIV73 = 0.01369863013698630136986301369863_dp
!DIR$ ATTRIBUTES ALIGN : 8 :: a0,a1,f00,deltaf,t0,t1
          real(R64P)               :: a0,a1,f00,deltaf,t0,t1
!DIR$ ATTRIBUTES ALIGN : 64 :: df
          real(R64P), dimension(5) :: df
          integer(I32P)            :: j
          ! Start of executable statements
          t0 = 0._dp
          t1 = 0._dp
          if(h0 .NE. 0._dp) then
              if(this%m_x0+h0 .NE. 0._dp) then
                  f00 = f1
              else
                  h0 = 0.875_dp*h0
                  f00 = this%m_pfunc(this%m_x0+h0)
              end if
              if(DABS(h1) .GE. 32._R64P*twoinf) h1 = h1*TWOQUART
              if(16._dp*DABS(h1) .GT. DABS(h0)) then
                  h1 = DSIGN(h1,1._dp)*DABS(h0)*FORQUART
              end if
              if(this%m_pfunc(this%m_x0+h0-h1) .EQ. f00) then
                  if(256._dp*DABS(h1) .LE. DABS(h0)) then
                      h1 = 2._dp*h1
                      do
                          if(this%m_pfunc(this%m_x0+h0-h1).NE.F00 .OR. &
                              256._dp*DABS(h1).GT.DABS(h0)) exit
                          h1 = 2._dp*h1
                      end do
                      h1 = 8._dp*h1
                  else
                      h1 = DSIGN(h1,1._dp)*DABS(h0)*FORQUART
                  end if
              else
                   if(256._dp*twoinf .LE. DABS(h0)) then
                       do
                           if(this%m_pfunc(this%m_x0+h0-h1*0.5_dp).EQ.f00 .OR. &
                               DABS(h1).LT.4.0_dp*twoinf) exit
                           h1 = h1*0.5_dp
                       end do
                       h1 = 8.0_dp*h1
                       if(16.0_dp*DABS(h1) .GT. DABS(h0)) h1 = DSIGN(h1,1.0_dp) * &
                           DABS(h0)*FORQUART
                   else
                       h1 = DSIGN(h1,1.0_dp)*DABS(h0)*FORQUART
                   end if
              end if
          else
              f00 = f0
          end if
          do j = 1, 5
              f2 = this%m_pfunc(this%m_x0+h0-DBLE(2*j-1)*h1)
              df(j) = f2-f00
              t0 = t0+df(j)
              t1 = t1+DBLE(2*j-1)*df(j)
          end do
          a0 = (33.0_dp*t0-5.0_dp*t1)*ONEDIV73
          a1 = (-5.0_dp*t0+1.2_dp*t1)*ONEDIV73
          facc = DABS(a0)
          do j = 1, 5
              deltaf = DABS(df(j)-(a0+DFLOAT(2*j-1)*a1))
              if(facc .LT. deltaf) facc = deltaf
          end do
          facc = 2.0_dp*facc
    end subroutine           
          
 
    
end module mod_neville