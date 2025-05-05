
module mod_vspecfuncs_functest

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks. 


  use mod_kinds, only : i1, i4, dp
  use mod_vspecfuncs
  use mod_fpcompare
  implicit none

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
  ! Major version
  integer(kind=4), parameter, public :: MOD_VSPECFUNCS_FUNCTEST_MAJOR = 1
  ! MInor
  integer(kind=4), parameter, public :: MOD_VSPECFUNCS_FUNCTEST_MINOR = 0
  ! Micro
  integer(kind=4), parameter, public :: MOD_VSPECFUNCS_FUNCTEST_MICRO = 0
  ! Module full version
  integer(kind=4), parameter, public :: MOD_VSPECFUNCS_FUNCTEST_FULLVER = 1000*MOD_VSPECFUNCS_FUNCTEST_MAJOR + &
                                                                             100*MOD_VSPECFUNCS_FUNCTEST_MINOR  + &
                                                                             10*MOD_VSPECFUNCS_FUNCTEST_MICRO
  
  ! Module creation date
  character(*),       parameter, public :: MOD_VSPECFUNCS_FUNCTEST_CREATION_DATE = "31/03/2019 17:01 PM GMT+2 (SUN 31 MAR 2019 17:01 -00200) "
  ! Module build date
  character(*),       parameter, public :: MOD_VSPECFUNCS_FUNCTEST_BUILD_DATE = __DATE__
  ! Module build time
  character(*),       parameter, public :: MOD_VSPECFUNCS_FUNCTEST_BUILD_TIME = __TIME__
  ! Module author info
  character(*),       parameter, public :: MOD_VSPECFUNCS_FUNCTEST_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
  ! Module short synopsis
  character(*),       parameter, public :: MOD_VSPECFUNCS_FUNCTEST_SYNOPSIS = "Functional tests of Vector Special Functions module."

  ! LOcals constants
  character(*),       parameter, public :: col = ":"
  character(*),       parameter, public :: demarc = "=============================================================================================="
  integer(kind=4), parameter, private :: BytesReal8 = 8
  integer(kind=4), parameter, private :: L1Dline    = 64
  integer(kind=4), parameter, private :: Page4KiB   = 4096
  integer(kind=4), parameter, private :: Page16MiB  = 4096*Page4KiB
  
contains

  subroutine test_v4_airya_pd()
    use mod_vectypes, only : YMM4r8_t
    use mod_fpcompare
    ! Locals only
    character(len=256), automatic :: emsg
    character(len=128), automatic :: filename
    character(len=40),  automatic, parameter :: header = "[TEST #1: test_v4_airya_pd -- START]"
    character(len=38),  automatic, parameter :: footer = "[TEST #1: test_v4_aitya_pd -- END]"
    character(len=22),    automatic, parameter :: OUTFILE = "test_v4_airya_pd.dat"
    character(len=10),  automatic :: t
    character(len=8),   automatic :: d
   ! integer(kind=int4), automatic, parameter :: L1Delems = L1Dline/BytesReal8
  !  integer(kind=int4), automatic, parameter :: P4KiBelems = Page4KiB/L1Delems
!DIR$ ATTRIBUTES ALIGN : 64 :: vxL1D
    type(YMM4r8_t),     automatic, dimension(2)     :: vxL1D
!DIR$ ATTRIBUTES ALIGN : 64 :: vaiL1D
    type(YMM4r8_t),     automatic, dimension(2)     :: vaiL1D
!DIR$ ATTRIBUTES ALIGN : 64 :: vbiL1D
    type(YMM4r8_t),     automatic, dimension(2)     :: vbiL1D
!DIR$ ATTRIBUTES ALIGN : 64 :: vadL1D
    type(YMM4r8_t),     automatic, dimension(2)     :: vadL1D
!DIR$ ATTRIBUTES ALIGN : 64 :: vbdL1D
    type(YMM4r8_t),     automatic, dimension(2)     :: vbdL1D
!DIR$ ATTRIBUTES ALIGN : 64 :: sxL1D
    real(kind=dp), automatic, dimension(0:7)        :: sxL1D
!DIR$ ATTRIBUTES ALIGN : 64 :: saiL1D
    real(kind=dp), automatic, dimension(0:7)        :: saiL1D
!DIR$ ATTRIBUTES ALIGN : 64 :: sbiL1D
    real(kind=dp), automatic, dimension(0:7)        :: sbiL1D
!DIR$ ATTRIBUTES ALIGN : 64 :: sadL1D
    real(kind=dp), automatic, dimension(0:7)        :: sadL1D
!DIR$ ATTRIBUTES ALIGN : 64 :: sbdL1D
    real(kind=dp), automatic, dimension(0:7)        :: sbdL1D
!DIR$ ATTRIBUTES ALIGN : 64 :: taiL1D,tbiL1D,tadL1D,tbdL1D
    real(kind=dp), automatic, dimension(0:7) :: taiL1D,tbiL1D,tadL1D,tbdL1D
!DIR$ ATTRIBUTES ALIGN : 64 :: fp_flags2
    logical(kind=int4), automatic, dimension(0:127,5) :: fp_flags2
!DIR$ ATTRIBUTES ALIGN : 64 :: fp_flags
    logical(kind=int4), automatic, dimension(2,5)   :: fp_flags
!DIR$ ATTRIBUTES ALIGN : 64 :: aires,bires,adres,bdres
    logical(kind=int4), automatic, dimension(0:7)   :: aires,bires,adres,bdres
!DIR$ ATTRIBUTES ALIGN : 64 :: vxP4KiB 
    type(YMM4r8_t),     automatic, dimension(0:127)   :: vxP4KiB
!DIR$ ATTRIBUTES ALIGN : 64 :: vaiP4KiB
    type(YMM4r8_t),     automatic, dimension(0:127)   :: vaiP4KiB
!DIR$ ATTRIBUTES ALIGN : 64 :: vbiP4KiB
    type(YMM4r8_t),     automatic, dimension(0:127)   :: vbiP4KiB
!DIR$ ATTRIBUTES ALIGN : 64 :: vadP4KiB
    type(YMM4r8_t),     automatic, dimension(0:127)   :: vadP4KiB
!DIR$ ATTRIBUTES ALIGN : 64 :: vbdP4KiB
    type(YMM4r8_t),     automatic, dimension(0:127)   :: vbdP4KiB
!DIR$ ATTRIBUTES ALIGN : 64 :: sxP4KiB
    real(kind=dp),      automatic, dimension(0:511) :: sxP4KiB
!DIR$ ATTRIBUTES ALIGN : 64 :: saiP4KiB
    real(kind=dp),      automatic, dimension(0:511) :: saiP4KiB
!DIR$ ATTRIBUTES ALIGN : 64 :: sbiP4KiB
    real(kind=dp),      automatic, dimension(0:511) :: sbiP4KiB
!DIR$ ATTRIBUTES ALIGN : 64 :: sadP4KiB
    real(kind=dp),      automatic, dimension(0:511) :: sadP4KiB
!DIR$ ATTRIBUTES ALIGN : 64 :: sbdP4KiB
    real(kind=dp),      automatic, dimension(0:511) :: sbdP4KiB
!DIR$ ATTRIBUTES ALIGN : 64 :: taiP4KiB,tbiP4KiB,tadP4KiB,tbdP4KiB
    real(kind=dp),      automatic, dimension(0:511) :: taiP4KiB,tbiP4KiB,tadP4KiB,tbdP4KiB
!DIR$ ATTRIBUTES ALIGN : 64 :: aires4KiB,bires4KiB,adres4KiB,bdres4KiB
    logical(kind=int4), automatic, dimension(0:511) :: aires4KiB,bires4KiB,adres4KiB,bdres4KiB
!!DIR$ ATTRIBUTES ALIGN : 64 :: x
 !   type(YMM4r8_t),     automatic :: x
!!DIR$ ATTRIBUTES ALIGN : 64 :: ai
!    type(YMM4r8_t),     automatic :: ai
!!DIR$ ATTRIBUTES ALIGN : 64 :: bi
!    type(YMM4r8_t),     automatic :: ad
!!DIR$ ATTRIBUTES ALIGN : 64 :: bd
!    type(YMM4r8_t),     automatc  :: bd
!!DIR$ ATTRIBUTES ALIGN : 64 :: vneg1
!    type(YMM4r8_t), automatic, parameter :: vneg1 = YMM4r8_t(-1.0_dp,-1.0_dp,-1.0_dp,-1.0_dp)
!!DIR$ ATTRIBUTES ALIGN : 64 :: vinv512
!    type(YMM4r8_t), automatic, parameter :: vinv512 = YMM4r8_t(0.001953125_dp,0.001953125_dp,0.001953125_dp,0.001953125_dp)
!!DIR$ ATTRIBUTES ALIGN : 64 :: vinv4
!    type(YMM4r8_t), automatic, parameter :: vinv4   = YMM4r8_t(0.25_dp,0.25_dp,0.25_dp,0.25_dp)
!!DIR$ ATTRIBUTES ALIGN : 64 :: vrand1,vrand2
!    type(YMM4r8_t), automatic :: vrand1, vrand2
    integer(kind=4), automatic :: i,j
    integer(kind=4), parameter :: IOUNIT = 102
    integer(kind=4), automatic :: ioerr
    integer(kind=4), automatic :: lstart, lend
    logical(kind=int1), automatic :: ioflag
    ! Exec code .....
    ! First memory touch
    vxL1D(1).v  = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    vxL1D(2).v  = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    vaiL1D(1).v = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    vaiL1D(2).v = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    vbiL1D(1).v = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    vbiL1D(2).v = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    vadL1D(1).v = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    vadL1D(2).v = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    vbdL1D(1).v = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    vbdL1D(2).v = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    sxL1D       = [0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    saiL1D      = [0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    sbiL1D      = [0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    sadL1D      = [0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    sbdL1D      = [0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp]
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
    do i=1, 127
        vxP4KiB(i).v   = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
        vaiP4KiB(i).v  = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
        vbiP4KiB(i).v  = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
        vadP4KiB(i).v  = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
        vbdP4KiB(i).v  = [0.0_dp,0.0_dp,0.0_dp,0.0_dp]
    end do
!DIR$ VECTOR ALIGNED
!DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
    do j=0, 511
       sxP4KiB(i)  = 0.0_dp
       saiP4KiB(i) = 0.0_dp
       sbiP4KiB(i) = 0.0_dp
       sadP4KiB(i) = 0.0_dp
       sbdP4KiB(i) = 0.0_dp
    end do
    ioerr = 0
    open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,ACCESS="SEQUENTIAL",STATUS="NEW")
    ioflag = ioerr == 0
    if(.not.ioflag) then
       print*, "[FATAL-ERROR] -- Open failed with an error message:"
       print*, emsg
       return
    end if
    call DATE_AND_TIME(date=d,time=t)
    filename = __FILE__
    lstart   = __LINE__
    print*, d,":",t,filename,lstart,header
    print*, "Writing an output to the file named: ", OUTFILE
    write(IOUNIT,'(A8,A1,A10,T22,A128)') d,col,t,filename
    write(IOUNIT,'(A4,I5,T27,A40)') 'loc:',lstart+3,header
    write(IOUNIT,'(A88)') demarc
    call RANDOM_SEED()
    call RANDOM_NUMBER(vxL1D(1).v)
    sxL1D(0:3) = vxL1D(1).v(0:3)
    write(IOUNIT,'(A35)') "1st vector argument to v4_airya_pd"
    write(IOUNIT,'(4F22.15)') vxL1D(1).v(0),vxL1D(1).v(1),vxL1D(1).v(2),vxL1D(1).v(3)
    write(IOUNIT,'(A65)') "1st quadruplet of arguments to reference scalar airya"
    write(IOUNIT,'(4F22.15)') sxL1D(0),sxL1D(1),sxL1D(2),sxL1D(3)
    call RANDOM_SEED()
    call RANDOM_NUMBER(vxL1D(2).v)
    sxL1D(4:7) = vxL1D(2).v(0:3)
    write(IOUNIT,'(A35)') "2nd vector argument to v4_airya_pd"
    write(IOUNIT,'(4F22.15)') vxL1D(2).v(0),vxL1D(2).v(1),vxL1D(2).v(2),vxL1D(2).v(3)
    write(IOUNIT,'(A65)') "2nd quadruplet of arguments to reference scalar airya"
    write(IOUNIT,'(4F22.15)') sxL1D(4),sxL1D(5),sxL1D(6),sxL1D(7)
    write(IOUNIT,'(A88)')  demarc
    call RANDOM_SEED()
    do i=0, 511
       call RANDOM_NUMBER(sxP4KiB)
    end do
 !DIR$ VECTOR ALIGNED
    do i=0, 127
       !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
       do j=0, 3
          vxP4KiB(i).v(j) = sxP4KiB(i*3+j)
       end do
    end do
    write(IOUNIT,'(A65)') "Page(4KiB)-sized argument array to reference scalar airya"
    do i=0, 511
       write(IOUNIT,'(A6,I5,F22.15)') "Index:", i, sxP4KiB(i)
    end do
    write(IOUNIT,'(A50)') "Page(4KiB)-sized argument array to vectorized airya"
    write(IOUNIT,'(T16,A6,T30,A4,T53,A4,T75,A4,T97,A4)') "Vector: v(0)", "v(1)", "v(2)", "(v3)"
    do i=0, 127
       write(IOUNIT,'(A6,I5,4F22.15)') "index:", i, vxP4KiB(i).v(0),vxP4KiB(i).v(1),vxP4KiB(i).v(2),vxP4KiB(i).v(3)
    end do
        
   
    write(IOUNIT,'(A88)') demarc
    write(IOUNIT,'(A58)') "Using floating-point reliable comparison functions"
    write(IOUNIT,'(A66)') "Execution started -- small stream (cache line) scalar dataset"
    ! Using floating-point reliable comparison functions
    ! Scalar function call first
    do i=0, 7
       call airya(sxL1D(i),saiL1D(i),sbiL1D(i),sadL1D(i),sbdL1D(i))
    end do
    write(IOUNIT,'(A14)') "Dumping results"
    write(IOUNIT,'(T15,A1,T36,A2,T58,A2,T80,A2,T103,A2)') "x",       "ai",            "bi",              "ad",                 "bd"
    do i=0, 7
       write(IOUNIT,'(5F22.15)') sxL1D(i), saiL1D(i), sbiL1D(i), sadL1D(i), sbdL1D(i)
    end do
    fp_flags = .false.
    write(IOUNIT,'(A88)') demarc
    ! Vector function call second
    write(IOUNIT,'(A45)') "small stream (cache line) 1st 4-tuple vector"
    call v4_airya_pd(vxL1D(1),vaiL1D(1),vbiL1D(1),vadL1D(1),vbdL1D(1),fp_flags(1,:))
    write(IOUNIT,'(A3,T15,4F22.15)') "[x]",  vxL1D(1).v(0), vxL1D(1).v(1), vxL1D(1).v(2), vxL1D(1).v(3)
    write(IOUNIT,'(A4,T15,4F22.15)') "[ai]", vaiL1D(1).v(0), vaiL1D(1).v(1), vaiL1D(1).v(2), vaiL1D(1).v(3)
    write(IOUNIT,'(A4,T15,4F22.15)') "[bi]", vbiL1D(1).v(0), vbiL1D(1).v(1), vbiL1D(1).v(2), vbiL1D(1).v(3)
    write(IOUNIT,'(A4,T15,4F22.15)') "[ad]", vadL1D(1).v(0), vadL1D(1).v(1), vadL1D(1).v(2), vadL1D(1).v(3)
    write(IOUNIT,'(A4,T15,4F22.15)') "[bd]", vbdL1D(1).v(0), vbdL1D(1).v(1), vbdL1D(1).v(2), vbdL1D(1).v(3)
    write(IOUNIT,'(A32,T35,5L1)') "Floating-point exception status", fp_flags(1,1),fp_flags(1,2),fp_flags(1,3),fp_flags(1,4),fp_flags(1,5)
    write(IOUNIT,'(A45)') "single cache line 2nd 4-tuple vector"
    call v4_airya_pd(vxL1D(2),vaiL1D(2),vbiL1D(2),vadL1D(2),vbdL1D(2),fp_flags(2,:))
    write(IOUNIT,'(A3,4F22.15)') "[x]",  vxL1D(2).v(0), vxL1D(2).v(1), vxL1D(2).v(2), vxL1D(2).v(3)
    write(IOUNIT,'(A4,4F22.15)') "[ai]", vaiL1D(2).v(0), vaiL1D(2).v(1), vaiL1D(2).v(2), vaiL1D(2).v(3)
    write(IOUNIT,'(A4,4F22.15)') "[bi]", vbiL1D(2).v(0), vbiL1D(2).v(1), vbiL1D(2).v(2), vbiL1D(2).v(3)
    write(IOUNIT,'(A4,4F22.15)') "[ad]", vadL1D(2).v(0), vadL1D(2).v(1), vadL1D(2).v(2), vadL1D(2).v(3)
    write(IOUNIT,'(A4,4F22.15)') "[bd]", vbdL1D(2).v(0), vbdL1D(2).v(1), vbdL1D(2).v(2), vbdL1D(2).v(3)
    write(IOUNIT,'(A12,A20,T35,A13,T48,A12,T61,A14)') "IEEE_INVALID", "IEEE_DIVIDE_BY_ZERO", "IEEE_OVERFLOW", "IEEE_INEXACT", "IEEE_UNDERFLOW"
    write(IOUNIT,'(T5,L1,T18,L1,T37,L1,T50,L1,T67,L1)')  fp_flags(2,1),fp_flags(2,2),fp_flags(2,3),fp_flags(2,4),fp_flags(2,5)
    write(IOUNIT,'(A88)') demarc
    write(IOUNIT,'(A70)') "Reliable comparison of results --  started"
    write(IOUNIT,'(A40)') "Calling: [EqualTo_Real_Double] function"
    taiL1D(0:3) = vaiL1D(1).v;taiL1D(4:7) = vaiL1D(2).v
    tbiL1D(0:3) = vbiL1D(1).v;tbiL1D(4:7) = vbiL1D(2).v
    tadL1D(0:3) = vadL1D(1).v;tadL1D(4:7) = vadL1D(2).v
    tbdL1D(0:3) = vbdL1D(1).v;tbdL1D(4:7) = vbdL1D(2).v
    do i=0, 7
       aires(i) = saiL1D(i).EqualTo.taiL1D(i)
       bires(i) = sbiL1D(i).EqualTo.tbiL1D(i)
       adres(i) = sadL1D(i).EqualTo.tadL1D(i)
       bdres(i) = sbdL1D(i).EqualTo.tbdL1D(i)
    end do
    write(IOUNIT,'(A70)') "[EqualTo_Real_Double] function executed -- dumping the results"
    do i=0, 7
       if(aires(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T18,A5,T24,I5,T26,L1)') "[EQUAL]", "ai", "Index", i, aires(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T18,A5,T24,I5,T26,L1)') "[NOT-EQUAL]", "ai", "Index", i, aires(i)
       end if
    end do
    do i=0, 7
       if(bires(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T18,A5,T24,I5,T26,L1)') "[EQUAL]", "bi", "Index", i, bires(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T18,A5,T24,I5,T26,L1)') "[NOT-EQUAL]", "bi", "Index", i, bires(i)
       end if
    end do
    do i=0, 7
       if(adres(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T18,A5,T24,I5,T26,L1)') "[EQUAL]", "ad", "Index", i, adres(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T18,A5,T24,I5,T26,L1)') "[NOT-EQUAL]", "ad", "Index", i, adres(i)
       end if
    end do
    do i=0, 7
       if(bdres(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T18,A5,T24,I5,T26,L1)') "[EQUAL]","bd", "Index", i, bdres(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T18,A5,T24,I5,T26,L1)') "[NOT-EQUAL]","bd", "Index", i, bdres(i)
       end if
    end do
    write(IOUNIT,'(A88)') demarc
    aires = .false.
    bires = .false.
    adres = .false.
    bdres = .false.
    write(IOUNIT,'(A40)') "Calling: [Compare_Real_Double] function"
    do i=0, 7
       aires(i) = Compare_Float(saiL1D(i), &
                                taiL1D(i), &
                                1)
       bires(i) = Compare_Float(sbiL1D(i), &
                                tbiL1D(i), &
                                1)
       adres(i) = Compare_Float(sadL1D(i), &
                                tadL1D(i), &
                                1)
       bdres(i) = Compare_Float(sbdL1D(i), &
                                tbdL1D(i), &
                                1)
    end do
    do i=0, 7
       if(aires(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T18,A5,T24,I5,T26,L1)') "[EQUAL]","ai", "Index", i, aires(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T18,A5,T24,I5,T26,L1)') "[NOT-EQUAL]","ai", "Index", i, aires(i)
       end if
    end do
    do i=0, 7
       if(bires(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T18,A5,T24,I5,T26,L1)') "[EQUAL]", "bi", "Index", i, bires(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T18,A5,T24,I5,T26,L1)') "[NOT-EQUAL]", "bi", "Index", i, bires(i)
       end if
    end do
    do i=0, 7
       if(adres(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T18,A5,T24,I5,T26,L1)') "[EQUAL]", "ad", "Index", i, adres(i)
       else
          write(IOUNIT,'(T8,A12,T14,A2,T18,A5,T24,I5,T26,L1)') "[NOT-EQUAL]", "ad", "Index", i, adres(i)
       end if
    end do
    do i=0, 7
       if(bdres(i)) then
          write(IOUNIT,'(T8,A8,T14,A2,T18,A5,T24,I5,T26,L1)') "[EQUAL]","bd", "Index", i, bdres(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T18,A5,T24,I5,T26,L1)') "[NOT-EQUAL]","bd", "Index", i, bdres(i)
       end if
    end do
    write(IOUNIT,'(A88)') demarc
    write(IOUNIT,'(A32)') "Calling: [cwt_Real_Double] function"
    aires = .false.
    bires = .false.
    adres = .false.
    bdres = .false.
    do i=0, 7
       aires(i) = Compares_Within_Tolerance(saiL1D(i), &
                                           taiL1D(i), &
                                           1,         &
                                           1.0e-15_dp)
       bires(i) = Compares_Within_Tolerance(sbiL1D(i), &
                                           tbiL1D(i), &
                                           1, &
                                           1.0e-15_dp)
       adres(i) = Compares_Within_Tolerance(sadL1D(i), &
                                           tadL1D(i), &
                                           1, &
                                           1.0e-15_dp)
       bdres(i) = Compares_Within_Tolerance(sbdL1D(i), &
                                           tbdL1D(i), &
                                           1, &
                                           1.0e-15_dp)
    end do
    do i=0, 7
       if(bires(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T18,A5,T24,I5,T26,L1)') "[EQUAL]", "bi", "Index", i, bires(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T18,A5,T24,I5,T26,L1)') "[NOT-EQUAL]", "bi", "Index", i, bires(i)
       end if
    end do
    do i=0, 7
       if(adres(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T18,A5,T24,I5,T26,L1)') "[EQUAL]", "ad", "Index", i, adres(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T18,A5,T24,I5,T26,L1)') "[NOT-EQUAL]", "ad", "Index", i, adres(i)
       end if
    end do
    do i=0, 7
       if(bdres(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T18,A5,T24,I5,T26,L1)') "[EQUAL]","bd", "Index", i, bdres(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T18,A5,T24,I5,T26,L1)') "[NOT-EQUAL]","bd", "Index", i, bdres(i)
       end if
    end do
    write(IOUNIT,'(A88)') demarc 
    write(IOUNIT,'(A45)') "Page(4KiB)-sized array(scalar,vector) datasets"
    taiP4KiB = 0.0_dp
    tbiP4KiB = 0.0_dp
    tadP4KiB = 0.0_dp
    tbdP4KiB = 0.0_dp
    
    do i=0, 511
       call airya(sxP4KiB(i),saiP4KiB(i),sbiP4KiB(i),sadP4KiB(i),sbdP4KiB(i))
    end do
    write(IOUNIT,'(T15,A1,T36,A2,T58,A2,T80,A2,T103,A2)')  "x",          "ai",            "bi",              "ad",                 "bd"
    do i=0, 511
       write(IOUNIT,'(5F22.15)') sxP4KiB(i),saiP4KiB(i),sbiP4KiB(i),sadP4KiB(i),sbdP4KiB(i)
    end do
    write(IOUNIT,'(A68)') demarc 
    do i=0, 127
       call v4_airya_pd(vxP4KiB(i),vaiP4KiB(i),vbiP4KiB(i),vadP4KiB(i),vbdP4KiB(i),fp_flags2(i,:))
    end do
    write(IOUNIT,'(A35)') "v4_airya_pd: dumping results"
    write(IOUNIT,'(T13,A4,T36,A4,T59,A4,T81,A4)') "x(0)",          "x(1)",           "x(2)",            "x(3)"
    do i=0, 127
       write(IOUNIT,'(4F22.15)') vxP4KiB(i).v(0), vxP4KiB(i).v(1), vxP4KiB(i).v(2), vxP4KiB(i).v(3)
    end do
    write(IOUNIT,'(T13,A5,T36,A5,T59,A5,T81,A5)') "ai(0)",         "ai(1)",          "ai(2)",           "ai(3)"
    do i=0, 127
       write(IOUNIT,'(4F22.15)') vaiP4KiB(i).v(0), vaiP4KiB(i).v(1), vaiP4KiB(i).v(2), vaiP4KiB(i).v(3)
    end do
    write(IOUNIT,'(T13,A5,T36,A5,T59,A5,T81,A5)') "bi(0)",         "bi(1)",          "bi(2)",           "bi(3)"
    do i=0, 127
       write(IOUNIT,'(4F22.15)') vbiP4KiB(i).v(0),  vbiP4KiB(i).v(1), vbiP4KiB(i).v(2), vbiP4KiB(i).v(3)
    end do
    write(IOUNIT,'(T13,A5,T36,A5,T59,A5,T81,A5)')  "ad(0)",        "ad(1)",          "ad(2)",           "ad(3)"
    do i=0, 127
       write(IOUNIT,'(4F22.15)') vadP4KiB(i).v(0),  vadP4KiB(i).v(1), vadP4KiB(i).v(2), vadP4KiB(i).v(3)
    end do
    write(IOUNIT,'(T13,A5,T36,A5,T59,A5,T81,A5)') "bd(0)",         "bd(1)",          "bd(2)",           "bd(3)"
    do i=0, 127
       write(IOUNIT,'(4F22.15)') vbdP4KiB(i).v(0),  vbdP4KiB(i).v(1), vbdP4KiB(i).v(2), vbdP4KiB(i).v(3)
    end do
    write(IOUNIT,'(T9,A12,T22,A20,T39,A13,T53,A12,T65,A14)') "IEEE_INVALID", "IEEE_DIVIDE_BY_ZERO", "IEEE_OVERFLOW", "IEEE_INEXACT", "IEEE_UNDERFLOW"
    do i=0, 127
       write(IOUNIT,'(T12,L1,T25,L1,T45,L1,T60,L1,T75,L1)') fp_flags2(i,1), fp_flags2(i,2), fp_flags2(i,3), fp_flags2(i,4), fp_flags(i,5)
    end do
    write(IOUNIT,'(A88)') demarc
    do i=0, 127
       do j=0, 3
           taiP4KiB(i*3+j) = vaiP4KiB(i).v(j)
           tbiP4KiB(i*3+j) = vbiP4KiB(i).v(j)
           tadP4KiB(i*3+j) = vadP4KiB(i).v(j)
           tbdP4KiB(i*3+j) = vbdP4KiB(i).v(j)
        end do
    end do
     
   
    aires4KiB = .false.
    bires4KiB = .false.
    adres4KiB = .false.
    bdres4KiB = .false.
    write(IOUNIT,'(A40)') "Calling: [EqualTo_Real_Double] function"
    do i=0, 511
       aires4KiB(i) = saiP4KiB(i).EqualTo.taiP4KiB(i)
       bires4KiB(i) = sbiP4KiB(i).EqualTo.tbiP4KiB(i)
       adres4KiB(i) = sadP4KiB(i).EqualTo.tadP4KiB(i)
       bdres4KiB(i) = sbdP4KiB(i).EqualTo.tbdP4KiB(i)
    end do
    write(IOUNIT,'(A70)') "[EqualTo_Real_Double] function executed -- dumping the results"
    do i=0, 511
       if(aires4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]","ai", "Index", i, aires4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]","ai", "Index", i, aires4KiB(i)
       end if
    end do
    do i=0, 511
       if(bires4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]", "bi", "Index", i, bires4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]", "bi", "Index", i, bires4KiB(i)
       end if
    end do
    do i=0, 511
       if(adres4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]", "ad", "Index", i, adres4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]", "ad", "Index", i, adres4KiB(i)
       end if
    end do
    do i=0, 511
       if(bdres4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]","bd", "Index", i, bdres4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]","bd", "Index", i, bdres4KiB(i)
       end if
    end do
    write(IOUNIT,'(A88)') demarc
    aires4KiB = .false.
    bires4KiB = .false.
    adres4KiB = .false.
    bdres4KiB = .false.
    write(IOUNIT,'(A40)') "Calling: [Compare_Real_Double] function"
    do i=0, 511
       aires4KiB(i) = Compare_Float(saiP4KiB(i), &
                                    taiP4KiB(i), &
                                    1)
       bires4KiB(i) = Compare_Float(sbiP4KiB(i), &
                                    tbiP4KiB(i), &
                                     1)
       adres4KiB(i) = Compare_Float(sadP4KiB(i), &
                                    tadP4KiB(i), &
                                    1)
       bdres4KiB(i) = Compare_Float(sbdP4KiB(i), &
                                    tbdP4KiB(i), &
                                    1)
    end do
    do i=0, 511
       if(aires4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]","ai", "Index", i, aires4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]","ai", "Index", i, aires4KiB(i)
       end if
    end do
    do i=0, 511
       if(bires4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]", "bi", "Index", i, bires4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]", "bi", "Index", i, bires4KiB(i)
       end if
    end do
    do i=0, 511
       if(adres4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]", "ad", "Index", i, adres4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]", "ad", "Index", i, adres4KiB(i)
       end if
    end do
    do i=0, 511
       if(bdres4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]","bd", "Index", i, bdres4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]","bd", "Index", i, bdres4KiB(i)
       end if
    end do
    write(IOUNIT,'(A88)') demarc
    aires4KiB = .false.
    bires4KiB = .false.
    adres4KiB = .false.
    bdres4KiB = .false.
    write(IOUNIT,'(A32)') "Calling: [cwt_Real_Double] function"
    do i=0, 511
       aires4KiB(i) = Compares_Within_Tolerance(saiP4KiB(i), &
                                           taiP4KiB(i), &
                                           1,         &
                                           1.0e-15_dp)
       bires4KiB(i) = Compares_Within_Tolerance(sbiP4KiB(i), &
                                           tbiP4KiB(i), &
                                           1, &
                                           1.0e-15_dp)
       adres4KiB(i) = Compares_Within_Tolerance(sadP4KiB(i), &
                                           tadP4KiB(i), &
                                           1, &
                                           1.0e-15_dp)
       bdres4KiB(i) = Compares_Within_Tolerance(sbdP4KiB(i), &
                                               tbdP4KiB(i), &
                                               1, &
                                           1.0e-15_dp)
    end do
    do i=0, 511
       if(aires4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]","ai", "Index", i, aires4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]","ai", "Index", i, aires4KiB(i)
       end if
    end do
    do i=0, 511
       if(bires4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]", "bi", "Index", i, bires4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]", "bi", "Index", i, bires4KiB(i)
       end if
    end do
    do i=0, 511
       if(adres4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]", "ad", "Index", i, adres4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]", "ad", "Index", i, adres4KiB(i)
       end if
    end do
    do i=0, 511
       if(bdres4KiB(i)) then
          write(IOUNIT,'(T1,A8,T14,A2,T17,A5,T20,I5,T25,L1)') "[EQUAL]","bd", "Index", i, bdres4KiB(i)
       else
          write(IOUNIT,'(T1,A12,T14,A2,T17,A5,T20,I5,T25,L1)') "[NOT-EQUAL]","bd", "Index", i, bdres4KiB(i)
       end if
    end do
    write(IOUNIT,'(A55)') "End of test_v4_airya_pd has been reached."
    call DATE_AND_TIME(date=d,time=t)
    lend = __LINE__
    write(IOUNIT,'(A8,A1,A10,A128)') d,col,t,filename
    write(IOUNIT,'(I5,A38)') lend+2, footer
    close(IOUNIT,STATUS='KEEP')
    print*, "End of test_v4_airya_pd has been reached." 
    print*, d,col,t,filename,lend,footer
    
  end subroutine test_v4_airya_pd

  !===============================================================================================!
  subroutine airya ( x, ai, bi, ad, bd )

!*****************************************************************************80
!
!! AIRYA computes Airy functions and their derivatives.
!
!  Licensing:
!
!    The original FORTRAN77 version of this routine is copyrighted by 
!    Shanjie Zhang and Jianming Jin.  However, they give permission to 
!    incorporate this routine into a user program that the copyright 
!    is acknowledged.
!
!  Modified:
!
!    30 June 2012
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!       
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the Airy function.
!
!    Output, real ( kind = 8 ) AI, BI, AD, BD, the values of Ai(x), Bi(x),
!    Ai'(x), Bi'(x).
!
  

  real ( kind = dp )  ad
  real ( kind = dp ) ai
  real ( kind = dp ) bd
  real ( kind = dp ) bi
  real ( kind = dp ) c1
  real ( kind = dp ) c2
  real ( kind = dp ) pir
  real ( kind = dp ) sr3
  real ( kind = dp ) vi1
   real ( kind = dp ) vi2
  real ( kind = dp ) vj1
  real ( kind = dp ) vj2
  real ( kind = dp ) vk1
  real ( kind = dp ) vk2
  real ( kind = dp ) vy1
  real ( kind = dp ) vy2
  real ( kind = dp ) x
  real ( kind = dp ) xa
  real ( kind = dp ) xq
  real ( kind = dp ) z

  xa = abs ( x )
  pir = 0.318309886183891e+00_dp
  c1 = 0.355028053887817e+00_dp
  c2 = 0.258819403792807e+00_dp
  sr3 = 1.732050807568877e+00_dp
  z = xa ** 1.5e+00_dp / 1.5e+00_dp
  xq = sqrt ( xa )

  call ajyik ( z, vj1, vj2, vy1, vy2, vi1, vi2, vk1, vk2 )    

  if ( x == 0.0D+00 ) then
    ai = c1
    bi = sr3 * c1
    ad = - c2
    bd = sr3 * c2
  else if ( 0.0e+00_dp < x ) then
    ai = pir * xq / sr3 * vk1
    bi = xq * ( pir * vk1 + 2.0e+00_dp / sr3 * vi1 ) ! pir * vk1 + 2.0 * invsr3 * vii
    ad = - xa / sr3 * pir * vk2
    bd = xa * ( pir * vk2 + 2.0e+00_dp / sr3 * vi2 )
  else
    ai = 0.5e+00_dp * xq * ( vj1 - vy1 / sr3 )
    bi = - 0.5e+00_dp * xq * ( vj1 / sr3 + vy1 )
    ad = 0.5e+00_dp * xa * ( vj2 + vy2 / sr3 )
    bd = 0.5e+00_dp * xa * ( vj2 / sr3 - vy2 )
  end if

  
end subroutine airya

subroutine ajyik ( x, vj1, vj2, vy1, vy2, vi1, vi2, vk1, vk2 )

!*****************************************************************************80
!
!! AJYIK computes Bessel functions Jv(x), Yv(x), Iv(x), Kv(x).
!
!  Discussion: 
!
!    Compute Bessel functions Jv(x) and Yv(x), and modified Bessel functions 
!    Iv(x) and Kv(x), and their derivatives with v = 1/3, 2/3.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However, 
!    they give permission to incorporate this routine into a user program 
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    31 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument.  X should not be zero.
!
!    Output, real ( kind = 8 ) VJ1, VJ2, VY1, VY2, VI1, VI2, VK1, VK2,
!    the values of J1/3(x), J2/3(x), Y1/3(x), Y2/3(x), I1/3(x), I2/3(x),
!    K1/3(x), K2/3(x).
!
  

  real ( kind = dp ) a0
  real ( kind = dp ) b0
  real ( kind = dp ) c0
  real ( kind = dp ) ck
  real ( kind = dp ) gn
   real ( kind = dp ) gn1
  real ( kind = dp ) gn2
  real ( kind = dp ) gp1
  real ( kind = dp ) gp2
  integer ( kind = 4 ) k
  integer ( kind = 4 ) k0
  integer ( kind = 4 ) l
  real ( kind = dp ) pi
  real ( kind = dp ) pv1
  real ( kind = dp ) pv2
  real ( kind = dp ) px
  real ( kind = dp ) qx
  real ( kind = dp ) r
  real ( kind = dp ) rp
  real ( kind = dp ) rp2
  real ( kind = dp ) rq
  real ( kind = dp ) sk
  real ( kind = dp ) sum
  real ( kind = dp ) uj1
  real ( kind = dp ) uj2
  real ( kind = dp ) uu0
  real ( kind = dp ) vi1
  real ( kind = dp ) vi2
  real ( kind = dp ) vil
  real ( kind = dp ) vj1
  real ( kind = dp ) vj2
  real ( kind = dp ) vjl
  real ( kind = dp ) vk1
  real ( kind = dp ) vk2
  real ( kind = dp ) vl
  real ( kind = dp ) vsl
  real ( kind = dp ) vv
  real ( kind = dp ) vv0
  real ( kind = dp ) vy1
  real ( kind = dp ) vy2
  real ( kind = dp ) x
  real ( kind = dp ) x2
  real ( kind = dp ) xk
   if ( x == 0.0e+00_dp ) then
    vj1 = 0.0e+00_dp
    vj2 = 0.0e+00_dp
    vy1 = -1.0e+300_dp
    vy2 = 1.0e+300_dp
    vi1 = 0.0e+00_dp
    vi2 = 0.0e+00_dp
    vk1 = -1.0e+300_dp
    vk2 = -1.0e+300_dp
    return
  end if

  pi = 3.141592653589793e+00_dp
  rp2 = 0.63661977236758e+00_dp
  gp1 = 0.892979511569249e+00_dp
  gp2 = 0.902745292950934e+00_dp
  gn1 = 1.3541179394264e+00_dp
  gn2 = 2.678938534707747e+00_dp
  vv0 = 0.444444444444444e+00_dp
  uu0 = 1.1547005383793e+00_dp
  x2 = x * x

  if ( x < 35.0e+00_dp ) then
    k0 = 12
  else if ( x < 50.0e+00_dp ) then
    k0 = 10
  else
    k0 = 8
 end if
  if ( x <= 12.0e+00_dp ) then

    do l = 1, 2
      vl = l / 3.0e+00_dp
      vjl = 1.0e+00_dp
      r = 1.0e+00_dp
      do k = 1, 40
        r = -0.25e+00_dp * r * x2 / ( k * ( k + vl ) )
        vjl = vjl + r
        if ( abs ( r ) < 1.0e-15_dp ) then
          exit
        end if
      end do

      a0 = ( 0.5e+00_dp * x ) ** vl
      if ( l == 1 ) then
        vj1 = a0 / gp1 * vjl
      else
        vj2 = a0 / gp2 * vjl
      end if

    end do

  else

    do l = 1, 2

      vv = vv0 * l * l
      px = 1.0e+00_dp
      rp = 1.0e+00_dp

      do k = 1, k0
        rp = - 0.78125e-02_dp * rp &
          * ( vv - ( 4.0e+00_dp * k - 3.0e+00_dp ) ** 2 ) &
          * ( vv - ( 4.0e+00_dp * k - 1.0e+00_dp ) ** 2 ) &
          / ( k * ( 2.0e+00_dp * k - 1.0e+00_dp ) * x2 )
        px = px + rp
      end do

      qx = 1.0e+00_dp
      rq = 1.0e+00_dp
      do k = 1, k0
        rq = - 0.78125e-02_dp * rq &
          * ( vv - ( 4.0e+00_dp * k - 1.0e+00_dp ) ** 2 ) &
          * ( vv - ( 4.0e+00_dp * k + 1.0e+00_dp ) ** 2 ) &
          / ( k * ( 2.0e+00_dp * k + 1.0e+00_dp ) * x2 )
        qx = qx + rq
      end do

      qx = 0.125e+00_dp * ( vv - 1.0e+00_dp ) * qx / x
      xk = x - ( 0.5e+00_dp * l / 3.0e+00_dp + 0.25e+00_dp ) * pi
      a0 = sqrt ( rp2 / x )
      ck = cos ( xk )
      sk = sin ( xk )
      if ( l == 1) then
        vj1 = a0 * ( px * ck - qx * sk )
        vy1 = a0 * ( px * sk + qx * ck )
      else
        vj2 = a0 * ( px * ck - qx * sk )
        vy2 = a0 * ( px * sk + qx * ck )
      end if

    end do

  end if

  if ( x <= 12.0e+00_dp ) then

    do l = 1, 2

      vl = l / 3.0e+00_dp
      vjl = 1.0e+00_dp
      r = 1.0e+00_dp
      do k = 1, 40
        r = -0.25e+00_dp * r * x2 / ( k * ( k - vl ) )
        vjl = vjl + r
        if ( abs ( r ) < 1.0e-15_dp ) then
          exit
        end if
      end do

      b0 = ( 2.0e+00_dp / x ) ** vl
      if ( l == 1 ) then
        uj1 = b0 * vjl / gn1
      else
         uj2 = b0 * vjl / gn2
      end if

    end do

    pv1 = pi / 3.0e+00_dp
    pv2 = pi / 1.5e+00_dp
    vy1 = uu0 * ( vj1 * cos ( pv1 ) - uj1 )
    vy2 = uu0 * ( vj2 * cos ( pv2 ) - uj2 )

  end if

  if ( x <= 18.0e+00_dp ) then

    do l = 1, 2
      vl = l / 3.0e+00_dp
      vil = 1.0e+00_dp
      r = 1.0e+00_dp
      do k = 1, 40
        r = 0.25e+00_dp * r * x2 / ( k * ( k + vl ) )
        vil = vil + r
        if ( abs ( r ) < 1.0e-15_dp ) then
          exit
        end if
      end do

      a0 = ( 0.5e+00_dp * x ) ** vl

      if ( l == 1 ) then
        vi1 = a0 / gp1 * vil
      else
        vi2 = a0 / gp2 * vil
      end if

    end do

  else

    c0 = exp ( x ) / sqrt ( 2.0e+00_dp * pi * x )

    do l = 1, 2
      vv = vv0 * l * l
      vsl = 1.0e+00_dp
      r = 1.0e+00_dp
      do k = 1, k0
        r = - 0.125e+00_dp * r &
          * ( vv - ( 2.0e+00_dp * k - 1.0e+00_dp ) ** 2 ) / ( k * x )
        vsl = vsl + r
      end do
      if ( l == 1 ) then
        vi1 = c0 * vsl
      else
        vi2 = c0 * vsl
      end if
    end do

  end if

  if ( x <= 9.0e+00_dp ) then

    do l = 1, 2
      vl = l / 3.0e+00_dp
      if ( l == 1 ) then
        gn = gn1
      else
        gn = gn2
      end if
      a0 = ( 2.0e+00_dp / x ) ** vl / gn
      sum = 1.0e+00_dp
      r = 1.0e+00_dp
      do k = 1, 60
        r = 0.25e+00_dp * r * x2 / ( k * ( k - vl ) )
        sum = sum + r
        if ( abs ( r ) < 1.0e-15_dp ) then
          exit
        end if
      end do

      if ( l == 1 ) then
        vk1 = 0.5e+00_dp * uu0 * pi * ( sum * a0 - vi1 )
      else
        vk2 = 0.5e+00_dp * uu0 * pi * ( sum * a0 - vi2 )
      end if

    end do

  else

    c0 = exp ( - x ) * sqrt ( 0.5e+00_dp * pi / x )

    do l = 1, 2
      vv = vv0 * l * l
      sum = 1.0e+00_dp
      r = 1.0e+00_dp
      do k = 1, k0
        r = 0.125e+00_dp * r * ( vv - ( 2.0e+00_dp * k - 1.0e+00_dp ) ** 2 ) / ( k * x )
        sum = sum + r
      end do
      if ( l == 1 ) then
        vk1 = c0 * sum
      else
        vk2 = c0 * sum
      end if
    end do

  end if

 
end subroutine  

end module mod_vspecfuncs_functest
