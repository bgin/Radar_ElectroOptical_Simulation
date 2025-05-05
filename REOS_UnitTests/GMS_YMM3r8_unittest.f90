
module mod_YMM3r8_unittest


    !!  ****************** Test Module ********************  !!
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_vectypes, only :  YMM3r8_t
    use mod_kinds,   only : int1, int4, dp
    implicit none
    public
    
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=int4), parameter, public :: MOD_YMM3r8_UNITTEST_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_YMM3r8_UNITTEST_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_YMM3r8_UNITTEST_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_YMM3r8_UNITTEST_FULLVER = 1000_int4*MOD_YMM3r8_UNITTEST_MAJOR + &
                                                                           100_int4*MOD_YMM3r8_UNITTEST_MINOR  + &
                                                                           10_int4*MOD_YMM3r8_UNITTEST_MICRO
                                                                           
    ! Module creation date
    character(*),       parameter, public :: MOD_YMM3r8_UNITTEST_CREATE_DATE =  "31-03-2019 11:26 +00200 (SUN 31 MAR 2019 GMT+2)"
    ! Module build date
    character(*),       parameter, public :: MOD_YMM3r8_UNITTEST_BUILD_DATE = " "
    ! Module author info
    character(*),       parameter, public :: MOD_YMM3r8_UNITTEST_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    ! Module short synopsis
    character(*),       parameter, public :: MOD_YMM3r8_UNITTEST_SYNOPSIS = " Unit Test of XMM3r8 packed SIMD-like(PAOS) type."

    character(len=71),  parameter, private :: demarcation = "====================================================================="
    character(len=1),   parameter, private :: dcol = ":"

    contains
    
    subroutine test_YMM3r8()
        ! Locals only
          character(len=256), automatic :: emsg
          character(len=256), automatic :: filename
          character(len=34),  automatic, parameter :: header = "[TEST #1: test_YMM3r8 -- START]"
          character(len=32),  automatic, parameter :: footer = "[TEST #1: test_YMM3r8 -- END]"
          character(len=15),  automatic, parameter :: OUTFILE = "test_YMM3r8.dat"
          character(len=10),  automatic :: t
          character(len=8),   automatic :: d
!DIR$   ATTRIBUTES ALIGN : 64 :: vmulr,vaddr,vsubr,vdivr,vpowr
          type(YMM3r8_t), automatic :: vmulr,vaddr,vsubr,vdivr,vpowr
!DIR$   ATTRIBUTES ALIGN : 64 :: vsinr,vcosr,vexpr,vtanr
          type(YMM3r8_t), automatic :: vsinr,vcosr,vexpr,vtanr
 !DIR$  ATTRIBUTES ALIGN : 64 :: vatanr,vatan2r,vasinr,vasinhr
          type(YMM3r8_t), automatic :: vatanr,vatan2r,vasinr,vasinhr
!DIR$   ATTRIBUTES ALIGN : 64 :: vabsr,vacosr,vbessj0r,vbessj1r,vbessjnr
          type(YMM3r8_t), automatic :: vabsr,vacosr,vbessj0r,vbessj1r,vbessjnr
!DIR$   ATTRIBUTES ALIGN : 64 :: vbessy0r,vbessy1r,vbessynr,vcotr,vcotdr
          type(YMM3r8_t), automatic :: vbessy0r,vbessy1r,vbessynr,vcotr,vcotdr
!DIR$   ATTRIBUTES ALIGN : 64 :: verfr,verfcr,verfcscalr,vgammar,vhypotr
          type(YMM3r8_t), automatic :: verfr,verfcr,verfcscalr,vgammar,vhypotr
!DIR$   ATTRIBUTES ALIGN : 64 :: vlogr,vlog10r,vlogammar,vmaxr,vsqrtr,vtanhr
          type(YMM3r8_t), automatic :: vlogr,vlog10r,vlogammar,vmaxr,vsqrtr,vtanhr
          ! Arguments for the results vector types
!DIR$   ATTRIBUTES ALIGN : 64 :: vmulv1,vmulv2,vaddv1,vaddv2,vsubv1,vsubv2,vdivv1,vdivv2,vpowv1,vpowv2
          type(YMM3r8_t), automatic :: vmulv1,vmulv2,vaddv1,vaddv2,vsubv1,vsubv2,vdivv1,vdivv2,vpowv1,vpowv2
!DIR$   ATTRIBUTES ALIGN : 64 :: vsinv,vcosv,vexpv,vtanv
          type(YMM3r8_t), automatic :: vsinv,vcosv,vexpv,vtanv
!DIR$   ATTRIBUTES ALIGN : 64 :: vatanv,vatan2v1,vatan2v2,vasinv,vasinhv
          type(YMM3r8_t), automatic :: vatanv,vatan2v1,vatan2v2,vasinv,vasinhv
!DIR$   ATTRIBUTES ALIGN : 64 :: vabsv,vacosv,vbessj0v,vbessj1v,vbessjnv
          type(YMM3r8_t), automatic :: vabsv,vacosv,vbessj0v,vbessj1v,vbessjnv
!DIR$   ATTRIBUTES ALIGN : 64 :: vbessy0v,vbessy1v,vbessynv,vcotv,vcotdv
          type(YMM3r8_t), automatic :: vbessy0v,vbessy1v,vbessynv,vcotv,vcotdv
!DIR$   ATTRIBUTES ALIGN : 64 :: verfv,verfcv,verfcscalv,vgammav,vhypotv1,vhypotv2
          type(YMM3r8_t), automatic :: verfv,verfcv,verfcscalv,vgammav,vhypotv1,vhypotv2
!DIR$   ATTRIBUTES ALIGN : 64 :: vlogv,vlog10v,vlogammav,vmaxv1,vmaxv2,vsqrtv,vtanhv
          type(YMM3r8_t), automatic :: vlogv,vlog10v,vlogammav,vmaxv1,vmaxv2,vsqrtv,vtanhv
          integer(kind=int4), parameter :: IOUNIT = 101
          integer(kind=int4), automatic :: ioerr
          integer(kind=int4), automatic :: lstart,lend
          logical(kind=int1), automatic :: ioflag
          ! EXec code ....
          ioerr = 0
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS="SEQUENTIAL",STATUS="NEW")
          ioflag = ioerr == 0
          call DATE_AND_TIME(date=d,time=t)
          filename = __FILE__
          lstart   = __LINE__
          print*, d,":",t, filename,lstart,header
          if(ioflag) then
               print*, "Writing an output to file: ", OUTFILE
               write(IOUNIT,'(A8,A1,A10,A128)') d,dcol,t,filename
               write(IOUNIT,'(I5,A34)') lstart+2,header
               call RANDOM_SEED()
               call RANDOM_NUMBER(vmulv1.v)
               call RANDOM_NUMBER(vmulv2.v)
               vmulr.v = vmulv1.v*vmulv2.v
               write(IOUNIT,'(A40)') "Packed YMM3r8 type multiplication"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vmulv1.v(0), vmulv1.v(1), vmulv1.v(2)
               write(IOUNIT,'(A9,3F22.15)') "2nd arg: ", vmulv2.v(0), vmulv2.v(1), vmulv2.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vmulr.v(0),  vmulr.v(1),  vmulr.v(2)
               write(IOUNIT,'(A71)')    demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vaddv1.v)
               call RANDOM_NUMBER(vaddv2.v)
               vaddr.v = vaddv1.v+vaddv2.v
               write(IOUNIT,'(A30)') "Packed YMM3r8 type addition"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vaddv1.v(0), vaddv1.v(1), vaddv1.v(2)
               write(IOUNIT,'(A9,3F22.15)') "2nd arg: ", vaddv2.v(0), vaddv2.v(1), vaddv2.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vaddr.v(0),  vaddr.v(1),  vaddr.v(2)
               write(IOUNIT,'(A71)') demarcation
                call RANDOM_SEED()
               call RANDOM_NUMBER(vsubv1.v)
               call RANDOM_NUMBER(vsubv2.v)
               vsubr.v = vsubv1.v-vsubv2.v
               write(IOUNIT,'(A35)') "Packed YMM3r8 type subtraction"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vsubv1.v(0), vsubv1.v(1), vsubv1.v(2)
               write(IOUNIT,'(A9,3F22.15)') "2nd arg: ", vsubv2.v(0), vsubv2.v(1), vsubv2.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vsubr.v(0),  vsubr.v(1),  vsubr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vdivv1.v)
               call RANDOM_NUMBER(vdivv2.v)
               if(vdivv1.v(0) == 0.0_dp) then
                    vdivv1.v(0) = 0.5_dp
               else if(vdivv1.v(1) == 0.0_dp) then
                    vdivv1.v(1) = 0.5_dp
               else if(vdivv1.v(2) == 0.0_dp) then
                    vdivv1.v(2) = 0.5_dp
               end if
               if(vdivv2.v(0) == 0.0_dp) then
                    vdivv2.v(0) = 0.5_dp
               else if(vdivv2.v(1) == 0.0_dp) then
                    vdivv2.v(1) = 0.5_dp
               else if(vdivv2.v(2) == 0.0_dp) then
                    vdivv2.v(2) = 0.5_dp
               end if
               vdivr.v = vdivv1.v/vdivv2.v
               write(IOUNIT,'(A35)') "Packed YMM3r8 type division"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vdivv1.v(0), vdivv1.v(1), vdivv1.v(2)
               write(IOUNIT,'(A9,3F22.15)') "2nd arg: ", vdivv2.v(0), vdivv2.v(1), vdivv2.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vdivr.v(0),  vdivr.v(1),  vdivr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vpowv1.v)
               call RANDOM_NUMBER(vpowv2.v)
               vpowr.v = vpowv1.v**vpowv2.v
               write(IOUNIT,'(A45)') "Packed YMM3r8 type exponentiation"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vpowv1.v(0), vpowv1.v(1), vpowv1.v(2)
               write(IOUNIT,'(A9,3F22.15)') "2nd arg: ", vpowv2.v(0), vpowv2.v(1), vpowv2.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vpowr.v(0),  vpowr.v(1),  vpowr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vsinv.v)
               vsinr.v = sin(vsinv.v)
               write(IOUNIT,'(A45)') "Packed YMM3r8 type sine function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vsinv.v(0), vsinv.v(1), vsinv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vsinr.v(0),  vsinr.v(1), vsinr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vcosv.v)
               vcosr.v = cos(vcosv.v)
               write(IOUNIT,'(A45)') "Packed YMM3r8 type cosine function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vcosv.v(0), vcosv.v(1), vcosv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vcosr.v(0),  vcosr.v(1), vcosr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vexpv.v)
               vexpr.v = exp(vexpv.v)
               write(IOUNIT,'(A45)') "Packed YMM3r8 type exponent function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vexpv.v(0), vexpv.v(1), vexpv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vexpr.v(0),  vexpr.v(1), vexpr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vtanv.v)
               vtanr.v = tan(vtanv.v)
               write(IOUNIT,'(A45)') "Packed YMM3r8 type tangent function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vtanv.v(0), vtanv.v(1), vtanv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vtanr.v(0),  vtanr.v(1), vtanr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vatanv.v)
               vatanr.v = atan(vatanv.v)
               write(IOUNIT,'(A45)') "Packed YMM3r8 type arctangent function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vatanv.v(0), vatanv.v(1), vatanv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vatanr.v(0),  vatanr.v(1), vatanr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vatan2v1.v)
               call RANDOM_NUMBER(vatan2v2.v)
               vatan2r.v = atan2(vatan2v1.v,vatan2v2.v)
               write(IOUNIT,'(A30)') "Packed YMM3r8 type arctangent2"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vatan2v1.v(0), vatan2v1.v(1), vatan2v1.v(2)
               write(IOUNIT,'(A9,3F22.15)') "2nd arg: ", vatan2v2.v(0), vatan2v2.v(1), vatan2v2.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vatan2r.v(0),  vatan2r.v(1), vatan2r.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vasinv.v)
               vasinr.v = asin(vasinv.v)
               write(IOUNIT,'(A45)') "Packed YMM3r8 type arcsin function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vasinv.v(0), vasinv.v(1), vasinv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vasinr.v(0),  vasinr.v(1), vasinr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vasinhv.v)
               vasinhr.v = asinh(vasinhv.v)
               write(IOUNIT,'(A50)') "Packed YMM3r8 type arcsin hyperbolic function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vasinhv.v(0), vasinhv.v(1), vasinhv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vasinhr.v(0),  vasinhr.v(1), vasinhr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vabsv.v)
               vabsr.v = abs(vabsv.v)
               write(IOUNIT,'(A40)') "Packed YMM3r8 type abs function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vabsv.v(0), vabsv.v(1),vabsv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vabsr.v(0),  vabsr.v(1), vabsr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vacosv.v)
               vacosr.v = acos(vacosv.v)
               write(IOUNIT,'(A47)') "Packed YMM3r8 type arccosine function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vacosv.v(0), vacosv.v(1), vacosv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vacosr.v(0),  vacosr.v(1), vacosr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vbessj0v.v)
               vbessj0r.v = bessel_j0(vbessj0v.v)
               write(IOUNIT,'(A45)') "Packed YMM3r8 type Bessel J0 function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vbessj0v.v(0), vbessj0v.v(1), vbessj0v.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vbessj0r.v(0),  vbessj0r.v(1), vbessj0r.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vbessj1v.v)
               vbessj1r.v = bessel_j1(vbessj1v.v)
               write(IOUNIT,'(A45)') "Packed YMM3r8 type Bessel J1 function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vbessj1v.v(0), vbessj1v.v(1), vbessj1v.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vbessj1r.v(0),  vbessj1r.v(1), vbessj1r.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vbessjnv.v)
               vbessjnr.v = bessel_jn(1,vbessjnv.v)
               write(IOUNIT,'(A60)') "Packed YMM3r8 type Bessel JN (order: 1) function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vbessjnv.v(0), vbessjnv.v(1), vbessjnv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vbessjnr.v(0),  vbessjnr.v(1), vbessjnr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vbessy0v.v)
               vbessy0r.v = bessel_y0(vbessy0v.v)
               write(IOUNIT,'(A50)') "Packed YMM3r8 type Bessel Y0 function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vbessy0v.v(0), vbessy0v.v(1),vbessy0v.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vbessy0r.v(0),  vbessy0r.v(1), vbessy0r.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vbessy1v.v)
               vbessy1r.v = bessel_y1(vbessy1v.v)
               write(IOUNIT,'(A50)') "Packed YMM3r8 type Bessel Y1 function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vbessy1v.v(0), vbessy1v.v(1), vbessy1v.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vbessy1r.v(0),  vbessy1r.v(1), vbessy1r.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vbessynv.v)
               vbessynr.v = bessel_yn(1,vbessynv.v)
               write(IOUNIT,'(A68)') "Packed YMM3r8 type Bessel YN (order: 1) function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vbessynv.v(0), vbessynv.v(1), vbessynv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vbessynr.v(0),  vbessynr.v(1), vbessynr.v(2)
               write(IOUNIT,'(A71)')  demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vcotv.v)
               if(vcotv.v(0) == 0.0_dp) then
                    vcotv.v(0) = 0.1_dp
               else if(vcotv.v(1) == 0.0_dp) then
                    vcotv.v(1) = 0.1_dp
               else if(vcotv.v(2) == 0.0_dp) then
                    vcotv.v(2) = 0.1_dp
               end if
               vcotr.v = cotan(vcotv.v)
               write(IOUNIT,'(A58)') "Packed YMM2r8 type contangent(rad) function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vcotv.v(0), vcotv.v(1), vcotv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vcotr.v(0),  vcotr.v(1), vcotr.v(2)
               write(IOUNIT,'(A71)')  demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vcotdv.v)
               if(vcotdv.v(0) == 0.0_dp) then
                    vcotdv.v(0) = 1.0_dp
               else if(vcotdv.v(1) == 0.0_dp) then
                    vcotdv.v(1) = 1.0_dp
               else if(vcotdv.v(2) == 0.0_dp) then
                    vcotdv.v(2) = 1.0_dp
               end if
               vcotdr.v = cotand(vcotdv.v)
               write(IOUNIT,'(A58)') "Packed YMM3r8 type contangent(deg) function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vcotdv.v(0), vcotdv.v(1), vcotdv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vcotdr.v(0),  vcotdr.v(1), vcotdr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(verfv.v)
               verfr.v = erf(verfv.v)
               write(IOUNIT,'(A45)') "Packed YMM3r8 type erf function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", verfv.v(0), verfv.v(1), verfv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", verfr.v(0),  verfr.v(1), verfr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(verfcv.v)
               verfcr.v = erfc(verfcv.v)
               write(IOUNIT,'(A45)') "Packed YMM3r8 type erfc function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", verfcv.v(0), verfcv.v(1),verfcv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", verfcr.v(0),  verfcr.v(1), verfcr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(verfcscalv.v)
               verfcscalr.v = erfc_scaled(verfcscalv.v)
               write(IOUNIT,'(A52)') "Packed YMM3r8 type erfc scaled function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", verfcscalv.v(0), verfcscalv.v(1), verfcscalv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", verfcscalr.v(0),  verfcscalr.v(1), verfcscalr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vgammav.v)
               vgammar.v = gamma(vgammav.v)
               if(vgammav.v(0) == 0.0_dp) then
                    vgammav.v(0) = 0.1_dp
               else if(vgammav.v(1) == 0.0_dp) then
                    vgammav.v(1) = 0.1_dp
               else if(vgammav.v(2) == 0.0_dp) then
                    vgammav.v(2) = 0.1_dp
               end if
               write(IOUNIT,'(A45)') "Packed YMM3r8 type gamma function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vgammav.v(0), vgammav.v(1), vgammav.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vgammar.v(0),  vgammar.v(1), vgammar.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vhypotv1.v)
               call RANDOM_NUMBER(vhypotv2.v)
               vhypotr.v = hypot(vhypotv1.v,vhypotv2.v)
               write(IOUNIT,'(A30)') "Packed YMM3r8 type hypotenuse"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vhypotv1.v(0), vhypotv1.v(1), vhypotv1.v(2)
               write(IOUNIT,'(A9,3F22.15)') "2nd arg: ", vhypotv2.v(0), vhypotv2.v(1), vhypotv2.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vhypotr.v(0),  vhypotr.v(1), vhypotr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vlogv.v)
               if(vlogv.v(0) == 0.0_dp) then
                    vlogv.v(0) = 0.1_dp
               else if(vlogv.v(1) == 0.0_dp) then
                    vlogv.v(1) = 0.1_dp
               else if(vlogv.v(2) == 0.0_dp) then
                    vlogv.v(2) = 0.1_dp
               end if
               vlogr.v = log(vlogv.v)
               write(IOUNIT,'(A32)') "Packed YMM3r8 type log function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vlogv.v(0), vlogv.v(1), vlogv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vlogr.v(0),  vlogr.v(1), vlogr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               CALL RANDOM_NUMBER(vlog10v.v)
               if(vlog10v.v(0) == 0.0_dp) then
                    vlog10v.v(0) = 0.1_dp
               else if(vlog10v.v(1) == 0.0_dp) then
                    vlog10v.v(1) = 0.1_dp
               else if(vlog10v.v(2) == 0.0_dp) then
                    vlog10v.v(2) = 0.1_dp
               end if
               vlog10r.v = log10(vlog10v.v)
               write(IOUNIT,'(A32)') "Packed YMM3r8 type log10 function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vlog10v.v(0), vlog10v.v(1), vlog10v.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vlog10r.v(0),  vlog10r.v(1), vlog10r.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vlogammav.v)
               if(vlogammav.v(0) == 0.0_dp) then
                    vlogammav.v(0) = 0.1_dp
               else if(vlogammav.v(1) == 0.0_dp) then
                    vlogammav.v(1) = 0.1_dp
               else if(vlogammav.v(2) == 0.0_dp) then
                    vlogammav.v(2) = 0.1_dp
               end if
               vlogammar.v = log_gamma(vlogammav.v)
               write(IOUNIT,'(A40)') "Packed YMM3r8 type log gamma function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vlogammav.v(0), vlogammav.v(1), vlogammav.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vlogammar.v(0),  vlogammar.v(1), vlogammar.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vmaxv1.v)
               call RANDOM_NUMBER(vmaxv2.v)
               vmaxr.v = max(vmaxv1.v,vmaxv2.v)
               write(IOUNIT,'(A45)') "Packed YMM3r8 type max function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vmaxv1.v(0), vmaxv1.v(1), vmaxv1.v(2)
               write(IOUNIT,'(A9,3F22.15)') "2nd arg: ", vmaxv2.v(0), vmaxv2.v(1), vmaxv2.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vmaxr.v(0),  vmaxr.v(1),  vmaxr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vsqrtv.v)
               vsqrtr.v = sqrt(vsqrtv.v)
               write(IOUNIT,'(A40)') "Packed YMM3r8 type sqrt function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vsqrtv.v(0), vsqrtv.v(1), vsqrtv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vsqrtr.v(0),  vsqrtr.v(1), vsqrtr.v(2)
               write(IOUNIT,'(A71)') demarcation
               call RANDOM_SEED()
               call RANDOM_NUMBER(vtanhv.v)
               vtanhr.v = tanh(vtanhv.v)
               write(IOUNIT,'(A55)') "Packed YMM3r8 type tangent hyperbolic function"
               write(IOUNIT,'(A9,3F22.15)') "1st arg: ", vtanhv.v(0), vtanhv.v(1), vtanhv.v(2)
               write(IOUNIT,'(A9,3F22.15)') "Result : ", vtanhr.v(0),  vtanhr.v(1), vtanhr.v(2)
               write(IOUNIT,'(A71)') demarcation
               write(IOUNIT,'(A78)') "============== End of XMM2r8-Test has been reached ===================="
               call DATE_AND_TIME(date=d,time=t)
               lend = __LINE__
               write(IOUNIT,'(A8,A1,A10,A128)') d, dcol, t, filename
               write(IOUNIT,'(I5,A32)') lend+2, footer
               close(UNIT=IOUNIT,STATUS='KEEP')
          else
               print*, emsg
               ERROR STOP "Failed to open file: -- exiting!!"
          end if
          print*, "============== End of YMM3r8-Test has been reached ===================="
    end subroutine test_YMM3r8
    
end module mod_YMM3r8_unittest
