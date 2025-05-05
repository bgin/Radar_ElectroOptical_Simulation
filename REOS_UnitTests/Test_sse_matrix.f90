
module mod_test_sse_matrix

     !!  ****************** Test Module ********************  !!
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    use  mod_sse_matrix
    use mod_kinds, only : int1, int4, sp
    
    character(len=1),        parameter, private :: dcol = ":"
     
    contains
    
    subroutine    test_M4x4_constructors()
          ! Locals only
          character(len=256) :: emsg
          character(len=128) :: filename
!DIR$     ATTRIBUTES ALIGN : 64 :: m1
          type(M4x4f32) :: m1
          character(len=43), parameter :: header = "[TEST #1: test_M4x4_constructors -- START]"
          character(len=41), parameter :: footer = "[TEST #1: test_M4x4_constructors -- END]"
          character(len=27), parameter :: OUTFILE = "test_M4x4_constructors.dat"
          character(len=10) :: t
          character(len=8)  :: d
          integer(kind=int4),parameter :: IOUNIT = 100_int4
          integer(kind=int4) :: ioerr = 0
          integer(kind=int4) :: lstart,lend
          logical(kind=int4) :: ioflag = .false.
          ! Exec code 
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          ioflag = ioerr == 0
          call DATE_AND_TIME(DATE=d,TIME=t)
          filename = __FILE__
          lstart = __LINE__
          print*, d , ":" , t , filename , lstart ,  header
          call M4x4f32_setzero(m1)
          print*, "Calling Fortran-to-C interface -- M4x4f32_setzero"
          print*, "Result of SIMD zero-initialization: ",     &
                  "row0: ", m1.row0,                          &
                  "row1: ", m1.row1,                          &
                  "row2: ", m1.row2,                          &
                  "row3: ", m1.row3
          if(ioflag) then
                write(IOUNIT,'(A8,A1,A10,A128)' )  d, dcol, t, filename
                write(IOUNIT,'(I5,A43)' ) lstart+3,header
                write(IOUNIT,'(A52)' ) "Calling Fortran-to-C interface -- M4x4f32_setzero"
                write(IOUNIT,'(A42)' )  "Result of SIMD zero-initialization: "
                write(IOUNIT,'(4F22.15)') m1.row0(0), m1.row0(1), m1.row0(2), m1.row0(3)
                write(IOUNIT,'(4F22.15)') m1.row1(0), m1.row1(1), m1.row1(2), m1.row1(3)
                write(IOUNIT,'(4F22.15)') m1.row2(0), m1.row2(1), m1.row2(2), m1.row2(3)
                write(IOUNIT,'(4F22.15)') m1.row3(0), m1.row3(1), m1.row3(2), m1.row3(3)
          end if
          call DATE_AND_TIME(date=d,time=t)
          lend = __LINE__
          print*, d , ":" , t , filename , lend ,  footer
          if(ioflag) then
               write(IOUNIT,'(A8,A1,A10,A128)' ) d, dcol, t, filename
               write(IOUNIT,'(I5,A41)') lend+3,footer
               close(UNIT=IOUNIT,STATUS='KEEP')   
          end if    
    end subroutine test_M4x4_constructors

end module mod_test_sse_matrix