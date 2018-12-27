
    
module mod_test_avx_bindings
  
    !!  ****************** Test Module ********************  !!
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    use mod_avx_bindings
    use mod_kinds, only : int1, int4, sp, dp

    character(len=1),        parameter, private :: dcol = ":"
    
    contains
    
    subroutine test_vec4f64_add_vec4f64()
          !use mod_avx_bindings, only : v4f64, vec4f64_add_vec4f64
          ! Locals only:
          character(len=256) :: emsg
          character(len=256) :: filename
!DIR$     ATTRIBUTES ALIGN : 64 :: vc,vb,va
          type(v4f64) :: vc,vb,va
          character(len=10) :: t
          character(len=8)  :: d
          
          character(len=46), parameter   :: header = "[TEST #1: test_vec4f64_add_vec4f64 -- START]"
          character(len=44), parameter   :: footer = "[TEST #1: test_vec4f64_add_vec4f64 -- END]"
          character(len=21), parameter   :: OUTFILE = "test_vec4f64_add_vec4f64.dat"
          integer(kind=int4), parameter  :: IOUNIT = 100
          integer(kind=int4) :: ioerr = 0
          integer(kind=int4) :: lstart,lend
          logical(kind=int4) :: ioflag = .false.
          ! Exec code ....
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          ioflag = ioerr == 0
          call DATE_AND_TIME(date=d,time=t)
          filename = __FILE__
          lstart   = __LINE__
          print*, d , ":" , t , filename , lstart ,  header
          vc.v = 0.0_8; vb.v = 3.14_8; va.v = 2.71_8
          vc = vec4f64_add_vec4f64(va,vb)
          
          print*, "Calling Fortran-to-C interface -- vec4f64_add_vec4f64"   
          print*, "Call parameters: first --", va.v, " second --",vb.v
          print*, "Result of SIMD addition: -- ", vc.v
          if(ioflag) then
              write(IOUNIT,'(A8,A1,A10,A128)' )  d, dcol, t, filename
              write(IOUNIT,'(I5,A46)' ) lstart+3,header
              write(IOUNIT,'(A56)')  "Calling Fortran-to-C interface -- vec4f64_add_vec4f64" 
              write(IOUNIT,'(A16)')  "Call parameters:"
              write(IOUNIT,'(4F22.15)') va.v(0), va.v(1), va.v(2), va.v(3)
              write(IOUNIT,'(4F22.15)') vb.v(0), vb.v(1), vb.v(2), vb.v(3)
              write(IOUNIT,'(A31)')   "Result of SIMD addition: -- "
              write(IOUNIT,'(4F22.15)') vc.v(0), vc.v(1), vc.v(2), vc.v(3)
          end if
         
          call DATE_AND_TIME(date=d,time=t)
          lend = __LINE__
          print*, d , ":" , t , filename , lend ,  footer
          if(ioflag) then
               write(IOUNIT,'(A8,A1,A10,A128)' ) d, dcol, t, filename
               write(IOUNIT,'(I5,A44)') lend+3,footer
               close(UNIT=IOUNIT,STATUS='KEEP')   
          end if    
    end subroutine test_vec4f64_add_vec4f64
    
    subroutine test_vec4f64_sub_vec4f64()
            ! Locals only:
          character(len=256) :: emsg
          character(len=256) :: filename
!DIR$     ATTRIBUTES ALIGN : 64 :: vc,vb,va
          type(v4f64) :: vc,vb,va
          character(len=10) :: t
          character(len=8)  :: d
          
          character(len=46), parameter   :: header = "[TEST #2: test_vec4f64_sub_vec4f64 -- START]"
          character(len=44), parameter   :: footer = "[TEST #2: test_vec4f64_sub_vec4f64 -- END]"
          character(len=21), parameter   :: OUTFILE = "test_vec4f64_sub_vec4f64.dat"
          integer(kind=int4), parameter  :: IOUNIT = 101_4
          integer(kind=int4) :: ioerr = 0_4
          integer(kind=int4) :: lstart = 0_4, lend = 0_4
          logical(kind=int4) :: ioflag = .false.
          ! Exec code ....
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          ioflag = ioerr == 0
          call DATE_AND_TIME(date=d,time=t)
          filename = __FILE__
          lstart   = __LINE__
          print*, d , ":" , t , filename , lstart ,  header
          vc.v = [0.0_8,0.0_8,0.0_8,0.0_8]
          vb.v = [1.0_8,2.0_8,3.0_8,4.0_8]
          va.v = [0.0_8,1.0_8,2.0_8,3.0_8]
          vc = vec4f64_sub_vec4f64(vb,va)
          
          print*, "Calling Fortran-to-C interface -- vec4f64_sub_vec4f64"   
          print*, "Call parameters: first --", va.v, " second --",vb.v
          print*, "Result of SIMD addition: -- ", vc.v
          if(ioflag) then
              write(IOUNIT,'(A8,A1,A10,A128)' )  d, dcol, t, filename
              write(IOUNIT,'(I5,A46)' ) lstart+3,header
              write(IOUNIT,'(A56)')  "Calling Fortran-to-C interface -- vec4f64_sub_vec4f64" 
              write(IOUNIT,'(A16)')  "Call parameters:"
              write(IOUNIT,'(4F22.15)') va.v(0), va.v(1), va.v(2), va.v(3)
              write(IOUNIT,'(4F22.15)') vb.v(0), vb.v(1), vb.v(2), vb.v(3)
              write(IOUNIT,'(A33)')   "Result of SIMD subtraction: -- "
              write(IOUNIT,'(4F22.15)') vc.v(0), vc.v(1), vc.v(2), vc.v(3)
          end if
          
          call DATE_AND_TIME(date=d,time=t)
          lend = __LINE__
          print*, d , ":" , t , filename , lend ,  footer
          if(ioflag) then
               write(IOUNIT,'(A8,A1,A10,A128)' ) d, dcol, t, filename
               write(IOUNIT,'(I5,A44)') lend+3,footer
               close(UNIT=IOUNIT,STATUS='KEEP')   
          end if 
    end subroutine test_vec4f64_sub_vec4f64
    
    subroutine test_vec4f64_mul_vec4f64()
          ! Locals only:
          character(len=256) :: emsg
          character(len=256) :: filename
!DIR$     ATTRIBUTES ALIGN : 64 :: vc,vb,va
          type(v4f64) :: vc,vb,va
          character(len=10) :: t
          character(len=8)  :: d
          
          character(len=46), parameter   :: header = "[TEST #3: test_vec4f64_mul_vec4f64 -- START]"
          character(len=44), parameter   :: footer = "[TEST #3: test_vec4f64_mul_vec4f64 -- END]"
          character(len=21), parameter   :: OUTFILE = "test_vec4f64_mul_vec4f64.dat"
          integer(kind=int4), parameter  :: IOUNIT = 101_4
          integer(kind=int4) :: ioerr = 0_4
          integer(kind=int4) :: lstart = 0_4, lend = 0_4
          logical(kind=int4) :: ioflag = .false.
          ! Exec code ....
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          ioflag = ioerr == 0
          call DATE_AND_TIME(date=d,time=t)
          filename = __FILE__
          lstart   = __LINE__
          print*, d , ":" , t , filename , lstart ,  header
          vc.v = [0.0_8,0.0_8,0.0_8,0.0_8]
          vb.v = [1.0_8,2.0_8,3.0_8,4.0_8]
          va.v = [1.5_8,2.5_8,3.5_8,4.5_8]
          vc = vec4f64_mul_vec4f64(vb,va)
          
          print*, "Calling Fortran-to-C interface -- vec4f64_mul_vec4f64"   
          print*, "Call parameters: first --", va.v, " second --",vb.v
          print*, "Result of SIMD multiplication: -- ", vc.v
          if(ioflag) then
              write(IOUNIT,'(A8,A1,A10,A128)' )  d, dcol, t, filename
              write(IOUNIT,'(I5,A46)' ) lstart+3,header
              write(IOUNIT,'(A56)')  "Calling Fortran-to-C interface -- vec4f64_mul_vec4f64" 
              write(IOUNIT,'(A16)')  "Call parameters:"
              write(IOUNIT,'(A5,4F22.15)') "first",  va.v(0), va.v(1), va.v(2), va.v(3)
              write(IOUNIT,'(A6,4F22.15)') "second", vb.v(0), vb.v(1), vb.v(2), vb.v(3)
              write(IOUNIT,'(A36)')   "Result of SIMD multiplication: -- "
              write(IOUNIT,'(4F22.15)') vc.v(0), vc.v(1), vc.v(2), vc.v(3)
          end if
         
          call DATE_AND_TIME(date=d,time=t)
          lend = __LINE__
          print*, d , ":" , t , filename , lend ,  footer
          if(ioflag) then
               write(IOUNIT,'(A8,A1,A10,A128)' ) d, dcol, t, filename
               write(IOUNIT,'(I5,A44)') lend+3,footer
               close(UNIT=IOUNIT,STATUS='KEEP')   
          end if 
    end subroutine test_vec4f64_mul_vec4f64
    
    
    
    subroutine test_vec4f64_xxx_vec4f64()
      
         !call test_vec4f64_add_vec4f64()
         !call test_vec4f64_sub_vec4f64()
          call test_vec4f64_mul_vec4f64()
    end subroutine test_vec4f64_xxx_vec4f64




end module mod_test_avx_bindings