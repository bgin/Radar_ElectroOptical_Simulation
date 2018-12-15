
module mod_test_quaternion

!!  ****************** Test Module ********************  !!
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_quaternion
    use mod_kinds, only : int1,int4,dp
    implicit none
    
    !character(*), parameter, private :: FILENAME = "TEST_Quaternion.txt"
    !integer(kind=int4), parameter, private :: IOUNIT = 1
    !integer(kind=int4), save :: ioerr
    
    type, private :: MatrixR3
          sequence
          real(kind=dp) :: a11,a12,a13
          real(kind=dp) :: a21,a22,a23
          real(kind=dp) :: a31,a32,a33
    end type MatrixR3
    
    type, private :: MatrixR4
          sequence
          real(kind=dp) :: a11,a12,a13,a14
          real(kind=dp) :: a21,a22,a23,a24
          real(kind=dp) :: a31,a32,a33,a34
          real(kind=dp) :: a41,a42,a43,a44
    end type MatrixR4
    
    real(kind=dp), parameter, private :: smallest = TINY(1.0_dp)
    
    contains
    
    
    subroutine test_default_init()
         
          ! Locals only:
          character(len=256) :: emsg
          character(len=256) :: filename
!DIR$     ATTRIBUTES ALIGN : 64 :: q1,q2,q3
          type(Quaternion_t) :: q1,q2,q3
          real(kind=dp), parameter :: dzero = 0.0E+00_dp
          character(len=10) :: t
          character(len=8)  :: d
          character(len=11), parameter   :: qstr1="Quaternion1", & 
                                            qstr2="Quaternion2", &
                                            qstr3="Quaternion2"
          character(len=37), parameter   :: header = "[TEST #1: test_default_init -- START]"
          character(len=35), parameter   :: footer = "[TEST #1: test_default_init -- END]"
          character(len=21),   parameter :: OUTFILE = "test_default_init.dat"
          integer(kind=int4), parameter  :: IOUNIT = 1
          integer(kind=int4) :: ioerr = 0
          integer(kind=int4) :: lstart,lend
          logical(kind=int4) :: ioflag
          ! Exec code ....
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          ioflag = ioerr == 0
          call DATE_AND_TIME(date=d,time=t)
          filename = __FILE__
          lstart   = __LINE__
          print*, d , ":" , t , filename , lstart ,  header
          call q1.default_init()
          call q2.default_init()
          call q3.default_init()
          if(ioflag) WRITE(IOUNIT,'(A37)') header
          if(q1.m_q.v(0) == dzero .and. &
             q1.m_q.v(1) == dzero .and. &
             q1.m_q.v(2) == dzero .and. &
             q1.m_q.v(3) == dzero     )  then
              print*, "q1 -- instantiation: succedded!!"
              print*, q1.m_q.v
              if(ioflag) then
                  
                  write(IOUNIT, '(A11)') qstr1
                  write(IOUNIT,'(4F22.15)')  q1.m_q.v(0),q1.m_q.v(1),q1.m_q.v(2),q1.m_q.v(3)
              end if
          else
              print*, "q1 -- instantiation: failed!!" 
          end if
          if(q2.m_q.v(0) == dzero .and. &
             q2.m_q.v(1) == dzero .and. &
             q2.m_q.v(2) == dzero .and. &
             q2.m_q.v(3) == dzero     )  then
              print*, "q2 -- instantiation: succedded!!"
              print*, q2.m_q.v
              if(ioflag) then
                  write(IOUNIT, '(A11)') qstr2
                  write(IOUNIT,'(4F22.15)')  q2.m_q.v(0),q2.m_q.v(1),q2.m_q.v(2),q2.m_q.v(3)
              end if
          else
              print*, "q2 -- instantiation: failed!!" 
          end if
          if(q3.m_q.v(0) == dzero .and. &
             q3.m_q.v(1) == dzero .and. &
             q3.m_q.v(2) == dzero .and. &
             q3.m_q.v(3) == dzero     )  then
              print*, "q3 -- instantiation: succedded!!"
              print*, q3.m_q.v
              if(ioflag) then
                  write(IOUNIT, '(A11)') qstr3
                   write(IOUNIT,'(4F22.15)')  q3.m_q.v(0),q3.m_q.v(1),q3.m_q.v(2),q3.m_q.v(3)
              end if
          else
              print*, "q3 -- instantiation: failed!!" 
          end if
          write(IOUNIT,'(A35)') footer
          call DATE_AND_TIME(date=d,time=t)
          lend = __LINE__
          print*, d , ":" , t , filename , lend ,  footer
          close( UNIT=IOUNIT,STATUS='KEEP')
    end subroutine test_default_init
    
    subroutine test_elemwise_init()
          ! Locals only
          character(len=256) :: emsg
          character(len=256) :: filename
!DIR$     ATTRIBUTES ALIGN : 64 :: q1,q2,q3,q4,q5,q6
          type(Quaternion_t) :: q1,q2,q3,q4,q5,q6
          character(len=37),   parameter  :: header = "[TEST #2: test_elemwise_init -- START]"
          character(len=35),   parameter  :: footer = "[TEST #2: test_elemwise_init -- END]"
          character(len=22),   parameter  :: OUTFILE = "test_elemwise_init.dat"
          character(len=11),   parameter  :: qstr1="Quaternion1",qstr2="Quaternion2",  &
                                             qstr3="Quaternion3",qstr4="Quaternion4",  &
                                             qstr5="Quaternion5",qstr6="Quaternion6"
          character(len=10)  :: t
          character(len=8)   :: d
          ! First 3 quaternions are initialized by real numbers with zero-value decimal part.
          ! Second 3 quaternions are initialized by real numbers with non-zero fractional part.
          real(kind=dp), parameter :: c0  = 1.0_dp,c1 = 2.0_dp,c2 = 3.0_dp,c3 = 4.0_dp
          real(kind=dp), parameter :: cf0 = 1.1_dp, cf1 = 2.1_dp, cf2 = 3.1_dp, cf3 = 4.1_dp
          integer(kind=int4), parameter :: IOUNIT = 2
          integer(kind=int4) :: ioerr = 0
          integer(kind=int4) :: lstart, lend
          logical(kind=int4) :: ioflag
          ! Exec code ...
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          ioflag = ioerr == 0
          call DATE_AND_TIME(DATE=d,TIME=t)
          filename = __FILE__
          lstart   = __LINE__
          print*, d , ":" , t , filename , lstart ,  header
          print*, " Real-numbers zero-valued decimal part."
          call q1.elemwise_init(1.0E+00_dp,2.0E+00_dp,3.0E+00_dp,4.0E+00_dp)
          call q2.elemwise_init(1.0E+00_dp,2.0E+00_dp,3.0E+00_dp,4.0E+00_dp)
          call q3.elemwise_init(1.0E+00_dp,2.0E+00_dp,3.0E+00_dp,4.0E+00_dp)
          if(ioflag) write(IOUNIT,'(A37)') header
          if( q1.m_q.v(0) == c0 .and.  &
              q1.m_q.v(1) == c1 .and.  &
              q1.m_q.v(2) == c2 .and.  &
              q1.m_q.v(3) == c3       ) then
                print*, "q1 -- instantiation: succedded!!"
                print*, q1.m_q.v
                if(ioflag) then
                    write(IOUNIT,'(A11)') qstr1
                    write(IOUNIT,'(4F22.15)') q1.m_q.v(0), q1.m_q.v(1), q1.m_q.v(2), q1.m_q.v(3)
                end if
           else
                print*, "q1 -- instantiation: failed!!"  
           end if
           if( q2.m_q.v(0) == c0 .and. &
               q2.m_q.v(1) == c1 .and. &
               q2.m_q.v(2) == c2 .and. &
               q2.m_q.v(3) == c3      )  then
                  print*, "q2 -- instantiation: succedded!!"
                  print*, q2.m_q.v
                  if(ioflag) then
                      write(IOUNIT,'(A11)') qstr2
                      write(IOUNIT,'(4F22.15)' ) q2.m_q.v(0), q2.m_q.v(1), q2.m_q.v(2), q2.m_q.v(3)
                  end if
            else
                  print*, "q2 -- instantiation: failed!!" 
            end if
            if( q3.m_q.v(0) == c0 .and. &
                q3.m_q.v(1) == c1 .and. &
                q3.m_q.v(2) == c2 .and. &
                q3.m_q.v(3) == c3      ) then
                    print*, "q3 -- instantiation: succedded!!"
                    print*, q3.m_q.v
                    if(ioflag) then
                        write(IOUNIT,'(A11)') qstr3
                        write(IOUNIT,'(4F22.15)' ) q3.m_q.v(0), q3.m_q.v(1), q3.m_q.v(2), q3.m_q.v(3)
                    end if
             else
                   print*, "q3 -- instantiation: failed!!"  
             end if
             print*, "Real-numbers non-zero valued decimal part."
             call q4.elemwise_init(1.1_dp,2.1_dp,3.1_dp,4.1_dp)
             call q5.elemwise_init(1.1_dp,2.1_dp,3.1_dp,4.1_dp)
             call q6.elemwise_init(1.1_dp,2.1_dp,3.1_dp,4.1_dp)
             print*, "Floating-point comparison -- using operator '==' "
             if( q4.m_q.v(0) == cf0 .and. &
                 q4.m_q.v(1) == cf1 .and. &
                 q4.m_q.v(2) == cf2 .and. &
                 q4.m_q.v(3) == cf3     ) then
                      print*, "q4 -- instantiation: succedded!!"
                      print*, q4.m_q.v
                      if(ioflag) then
                          write(IOUNIT,'(A11)') qstr4
                          write(IOUNIT,'(4F22.15)' ) q4.m_q.v(0), q4.m_q.v(1), q4.m_q.v(2), q4.m_q.v(3)
                      end if
              else
                  print*, "q4 -- instantiation: failed!!"  
              end if
              if( q5.m_q.v(0) == cf0 .and. &
                  q5.m_q.v(1) == cf1 .and. &
                  q5.m_q.v(2) == cf2 .and. &
                  q5.m_q.v(3) == cf3    ) then
                      print*, "q5 -- instantiation: succedded!!"  
                      print*, q5.m_q.v
                      if(ioflag) then
                          write(IOUNIT,'(A11)' ) qstr5
                          write(IOUNIT,'(4F22.15)' ) q5.m_q.v(0), q5.m_q.v(1), q5.m_q.v(2), q5.m_q.v(3)
                      end if
              else
                      print*, "q5 -- instantiation: failed!!" 
              end if
              if( q6.m_q.v(0) == cf0 .and. &
                  q6.m_q.v(1) == cf1 .and. &
                  q6.m_q.v(2) == cf2 .and. &
                  q6.m_q.v(3) == cf3    ) then
                        print*, "q6 -- instantiation: succedded!!"  
                        print*,  q6.m_q.v
                        if(ioflag) then
                            write(IOUNIT,'(A11)' ) qstr6
                            write(IOUNIT,'(4F22.15)' ) q6.m_q.v(0), q6.m_q.v(1), q6.m_q.v(2), q6.m_q.v(3)
                        end if
              else
                      print*, "q6 -- instantiation: failed!!" 
              end if
              write(IOUNIT,'(A35)' ) footer
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d ,":", t , filename , lend ,  footer
              close(UNIT=IOUNIT,STATUS='KEEP')
    end subroutine test_elemwise_init
    
    subroutine test_complex_init()
          ! Locals only
          character(len=256) :: emsg
          character(len=256) :: filename
!DIR$     ATTRIBUTES ALIGN : 64 :: q1,q2,q3
          type(Quaternion_t) :: q1,q2,q3
          character(len=21), parameter :: OUTFILE = "test_complex_init.dat"
          character(len=40), parameter :: header   = "[TEST #3: test_complex_init -- START]"
          character(len=37), parameter :: footer   = "[TEST #3: test_complex_init -- END]"
          character(len=11), parameter :: qstr1 = "Quaternion1",  &
                                          qstr2 = "Quaternion2",  &
                                          qstr3 = "Quaternion3"
          character(len=1), parameter :: dcol = ":"
          character(len=10)  :: t
          character(len=8)   :: d
          complex(kind=dp), parameter :: c1 = CMPLX(1.0_dp,1.0_dp,kind=dp),  &
                                         c2 = CMPLX(2.0_dp,2.0_dp,kind=dp),  &
                                         c3 = CMPLX(3.0_dp,3.0_dp,kind=dp),  &
                                         c4 = CMPLX(4.0_dp,4.0_dp,kind=dp),  &
                                         c5 = CMPLX(5.0_dp,5.0_dp,kind=dp),  &
                                         c6 = CMPLX(6.0_dp,6.0_dp,kind=dp)
          integer(kind=int4), parameter :: IOUNIT = 3
          integer(kind=int4)            :: ioerr = 0
          integer(kind=int4)            :: lstart,lend
          logical(kind=int4)            :: ioflag
          ! Exec code ....
          
          filename   = __FILE__
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          ioflag = ioerr == 0
          call DATE_AND_TIME(DATE=d,TIME=t)
          lstart     = __LINE__
          print*, d , dcol , t , filename ,lstart+1 ,  header
          if(ioflag) write(IOUNIT,'(A8,A1,A10,A256,I5,A40)' ) d, dcol, t, filename, lstart+2, header
          call q1.complex_init(c1,c2)
          call q2.complex_init(c3,c4)
          call q3.complex_init(c5,c6)
          if(   q1.m_q.v(0) == REAL(c1,kind=dp) .and. &
                q1.m_q.v(1) == AIMAG(c1)        .and. &
                q1.m_q.v(2) == REAL(c2,kind=dp) .and. &
                q1.m_q.v(3) == AIMAG(c2)    )   then
                      print*, "q1 -- instantiation: succedded!!"
                      print*, q1.m_q.v
                      if(ioflag) then
                          write(IOUNIT,'(A11)' )        qstr1
                          write(IOUNIT,'(4F22.15)' )    q1.m_q.v(0), q1.m_q.v(1), q1.m_q.v(2), q1.m_q.v(3)
                      end if
           else
               print*,  "q1 -- instantiation: failed!!"  
           end if
           if(  q2.m_q.v(0) == REAL(c3,kind=dp) .and. &
                q2.m_q.v(1) == AIMAG(c3)        .and. &
                q2.m_q.v(2) == REAL(c4,kind=dp) .and. &
                q2.m_q.v(3) == AIMAG(c4)   )   then
                     print*, "q2 -- instantiation: succedded!!"
                     print*, q2.m_q.v
                     if(ioflag) then
                         write(IOUNIT,'(A11)' )      qstr2
                         write(IOUNIT,'(4F22.15)'  ) q2.m_q.v(0), q2.m_q.v(1), q2.m_q.v(2), q2.m_q.v(3)
                     end if
          else
                    print*, "q2 -- instantiation: failed!!"
          end if
          if(  q3.m_q.v(0) == REAL(c5,kind=dp) .and. &
               q3.m_q.v(1) == AIMAG(c5)        .and. &
               q3.m_q.v(2) == REAL(c6,kind=dp) .and. &
               q3.m_q.v(3) == AIMAG(c6)     )  then
                    print*, "q3 -- instantiation: succedded!!"  
                    print*, q3.m_q.v
                    if(ioflag) then
                        write(IOUNIT,'(A11)' )    qstr3
                        write(IOUNIT,'(4F22.15)' ) q3.m_q.v(0), q3.m_q.v(1), q3.m_q.v(2), q3.m_q.v(3)
                    end if
          else
               print*, "q3 -- instantiation: failed!!"  
          end if
          call DATE_AND_TIME(DATE=d,TIME=t)
          lend = __LINE__
          print*, d , dcol , t , filename ,lend+1 ,  footer
          if(ioflag) write(IOUNIT,'(A8,A1,A10,A256,I5,A37)' ) d, dcol, t, filename, lend+2, footer
          close(UNIT=IOUNIT,STATUS='KEEP')
    end subroutine test_complex_init
    
    subroutine test_array_init()
          ! Locals only
          character(len=256) :: emsg
          character(len=256) :: filename
!DIR$     ATTRIBUTES ALIGN : 64 :: q1,q2,q3
          type(Quaternion_t)           :: q1,q2,q3
          character(len=19),     parameter :: OUTFILE = "test_array_init.dat"
          character(len=35),     parameter :: header  = "[TEST #4: test_array_init -- START]"
          character(len=33),     parameter :: footer  = "[TEST #3: test_array_init -- END]"
          character(len=11),     parameter :: qstr1 = "Quaternion1",  &
                                              qstr2 = "Quaternion2",  &
                                              qstr3 = "Quaternion3"
          character(len=1),      parameter :: dcol = ":"
          character(len=10)  :: t
          character(len=8)   :: d
!DIR$     ATTRIBUTES ALIGN : 64 :: a0,b0,c0
          real(kind=dp), dimension(0:3), parameter :: a0 = [0.0_dp,1.0_dp,2.0_dp,3.0_dp]
          real(kind=dp), dimension(0:3), parameter :: b0 = [0.1_dp,0.2_dp,0.3_dp,0.4_dp]
          real(kind=dp), dimension(0:3), parameter :: c0 = [1.1_dp,2.1_dp,3.1_dp,4.1_dp]
          integer(kind=int4),            parameter :: IOUNIT = 4
          integer(kind=int4)                       :: ioerr = 0
          integer(kind=int4)                       :: lstart,lend
          logical(kind=int4)                       :: ioflag
          ! Exec code ....
         
          filename = __FILE__
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          ioflag = ioerr == 0
          call DATE_AND_TIME(DATE=d,TIME=t)
          lstart     = __LINE__
          print*, d , dcol , t , filename ,lstart+1 ,  header
          if(ioflag) write(IOUNIT,'(A8,A1,A10,A256,I5,A35)' ) d, dcol, t, filename, lstart+2, header
          call q1.array_init([0.0_dp,1.0_dp,2.0_dp,3.0_dp])
          call q2.array_init([0.1_dp,0.2_dp,0.3_dp,0.4_dp])
          call q3.array_init([1.1_dp,2.1_dp,3.1_dp,4.1_dp])
          if(   q1.m_q.v(0) == a0(0) .and. &
                q1.m_q.v(1) == a0(1) .and. &
                q1.m_q.v(2) == a0(2) .and. &
                q1.m_q.v(3) == a0(3)   )   then
                      print*, "q1 -- instantiation: succedded!!"
                      print*, q1.m_q.v
                      if(ioflag) then
                          write(IOUNIT,'(A11)' ) qstr1
                          write(IOUNIT,'(4F22.15)' ) q1.m_q.v(0), q1.m_q.v(1), q1.m_q.v(2), q1.m_q.v(3)
                      end if
          else
                   print*, "q1 -- instantiation: failed!!" 
         end if
         if(  q2.m_q.v(0) == b0(0) .and.  &
              q2.m_q.v(1) == b0(1) .and.  &
              q2.m_q.v(2) == b0(2) .and.  &
              q2.m_q.v(3) == b0(3)    )   then
                     print*, "q2 -- instantiation: succedded!!"
                     print*, q2.m_q.v
                     if(ioflag) then
                         write(IOUNIT,'(A11)' ) qstr2
                         write(IOUNIT,'(4F22.15)' ) q2.m_q.v(0), q2.m_q.v(1), q2.m_q.v(2), q2.m_q.v(3)
                     end if
         else
               print*, "q2 -- instantiation: failed!!"
         end if
         if(    q3.m_q.v(0) == c0(0) .and. &
                q3.m_q.v(1) == c0(1) .and. &
                q3.m_q.v(2) == c0(2) .and. &
                q3.m_q.v(3) == c0(3)    )   then
                      print*, "q3 -- instantiation: succedded!!"  
                      print*, q3.m_q.v  
                      if(ioflag)  then
                          write(IOUNIT,'(A11)' ) qstr3
                          write(IOUNIT,'(4F22.15)' ) q3.m_q.v(0), q3.m_q.v(1), q3.m_q.v(2), q3.m_q.v(3)
                      end if
         else
                    print*, "q3 -- instantiation: failed!!" 
         end if
         call DATE_AND_TIME(DATE=d,TIME=t)
         lend = __LINE__
         print*, d , dcol , t , filename ,lend+1 ,  footer
         if(ioflag) write(IOUNIT,'(A8,A1,A10,A256,I5,A37)' ) d, dcol, t, filename, lend+2, footer
         close(UNIT=IOUNIT,STATUS='KEEP')       
    end subroutine test_array_init
    
   subroutine test_scpart_init()
          ! Locals
          character(len=256) :: emsg
          character(len=256) :: filename
!DIR$     ATTRIBUTES ALIGN : 64 :: q1,q2,q3
          type(Quaternion_t) :: q1,q2,q3
          character(len=20),       parameter :: OUTFILE = "test_scpart_init.dat"
          character(len=36),       parameter :: header  = "[TEST #4: test_scpart_init -- START]"
          character(len=34),       parameter :: footer  = "[TEST #4: test_scpart_init -- END]"
          character(len=11),       parameter :: qstr1 = "Quaternion1",  &
                                                qstr2 = "Quaternion2",  &
                                                qstr3 = "Quaternion3"
          character(len=1),        parameter :: dcol = ":"
          character(len=10)  :: t
          character(len=8)   :: d
          real(kind=dp),           parameter :: c0 = 0.0_dp,   &
                                                c1 = 1.0_dp,   &
                                                c2 = 2.0_dp
          integer(kind=int4),      parameter  :: IOUNIT = 5
          integer(kind=int4)                       :: ioerr = 9999
          integer(kind=int4)                       :: lstart,lend
          logical(kind=int4)                       :: ioflag
          ! Exec code ....
          filename = __FILE__
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          ioflag = ioerr == 0
          call DATE_AND_TIME(DATE=d,TIME=t)
          lstart = __LINE__
          print*, d , dcol , t , filename ,lstart+1 ,  header
          if(ioflag) then 
              write(IOUNIT,'(A8,A1,A10,A256)' )  d, dcol, t, filename
              write(IOUNIT,'(I5,A36)' ) lstart+3,header
          end if
          call q1.scpart_init(0.0_dp)
          call q2.scpart_init(1.0_dp)
          call q3.scpart_init(2.0_dp)
          if(   q1.m_q.v(0) == c0      .and. &
                q1.m_q.v(1) == 0.0_dp  .and. &
                q1.m_q.v(2) == 0.0_dp  .and. &
                q1.m_q.v(3) == 0.0_dp   ) then
                       print*, "q1 -- instantiation: succedded!!"
                       print*, q1.m_q.v 
                       if(ioflag) then
                           write(IOUNIT,'(A11)') qstr1
                           write(IOUNIT,'(4F22.15)' ) q1.m_q.v(0), q1.m_q.v(1), q1.m_q.v(2), q1.m_q.v(3)
                       end if
          else
                 print*, "q1 -- instantiation: failed!!"   
          end if
          if(   q2.m_q.v(0) == c1     .and.  &
                q2.m_q.v(1) == 0.0_dp .and.  &
                q2.m_q.v(2) == 0.0_dp .and.  &
                q2.m_q.v(3) == 0.0_dp     ) then
                        print*, "q2 -- instantiation: succedded!!"
                        print*, q2.m_q.v
                        if(ioflag) then
                            write(IOUNIT,'(A11)' ) qstr2
                            write(IOUNIT,'(4F22.15) ' ) q2.m_q.v(0), q2.m_q.v(1), q2.m_q.v(2), q2.m_q.v(3)
                        end if
          else
                print*, "q2 -- instantiation: failed!!"
          end if
          if(   q3.m_q.v(0)  == c2     .and. &
                q3.m_q.v(1)  == 0.0_dp .and. &
                q3.m_q.v(2)  == 0.0_dp .and. &
                q3.m_q.v(3)  == 0.0_dp     )  then
                         print*, "q3 -- instantiation: succedded!!"  
                         print*, q3.m_q.v  
                         if(ioflag) then
                             write(IOUNIT,'(A11)') qstr3
                             write(IOUNIT,'(4F22.15)' ) q3.m_q.v(0), q3.m_q.v(1), q3.m_q.v(2), q3.m_q.v(3)
                         end if
          else
                        print*, "q3 -- instantiation: failed!!"
          end if
          call DATE_AND_TIME(DATE=d,TIME=t)
          lend = __LINE__
          print*, d , dcol , t , filename ,lend+1 ,  footer
          if(ioflag) then 
              write(IOUNIT,'(A8,A1,A10,A256)' ) d, dcol, t, filename
              write(IOUNIT,'(I5,A34)') lend+3,footer
              close(UNIT=IOUNIT,STATUS='KEEP')      
          end if
            
   end subroutine test_scpart_init
   
   subroutine test_conjugate()
          ! Locals only
          character(len=256) :: emsg
          character(len=128) :: filename
!DIR$     ATTRIBUTES ALIGN : 64 :: q1,q2
          type(Quaternion_t) :: q1,q2
          character(len=18),       parameter :: OUTFILE = "test_conjugate.dat"
          character(len=34),       parameter :: header  = "[TEST #8: test_conjugate -- START]"
          character(len=32),       parameter :: footer  = "[TEST #8: test_conjugate -- END]"
          character(len=11),       parameter :: qstr1   = "Quaternion1"
          character(len=1),        parameter :: dcol = ":"
          character(len=10)                  :: t
          character(len=8)                   :: d
          integer(kind=int4),      parameter :: IOUNIT = 5
          integer(kind=int4)                       :: ioerr = 9999
          integer(kind=int4)                       :: lstart,lend
          logical(kind=int4)                       :: ioflag  = .false.
          ! Exec code ....
          filename = __FILE__
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          ioflag = ioerr == 0
          call DATE_AND_TIME(DATE=d,TIME=t)
          lstart = __LINE__
          print*, d , dcol , t , filename ,lstart+1 ,  header
          if(ioflag) then 
              write(IOUNIT,'(A8,A1,A10,A128)' )  d, dcol, t, filename
              write(IOUNIT,'(I5,A34)' ) lstart+3,header
          end if
          call q1.array_init([0.1458_dp,0.5487_dp,0.9854_dp,1.021_dp])
          q2 = q1.conjugate()
          if( q2.m_q.v(0) > 0.0_dp .and.   &
              q2.m_q.v(1) < 0.0_dp .and.   &
              q2.m_q.v(2) < 0.0_dp .and.   &
              q2.m_q.v(3) < 0.0_dp     )   then
                    print*, "========== TEST #9 -- PASSED!! =========="
                    print*, "Conjugated quaternion:     ", q2.m_q.v  
                    print*, "Non-conjugated quaternion: ", q1.m_q.v
                    if(ioflag) then
                        write(IOUNIT,'(A45)' ) "========== TEST #9 -- PASSED!! =========="
                        write(IOUNIT,'(A14,4F22.15)') "Conjugated:    ", q2.m_q.v(0), q2.m_q.v(1), q2.m_q.v(2), q2.m_q.v(3)
                        write(IOUNIT,'(A14,4F22.15)') "Non-conjugated:", q1.m_q.v(0), q1.m_q.v(1), q1.m_q.v(2), q1.m_q.v(3)
                    end if
          else
                  print*, "========== TEST #9 -- FAILED!! =========="
                  if(ioflag) write(IOUNIT,'(A45)') "========== TEST #9 -- FAILED!! =========="
         end if
         call DATE_AND_TIME(DATE=d,TIME=t)
         lend = __LINE__
         print*, d , dcol , t , filename ,lend+1 ,  footer
         if(ioflag) then 
              write(IOUNIT,'(A8,A1,A10,A128)' ) d, dcol, t, filename
              write(IOUNIT,'(I5,A32)') lend+3,footer
              close(UNIT=IOUNIT,STATUS='KEEP')      
         end if     
   end subroutine test_conjugate
   
   ! Functional tests
   
   function quaternion_to_R3_rotation(q) result(matr3)
          ! Based on Boost 'Quaternion' examples
          type(Quaternion_t),       intent(in) :: q
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: matr3
          type(MatrixR3) :: matr3
          type :: Holder_t 
               sequence
               real(kind=dp) :: a,b,c,d
               real(kind=dp) :: aa,ab,ac,ad,  &
                                bb,bc,bd,cc,  &
                                cd,dd,norm_carre
               integer(kind=int1), dimension(8) :: pad
          end type Holder_t
!DIR$     ATTRIBUTES ALIGN : 64 ::  h
          type(Holder_t) :: h
          ! Exec code .....
          h.a = q.m_q.v(0)
          h.b = q.m_q.v(1)
          h.c = q.m_q.v(2)
          h.d = q.m_q.v(3)
          h.aa = h.a*h.a
          h.ab = h.a*h.b
          h.ac = h.a*h.c
          h.ad = h.a*h.d
          h.bb = h.b*h.b
          h.bc = h.b*h.c
          h.bd = h.b*h.d
          h.cc = h.c*h.c
          h.cd = h.c*h.d
          h.dd = h.d*h.d
          h.norm_carre = h.aa+h.bb+h.cc+h.dd
          if(h.norm_carre <= EPSILON(1.0_dp)) then
              matr3.a11 = smallest
              matr3.a12 = smallest
              matr3.a13 = smallest
              matr3.a21 = smallest
              matr3.a22 = smallest
              matr3.a23 = smallest
              matr3.a31 = smallest
              matr3.a32 = smallest
              matr3.a33 = smallest
          end if
          matr3.a11 = (h.aa+h.bb-h.cc-h.dd)/h.norm_carre
          matr3.a12 = 2.0_dp*(-h.ad+h.bc)/h.norm_carre
          matr3.a13 = 2.0_dp*(h.ac+h.bd)/h.norm_carre
          matr3.a21 = 2.0_dp*(h.ad+h.bc)/h.norm_carre
          matr3.a22 = (h.aa-h.bb+h.cc-h.dd)/h.norm_carre
          matr3.a23 = 2.0_dp*(-h.ab+h.cd)/h.norm_carre
          matr3.a31 = 2.0_dp*(-h.ac+h.bd)/h.norm_carre
          matr3.a32 = 2.0_dp*(h.ab+h.cd)/h.norm_carre
          matr3.a33 = (h.aa-h.bb-h.cc+h.dd)/h.norm_carre
   end function quaternion_to_R3_rotation
   
   subroutine test_quaternion_to_r3_conversions()
          ! Locals only
          character(len=256) :: emsg
          character(len=128) :: filename
          character(len=37),   parameter          :: OUTFILE =  "test_quaternion_to_r3_conversions.dat"
          character(len=53),   parameter          :: header  =  "[TEST #6: test_quaternion_to_r3_conversions -- START]"
          character(len=51),   parameter          :: footer  =  "[TEST #6: test_quaternion_to_r3_conversions -- END]"
!DIR$     ATTRIBUTES ALIGN : 64 :: mat
          type(MatrixR3)     :: mat
!DIR$     ATTRIBUTES ALIGN : 64 :: q
          type(Quaternion_t) :: q
          character(len=1),        parameter       :: dcol = ":"
          character(len=10)  :: t
          character(len=8)   :: d
          integer(kind=int4),      parameter       :: IOUNIT = 10
          integer(kind=int4)                       :: ioerr = 9999
          integer(kind=int4)                       :: lstart,lend
          logical(kind=int4)                       :: ioflag
          ! Exec code.....
          filename = __FILE__
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
           ioflag = ioerr == 0
          call DATE_AND_TIME(DATE=d,TIME=t)
          lstart = __LINE__
          print*, d , dcol , t , filename ,lstart+1 ,  header
          if(ioflag) then 
              write(IOUNIT,'(A8,A1,A10,A128,A1,I5)' )  d, dcol, t, filename, ":", lstart+2 
              write(IOUNIT,'(A51)' ) , header
          end if
          call q.array_init([0.5_dp,1.41_dp,2.54_dp,0.98_dp])
          ! Memory first touch
          mat = MatrixR3(0.0_dp,0.0_dp,0.0_dp,  &
                         0.0_dp,0.0_dp,0.0_dp,  &
                         0.0_dp,0.0_dp,0.0_dp)
          mat = quaternion_to_R3_rotation(q)
          if(mat.a11 == smallest .or.   &
             mat.a12 == smallest .or.   &
             mat.a13 == smallest .or.   &
             mat.a21 == smallest .or.   &
             mat.a22 == smallest .or.   &
             mat.a23 == smallest .or.   &
             mat.a31 == smallest .or.   &
             mat.a32 == smallest .or.   &
             mat.a33 == smallest       )   then
                    print*, "Norm_Carre < EPSILON(1.0_dp) -- "
                    if(ioflag) then
                          write(IOUNIT,'(A32)' )  "Norm_Carre < EPSILON(1.0_dp) -- "
                          close(IOUNIT,STATUS='KEEP')
                    end if
                    return
          end if
          call DATE_AND_TIME(DATE=d,TIME=t)
          lend = __LINE__
          print*, d , dcol , t , filename ,lend+1 ,  footer   
          print*, "1st row:  ", mat.a11, mat.a12, mat.a13
          print*, "2nd row:  ", mat.a21, mat.a22, mat.a23
          print*, "3rd row:  ", mat.a31, mat.a32, mat.a33
          if(ioflag) then
              write(IOUNIT,'(A7,3F22.15)' )    "1st row", mat.a11, mat.a12, mat.a13
              write(IOUNIT,'(A7,3F22.15)' )    "2nd row", mat.a21, mat.a22, mat.a23
              write(IOUNIT,'(A7,3F22.15)' )    "3rd row", mat.a31, mat.a32, mat.a33
              write(IOUNIT,'(A8,A1,A10,A128,A1,I5)' ) d, dcol, t, filename, ":", lend+2
              write(IOUNIT,'(A51)')  footer
              close(IOUNIT,STATUS='KEEP')
          end if
         
   end subroutine test_quaternion_to_r3_conversions
   
   subroutine find_vector_for_BOD(x,y,z,u,v,w,r,s,t)
          real(kind=dp), intent(in)  :: x,y,z
          real(kind=dp), intent(in)  :: u,v,w
          real(kind=dp), intent(out) :: r,s,t
          ! Exec code ....
          r = +y*w-x*v
          s = -x*w+z*u
          t = +x*v-y*u
   end subroutine find_vector_for_BOD
   
   function quaternion_to_R4_rotation(q1,q2) result(matr4)
          type(Quaternion_t), intent(in) :: q1,q2
!DIR$     ATTRIBUTES ALIGN : 64 :: matr4
          type(MatrixR4) :: matr4
          type :: Holder_t
               sequence
               real(kind=dp) :: a0,b0,c0,d0,a1,b1,c1,d1
              
               real(kind=dp) :: nc0,nc1,invpnorm, a0a1,a0b1,a0c1,a0d1,  &
                                b0a1
               real(kind=dp) :: b0b1,b0c1,b0d1,  &
                                c0a1,c0b1,c0c1,c0d1,  &
                                d0a1
               real(kind=dp) :: d0b1,d0c1,d0d1,pnorm
               integer(kind=int1), dimension(32) :: pad
          end type Holder_t
          ! Exec code .....
!DIR$     ATTRIBUTES ALIGN : 64 :: h
          type(Holder_t) :: h
          ! Memory first touch
          h.a0 = q1.m_q.v(0)
          h.b0 = q1.m_q.v(1)
          h.c0 = q1.m_q.v(2)
          h.d0 = q1.m_q.v(3)
          
          h.a1 = q2.m_q.v(0)
          h.b1 = q2.m_q.v(1)
          h.c1 = q2.m_q.v(2)
          h.d1 = q2.m_q.v(3)
          h.nc0 = 0.0_dp
          h.nc1 = 0.0_dp
          h.invpnorm = 0.0_dp
          h.a0a1 = 0.0_dp
          h.a0b1 = 0.0_dp
          h.a0c1 = 0.0_dp
          h.a0d1 = 0.0_dp
          h.b0a1 = 0.0_dp
          h.b0b1 = 0.0_dp
          h.b0c1 = 0.0_dp
          h.b0d1 = 0.0_dp
          h.c0a1 = 0.0_dp
          h.c0b1 = 0.0_dp
          h.c0c1 = 0.0_dp
          h.c0d1 = 0.0_dp
          h.d0a1 = 0.0_dp
          h.d0b1 = 0.0_dp
          h.d0c1 = 0.0_dp
          h.d0d1 = 0.0_dp
          h.pnorm = 0.0_dp
          h.nc0 = h.a0*h.a0+h.b0*h.b0+h.c0*h.c0+h.d0*h.d0
          h.nc1 = h.a1*h.a1+h.b1*h.b1+h.c1*h.c1+h.d1*h.d1
          if(h.nc0 <= EPSILON(1.0_dp) .or. &
             h.nc1 <= EPSILON(1.0_dp) ) then
                matr4.a11=smallest;matr4.a12=smallest
                matr4.a13=smallest;matr4.a14=smallest
                matr4.a21=smallest;matr4.a22=smallest
                matr4.a23=smallest;matr4.a23=smallest
                matr4.a31=smallest;matr4.a32=smallest
                matr4.a33=smallest;matr4.a33=smallest
                matr4.a41=smallest;matr4.a42=smallest
                matr4.a43=smallest;matr4.a44=smallest
          end if
          h.pnorm = h.nc0*h.nc1
          h.invpnorm = 1.0_dp/h.pnorm
          h.a0a1=h.a0*h.a1;h.a0b1=h.a0*h.b1
          h.a0c1=h.a0*h.c1;h.a0d1=h.a0*h.d1
          h.b0a1=h.b0*h.a1;h.b0b1=h.b0*h.b1
          h.b0c1=h.b0*h.c1;h.b0d1=h.b0*h.d1
          h.c0a1=h.c0*h.a1;h.c0b1=h.c0*h.b1
          h.c0c1=h.c0*h.c1;h.c0d1=h.c0*h.d1
          h.d0a1=h.d0*h.a1;h.d0b1=h.d0*h.b1
          h.d0c1=h.d0*h.c1;h.d0d1=h.d0*h.d1
          matr4.a11 =  (h.a0a1+h.b0b1+h.c0c1+h.d0d1)*h.invpnorm
          matr4.a12 =  (h.a0b1-h.b0a1-h.c0d1+h.d0c1)*h.invpnorm
          matr4.a13 =  (h.a0c1+h.b0d1-h.c0a1-h.d0b1)*h.invpnorm
          matr4.a14 =  (h.a0d1-h.b0c1+h.c0b1-h.d0a1)*h.invpnorm
          matr4.a21 =  (-h.a0b1+h.b0a1-h.c0d1+h.d0c1)*h.invpnorm
          matr4.a22 =  (h.a0a1+h.b0b1-h.c0c1-h.d0d1)*h.invpnorm
          matr4.a23 =  (-h.a0d1+h.b0c1+h.c0b1-h.d0a1)*h.invpnorm
          matr4.a24 =  (h.a0c1+h.b0d1+h.c0a1+h.d0b1)*h.invpnorm
          matr4.a31 =  (-h.a0c1+h.b0d1+h.c0a1-h.d0b1)*h.invpnorm
          matr4.a32 =  (h.a0d1+h.b0c1+h.c0b1+h.d0a1)*h.invpnorm
          matr4.a33 =  (-h.a0a1-h.b0b1+h.c0c1-h.d0d1)*h.invpnorm
          matr4.a34 =  (-h.a0b1-h.b0a1+h.c0d1+h.d0c1)*h.invpnorm
          matr4.a41 =  (-h.a0d1-h.b0c1+h.c0b1+h.d0a1)*h.invpnorm
          matr4.a42 =  (-h.a0c1+h.b0d1-h.c0a1+h.d0b1)*h.invpnorm
          matr4.a43 =  ( h.a0b1+h.b0a1+h.c0d1+h.d0c1)*h.invpnorm
          matr4.a44 =  ( h.a0a1-h.b0b1-h.c0c1+h.d0d1)*h.invpnorm
   end function quaternion_to_R4_rotation
   
   
   subroutine test_quaternion_to_R4_rotation()
          ! Locals only.
          character(len=256) :: emsg
          character(len=128) :: filename
          character(len=34),     parameter :: OUTFILE = "test_quaternion_to_R4_rotation.dat"
          character(len=50),     parameter :: header  = "[TEST #7: test_quaternion_to_R4_rotation -- START]"
          character(len=47),     parameter :: footer  = "[TEST #7: test_quaternion_to_R4_rotation -- END]"
!DIR$     ATTRIBUTES ALIGN : 64 :: mat
          type(MatrixR4) :: mat
!DIR$     ATTRIBUTES ALIGN : 64 :: q1,q2
          type(Quaternion_t) :: q1,q2
          character(len=1),        parameter       :: dcol = ":"
          character(len=10)  :: t
          character(len=8)   :: d
          integer(kind=int4),      parameter       :: IOUNIT = 11
          integer(kind=int4)                       :: ioerr = 9999
          integer(kind=int4)                       :: lstart,lend
          logical(kind=int4)                       :: ioflag = .false.,invalid = .false.
          ! Exec code ....
          filename = __FILE__
          open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          ioflag = ioerr == 0
          call DATE_AND_TIME(DATE=d,TIME=t)
           lstart = __LINE__
          print*, d , dcol , t , filename ,lstart+1 ,  header
          if(ioflag) then 
              write(IOUNIT,'(A8,A1,A10,A128,A1,I5)' )  d, dcol, t, filename, ":", lstart+2 
              write(IOUNIT,'(A50)' ) , header
          end if
          call q1.array_init([1.0_dp,0.8_dp,0.5214_dp,3.2256411_dp])
          call q2.array_init([0.001_dp,0.0009875_dp,0.0012854_dp,0.0089751_dp])
          ! Memory first touch
          mat = MatrixR4(0.0_dp,0.0_dp,0.0_dp,0.0_dp,   &
                         0.0_dp,0.0_dp,0.0_dp,0.0_dp,   &
                         0.0_dp,0.0_dp,0.0_dp,0.0_dp,   &
                         0.0_dp,0.0_dp,0.0_dp,0.0_dp  )
          mat = quaternion_to_R4_rotation(q1,q2)
          invalid = (mat.a11 == smallest) .or.  &
                    (mat.a12 == smallest) .or.  &
                    (mat.a13 == smallest) .or.  &
                    (mat.a14 == smallest) .or.  &
                    (mat.a21 == smallest) .or.  &
                    (mat.a22 == smallest) .or.  &
                    (mat.a23 == smallest) .or.  &
                    (mat.a24 == smallest) .or.  &
                    (mat.a31 == smallest) .or.  &
                    (mat.a32 == smallest) .or.  &
                    (mat.a33 == smallest) .or.  &
                    (mat.a34 == smallest) .or.  &
                    (mat.a41 == smallest) .or.  &
                    (mat.a42 == smallest) .or.  &
                    (mat.a43 == smallest) .or.  &
                    (mat.a44 == smallest)      
          if(invalid)  then
                print*, "Norm_Carre < EPSILON(1.0_dp) -- "
                if(ioflag) then
                      write(IOUNIT,'(A43)'   )  "=========== TEST #7 -- FAILED!! ==========="
                      write(IOUNIT,'(A32)'   )  "Norm_Carre < EPSILON(1.0_dp) -- "
                      close(IOUNIT,STATUS='KEEP')
                 end if
                 return 
          end if
          call DATE_AND_TIME(DATE=d,TIME=t)
          lend = __LINE__
          print*, d , dcol , t , filename ,lend+1 ,  footer 
          print*, "========= TEST #7 -- PASSED!! ==========="
          print*, "1st row:  ", mat.a11, mat.a12, mat.a13, mat.a14
          print*, "2nd row:  ", mat.a21, mat.a22, mat.a23, mat.a24
          print*, "3rd row:  ", mat.a31, mat.a32, mat.a33, mat.a33
          print*, "4rd row:  ", mat.a41, mat.a42, mat.a43, mat.a44
          if(ioflag)   then
              write(IOUNIT,'(A43)' )           "=========== TEST #7 -- PASSED!! ==========="
              write(IOUNIT,'(A7,4F22.15)' )    "1st row", mat.a11, mat.a12, mat.a13, mat.a14
              write(IOUNIT,'(A7,4F22.15)' )    "2nd row", mat.a21, mat.a22, mat.a23, mat.a24
              write(IOUNIT,'(A7,4F22.15)' )    "3rd row", mat.a31, mat.a32, mat.a33, mat.a34
              write(IOUNIT,'(A7,4F22.15)' )    "4rd row", mat.a41, mat.a42, mat.a43, mat.a44
              write(IOUNIT,'(A8,A1,A10,A128,A1,I5)' ) d, dcol, t, filename, ":", lend+2
              write(IOUNIT,'(A47)')  footer
              close(IOUNIT,STATUS='KEEP') 
          end if
   end subroutine test_quaternion_to_R4_rotation
   
          

   subroutine test_quaternion_ctors()
           ! Unit tests of constructor subroutines.
           call test_default_init()
           call test_elemwise_init()
           call test_complex_init()
           call test_array_init()
           call test_scpart_init()
           
   end subroutine test_quaternion_ctors
    
   subroutine test_quaternion_procedures()
          call test_conjugate()
   end subroutine test_quaternion_procedures

end module mod_test_quaternion