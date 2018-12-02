
module mod_test_quaternion

!!  ****************** Test Module ********************  !!
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_quaternion
    use mod_kinds, only : int4,dp
    implicit none
    
    character(*), parameter, private :: FILENAME = "TEST_Quaternion.txt"
    
    contains
    
    
    subroutine test_default_init()
         
          ! Locals only:
          character(len=256) :: emsg
!DIR$     ATTRIBUTES ALIGN : 64 :: q1,q2,q3
          type(Quaternion_t) :: q1,q2,q3
          real(kind=dp), parameter :: dzero = 0.0E+00_dp
          character(len=10) :: t
          character(len=8)  :: d
          character(len=11)  :: Quaternion1,Quaternion2,Quaternion3
          integer(kind=int4) :: ioerr
          ! Exec code ....
          open(UNIT=1,FILE=FILENAME,IOMSG=emsg,IOSTAT=ioerr,ACCESS='SEQUENTIAL',STATUS='NEW')
          call DATE_AND_TIME(date=d,time=t)
          print*, d,":",t, "Test_quaternion.f90" , 25 ,  "[TEST #1: test_default_init -- START]"
          call q1.default_init()
          call q2.default_init()
          call q3.default_init()
          if(q1.m_q.v(0) == dzero .and. &
             q1.m_q.v(1) == dzero .and. &
             q1.m_q.v(2) == dzero .and. &
             q1.m_q.v(3) == dzero     )  then
              print*, "q1 -- instantiation: succedded!!"
              print*, q1.m_q.v
              if(ioerr == 0) then
                  write(1,'(A11,4F22.15)') Quaternion1, q1.m_q.v(0),q1.m_q.v(1),q1.m_q.v(2),q1.m_q.v(3)
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
              if(ioerr == 0) then
                  write(1,'(A11,4F22.15)') Quaternion2, q2.m_q.v(0),q2.m_q.v(1),q2.m_q.v(2),q2.m_q.v(3)
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
              if(ioerr == 0) then
                   write(1,'(A11,4F22.15)') Quaternion3, q3.m_q.v(0),q3.m_q.v(1),q3.m_q.v(2),q3.m_q.v(3)
              end if
          else
              print*, "q3 -- instantiation: failed!!" 
          end if
          call DATE_AND_TIME(date=d,time=t)
          print*, d,":",t,"Test_quaternion.f90" , 57,  "[TEST #1: test_default_init -- END]"
    end subroutine test_default_init
    
    



end module mod_test_quaternion