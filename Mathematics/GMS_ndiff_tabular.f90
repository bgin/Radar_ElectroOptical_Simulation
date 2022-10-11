

module  ndiff_tabular

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         ndiff_tabular
 !          
 !          Purpose:
 !                    
 !                   NUMERTCAC DIFFERENTTATTON OF TARULAR FUNCTTON WITH AUTOMATIC
 !                   CHOICE OF OPTIMUM STEP SIZE. 
 !
 !                     
 !          History:
 !                        Date: 10-10-2022
 !                        Time: 08:11 GMT+2
 !          
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !    
 !          Author: 
 !                           John Michael McNamee
 !
 !          Modified by:
 !                           Bernard Gingold
 !           
 !          References:
 !                            
 !                            NUMERICAL DIFFERENTIATION OF TABULATED
 !                            FUNCTIONS WITH AUTOMATIC CHOICE
 !                            OF STEP-SIZE
 !                            JOHN MICHAEL McNAMEE
 !                            York University Downsview, Ontario, Canada 
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
 
    use mod_kinds, only : i4,sp,dp
    implicit none
    public

     ! File and module metadata
     !=============================================================================================================================================================!
       integer(kind=i4),   parameter, public :: NDIFF_TABULAR_MAJOR = 1
       integer(kind=i4),   parameter, public :: NDIFF_TABULAR_MINOR = 0
       integer(kind=i4),   parameter, public :: NDIFF_TANULAR_MICRO = 0
       integer(kind=i4),   parameter, public :: NDIFF_TABULAR_FULLVER =      1000*NDIFF_TABULAR_MAJOR + &
                                                                             100*NDIFF_TABULAR_MINOR  + &
                                                                             10*NDIFF_TABULAR_MICRO
       character(*),         parameter, public :: NDIFF_TABULAR_CREATION_DATE = "10-10-2022 08:17 +00200 (MON 10 OCT 2022 GMT+2)"
       character(*),         parameter, public :: NDIFF_TABULAR_BUILD_DATE    = __DATE__ ":" __TIME__
       character(*),         parameter, public :: NDIFF_TABULAR_AUTHOR        = "Programmer: John Michael McNamee, York University Downsview, Ontario, Canada"
       character(*),         parameter, public :: NDIFF_TABULAR_SYNOPSIS      = "Numerical Differentiation of tabulated functions, automated step-size."
      !============================================================================================================================================================!

    
    contains

    !n,            NUMBER OF POINTS AT WICH DERIVATIVE IS TO BE FOUND.
    !x0,           FIRST POINT AT WHICH DERIVATIVE IS TO BE FOUND
    !step,         INTERVAL BETWEEN POINTS AT WHICH DERIVATIVE IS TO BE found
    subroutine diff_r4(n,x0,step,h0,ne,x1,ht,y,dy,r1,s1,npitl, &
                       nprts,ierrfl,nerest,itmany)
       !dir$ optimize:3
       !dir$ attributes code_align : 32 :: diff
       implicit none
       integer(kind=i4),               intent(in) :: n
       real(kind=sp),                  intent(in) :: x0
       real(kind=sp),                  intent(in) :: step
       real(kind=sp),                  intent(in) :: h0
       integer(kind=i4),               intent(in) :: ne
       real(kind=sp),                  intent(in) :: x1
       real(kind=sp),                  intent(in) :: ht
       real(kind=sp), dimension(1:ne), intent(in) :: y
       real(kind=sp), dimension(1:n),  intent(out):: dy
       real(kind=sp),                  intent(out):: r1
       real(kind=sp),                  intent(out):: s1
       integer(kind=i4),               intent(out):: npitl
       integer(kind=i4),               intent(out):: nprts
       integer(kind=i4),               intent(out):: ierrfl
       integer(kind=i4),               intent(out):: nerest
       integer(kind=i4),               intent(out):: itmany
       ! Locals
       real(kind=sp), dimension(3) :: del
       real(kind=sp), dimension(4) :: der
       real(kind=sp), automatic :: b
       real(kind=sp), automatic :: d
       real(kind=sp), automatic :: dav
       real(kind=sp), automatic :: esterr
       real(kind=sp), automatic :: fd
       real(kind=sp), automatic :: hd
       real(kind=sp), automatic :: hf
       real(kind=sp), automatic :: ht
       real(kind=sp), automatic :: h1
       real(kind=sp), automatic :: t
       real(kind=sp), automatic :: t1
       real(kind=sp), automatic :: x
       real(kind=sp), automatic :: xe
       integer(kind=i4), automatic :: i
       integer(kind=i4), automatic :: idecr
       integer(kind=i4), automatic :: incfst
       integer(kind=i4), automatic :: idf
       integer(kind=i4), automatic :: ief
       integer(kind=i4), automatic :: isf
       integer(kind=i4), automatic :: it
       integer(kind=i4), automatic :: j
       integer(kind=i4), automatic :: k
       logical(kind=i4), automatic :: b0,b1
       ! Exec code ....
       ierrfl = 0
       nerest = 0
       itmany = 0
       if(ht<0.0_sp) then
          ierrfl = -3
          return 
       end if
       h0 = abs(h0)
       xe = x1+ht*real(ne-1,kind=sp)
       npitl = 0
       nprts = 0
       s1    = 0.0_sp
       r1    = 0.0_sp
       b0    = (x0-h0)>x1
       b1    = (x0+h0)<=x0
       if(b0 .and. b1) goto 20
       h = h0
       goto 30
    20 fd = (f_r4(x0+h0,x1,ht,y)-f_r4(x0-h0,x1,ht,y))/(2.0_sp*h0)
       h = h0
       if(fd /= 0.0_sp) h = abs(f_r4(x0,x1,ht,y)/fd)
       h = amax1(h,h0)
    30 if(h>ht) goto 50
       h = ht*4.0_sp
       goto 70
    50 do k=2,6
          h1 = ht*(2.0_sp**k)
          if(h>h1) goto 60
          h = h1
          goto 70
       end do
       h = ht*64.0_sp
     70 do j=1,n
           x  = x0+step*real(j-1,kind=sp)
           t  = abs((x-x1)/ht)
           k  = t+0.5_sp
           t1 = k
           t  = abs((t-t1)/t)
           if(t<0.00001_sp) goto 90
           ierrfl = -1
           return
      90   isf = 0
           h1  = h
           if(x>x1 .and. x<=xe) goto 120
           ierrfl = -2
           return
      120  do i=1,4
              call deriv_r4(x,h,d,isf,ief,y,x1,ht,ne,xe,npitl,nprts)
              if(ief==1) goto 300
              der(i) = d
              if(i>1) del(i-1) = abs(der(i)-der(i-1))
              h = 0.5_sp*h
            end do
            idecr = 1
            incfst = 1
            do it=1,5
               b0 = del(2)==0.0_sp
               b1 = del(3)==0.0_sp
               if(b0 .and. b1) goto 340
               if(del(1)<=del(2)) goto 190
               goto 340
       160     do i=1,3
                  der(i)=der(i+1)
               end do
               del(1) = del(2)
               del(2) = del(3)
               i = 4
               call deriv_r4(x,h,d,isf,ief,y,x1,ht,ne,xe,npitl,nprts)
               if(ief==1) goto 300
               der(4) = d
               del(3) = abs(der(3)-der(4))
               h = 0.5_sp*h
               idecr = 1
               goto 280
        190    if(del(2)>=del(3)) goto 220
               if(incfst==1) h = h1
               incfst = 0
               der(4) = der(3)
               der(3) = der(2)
               der(2) = der(1)
               der(3) = der(2)
               der(2) = der(1)
               h = 0.5_sp*h
               i = 4
               idecr = 0
               isf = 0
               call deriv_r4(x,h,d,isf,ief,y,x1,ht,ne,xe,npitl,nprts)
               if(ief==1) goto 300
               if(isf==0) goto 200
               dav = der(2)
               esterr = del(2)
               goto 360
         200   der(1) = d
               del(1) = abs(der(1)-der(2))
               goto 280
         220   if(incfst==1) h = h1
               incfst = 0
               der(4) = der(3)
               der(3) = der(2)
               der(2) = der(1)
               del(3) = del(2)
               del(2) = del(1)
               h = 0.5_sp*h
               i = 4
               idecr = 0
               isf = 0
               call deriv_r4(x,h,d,isf,ief,y,x1,ht,ne,xe,npitls,nprts)
               if(ief==1) goto 300
               if(isf==0) goto 230
               dav = der(2)
               esterr = del(2)
               goto 360
         230   der(1) = d
               del(1) = abs(der(1)-der(1))
               goto 280
         250   do i=1,3
                  der(i) = der(i+1)
               end do
               del(1) = del(2)
               del(2) = del(3)
               i = 4
               call deriv_r4(x,h,d,isf,ief,y,x1,ht,ne,xe,npitl,nprts)
               if(ief==0) goto 300
               der(4) = d
               del(3) = abs(der(3)-der(4))
               h = 0._sp*h
               idecr = 1
             end do
          280  itmany = itmany+1
               goto 340
          300  dav = d
               if(i<=3) goto 310
               esterr = del(i-2)
               h = 8.0_sp-ht
               goto 360
          310  esterr = 0.0_sp
               nerest = nerest+1
          340  dav = (der(2)+der(3))*0.5_sp
               esterr = del(2)
               if(idecr==1) h=h*16.0_sp
          360  b = abs(esterr)
               s1 = s1+b
               dy(j) = dav
               if(b>r1) r1 = b
          end do
          s1 = s1/real(n,kind=sp)
    end subroutine diff_r4


    pure function f_r4(z,x1,ht,y) result(fz)
       !dir$ optimize:3
       !dir$ attributes forceinline :: f_r4
       !dir$ attributes code_align : 32 :: f_r4
       real(kind=sp),               intent(in) :: z
       real(kind=sp),               intent(in) :: x1
       real(kind=sp),               intent(in) :: ht
       real(kind=sp), dimension(:), intent(in) :: y
       real(kind=sp) :: fz
       real(kind=sp) :: t
       integer(kind=sp) :: i
       t  = (z-x1)/ht+1.0_sp
       i  = abs(t)+0.5_sp
       fz = y(i) 
    end function f_r4 


    subroutine deriv_r4(x,h,d,isf,ief,y,x1,  &
                        ht,ne,xe,npitl,nprts)
      !dir$ optimize:3
      !dir$ attributes forceinline :: deriv_r4
      !dir$ attributes code_align : 32 :: deriv_r4
       real(kind=sp),                intent(in) :: x
       real(kind=sp),                intent(in) :: h
       real(kind=sp),                intent(out):: d
       integer(kind=i4),             intent(out):: isf
       integer(kind=i4),             intent(out):: ief
       real(kind=sp), dimension(ne), intent(in) :: y
       real(kind=sp),                intent(in) :: x1
       real(kind=sp),                intent(in) :: ht
       integer(kind=i4),             intent(in) :: ne
       real(kind=sp),                intent(in) :: xe
       integer(kind=i4),             intent(inout) :: npitl
       integer(kind=i4),             intent(inout) :: nprts
      ! Locals
       real(kind=sp), automatic :: h,hc,t,t0,t1,t2,t3
      ! Exec code ...
       ief = 0
       hc  = ht*0.9_sp
   10  if(h>hc) goto 20
       npitl=npitl+1
       h = ht
       ief = 1
       return
   20  t = x-4.0_sp*h
       if(t<x1) goto 550
       t = x+4.0_sp*h
       if(t>xe) goto 30
       t0 = 3.0_sp*(f_r4(x-4.0_sp*h,x1,ht,y)-f_r4(x+4.0_sp*h,x1,ht,y))
       t1 = 32.0_sp*(f_r4(x+3.0_sp*h,x1,ht,y)-f_r4(x-3.0_sp*h,x1,ht,y))
       t2 = 168.0_sp*(f_r4(x-2.0_sp*h,x1,ht,y)-f_r4(x+2.0_sp*h,x1,ht,y))
       t3 = 672.0_sp*(f_r4(x+h,x1,ht,y)-f_r4(x-h,x1,ht,y))
       d  = (t0+t1+t2+t3)/(840.0_sp*h)  
       return
   30  t = x-3.0*h
       if(t<x1) goto 40
       d = (11.0_sp*f_r4(x,x1,ht,y)-18.0_sp*f_r4(x-h,x1,ht,y)+ &
            9.0_sp*f_r4(x-2.0_sp*h,x1,ht,y)-2.0_sp*f_r4(x-3.0_sp*h,x1,ht,y))/(6.0_sp*h)
       return
   40  nprts = nprts+1
       h = 0.5_sp*h
       isf = 1
       goto 10
   50  t = x+3.0_sp*h
       if(t>xe) goto 60
       d = (2.0_sp*f_r4(x+3.0_sp*h,x1,ht,y)-9.0_sp*f_r4(x+2.0_sp*h,x1,ht,y) &
           + 18.0_sp*f_r4(x+h,x1,ht,y)-11.0_sp*f_r4(x,x1,ht,y))/(6.0_sp*h)
       return
   60  nprts = nprts+1
       h = 0.5_sp*h
       isf = 1
       goto 10 
    end subroutine deriv_r4


    

    subroutine diff_r8(n,x0,step,h0,ne,x1,ht,y,dy,r1,s1,npitl, &
                       nprts,ierrfl,nerest,itmany)
       !dir$ optimize:3
       !dir$ attributes code_align : 32 :: diff_r8
       implicit none
       integer(kind=i4),               intent(in) :: n
       real(kind=dp),                  intent(in) :: x0
       real(kind=dp),                  intent(in) :: step
       real(kind=dp),                  intent(in) :: h0
       integer(kind=i4),               intent(in) :: ne
       real(kind=dp),                  intent(in) :: x1
       real(kind=dp),                  intent(in) :: ht
       real(kind=dp), dimension(1:ne), intent(in) :: y
       real(kind=dp), dimension(1:n),  intent(out):: dy
       real(kind=dp),                  intent(out):: r1
       real(kind=dp),                  intent(out):: s1
       integer(kind=i4),               intent(out):: npitl
       integer(kind=i4),               intent(out):: nprts
       integer(kind=i4),               intent(out):: ierrfl
       integer(kind=i4),               intent(out):: nerest
       integer(kind=i4),               intent(out):: itmany
       ! Locals
       real(kind=dp), dimension(3) :: del
       real(kind=dp), dimension(4) :: der
       real(kind=dp), automatic :: b
       real(kind=dp), automatic :: d
       real(kind=dp), automatic :: dav
       real(kind=dp), automatic :: esterr
       real(kind=dp), automatic :: fd
       real(kind=dp), automatic :: hd
       real(kind=dp), automatic :: hf
       real(kind=dp), automatic :: ht
       real(kind=dp), automatic :: h1
       real(kind=dp), automatic :: t
       real(kind=dp), automatic :: t1
       real(kind=dp), automatic :: x
       real(kind=dp), automatic :: xe
       integer(kind=i4), automatic :: i
       integer(kind=i4), automatic :: idecr
       integer(kind=i4), automatic :: incfst
       integer(kind=i4), automatic :: idf
       integer(kind=i4), automatic :: ief
       integer(kind=i4), automatic :: isf
       integer(kind=i4), automatic :: it
       integer(kind=i4), automatic :: j
       integer(kind=i4), automatic :: k
       logical(kind=i4), automatic :: b0,b1
       ! Exec code ....
       ierrfl = 0
       nerest = 0
       itmany = 0
       if(ht<0.0_dp) then
          ierrfl = -3
          return 
       end if
       h0 = abs(h0)
       xe = x1+ht*real(ne-1,kind=dp)
       npitl = 0
       nprts = 0
       s1    = 0.0_dp
       r1    = 0.0_dp
       b0    = (x0-h0)>x1
       b1    = (x0+h0)<=x0
       if(b0 .and. b1) goto 20
       h = h0
       goto 30
    20 fd = (f_r4(x0+h0,x1,ht,y)-f_r4(x0-h0,x1,ht,y))/(2.0_dp*h0)
       h = h0
       if(fd /= 0.0_dp) h = abs(f_r4(x0,x1,ht,y)/fd)
       h = amax1(h,h0)
    30 if(h>ht) goto 50
       h = ht*4.0_dp
       goto 70
    50 do k=2,6
          h1 = ht*(2.0_dp**k)
          if(h>h1) goto 60
          h = h1
          goto 70
       end do
       h = ht*64.0_dp
     70 do j=1,n
           x  = x0+step*real(j-1,kind=dp)
           t  = abs((x-x1)/ht)
           k  = t+0.5_dp
           t1 = k
           t  = abs((t-t1)/t)
           if(t<0.00001_dp) goto 90
           ierrfl = -1
           return
      90   isf = 0
           h1  = h
           if(x>x1 .and. x<=xe) goto 120
           ierrfl = -2
           return
      120  do i=1,4
              call deriv_r8(x,h,d,isf,ief,y,x1,ht,ne,xe,npitl,nprts)
              if(ief==1) goto 300
              der(i) = d
              if(i>1) del(i-1) = abs(der(i)-der(i-1)
              h = 0.5_dp*h
            end do
            idecr = 1
            incfst = 1
            do it=1,5
               b0 = del(2)==0.0_dp
               b1 = del(3)==0.0_dp
               if(b0 .and. b1) goto 340
               if(del(1)<=del(2)) goto 190
               goto 340
       160     do i=1,3
                  der(i)=der(i+1)
               end do
               del(1) = del(2)
               del(2) = del(3)
               i = 4
               call deriv_r8(x,h,d,isf,ief,y,x1,ht,ne,xe,npitl,nprts)
               if(ief==1) goto 300
               der(4) = d
               del(3) = abs(der(3)-der(4))
               h = 0.5_dp*h
               idecr = 1
               goto 280
        190    if(del(2)>=del(3)) goto 220
               if(incfst==1) h = h1
               incfst = 0
               der(4) = der(3)
               der(3) = der(2)
               der(2) = der(1)
               der(3) = der(2)
               der(2) = der(1)
               h = 0.5_dp*h
               i = 4
               idecr = 0
               isf = 0
               call deriv_r8(x,h,d,isf,ief,y,x1,ht,ne,xe,npitl,nprts)
               if(ief==1) goto 300
               if(isf==0) goto 200
               dav = der(2)
               esterr = del(2)
               goto 360
         200   der(1) = d
               del(1) = abs(der(1)-der(2))
               goto 280
         220   if(incfst==1) h = h1
               incfst = 0
               der(4) = der(3)
               der(3) = der(2)
               der(2) = der(1)
               del(3) = del(2)
               del(2) = del(1)
               h = 0.5_dp*h
               i = 4
               idecr = 0
               isf = 0
               call deriv_r8(x,h,d,isf,ief,y,x1,ht,ne,xe,npitls,nprts)
               if(ief==1) goto 300
               if(isf==0) goto 230
               dav = der(2)
               esterr = del(2)
               goto 360
         230   der(1) = d
               del(1) = abs(der(1)-der(1))
               goto 280
         250   do i=1,3
                  der(i) = der(i+1)
               end do
               del(1) = del(2)
               del(2) = del(3)
               i = 4
               call deriv_r8(x,h,d,isf,ief,y,x1,ht,ne,xe,npitl,nprts)
               if(ief==0) goto 300
               der(4) = d
               del(3) = abs(der(3)-der(4))
               h = 0._dp*h
               idecr = 1
            end do
          280  itmany = itmany+1
               goto 340
          300  dav = d
               if(i<=3) goto 310
               esterr = del(i-2)
               h = 8.0_dp-ht
               goto 360
          310  esterr = 0.0_dp
               nerest = nerest+1
          340  dav = (der(2)+der(3))*0.5_dp
               esterr = del(2)
               if(idecr==1) h=h*16.0_dp
          360  b = abs(esterr)
               s1 = s1+b
               dy(j) = dav
               if(b>r1) r1 = b
          end do
          s1 = s1/real(n,kind=dp)
    end subroutine diff_r8


    pure function f_r8(z,x1,ht,y) result(fz)
       !dir$ optimize:3
       !dir$ attributes forceinline :: f_r8
       !dir$ attributes code_align : 32 :: f_r8
       real(kind=dp),               intent(in) :: z
       real(kind=dp),               intent(in) :: x1
       real(kind=dp),               intent(in) :: ht
       real(kind=dp), dimension(:), intent(in) :: y
       real(kind=dp) :: fz
       real(kind=dp) :: t
       integer(kind=sp) :: i
       t  = (z-x1)/ht+1.0_dp
       i  = abs(t)+0.5_dp
       fz = y(i) 
    end function f_r8 


    subroutine deriv_r8(x,h,d,isf,ief,y,x1,  &
                        ht,ne,xe,npitl,nprts)
      !dir$ optimize:3
      !dir$ attributes forceinline :: deriv_r8
      !dir$ attributes code_align : 32 :: deriv_r8
       real(kind=dp),                intent(in) :: x
       real(kind=dp),                intent(in) :: h
       real(kind=dp),                intent(out):: d
       integer(kind=i4),             intent(out):: isf
       integer(kind=i4),             intent(out):: ief
       real(kind=dp), dimension(ne), intent(in) :: y
       real(kind=dp),                intent(in) :: x1
       real(kind=dp),                intent(in) :: ht
       integer(kind=i4),             intent(in) :: ne
       real(kind=dp),                intent(in) :: xe
       integer(kind=i4),             intent(inout) :: npitl
       integer(kind=i4),             intent(inout) :: nprts
      ! Locals
       real(kind=dp), automatic :: h,hc,t,t0,t1,t2,t3
      ! Exec code ...
       ief = 0
       hc  = ht*0.9_dp
   10  if(h>hc) goto 20
       npitl=npitl+1
       h = ht
       ief = 1
       return
   20  t = x-4.0_dp*h
       if(t<x1) goto 550
       t = x+4.0_dp*h
       if(t>xe) goto 30
       t0 = 3.0_dp*(f_r8(x-4.0_dp*h,x1,ht,y)-f_r8(x+4.0_dp*h,x1,ht,y))
       t1 = 32.0_dp*(f_r8(x+3.0_dp*h,x1,ht,y)-f_r8(x-3.0_dp*h,x1,ht,y))
       t2 = 168.0_dp*(f_r8(x-2.0_dp*h,x1,ht,y)-f_r8(x+2.0_dp*h,x1,ht,y))
       t3 = 672.0_dp*(f_r8(x+h,x1,ht,y)-f_r8(x-h,x1,ht,y))
       d  = (t0+t1+t2+t3)/(840.0_dp*h)  
       return
   30  t = x-3.0_dp*h
       if(t<x1) goto 40
       d = (11.0_dp*f_r4(x,x1,ht,y)-18.0_dp*f_r4(x-h,x1,ht,y)+ &
            9.0_dp*f_r4(x-2.0_dp*h,x1,ht,y)-2.0_dp*f_r4(x-3.0_dp*h,x1,ht,y))/(6.0_dp*h)
       return
   40  nprts = nprts+1
       h = 0.5_dp*h
       isf = 1
       goto 10
   50  t = x+3.0_dp*h
       if(t>xe) goto 60
       d = (2.0_dp*f_r4(x+3.0_dp*h,x1,ht,y)-9.0_dp*f_r4(x+2.0_dp*h,x1,ht,y) &
           + 18.0_dp*f_r4(x+h,x1,ht,y)-11.0_dp*f_r4(x,x1,ht,y))/(6.0_dp*h)
       return
   60  nprts = nprts+1
       h = 0.5_dp*h
       isf = 1
       goto 10 
    end subroutine deriv_r8












end module ndiff_tabular
