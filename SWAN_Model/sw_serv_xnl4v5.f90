module serv_xnl4v5
    
    
    use mod_kinds, only : int4, sp, dp
    implicit none
    
    contains
    
    
SUBROUTINE y_gauleg(x1,x2,x,w,n)
!-------------------------------------------------------------------
INTEGER(int4), intent(in) ::  n    ! Number of intervals
real(sp), intent(in)    ::  x1   ! lower limit of integration interval
real(sp), intent(in)    ::  x2   ! upper limit of integration interval
real(sp), intent(out)   ::  x(n) ! Positions for function evaluations
real(sp), intent(out)   ::  w(n) ! Weights
!
!-----------------------------------------------------------------------
real(dp) :: EPS
PARAMETER (EPS=3.0-14_dp)
INTEGER i,j,m
real(dp) :: p1,p2,p3,pp,xl,xm,z,z1
!-----------------------------------------------------------------------
      m=(n+1)/2
      xm=0.5_dp*(x2+x1)
      xl=0.5_dp*(x2-x1)
      do 12 i=1,m
        z=cos(3.141592654_dp*(i-.25_dp)/(n+.5_dp))
1       continue
          p1=1._dp
          p2=0._dp
          do 11 j=1,n
            p3=p2
            p2=p1
            p1=((2._dp*j-1._dp)*z*p2-(j-1._dp)*p3)/j
11        continue
          pp=n*(z*p1-p2)/(z*z-1._dp)
          z1=z

          z=z1-p1/pp
        if(abs(z-z1).gt.EPS) goto 1
        x(i)=xm-xl*z
        x(n+1-i)=xm+xl*z
        w(i)=2._dp*xl/((1._dp-z*z)*pp*pp)
        w(n+1-i)=w(i)
12    continue
!
return
END subroutine

!-----------------------------------------------------------------------------!
subroutine z_cmpcg(sigma,depth,grav,cg)
!-----------------------------------------------------------------------------!
!
!   +-------+    ALKYON Hydraulic Consultancy & Research
!   |       |    Gerbrant van Vledder
!   |   +---+
!   |   | +---+
!   +---+ |   |
!         +---+
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
implicit none
!
!  0. Update history
!
!     12/01/2001  Initial version
!     11/04/2001  Check included for the cases tat sigma < 0 or depth <0
!                 Result is cg = -10
!
!  1. Purpose:
!
!     Compute group velocity for a given radian frequency and depth
!
!  2. Method
!
!     Linear wave theory
!
!  3. Parameter list:
!
!Type   I/O           Name    Description
!------------------------------------------------------------------------------
real(sp), intent(in)  :: sigma  !  radian frequency (rad)
real(sp), intent(in)  :: depth  !  water depth (m)
real(sp), intent(in)  :: grav   !  gravitational acceleration (m/s^2)
real(sp), intent(out) :: cg     !  group velocity (m/s)
!
real(sp) :: k                      !  wave number
!/A
!!real z_wnumb                !  compute wave number
!/Z
!-----------------------------------------------------------------------------
k = z_wnumb(sigma,depth,grav)
!
if(depth <= 0. .or. sigma <= 0.) then
  cg = -10._sp
else
  if(depth*k > 30._sp) then
    cg = grav/(2._sp*sigma)
  else
    cg = sigma/k*(0.5_sp+depth*k/sinh(2._sp*depth*k))
  end if
end if
!
return
end subroutine
!
!-----------------------------------------------------------------------------!
subroutine z_intp1(x1,y1,x2,y2,n1,n2,ierr)                                    !
!-----------------------------------------------------------------------------!
!
!   +-------+    ALKYON Hydraulic Consultancy & Research
!   |       |    Gerbrant van Vledder
!   |   +---+
!   |   | +---+
!   +---+ |   |
!         +---+
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
implicit none
!
!  0. Update history
!
!     30/03/1999 Initical version
!      9/04/1999 Check included for monotonicity of x-data
!     11/10/1999 Error messages added and updated
!     18/01/2001 Check include if n1==1
!     24/01/2001 Check for equality of y2 data loosened if n2==1
!     13/09/2001 Documentation updated
!
!  1. Purpose
!
!     Interpolate function values
!
!  2. Method
!
!     Linear interpolation

!     If a requested point falls outside the input domain, then
!     the nearest point is used (viz. begin or end point of x1/y1 array
!
!     If the input array has only one point. A constant value is assumed
!
!  3. Parameter list
!
!     Name    I/O  Type  Description
!
integer(int4), intent(in) ::  n1   !   number of data points in x1-y1 arrays
integer(int4), intent(in) ::  n2   !   number of data points in x2-y2 arrays
real(int4), intent(in) ::  x1(n1)  !   x-values of input data
real(int4), intent(in) ::  y1(n1)  !   y-values of input data
real(int4), intent(in) ::  x2(n2)  !   x-values of output data
real(int4), intent(out) :: y2(n2)  !   y-values of output data
integer(int4), intent(out) :: ierr !   Error indicator
!
!  4. Subroutines used
!
!  5. Error messages
!
!     ierr = 0    No errors detected
!          = 1    x1-data not monotonic increasing
!          = 10   x2-data not monotonic increasing
!          = 11   x1- and x2 data not monotonic increasing
!          = 2    x1-data not monotonic decreasing
!          = 20   x1-data not monotonic decreasing
!          = 22   x1- and x2 data not monotonic decreasing
!
!          = 2    No variation in x1-data
!          = 3    No variation in x2-data is allowed if n2=1
!
!  6. Remarks
!
!     It is assumed that the x1- and x2-data are either
!     monotonic increasing or decreasing
!
!     If a requested x2-value falls outside the range of x1-values
!     it is assumed that the corresponding y2-value is equal to
!     the nearest boundary value of the y1-values
!
!     Example: x1 = [0 1 2 3]
!              y1 = [1 2 1 0]
!
!              x2 = -1,  y2 = 1
!              x2 =  5,  y2 = 0
!
!------------------------------------------------------------------------------
integer(int4) :: i1,i2        ! counters
!
real(sp) :: ds            ! step size
real(sp) :: fac           ! factor in linear interpolation
real(sp) :: s1,s2         ! search values
real(sp) :: xmin1,xmax1   ! minimum and maximum of x1-data
real(sp) :: xmin2,xmax2   ! minimum and maximum of x2-data
!
real(sp), parameter :: eps=1.e-20_sp
!------------------------------------------------------------------------------
!   initialisation
!
ierr = 0
!
!  check number of points of input array
!
if(n1==1) then
  y2 = y1(1)
  goto 9999
end if
!
!  check minimum and maximum data values
!
xmin1 = minval(x1)
xmax1 = maxval(x1)
xmin2 = minval(x2)
xmax2 = maxval(x2)
!
if (abs(xmin1-xmax1) < eps .or. abs(x1(1)-x1(n1)) < eps) then
  ierr = 2
  goto 9999
end if
!
if ((abs(xmin2-xmax2) < eps .or. abs(x2(1)-x2(n2)) < eps) .and. n2 > 1) then
  ierr = 3
  goto 9999
end if
!
! check input data for monotonicity
!
if(x1(1) < x1(n1)) then             ! data increasing
  do i1=1,n1-1
    if(x1(i1) > x1(i1+1)) then
      ierr=1
      write(*,*) 'z_intp1: i1 x1(i1) x1(i1+1):',i1,x1(i1),x1(i1+1)
      goto 9999
    end if
  end do
!
  do i2=1,n2-1
    if(x2(i2) > x2(i2+1)) then
      ierr=ierr+10
      write(*,*) 'z_intp1: i2 x2(i2) x2(i2+1):',i2,x2(i2),x2(i2+1)
      goto 9999
    end if
  end do
!
else                                 ! data decreasing
  do i1=1,n1-1
    if(x1(i1) < x1(i1+1)) then
      ierr=2
      write(*,*) 'z_intp1: i1 x1(i1) x1(i1+1):',i1,x1(i1),x1(i1+1)
      goto 9999
    end if
  end do
!
  do i2=1,n2-1
    if(x2(i2) < x2(i2+1)) then
      ierr=ierr + 20
      write(*,*) 'z_intp1: i2 x2(i2) x2(i2+1):',i2,x2(i2),x2(i2+1)
      goto 9999
    end if
  end do
end if
!
!------------------------------------------------------------------------------
! initialize
!------------------------------------------------------------------------------
if(ierr==0) then
  i1 = 1
  s1 = x1(i1)
!
  do i2 = 1,n2
    s2 = x2(i2)
    do while (s1 <= s2 .and. i1 < n1)
      i1 = i1 + 1
      s1 = x1(i1)
    end do
!
!  special point
!  choose lowest s1-value if x2(:) < x1(1)
!
    if(i1 ==1) then
      y2(i2) = y1(i1)
    else
      ds = s2 - x1(i1-1)
      fac = ds/(x1(i1)-x1(i1-1))
      y2(i2) = y1(i1-1) + fac*(y1(i1)-y1(i1-1))
    end if
!
! special case at end: choose s2(n2) > s1(n1), choose last value of y1(1)
!
    if(i2==n2 .and. s2>s1) y2(n2) = y1(n1)
  end do
end if
!
9999 continue
!
return
end subroutine
!
!-----------------------------------------------------------------------------!
subroutine z_polyarea(xpol,ypol,npol,area)
!-----------------------------------------------------------------------------!
!
!   +-------+    ALKYON Hydraulic Consultancy & Research
!   |       |    P.O. Box 248
!   |   +---+    8300 AE Emmeloord
!   |   | +---+  Tel: +31 527 620909
!   +---+ |   |  Fax: +31 527 610020
!         +---+  http://www.alkyon.nl
!
!         Gerbrant van Vledder
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Update history
!
!     0.01  12/06/2003  Initial version
!
!  1. Purpose
!
!     Computes area of a closed polygon
!
!  2. Method
!
!     The area of the polygon
!
!  3. Parameter list
!
!     Name    I/O  Type  Description
!
integer(int4), intent(in)  ::  npol       ! Number of points of polygon
real(sp), intent(in)     ::  xpol(npol) ! x-coodinates of polygon
real(sp), intent(in)     ::  ypol(npol) ! y-coordinates of polygon
real(sp), intent(out)    ::  area       ! area of polygon
!
!  4. Subroutines used
!
!  5. Error messages
!
!  6. Remarks
!
integer(int4) :: ipol,ipol1         ! counters
real(sp) :: xmin,xmax,ymin,ymax   ! minima and maxima of polygon
real(sp) :: xmean,ymean           ! mean values
real(sp) :: xa,ya,xb,yb           ! temporary variables
real(sp) :: sumx,sumy             ! sums
real(sp) :: darea                 ! piece of area
!-------------------------------------------------------------------------------
if(npol<=1) then
  crf  = 0.
  xz   = 0.
  yz   = 0.
  area = 0.
  return
end if
!
! compute minimum and maximum coordinates
!
xmin = minval(xpol)
xmax = maxval(xpol)
ymin = minval(ypol)
ymax = maxval(ypol)
!
!  compute mean of range of x- and y-coordinates
!
xmean = 0.5_sp*(xmin + xmax)
ymean = 0.5_sp*(ymin + ymax)
!
! compute area and center of gravity
! do loop over all line pieces of polygon
!
area = 0.
sumx = 0.
sumy = 0.
!
do ipol=1,npol
  ipol1 = ipol + 1
  if(ipol==npol) ipol1 = 1
  xa = xpol(ipol)
  ya = ypol(ipol)
  xb = xpol(ipol1)
  yb = ypol(ipol1)
!
  darea = 0.5_sp*((xa-xmean)*(yb-ymean) - (xb-xmean)*(ya-ymean))
  area  = area + darea
  sumx  = sumx + darea*(xa+xb+xmean)/3._sp
  sumy  = sumy + darea*(ya+yb+ymean)/3._sp
end do
!
return
end subroutine
!
!-----------------------------------------------------------------------------!
subroutine z_steps(x,dx,nx)
!-----------------------------------------------------------------------------!
!
!
!   +-------+    ALKYON Hydraulic Consultancy & Research
!   |       |    Gerbrant van Vledder
!   |   +---+
!   |   | +---+  Creation date:  September 28, 1998
!   +---+ |   |  Last Update:    march 19, 2003
!         +---+
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Update history
!
!     19/03/2003   Input argument defined using intent option
!                  check included nx > 0
!
!  1. Purpose
!
!     Compute bandwidth of spectral discretization
!
implicit none
!
integer(int4), intent(in) :: nx     ! Number of elements in array
real(sp), intent(in)    :: x(nx)  ! Input data array with elements
real(sp), intent(out)   :: dx(nx) ! Output array with step sizes
!
integer ix                    ! counter
!------------------------------------------------------------------------------
if (nx<1) then
  return
!
elseif (nx==1) then
  dx = 0
else
  do ix=2,nx-1
    dx(ix) = 0.5_sp * (x(ix+1) - x(ix-1))
  end do
!
  if (nx >= 4) then
    dx(1)  = dx(2)*dx(2)/dx(3)
    dx(nx) = dx(nx-1)*dx(nx-1)/dx(nx-2)
  else
    dx(1)  = dx(2)
    dx(nx) = dx(nx-1)
  end if
end if
!
return
end subroutine
!-----------------------------------------------------------------------------!
real function z_root2(func,x1,x2,xacc,iprint,ierr)
!-----------------------------------------------------------------------------!
!
!   +-------+    ALKYON Hydraulic Consultancy & Research
!   |       |
!   |   +---+
!   |   | +---+
!   +---+ |   |
!         +---+
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Update history
!
!     Version Date       Modification
!
!     0.01    29/11/1999 Initial version
!     0.02    07/11/1999 Test added to check boundaries, and reverse if necessary
!                        Bug fixed in assigning answer
!     0.03    02/09/2002 Maximum number of iterations set to 20, instead of 10
!
!  1. Purpose
!
!     Find zero crossing point of function FUNC between the
!     initial values on either side of zero crossing
!
!  2. Method
!
!     Ridders method of root finding
!
!     adapted from routine zridddr
!     Numerical Recipes
!     The art if scientific computing, second edition, 1992
!     W.H. Press, S.A. Teukolsky, W.T. Vetterling and B.P. Flannery
!
!  3. Parameter list
!
!     Name    I/O  Type  Description
!
!     func     i    r    real function
!     x1       i    r    initial x-value on left/right side of zero-crossing
!     x2       i    r    initial x-value on right/left side of zero-crossing
!     xacc     i    r    accuracy, used as |x1(i)-x2(i)|< xacc
!     iprint   i    i    Output channel and test level
!     ierr     o    i    Error indicator
!
!  4. Subroutines used
!
!     Func      user supplied real function
!
!  5. Error messages
!
!     ierr = 0   No errors occured during iteration process
!            1   Iteration halted in dead end, this combination may NEVER occur
!            2   Maximum number of iterations exceeded
!            3   Solution jumped outside interval
!
!  6. Remarks
!
!     It is assumed that the x1- and x2-coordinate lie
!     on different sides of the actual zero crossing
!
!     The input parameter IPRINT is used to generate test output.
!     If IPRINT==0, no test output is created
!              > 0, test output is directed to the file connected to unit LUPRINT=IPRINT
!                   if no file is connected to this unit, no output is written
!
!
implicit none
!
real func                         ! external function
real(sp), intent (in) :: x1           ! x-value at one side of interval
real(sp), intent (in) :: x2           ! x-value at other side of interval
real(sp), intent (in) :: xacc         ! requested accuracy
integer(int4), intent (in) :: iprint    ! number of output channel, only used when
integer(int4), intent (out) :: ierr     ! error indicator
!
real(sp) :: unused                       ! default value
real(sp) :: zriddr                       ! intermediate function value
real(sp) :: xx1,xx2,xx                   ! local boundaries during iteration
integer(int4) :: maxit                     ! maximum number of iteration
integer(int4) :: luprint                   ! unit of test output
logical(int4) :: lopen                     ! check if a file is opened

parameter (maxit = 20)
external func
!
integer(int4) :: iter      ! counter for number of iterations
real(sp) :: fh           ! function value FUNC(xh)
real(sp) :: fl           ! function value FUNC(xl)
real(sp) :: fm           ! function value FUNC(xm)
real(sp) :: fnew         ! function value FUNC(xnew)
real(sp) :: s            ! temp. function value, used for inverse quadratic interpolation
real(sp) :: xh           ! upper (high) boundary of interval
real(sp) :: xl           ! lower boundary of interval
real(sp) :: xm           ! middle point of interval
real(sp) :: xnew         ! new estimate according to Ridders method
!
ierr   = 0        ! set error level
unused =-1.11e30_sp  ! set start value
!
xx1 = x1          ! copy boundaries of interval to local variables
xx2 = x2
!
luprint = iprint
!
if(luprint > 0) then
  inquire(unit=luprint,opened=lopen)
  if(.not.lopen) then
    luprint = 0
    write(*,'(a,i4)') 'Z_ROOT2: invalid unit number:',iprint
  end if
end if
!
! check boundaries on requirement x2 > x1
!
if(xx1 > xx2) then
  xx  = xx1
  xx1 = xx2
  xx2 = xx
end if
!
fl = func(xx1)
fh = func(xx2)
!
!if(luprint > 0) write(luprint,'(a,4e13.5)') &
!&  'Z_ROOT2: xx1 xx2 fl fh:',xx1,xx2,fl,fh
!
if((fl > 0. .and. fh < 0.) .or. (fl < 0. .and. fh > 0.))then
   xl = xx1
   xh = xx2
   zriddr = unused
!
   do iter=1,maxit
      xm = 0.5_sp*(xl+xh)
      fm = func(xm)
      s = sqrt(fm**2-fl*fh)
      if(s == 0.) goto 9000
      xnew = xm+(xm-xl)*(sign(1._sp,fl-fh)*fm/s)
!
!      if(luprint>0) write(luprint,'(a,4e13.5)') &
!&       'Z_ROOT2: xm,fm,s,xnew:',xm,fm,s,xnew
!
      if (abs(xnew-zriddr) <= xacc) then
!        if(luprint>0) write(luprint,'(a)') 'Z_ROOT2: xnew=zriddr'
        goto 9000
      end if
!
      zriddr = xnew
      fnew = func(zriddr)
      if (fnew == 0.) goto 9000
!
      if(sign(fm,fnew) /= fm) then
         xl = xm
         fl = fm
         xh = zriddr
         fh = fnew
      elseif(sign(fl,fnew) /= fl) then
         xh = zriddr
         fh = fnew
      elseif(sign(fh,fnew) /= fh) then
         xl = zriddr
         fl = fnew
      else
         ierr = 1
         goto 9000
      endif
!
      if(abs(xh-xl) <= xacc) goto 9000
!
      if(luprint > 0) write(luprint,'(a,i4,5e14.6)') &
&     'Z_ROOT2: iter,x1,x2,|x1-x2|,xacc,z:', iter,xl,xh,abs(xl-xh),xacc,fnew
!
   end do
   ierr = 2
   if(luprint > 0) write(luprint,'(a)') 'Z_ROOT2: -> ierr=2'
   goto 9000
else if (fl == 0.) then
  zriddr = xx1
else if (fh == 0.) then
  zriddr = xx2
else
  ierr = 3
  goto 9999
! 'root must be bracketed in zriddr'
endif
!
9000 continue
!
z_root2 = zriddr
!
if(luprint > 0) write(luprint,'(a,2i3,5e13.5)') &
&     'Z_ROOT2: ierr,iter,xl,xh,acc,x0,z0:', ierr,iter,xl,xh,xacc,z_root2,func(z_root2)
!
9999 continue
!
return
end function
!
!-----------------------------------------------------------------------------!
subroutine z_upper(str)
!-----------------------------------------------------------------------------!
!
!   +-------+    ALKYON Hydraulic Consultancy & Research
!   |       |    Gerbrant van Vledder
!   |   +---+
!   |   | +---+  Creation date:  July 3,1998
!   +---+ |   |  Last Update:
!         +---+
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!  0. Update history
!
!  1. Purpose
!
!     Transform all lower capitals to UPPER CAPITALS in string STR
!
!  2. Method
!
!  3. Parameter list
!
!     Name    I/O  Type  Description
!
implicit none
character(len=*), intent(inout) :: str  ! Character string to be converted
!
!  4. Subroutines used
!
!  5. Error messages
!
!  6. Remarks
!
integer(int4) :: nlen
integer(int4) :: i,ial,iau,izl
!
nlen = len(str)
!
ial = ichar('a')
iau = ichar('A')
izl = ichar('z')
!
do i=1,nlen
  if(ichar(str(i:i)) >= ial.and. ichar(str(i:i)) <= izl) then
    str(i:i) = char(ichar(str(i:i))-ial+iau)
  end if
end do
!
return
end subroutine
!
!-----------------------------------------------------------------------------!
real function z_wnumb(w,d,grav)
!-----------------------------------------------------------------------------!
!
!   +-------+    ALKYON Hydraulic Consultancy & Research
!   |       |    Gerbrant van Vledder
!   |   +---+
!   |   | +---+  Last update: 17 January 2006
!   +---+ |   |
!         +---+
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2007  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
implicit none
!
!  0. Update history
!
!     01/04/1999  Initial version
!     12/01/2001  grav added as input parameter
!     11/04/2001  Check included for the case w < 0 or d < 0
!     17/01/2006  Short cut added if y>15
!
!  1. Purpose:
!
!     Compute wave number k for a given radian frequency and water depth
!
!  2. Method
!
!     finite depth linear dispersion relation, using a Pade approximation
!
!  3. Parameter list:
!
!Type   I/O           Name     Description
!------------------------------------------------------------------------------
real(sp), intent(in)  ::  w      ! radian frequency (rad)
real(sp), intent(in)  ::  d      ! water depth (m)
real(sp), intent(in)  ::  grav   ! graviational acceleration (m/s^2)
!
!  4. Subroutines used
!
!  5. Error messages
!
!  6. Remarks
!
!     The Pade approximation has been described in .
!     Hunt, J.N, 1979: Direct solution of the wave dispersion
!       equation. Journal of the Waterway, Port, Coastal and
!       Ocean Division, ASCE, Vol. 105, No. WW4, 457-459.
!
!
real(sp) :: x,xx,y,omega
!
if(d<=0 .or. w<= 0.) then
  z_wnumb = -10._sp
else
  omega   = w**2/grav
  y       = omega*d
  if (y>15.) then
    x = y
  else
    xx      = y*(y+1._sp/(1._sp+y*(0.66667_sp+y*(0.35550_sp+y*(0.16084_sp+y*(0.06320_sp+y* &
&             (0.02174_sp+y*(0.00654_sp+y*(0.00171_sp+y*(0.00039_sp+y*0.00011_sp))))))))))
    x       = sqrt(xx)
  end if
  z_wnumb = x/d
end if
!
return
end function


end module
