

module mod_matrix_computations

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_matrix_computations'
 !          
 !          Purpose:
  !                   Helper complex matrix computations
 !          History:
 !                        
 !                          Date: 25-04-2020
  !                         Time: 15:48 GMT+2
  !                         
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 2
 !                      Micro: 0
 !
 !         
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.

     use mod_kinds, only : int4,sp
     implicit none
     public
     ! File version info
     ! Major version
     integer(kind=int4),  parameter :: MOD_MATRIX_COMPUTATIONS_MAJOR = 1
     ! Minor version
     integer(kind=int4),  parameter :: MOD_MATRIX_COMPUTATIONS_MINOR = 0
     ! mICRO VERSION
     integer(kind=int4),  parameter :: MOD_MATRIX_COMPUTATIONS_MICRO = 0
     ! Full version
     integer(kind=int4),  parameter :: MOD_MATRIX_COMPUTATIONS_FULLVER = &
          1000*MOD_MATRIX_COMPUTATIONS_MAJOR+100*MOD_MATRIX_COMPUTATIONS_MINOR + &
          10*MOD_MATRIX_COMPUTATIONS_MICRO
     ! Module create date
     character(*),        parameter :: MOD_MATRIX_COMPUTATIONS_CREATE_DATE = "25-04-2020 16:03 +00200 (SAT 25 APR 2020 GMT+2)"
     ! Module build date (set by pre-processor)
     character(*),        parameter :: MOD_MATRIX_COMPUTATIONS_BUILD_DATE  = __DATE__ " " __TIME__

     ! Module version ID
     character(*),        parameter :: MOD_MATRIX_COMPUTATIONS_ID = &
                            "$Id: GMS_matrix_computations.f90 1000 +00200 2020-04-25 16:03 beniekg@gmail.com $"

     contains

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
       subroutine expmat4x4_cr4v1(l,q,invq,z,res) !GCC$ ATTRIBUTES hot :: expmat4x4_cr4v1 !GCC$ ATTRIBUTES aligned(16) :: expmat4x4_cr4v1
#elif defined __ICC || defined __INTEL_COMPILER
       subroutine expmat4x4_cr4v1(l,q,invq,z,res)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: expmat4x4_cr4v1
#endif
           complex(kind=sp),    dimension(4),    intent(in)    :: l
           complex(kind=sp),    dimension(4,4),  intent(in)    :: q
           complex(kind=sp),    dimension(4,4),  intent(in)    :: invq
           real(kind=sp),                        intent(in)    :: z
           complex(kind=sp),    dimension(4,4),  intent(inout) :: res
           ! locals
           complex(kind=sp), dimension(4,4) :: det
           integer(kind=int4) :: i,j
           ! Exec code ....
           ! First touch (this loop should be executed from LSD)
           do j=1,4
              do i=1,4
                 det(i,j) = cmplx(0.0_sp,0.0_sp)
              end do
           end do
           do i=1,4
              det(i,i) = cexp(l(i)*z)
           end do
           ! call mat4x4mul
              
       end subroutine expmat4x4_cr4v1

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
       subroutine expmat4x4_cr4v2(l,q,invq,z,res) !GCC$ ATTRIBUTES hot :: expmat4x4_crv2 !GCC$ ATTRIBUTES aligned(16) :: expmat4x4_cr4v2
#elif defined __ICC || defined __INTEL_COMPILER
       subroutine expmat4x4_cr4v2(l,q,invq,z,res)
           !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: expmat4x4_crv2
#endif 
           complex(kind=sp),    dimension(4),    intent(in)    :: l
           complex(kind=sp),    dimension(4,4),  intent(in)    :: q
           complex(kind=sp),    dimension(4,4),  intent(in)    :: invq
           real(kind=sp),                        intent(in)    :: z
           complex(kind=sp),    dimension(4,4),  intent(inout) :: res
           ! locals
           complex(kind=sp), dimension(4,4) :: det
           real(kind=sp),    dimension(4,4) :: qre
           real(kind=sp),    dimension(4,4) :: qim
           real(kind=sp),    dimension(4,4) :: lre
           real(kind=sp),    dimension(4,4) :: lim
           real(kind=sp),    dimension(4,4) :: iqre
           real(kind=sp),    dimension(4,4) :: iqim
           real(kind=sp),    dimension(4,4) :: FRre
           real(kind=sp),    dimension(4,4) :: FRim
           real(kind=sp),    dimension(4,4) :: FIre
           real(kind=sp),    dimension(4,4) :: FIim
           real(kind=sp),    dimension(4,4) :: diff
           real(kind=sp),    dimension(4,4) :: sum
           real(kind=sp)      :: s0,s1,s2,s3
           real(kind=sp)      :: r1,r2,r3,r4
           real(kind=sp)      :: r5,r6,r7.r8
           integer(kind=int4) :: j,k,i
           ! Exec code ....
           do j=1,4
#if defined __ICC || defined __INTEL_COMPILER
              !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
              !GCC$ VECTOR
#endif
              do i=1,4
                 det(i,j)  = cmplx(0.0_sp,0.0_sp)
                 lre(i,j)  = 0.0_sp
                 lim(i,j)  = 0.0_sp
                 qre(i,j)  = real(q(i,j),kind=sp)
                 qim(i,j)  = aimag(q(i,j),kind=sp)
                 iqre(i,j) = real(invq(i,j),kind=sp)
                 iqim(i,j) = aimag(invq(i,j),kind=sp)
              end do
           end do
#if defined __ICC || defined __INTEL_COMPILER
              !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
              !GCC$ VECTOR
#endif           
           do i=1, 4
              det(i,i) = cexp(l(i)*z)
              lre(i,i) = real(det(i,i),kind=sp)
              lim(i,i) = aimag(det(i,i),kind=sp)
           end do
           do j=1, 4
#if defined __ICC || defined __INTEL_COMPILER
              !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
              !GCC$ VECTOR
#endif                
              do i=1, 4
                 FRre(i,j) = iqre(i,j)*lre(i,j)
                 FIre(i,j) = iqim(i,j)*lre(i,j)
                 FRim(i,j) = iqre(i,j)*lim(i,j)
                 FIim(i,j) = iqim(i,j)*lim(i,j)
              end do
           end do

           do j=1, 4
#if defined __ICC || defined __INTEL_COMPILER
              !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
              !GCC$ VECTOR
#endif  
              do i=1, 4
                 diff(i,j) = FRre(i,j)-FIim(i,j)
                 sum(i,j)  = FRim(i,j)+FIre(i,j)
              end do
           end do
           r1 = 0.0_sp
           r2 = 0.0_sp
           r3 = 0.0_sp
           r4 = 0.0_sp
           r5 = 0.0_sp
           r6 = 0.0_sp
           r7 = 0.0_sp
           r8 = 0.0_sp
           do i=1,4,2
              do j=1,4,2
                 s0 = 0.0_sp
                 s1 = 0.0_sp
                 s2 = 0.0_sp
                 s3 = 0.0_sp
                 do k=1, 4
                    r1 = qre(j,k)
                    r2 = diff(k,i)
                    r3 = qim(j,k)
                    r4 = sum(k,i)
                    s0 = s0+r1*r2-r3*r4
                    r5 = qre(j+1,k)
                    r6 = diff(k,i+1)
                    r7 = qim(j+1,k)
                    s1 = s1+r5*r2-r7*r4
                    r8 = sum(k,i+1)
                    s2 = s2+r1*r6-r3*r8
                    s3 = s3+r5*r6-r7*r8
                 end do
                 res(i,j)     = s0
                 res(i+1,j)   = s1
                 res(i,j+1)   = s2
                 res(i+1,j+1) = s3
              end do
           end do
                 
     end subroutine expmat4x4_cr4v2

         
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine invmat4x4_cr4(in,out) !GCC$ ATTRIBUTES hot :: invmat4x4_cr4 !GCC$ ATTRIBUTES aligned(64) :: invmat4x4_cr4
#elif defined __ICC || defined __INTEL_COMPILER
     subroutine invmat4x4_cr4(in,out)
         !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: invmat4x4_cr4
#endif
          complex(kind=sp),   dimension(4,4) :: in
          complex(kind=sp),   dimension(4,4) :: out
          ! Locals
          complex(kind=sp),   dimension(4) :: svec
          complex(kind=sp),   dimension(2) :: det
          integer(kind=int4), dimension(4) :: ipvt
          real(kind=sp)      :: r
          integer(kind=int4) :: job
          ! Exec code
          out = in
          call cgeco(out,4,4,ipvt,r,svec)
          job = 1
          call cgedi(out,4,4,ipvt,det,svec,job)
      end subroutine invmat4x4_cr4
!  Modified:
!
!    07 May 2006
!    26-05-2020 Bernard Gingold 
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      subroutine cgeco(a,lda,n,ipvt,rcond,z) !GCC$ ATTRIBUTES hot :: cgeco !GCC$ ATTRIBUTES aligned(16) :: cgeco
#elif defined __ICC || defined __INTEL_COMPILER
        subroutine cgeco(a,lda,n,ipvt,rcond,z)
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: cgeco
#endif
          complex(kind=sp),    dimension(lda,n), intent(inout)    :: a
          integer(kind=int4),                    intent(in)       :: lda
          integer(kind=int4),                    intent(in)       :: n
          integer(kind=int4),  dimension(n),     intent(inout)    :: ipvt
          real(kind=sp),                         intent(out)      :: rcond
          complex(kind=sp),    dimension(n),     intent(inout)    :: z
          ! LOcals
          complex(kind=sp)   :: ek
          complex(kind=sp)   :: t
          complex(kind=sp)   :: wk
          complex(kind=sp)   :: wkm
          real(kind=sp)      :: t0,t1,t2,t3,t4,t5
          real(kind=sp)      :: anorm
          real(kind=sp)      :: s
          real(kind=sp)      :: sm
          real(kind=sp)      :: ynorm
          integer(kind=int4) :: info
          integer(kind=int4) :: j,k,l
          ! EXec code ....
          anorm = 0.0e+00_sp
          do j=1, n
             anorm = max(anorm,scasum(n,a(1:n,j),1))
          end do
          info = 0
          call cgefa(a,lda,n,ipvt,info)
          ek = cmplx(1.0E+00_sp,0.0E+00_sp)
          z(1:n) = cmplx(0.0E+00_sp,0.0E+00_sp)
          do k = 1, n
             if(cabs1 z(k)) /= 0.0E+00_sp) then
                ek = csign1 ( ek, -z(k) )
             end if
             t0 = cabs1(a(k,k))
             t1 = cabs1(ek-z(k))
             if(t0 < t1) then
                  s = t0 / t1
                  z(1:n) = z(1:n) * s
                  ek = cmplx(s,0.0E+00_sp) * ek
             end if
             wk = ek - z(k)
             wkm = -ek - z(k)
             s = cabs1(wk)
             sm = cabs1(wkm)
             if(t0 /= 0.0E+00_sp) then
                wk = wk / conjg(t0)
                wkm = wkm / conjg(t0)
             else
                wk = cmplx(1.0E+00_sp, 0.0E+00_sp)
                wkm = cmplx(1.0E+00_sp, 0.0E+00_sp)
             end if
             do j = k+1, n
                sm = sm + cabs1 ( z(j) + wkm * conjg ( a(k,j) ) )
                z(j) = z(j) + wk * conjg ( a(k,j) )
                s = s + cabs1 ( z(j) )
             end do
             if( s < sm) then
                 t = wkm - wk
                 wk = wkm
                 z(k+1:n) = z(k+1:n) + t * conjg(a(k,k+1:n))
             end if
             z(k) = wk
         end do
         s = 1.0E+00_sp / scasum(n,z,1)
         z(1:n) = z(1:n) * s
!
!  Solve hermitian(L) * Y = W.
!
         do k = n, 1, -1
            if (k < n) then
!     z(k) = z(k) + cdotc ( n-k, a(k+1:n,k), 1, z(k+1:n), 1 )
                z(k) = z(k) + dot_product (a(k+1:n,k),z(k+1:n) )
             end if
             t2 = cabs1(z(k))
             if(1.0E+00_sp < t2) then
               s = 1.0E+00_sp / t2
               z(1:n) = z(1:n) * s
             end if
             l = ipvt(k)
             t    = z(l)
             z(l) = z(k)
             z(k) = t
         end do
         s = 1.0E+00_sp / scasum(n, z, 1)
         z(1:n) = z(1:n) * s
         ynorm = 1.0E+00_sp
!
!  Solve L * V = Y.
!
         do k = 1, n
            l = ipvt(k)
            t = z(l)
            z(l) = z(k)
            z(k) = t
            if( k < n ) then
                z(k+1:n) = z(k+1:n) + t * a(k+1:n,k)
             end if
            t3 = cabs1(z(k))
            if(1.0E+00_sp < t3) then
               s = 1.0E+00_sp / t3
               z(1:n) = z(1:n) * s
               ynorm = s * ynorm
            end if
         end do
         s = 1.0E+00_sp / scasum(n,z,1)
         z(1:n) = z(1:n) * s
         ynorm = s * ynorm
!
!  Solve U * Z = V.
!
         do k = n, 1, -1
            t4 = cabs1(a(k,k))
            t5 = cabs1(z(k))
            if(t4 < t5) then
               s = t4 / t5
               z(1:n) = z(1:n) * s
               ynorm = s * ynorm
            end if
            if(t4 /= 0.0E+00_sp) then
               z(k) = z(k) / a(k,k)
            else
               z(k) = cmplx(1.0E+00_sp, 0.0E+00_sp)
            end if
            t = -z(k)
            z(1:k-1) = z(1:k-1) + t * a(1:k-1,k)
          end do
!
!  Make ZNORM = 1.
!
          s = 1.0E+00_sp / scasum(n, z, 1)
          z(1:n) = z(1:n) * s
          ynorm = s * ynorm
          if(anorm /= 0.0E+00_sp) then
             rcond = ynorm / anorm
          else
             rcond = 0.0E+00
          end if
     end subroutine cgeco

!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
     !    LC: QA214.L56.

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine cgefa(a,lda,n,ipvt,info) !GCC$ ATTRIBUTES hot :: cgefa !GCC$ ATTRIBUTES aligned(16) :: cgefa !GCC$ ATTRIBUTES inline :: cgefa
#elif defined __ICC || defined __INTEL_COMPILER
       !DIR$ ATTRIBUTES INLINE :: cgefa
     subroutine cgefa(a,lda,n,ipvt,info)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: cgefa
#endif
          complex(kind=sp),    dimension(lda,n),  intent(inout) :: a
          integer(kind=int4),                     intent(in)    :: lda
          integer(kind=int4),                     intent(in)    :: n
          integer(kind=int4),  dimension(n),      intent(out)   :: ipvt
          integer(kind=int4),                     intent(out)   :: info
          ! Locals
          complex(kind=sp) :: t
          integer(kind=int4) :: j,k,l
          real(kind=sp) :: t0
!
!  Gaussian elimination with partial pivoting.
!
          info = 0
          t0   = 0.0_sp
          do k = 1, n - 1
!
!  Find L = pivot index.
!
               l = icamax(n-k+1,a(k:n,k),1) + k - 1
               ipvt(k) = l
!
!  Zero pivot implies this column already triangularized.
            !
               t0 = cabs1(a(l,k))
               if(t0 == 0.0E+00_sp ) then
                   info = k
                   cycle
               end if
!
!  Interchange if necessary.
!
               if(l /= k) then
                  t      = t0
                  a(l,k) = a(k,k)
                  a(k,k) = t
               end if
!
!  Compute multipliers
!
               t = -cmplx(1.0E+00_sp, 0.0E+00_sp) / a(k,k)
               a(k+1:n,k) = a(k+1:n,k) * t
!
!  Row elimination with column indexing
!
               do j = k+1, n
                   t = a(l,j)
                   if(l /= k) then
                      a(l,j) = a(k,j)
                      a(k,j) = t
                   end if
                   a(k+1:n,j) = a(k+1:n,j) + t * a(k+1:n,k)
               end do
          end do
          ipvt(n) = n
          if(cabs1(a(n,n)) == 0.0E+00_sp ) then
              info = n
           end if
     end subroutine cgefa

!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
     !    LC: QA214.L56.

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     function icamax(n,x,incx) !GCC$ ATTRIBUTES hot :: icamax !GCC$ ATTRIBUTES aligned(64) :: icamax !GCC$ ATTRIBUTES inline :: icamax
#elif defined __ICC || defined __INTEL_COMPILER
       !DIR$ ATTRIBUTES INLINE :: icamax
     function icamax(n,x,incx)
         !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: icamax
         integer(kind=int4),              intent(in) :: n
         complex(kind=sp),  dimension(*), intent(in) :: x
         integer(kind=int4),              intent(in) :: incx
         ! Locals
         integer(kind=int4) :: icamax
         real(kind=sp) :: smax
         icamax = 0
         if(n < 1 .or.incx <= 0) then
              return
         end if
         if(n == 1) then
             return
         end if  
         icamax = 1
         if(incx /= 1) then
            ix = 1
            smax = cabs1(x(1))
            ix = ix + incx
            do i = 2, n
               if(smax < cabs1(x(ix))) then
                  icamax = i
                  smax = cabs1(x(ix))
               end if
               ix = ix + incx
            end do
         else
            smax = cabs1(x(1))
            do i = 2, n
               if(smax < cabs1(x(i))) then
                  icamax = i
                  smax = cabs1(x(i))
               end if
            end do
         end if
     end function icamax

!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2004
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
     !
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     function csign1(z1,z2) !GCC$ ATTRIBUTES hot :: csign1 !GCC$ ATTRIBUTES aligned(64) :: csign1 !GCC$ ATTRIBUTES inline :: csign1
#elif defined __ICC || defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: csign1
     function csign4(z1,z2)
       !DIR$ CODE_ALIGN : 64 :: csign1
#endif
           complex(kind=sp),    intent(in) :: z1
           complex(kind=sp),    intent(in) :: z2
           ! LOcals
           complex(kind=sp) :: csign1
           if(cabs1(z2) == 0.0E+00_sp) then
                csign1 = cmplx(0.0E+00_sp, 0.0E+00_sp)
           else
                csign1 = cabs1(z1) * (z2 / cabs1(z2))
           end if
     end function csign1
 
     
        
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 May 2002
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     function cabs1(z) !GCC$ ATTRIBUTES hot :: cabs1 !GCC$ ATTRIBUTES aligned(64) :: cabs1 !GCC$ ATTRIBUTES inline :: cabs1
#elif defined __ICC || defined __INTEL_COMPILER
       !DIR$ ATTRIBUTES INLINE :: cabs1
       function cabs1(z)
       !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: cabs1
#endif
         complex(kind=sp),     intent(in) :: z
         real(kind=sp) :: cabs1
         cabs1 = abs(real(z)) + abs(aimag(z))
     end function cabs1

!  Modified:
!
!    11 April 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for FORTRAN usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, pages 308-323, 1979.
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     function scasum(n,x,incx) !GCC$ ATTRIBUTES hot :: scasum !GCC$ ATTRIBUTES aligned(64) :: scasum !GCC$ ATTRIBUTES inline :: scasum
#elif defined __ICC || defined __INTEL_COMPILER
       !DIR$ ATTRIBUTES INLINE :: scasum
     function scasum(n,x,incx)
         !DIR$ ATTRIBUTES CODE_ALIGN : 64 :: scasum
#endif
          integer(kind=int4),             intent(in) :: n
          complex(kind=sp), dimension(*), intent(in) :: x
          integer(kind=int4),             intent(in) :: incx
          ! Locals
          real(kind=sp) :: scasum
          if(n <= 0 .or. incx <= 0) then
             return
          end if
          scasum = 0.0_sp
          if(incx == 1) then
               scasum = sum(abs(real(x(1:n))) + abs(aimag(x(1:n))))
          else
               nincx = n * incx
               scasum = sum(abs(real(x(1:nincx:incx))) &
                      + abs(aimag(x(1:nincx:incx))))
          end if
     end function scasum

        














end module mod_matrix_computations
