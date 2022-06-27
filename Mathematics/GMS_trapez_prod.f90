
#include "GMS_config.fpp" 

module trapeze_prod  

     use mod_kinds, only : i4,dp
     public

     contains

     SUBROUTINE PTRAP ( A, B, N, FN, GN, VINT )
#if (USE_OMP) == 1
       use omp_lib
#endif
!C
!C  THIS SUBROUTINE USES THE PRODUCT TYPE TRAPEZOIDAL RULE
!C  COMPOUNDED N TIMES TO APPROXIMATE THE INTEGRAL FROM A TO B
!C  OF THE FUNCTION FN(X) * GN(X).  FN AND GN ARE FUNCTION
!C  SUBPROGRAMS WHICH MUST BE SUPPLIED BY THE USER.  THE
!C  RESULT IS STORED IN VINT.
!C
!  ref: TOMS 15,12 (Dec 1972) 1070
!  for: Product Type Trapezoidal Integration
!  by: W. R. Boland 
      real(dp) A, AG, AM(2,2), B, F(2),G(2), &
      H, VINT, X
      INTEGER(i4) I, N, J, K
      interface
         real(dp) function FN(x)
             import :: dp
             real(dp), intent(in) :: x
         end function FN
         real(dp) function GN(x)
              import :: dp
              real(dp), intent(in) :: x
         end function GN
      end interface
      DATA AM(1,1), AM(2,2) /2 * 2.0_dp /, AM(1,2), AM(2,1) &
          / 2 * 1.0_dp/
      H = ( B - A ) / DBLE ( FLOAT ( N ) )
      VINT = 0.0_dp
      X = A
      F(2) = FN ( A )
      G(2) = GN ( A )
#if (USE_OMP) == 1
!$omp parallel do default(none) schedule(static) if(N>=250) &
!$omp private(I,F,G,X,J,AG,K) lastprivate(vint)             &
!$omp shared(N,H,AM)
#endif
      DO 3 I  =  1, N
        F(1) = F(2)
        G(1) = G(2)
        X = X + H
        F(2) = FN ( X )
        G(2) = GN ( X )
        DO 2 J = 1, 2
          AG = 0.0_dp
          DO 1 K = 1, 2
            AG = AG + AM(J,K) * G(K)
1     CONTINUE
          VINT = VINT + F(J) * AG
2     CONTINUE
3     CONTINUE
#if (USE_OMP) == 1
!$omp end parallel do
#endif
      VINT = H * VINT / 6.0_dp
    
      END SUBROUTINE


end module trapeze_prod


#if 0

      program main

c***********************************************************************
c
cc TOMS436_PRB tests TOMS436.
c
      implicit none

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS436_PRB'
      write ( *, '(a)' ) '  Test TOMS algorithm 436, product-type'
      write ( *, '(a)' ) '  trapezoidal integration.'

      call test01
      call test02

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS436_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'

      stop
      end
      subroutine test01

c***********************************************************************
c
cc TEST01 tests the rule with one factor equal to 1.
c
      implicit none

      double precision a
      double precision b
      double precision exact
      double precision fn00, gn00
      external fn00
      external gn00
      integer n
      double precision vint

      a = -4.0D+00
      b =  4.0D+00
      exact = 2.0D+00 * atan ( 4.0D+00 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Integral of F(X) * G(X) from -1 to 1,'
      write ( *, '(a)' ) '  with F(X) = 1, G(X) = 1/(1+x**2 )'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    VINT'
      write ( *, '(a)' ) ' '

      do n = 1, 10
        call ptrap ( a, b, n, fn00, gn00, vint )
        write ( *, '(2x,i6,2x,g14.6)' ) n, vint
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Exact:  ', exact

      return
      end
      subroutine test02

c***********************************************************************
c
cc TEST02 tests the rule with factors exp(-x) and cos(x).
c
      implicit none

      double precision a
      double precision b
      double precision exact
      double precision gn01, gn02
      external gn01
      external gn02
      integer n
      double precision vint

      a =  0.0D+00
      b =  3.141592653589793D+00

      exact = 0.5 * exp ( -b ) * ( sin ( b ) - cos ( b ) )
     &      - ( 0.5 * exp ( -a ) * ( sin ( a ) - cos ( a ) ) )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Integral of F(X) * G(X) from 0 to PI,'
      write ( *, '(a)' ) '  with F(X) = EXP(-X), G(X) = COS(X)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    VINT'
      write ( *, '(a)' ) ' '

      do n = 1, 10
        call ptrap ( a, b, n, gn01, gn02, vint )
        write ( *, '(2x,i6,2x,g14.6)' ) n, vint
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Exact:  ', exact

      return
      end
      function fn00 ( x )

c***********************************************************************
c
cc FN00 evaluates the function 1.
c
      implicit none

      double precision fn00
      double precision x

      fn00 = 1.0D+00

      return
      end
      function fn01 ( x )

c***********************************************************************
c
cc FN01 evaluates the function X.
c
      implicit none

      double precision fn01
      double precision x

      fn01 = x

      return
      end
      function gn00 ( x )

c***********************************************************************
c
cc GN00 evaluates the function 1/(1+X**2).
c
      implicit none

      double precision gn00
      double precision x

      gn00 = 1.0D+00 / ( 1.0D+00 + x * x )

      return
      end
      function gn01 ( x )

c***********************************************************************
c
cc GN01 evaluates the function exp(-x).
c
      implicit none

      double precision gn01
      double precision x

      gn01 = exp ( - x )

      return
      end
      function gn02 ( x )

c***********************************************************************
c
cc GN02 evaluates the function cos(x).
c
      implicit none

      double precision gn02
      double precision x

      gn02 = cos ( x )

      return
      end



#endif
