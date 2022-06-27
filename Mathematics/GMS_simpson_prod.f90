 
#include "GMS_config.fpp"

module simpson_prod     
     
     use mod_kinds, only : i4,dp
     public
     
     contains


     SUBROUTINE PSIMP ( A, B, N, FN, GN, VINT )
       
#if (USE_OMP) == 1
        use omp_lib
#endif 
!C
!C  THIS SUBROUTINE USES THE PRODUCT TYPE SIMPSON RULE
!C  COMPOUNDED N TIMES TO APPROXIMATE THE INTEGRAL FROM A TO B
!C  OF THE FUNCTION FN(X) * GN(X).  FN AND GN ARE FUNCTION
!C  SUBPROGRAMS WHICH MUST BE SUPPLIED BY THE USER.  THE
!C  RESULT IS STORED IN VINT.
!C
!
! ref: TOMS 15,12 (Dec 1972) 1070
! for: Product Type {Simpson}'s Integration
! by: W. R. Boland
! size: 8 kB
!
     
      INTEGER(i4) ::  I,N, J, K
      !REAL(dp) A, AG, AM(3,3), B, F(3), FN, G(3), &
       !GN, H, VINT, X(2)
      REAL(dp) A, AG, AM(3,3), B, F(3),G(3), &
               H, VINT, X(2)
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
      
      DATA AM(1,1), AM(3,3) /2 * 4.0_dp /, AM(1,2), AM(2,1),
     &  AM(2,3), AM(3,2) / 4 * 2.0_dp/, AM(1,3), AM(3,1)
     &  /2 * -1.D0/, AM(2,2) / 16.0_dp /
      H = ( B - A ) / DBLE ( FLOAT ( N ) )
      X(1) = A + H / 2.D0
      X(2) = A + H
      VINT = 0.D0
      F(3) = FN ( A )
      G(3) = GN ( A )
#if (USE_OMP) == 1
!$omp parallel do default(none) if(N>=250)   &
!$omp schedule(static) private(I,F,K,G,X,J,AG) &
!$omp lastprivate(VINT) shared(N,H,FN,GN,AM)
#endif
      DO 4 I  =  1, N
        F(1) = F(3)
        G(1) = G(3)
        DO 1 J = 1, 2
          F(J+1) = FN ( X(J) )
          G(J+1) = GN ( X(J) )
          X(J) = X(J) + H
1     CONTINUE
        DO 3 J = 1, 3
          AG = 0.0_dp
          DO 2 K = 1, 3
            AG = AG + AM(J,K) * G(K)
2     CONTINUE
        VINT = VINT + F(J) * AG
3     CONTINUE
4     CONTINUE
      VINT = H * VINT / 30.0_dp
    
      END SUBROUTINE





end module simpson_prod


#if 0

      program main

c***********************************************************************
c
cc TOMS437_PRB tests TOMS437.
c
      implicit none

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS437_PRB'
      write ( *, '(a)' ) '  Test TOMS algorithm 437, product-type'
      write ( *, '(a)' ) '  Simpson''s integration.'

      call test01
      call test02

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS437_PRB'
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
        call psimp ( a, b, n, fn00, gn00, vint )
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
        call psimp ( a, b, n, gn01, gn02, vint )
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

