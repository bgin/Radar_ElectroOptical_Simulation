

module wavelet_nr

!===================================================!
! Numerical Recipes public domain wavelet routines. !
!===================================================!

use mod_kinds, only : i4, sp
public
implicit none

integer(i4), parameter, private :: NMAX  = 2048
integer(i4), parameter, private :: NCMAX = 50

!Common /pwtcom/
real(sp), dimension(NMAX), private :: cc
real(sp), dimension(NCMAX),private :: cr
integer(i4),               private :: ncoff
integer(i4),               private :: ioff
integer(i4),               private :: joff
! End common
! Static arrays
real(sp),dimension(4),parameter,private  :: c4  = [0.4829629131445341_sp,0.8365163037378079_sp, &
                                                   0.2241438680420134_sp,-0.1294095225512604_sp]
real(sp),dimension(12),parameter,private :: c12 = [.111540743350_sp, .494623890398_sp, .751133908021_sp,                  &
                                                   .315250351709_sp,-.226264693965_sp,-.129766867567_sp,.097501605587_sp, &
                                                   .027522865530_sp,-.031582039318_sp,.000553842201_sp, .004777257511_sp, &
                                                   -.001077301085_sp]
real(sp),dimension(20),parameter,private :: c20 = [.026670057901_sp, .188176800078_sp, .527201188932_sp,                    &
                                                   .688459039454_sp, .281172343661_sp,-.249846424327_sp,-.195946274377_sp,  &
                                                   .127369340336_sp, .093057364604_sp,-.071394147166_sp,-.029457536822_sp,  &
                                                   .033212674059_sp,.003606553567_sp,-.010733175483_sp, .001395351747_sp,   &
                                                   .001992405295_sp,-.000685856695_sp,-.000116466855_sp,.000093588670_sp,   &
                                                   -.000013264203_sp]
                                                 

contains

SUBROUTINE wt1(a,n,isign,wtstep)
       !dir$ attributes forceinline :: wt1
       !dir$ optimize : 3
      INTEGER(i4) isign,n
      REAL(sp) a(n)
      EXTERNAL wtstep
!CU    USES wtstep
      INTEGER nn
      if (n.lt.4) return
      if (isign.ge.0) then
        nn=n
1       if (nn.ge.4) then
          call wtstep(a,nn,isign)
          nn=nn/2
        goto 1
        endif
      else
        nn=4
2       if (nn.le.n) then
          call wtstep(a,nn,isign)
          nn=nn*2
        goto 2
        endif
      endif
      return
END


SUBROUTINE daub4(a,n,isign)
       !dir$ attributes forceinline :: daub4
       !dir$ optimize : 3
      INTEGER(i4) n,isign,NMAX
      REAL(sp) a(n),C3,C2,C1,C0
      PARAMETER (C0=0.4829629131445341_sp,C1=0.8365163037378079_sp, &
      C2=0.2241438680420134_sp,C3=-0.1294095225512604_sp,NMAX=1024)
      REAL(sp) wksp(NMAX)
      INTEGER(i4) nh,nh1,i,j
      if(n.lt.4)return
      if(n.gt.NMAX) stop 'wksp too small in daub4'
      nh=n/2
      nh1=nh+1
      if (isign.ge.0) then
        i=1
        !dir$ assume_aligned wksp:64,a:64
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do 11 j=1,n-3,2
          wksp(i)=C0*a(j)+C1*a(j+1)+C2*a(j+2)+C3*a(j+3)
          wksp(i+nh)=C3*a(j)-C2*a(j+1)+C1*a(j+2)-C0*a(j+3)
          i=i+1
11      continue
        wksp(i)=C0*a(n-1)+C1*a(n)+C2*a(1)+C3*a(2)
        wksp(i+nh)=C3*a(n-1)-C2*a(n)+C1*a(1)-C0*a(2)
      else
        wksp(1)=C2*a(nh)+C1*a(n)+C0*a(1)+C3*a(nh1)
        wksp(2)=C3*a(nh)-C0*a(n)+C1*a(1)-C2*a(nh1)
        j=3
        !dir$ assume_aligned wksp:64,a:64
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do 12 i=1,nh-1
          wksp(j)=C2*a(i)+C1*a(i+nh)+C0*a(i+1)+C3*a(i+nh1)
          wksp(j+1)=C3*a(i)-C0*a(i+nh)+C1*a(i+1)-C2*a(i+nh1)
          j=j+2
12      continue
      endif
      do 13 i=1,n
        a(i)=wksp(i)
13    continue
      return
END

SUBROUTINE pwtset(n)
       !dir$ attributes forceinline :: pwtset
       !dir$ optimize : 3
      INTEGER(i4) n !,NCMAX,ncof,ioff,joff
      !PARAMETER (NCMAX=50)
      !REAL cc(NCMAX),cr(NCMAX)
      !COMMON /pwtcom/ cc,cr,ncof,ioff,joff
      INTEGER(i4) k
      REAL(sp) sig,c4(4),c12(12),c20(20)
      !SAVE c4,c12,c20 !,/pwtcom/
      !DATA c4/0.4829629131445341, 0.8365163037378079,0.2241438680420134,
      !*-0.1294095225512604/
      !DATA c12 /.111540743350, .494623890398, .751133908021,
      !*.315250351709,-.226264693965,-.129766867567,.097501605587, 
      !*.027522865530,-.031582039318,.000553842201, .004777257511,
      !*-.001077301085/
      ! DATA c20 /.026670057901, .188176800078, .527201188932,
      !*.688459039454, .281172343661,-.249846424327,-.195946274377, 
      !*.127369340336, .093057364604,-.071394147166,-.029457536822, 
      !*.033212674059,.003606553567,-.010733175483, .001395351747,
      !*.001992405295,-.000685856695,-.000116466855,.000093588670,
      !*-.000013264203 /
      ncof=n
      sig=-1.
      do 11 k=1,n
        if(n.eq.4)then
          cc(k)=c4(k)
        else if(n.eq.12)then
          cc(k)=c12(k)
        else if(n.eq.20)then
          cc(k)=c20(k)
        else
          stop 'unimplemented value n in pwtset'
        endif
        cr(ncof+1-k)=sig*cc(k)
        sig=-sig
11    continue
      ioff=-n/2
      joff=-n/2
      return
END


SUBROUTINE pwt(a,n,isign)
       !dir$ attributes forceinline :: pwt
       !dir$ optimize : 3
      use omp_lib
      INTEGER isign,n !NMAX,NCMAX,ncof,ioff,joff
      !PARAMETER (NMAX=2048,NCMAX=50)
      REAL a(n),wksp(NMAX)!cc(NCMAX),cr(NCMAX)
      !COMMON /pwtcom/ cc,cr,ncof,ioff,joff
      INTEGER i,ii,j,jf,jr,k,n1,ni,nj,nh,nmod
      REAL ai,ai1
      if (n.lt.4) return
      nmod=ncof*n
      n1=n-1
      nh=n/2
      do 11 j=1,n
        wksp(j)=0.
11    continue
      if (isign.ge.0) then
        ii=1
        do 13 i=1,n,2
          ni=i+nmod+ioff
          nj=i+nmod+joff
          do 12 k=1,ncof
            jf=iand(n1,ni+k)
            jr=iand(n1,nj+k)
            wksp(ii)=wksp(ii)+cc(k)*a(jf+1)
            wksp(ii+nh)=wksp(ii+nh)+cr(k)*a(jr+1)
12        continue
          ii=ii+1
13      continue
      else
        ii=1
        do 15 i=1,n,2
          ai=a(ii)
          ai1=a(ii+nh)
          ni=i+nmod+ioff
          nj=i+nmod+joff
          !dir$ assume_aligned wksp:64
          !$omp simd reduction(+:wksp) private(jf,jr)
          do 14 k=1,ncof
            jf=iand(n1,ni+k)+1
            jr=iand(n1,nj+k)+1
            wksp(jf)=wksp(jf)+cc(k)*ai
            wksp(jr)=wksp(jr)+cr(k)*ai1
14        continue
          ii=ii+1
15      continue
      endif
      do 16 j=1,n
        a(j)=wksp(j)
16    continue
      return
END


SUBROUTINE wtn(a,nn,ndim,isign,wtstep)
      INTEGER*i4) isign,ndim,nn(ndim),NMAX
      REAL(sp) a(*)
      EXTERNAL wtstep
      PARAMETER (NMAX=1024)
CU    USES wtstep
      INTEGER(i4) i1,i2,i3,idim,k,n,nnew,nprev,nt,ntot
      REAL(sp) wksp(NMAX)
      ntot=1
      do 11 idim=1,ndim
        ntot=ntot*nn(idim)
11    continue
      nprev=1
      do 16 idim=1,ndim
        n=nn(idim)
        nnew=n*nprev
        if (n.gt.4) then
          do 15 i2=0,ntot-1,nnew
            do 14 i1=1,nprev
              i3=i1+i2
              do 12 k=1,n
                wksp(k)=a(i3)
                i3=i3+nprev
12            continue
              if (isign.ge.0) then
                nt=n
1               if (nt.ge.4) then
                call wtstep(wksp,nt,isign)
                nt=nt/2
                goto 1
                endif
              else
                nt=4
2               if (nt.le.n) then
                call wtstep(wksp,nt,isign)
                nt=nt*2
                goto 2
                endif
              endif
              i3=i1+i2
              do 13 k=1,n
                a(i3)=wksp(k)
                i3=i3+nprev
13            continue
14          continue
15        continue
        endif
        nprev=nnew
16    continue
      return
END




end module wavele_nr
