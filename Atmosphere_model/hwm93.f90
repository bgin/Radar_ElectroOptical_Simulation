!module hwm93

!contains

      subroutine gws5(iyd,sec,alt,glat,glong,stl,f107a,f107,ap,w) 
!      horizontal wind model hwm93 covering all altitude regions        
!      a. e. hedin  (1/25/93) (4/9/93)                                  
!      calling argument list made similar to gts5 subroutine for        
!       msis-86 density model and gws4 for thermospheric winds.         
!        iyd - year and day as yyddd                                    
!        sec - ut(sec)  (not important in lower atmosphere)             
!        alt - altitude(km)                                             
!        glat - geodetic latitude(deg)                                  
!        glong - geodetic longitude(deg)                                
!        stl - local apparent solar time(hrs)                           
!        f107a - 3 month average of f10.7 flux (use 150 in lower atmos.)
!        f107 - daily f10.7 flux for previous day ( " )                 
!        ap - two element array with                                    
!             ap(1) = magnetic index(daily) (use 4 in lower atmos.)     
!             ap(2)=current 3hr ap index (used only when sw(9)=-1.)     
!     note:  ut, local time, and longitude are used independently in the
!            model and are not of equal importance for every situation. 
!            for the most physically realistic calculation these three  
!            variables should be consistent.                            
!      output                                                           
!        w(1) = meridional (m/sec + northward)                          
!        w(2) = zonal (m/sec + eastward)                                
!          additional comments                                          
!               to turn on and off particular variations call tselec_hwm
!               where sw is a 25 element array containing 0. for off, 1.
!               for on, or 2. for main effects off but cross terms on   
!               for the following variations                            
!               1 - f10.7 effect on mean  2 - time independent          
!               3 - symmetrical annual    4 - symmetrical semiannual    
!               5 - asymmetrical annual   6 - asymmetrical semiannual   
!               7 - diurnal               8 - semidiurnal               
!               9 - daily ap             10 - all ut/long effects       
!              11 - longitudinal         12 - ut and mixed ut/long      
!              13 - mixed ap/ut/long     14 - terdiurnal                
!              16 - all windf var        17 - all wzl var               
!              18 - all un1 var          19 - all wdzl var              
!              24 - all b fields (div)   25 - all c fields (curl)       
!                                                                       
!              to get current values of sw: call tretrv_hwm(sw)         
!                                                                       
!             for example, to get zonal averages (no diurnal or         
!             longitudinal variations) set sw(7),sw(8), sw(14),         
!             and sw(10) equal to 0.  to just remove tidal variations   
!             set sw(7),sw(8), and sw(14) equal to 0.                   
      parameter (mn1=5,mn2=14) 
      dimension ap(2),w(2),windf(2),ww(2),sv(25) 
      dimension wzl(2),wdzl(2) 
      dimension zn1(mn1),un1(mn1,2),ugn1(2,2) 
      dimension zn2(mn2),un2(mn2,2),ugn2(2,2) 
      common/parmw5/pwb(200),pwc(200),pwbl(150),pwcl(150),pwbld(150),   &
     & pwcld(150),pb12(150),pc12(150),pb13(150),pc13(150),              &
     & pb14(150),pc14(150),pb15(150),pc15(150),                         &
     & pb15d(150),pc15d(150),pwp(100,26)                                
      common/csw/sw(25),isw,swc(25) 
      common/hwmc/wbt(2),wct(2) 
      common/datw/isd(3),ist(2),nam(2) 
      common/datime/isdate(3),istime(2),name(2) 
      save 
      external initw5,gwsbk5 
      data s/.016/,zl/200./,sv/25*1./,nnn/3/,mn2s/1/,mn2m/1/ 
      data zn1/200.,150.,130.,115.,100./ 
      data zn2/100.,90.,82.5,75.,67.5,60.,52.5,45.,37.5,30.,22.5,       &
     & 15.,7.5,0/                                                       

!dbg20110729
!dbg      print *,' sub gws5',iyd,sec,alt,glat,glong,stl,f107a,f107,ap,w 

!      put identification data into common/datime/                      
      do 1 i=1,3 
        isdate(i)=isd(i) 
    1 end do 
      do 2 i=1,2 
        istime(i)=ist(i) 
        name(i)=nam(i) 
    2 end do 
      if(isw.ne.64999) call tselec_hwm(sv) 
      yrd=iyd 
      ww(1)=w(1) 
      ww(2)=w(2) 
!                                                                       
      if(alt.le.zn1(mn1)) goto 50 
!                                                                       
!       exosphere wind                                                  
      call glbw5e(yrd,sec,glat,glong,stl,f107a,f107,ap,pwb,pwc,windf) 
      windf(1)=sw(16)*windf(1) 
      windf(2)=sw(16)*windf(2) 
!       wind  at zl                                                     
      call glbw5m(yrd,sec,glat,glong,stl,f107a,f107,ap,pwbl,pwcl,ww) 
      wzl(1)=(pwbl(1)*windf(1)+ww(1))*sw(17)*sw(18) 
      wzl(2)=(pwbl(1)*windf(2)+ww(2))*sw(17)*sw(18) 
      un1(1,1)=wzl(1) 
      un1(1,2)=wzl(2) 
!       wind derivative at zl                                           
      ww(1)=0 
      ww(2)=0 
      call glbw5m(yrd,sec,glat,glong,stl,f107a,f107,ap,pwbld,pwcld,ww) 
      wdzl(1)=(pwbld(1)*windf(1)+ww(1))*sw(19)*sw(18) 
      wdzl(2)=(pwbld(1)*windf(2)+ww(2))*sw(19)*sw(18) 
      ugn1(1,1)=wdzl(1)*s 
      ugn1(1,2)=wdzl(2)*s 
!                                                                       
      if(alt.ge.zl) goto 90 
!                                                                       
!        wind at zn1(2) (150)                                           
      call glbw5m(yrd,sec,glat,glong,stl,f107a,f107,ap,pb12,pc12,ww) 
      un1(2,1)=(pb12(1)*windf(1)+ww(1))*sw(18) 
      un1(2,2)=(pb12(1)*windf(2)+ww(2))*sw(18) 
!        wind at zn1(3) (130)                                           
      call glbw5m(yrd,sec,glat,glong,stl,f107a,f107,ap,pb13,pc13,ww) 
      un1(3,1)=ww(1)*sw(18) 
      un1(3,2)=ww(2)*sw(18) 
!        wind at zn1(4) (115)                                           
      call glbw5m(yrd,sec,glat,glong,stl,f107a,f107,ap,pb14,pc14,ww) 
      un1(4,1)=ww(1)*sw(18) 
      un1(4,2)=ww(2)*sw(18) 
!                                                                       
   50 continue 
      mnn=max(1,min(mn2,nnn+1)) 
      if(alt.lt.zn2(mnn)) goto 40 
!                                                                       
!        wind at zn1(5) (100)                                           
      call glbw5m(yrd,sec,glat,glong,stl,f107a,f107,ap,pb15,pc15,ww) 
      un1(5,1)=ww(1)*sw(18) 
      un1(5,2)=ww(2)*sw(18) 
!         wind derivative at zn1(5) (100)                               
      call glbw5m(yrd,sec,glat,glong,stl,f107a,f107,ap,pb15d,pc15d,ww) 
      ugn1(2,1)=ww(1)*sw(18) 
      ugn1(2,2)=ww(2)*sw(18) 
!                                                                       
      if(alt.ge.zn1(mn1)) goto 90 
!                                                                       
      ugn2(1,1)=ugn1(2,1) 
      ugn2(1,2)=ugn1(2,2) 
      un2(1,1)=un1(5,1) 
      un2(1,2)=un1(5,2) 
      goto 45 
   40 continue 
      ugn2(1,1)=1.e30 
      ugn2(1,2)=1.e30 
      un2(1,1)=0 
      un2(1,2)=0 
   45 continue 
!                                                                       
      do 10 i=1,mn2 
        if(alt.gt.zn2(i)) goto 12 
   10 end do 
      i=mn2 
   12 iz=i 
      mn2s=max(1,min(iz-1,iz-nnn)) 
      mn2e=min(mn2,max(mn2s+1,iz-1+nnn)) 
      do 20 i=mn2s,mn2e 
        ii=2*(i-2)+1 
        if(i.gt.1) then 
          call glbw5s(iyd,glat,glong,stl,pwp(1,ii),pwp(1,ii+1),ww) 
          un2(i,1)=ww(1)*sw(20) 
          un2(i,2)=ww(2)*sw(20) 
        endif 
   20 end do 
      mn2m=mn2e-mn2s+1 
      ugn2(2,1)=1.e30 
      ugn2(2,2)=1.e30 
   90 continue 
!       wind at altitude                                                
      if(w(1).ne.9898)                                                  &
     & w(1)= wprof(alt,zl,s,windf(1),wzl(1),wdzl(1),                    &
     &  mn1,zn1,un1(1,1),ugn1(1,1),mn2m,zn2(mn2s),un2(mn2s,1),ugn2(1,1))
      if(w(2).ne.9898)                                                  &
     & w(2)= wprof(alt,zl,s,windf(2),wzl(2),wdzl(2),                    &
     &  mn1,zn1,un1(1,2),ugn1(1,2),mn2m,zn2(mn2s),un2(mn2s,2),ugn2(1,2))

!dbg20110729
!dbg      print *,'(1)w=',w
      return 
!       set number of nodes calculated each side of required altitude   
!         to adjust profile accuracy vs efficiency                      
      entry setnw5(nnw) 
      nnn=nnw 
      end                                           
!-----------------------------------------------------------------------
      function wprof(z,zl,s,uinf,ulb,ulbd,mn1,zn1,un1,ugn1,             &
     &   mn2,zn2,un2,ugn2)                                              
      dimension zn1(mn1),un1(mn1),ugn1(2),xs(15),ys(15),y2out(15) 
      dimension zn2(mn2),un2(mn2),ugn2(2) 
      save 
      if(z.ge.zl) then 
        x=s*(z-zl) 
        f=exp(-x) 
        wprof=uinf+(ulb-uinf)*f+(ulb-uinf+ulbd)*x*f 
        return 
      endif 
      if(z.ge.zn1(mn1).and.z.lt.zn1(1)) then 
        mn=mn1 
        z1=zn1(1) 
        z2=zn1(mn) 
        zdif=z2-z1 
        do 10 k=1,mn 
          xs(k)=(zn1(k)-z1)/zdif 
          ys(k)=un1(k) 
   10   continue 
        yd1=ugn1(1)*zdif 
        yd2=ugn1(2)*zdif 
        call spline_hwm(xs,ys,mn,yd1,yd2,y2out) 
!      eq.                                                              
        x=(z-z1)/zdif 
!      eq.                                                              
        call splint_hwm(xs,ys,y2out,mn,x,y) 
        wprof=y 
        return 
      endif 
      if(z.lt.zn2(1)) then 
        mn=mn2 
        z1=zn2(1) 
        z2=zn2(mn) 
        zdif=z2-z1 
        do 20 k=1,mn 
          xs(k)=(zn2(k)-z1)/zdif 
          ys(k)=un2(k) 
   20   continue 
        yd1=ugn2(1) 
        if(ugn2(1).lt.1.e30) yd1=ugn2(1)*zdif 
        yd2=ugn2(2) 
        if(ugn2(2).lt.1.e30) yd2=ugn2(2)*zdif 
        call spline_hwm(xs,ys,mn,yd1,yd2,y2out) 
!      eq.                                                              
        x=(z-z1)/zdif 
!      eq.                                                              
        call splint_hwm(xs,ys,y2out,mn,x,y) 
        wprof=y 
        return 
      endif 
      return 
      end                                           
!-----------------------------------------------------------------------
      subroutine glbw5e(yrd,sec,lat,long,stl,f107a,f107,ap,pb,pc,ww) 
      real lat,long 
      dimension wb(2,15),wc(2,15),pb(200),pc(200),ww(2) 
      dimension ap(2) 
      common/csw/sw(25),isw,swc(25) 
      common/hwmc/wbt(2),wct(2) 
!      common/vpoly/bt(20,20),bp(20,20),cstl,sstl,c2stl,s2stl,          
!     $ c3stl,s3stl,iyr,day,df,dfa,dfc,apd,apdf,apdfc,apt,slt           
      common/vpoly2/xvl,lvl,mvl,clat,slat,bt(20,20),bp(20,20) 
      common/ltcomp/tll,nsvl,cstl,sstl,c2stl,s2stl,c3stl,s3stl 
      common/lgcomp/xll,ngvl,clong,slong,c2long,s2long 
      save 
      data dgtr/.017453/,sr/7.2722e-5/,hr/.2618/,dr/1.72142e-2/ 
      data nsw/14/,wb/30*0/,wc/30*0/ 
      data pb14/-1./,pb18/-1./ 
      data sw9/1./,lv/12/,mv/3/,nsv/3/,ngv/2/,pset/3./ 
      g0(a)=(a-4.+(pb(26)-1.)*(a-4.+(exp(-abs(pb(25))*(a-4.))-1.)/      &
     & abs(pb(25))))                                                    
!       confirm parameter set                                           
      if(pb(100).eq.0) pb(100)=pset 
      if(pb(100).ne.pset) then 
        write(6,900) pb(100),pc(100) 
  900   format(1x,'wrong parameter set for glbw5e',3f10.1) 
        stop 
      endif 
!                                                                       
      do 10 j=1,nsw 
        wb(1,j)=0 
        wb(2,j)=0 
        wc(1,j)=0 
        wc(2,j)=0 
   10 end do 
      if(sw(9).gt.0) sw9=1. 
      if(sw(9).lt.0) sw9=-1. 
      iyr = yrd/1000. 
      day = yrd - iyr*1000. 
      if(xvl.ne.lat.or.lv.gt.lvl.or.mv.gt.mvl) then 
        slat=sin(dgtr*lat) 
        clat=cos(dgtr*lat) 
        call vsphr1(slat,clat,lv,mv,bt,bp,20) 
        xvl=lat 
        lvl=lv 
        mvl=mv 
      endif 
      if(tll.ne.stl.or.nsv.gt.nsvl)  then 
        sstl = sin(hr*stl) 
        cstl = cos(hr*stl) 
        s2stl = sin(2.*hr*stl) 
        c2stl = cos(2.*hr*stl) 
        s3stl = sin(3.*hr*stl) 
        c3stl = cos(3.*hr*stl) 
        tll = stl 
        nsvl=nsv 
      endif 
      if(day.ne.dayl.or.pb(14).ne.pb14) then 
        cd14=cos(dr*(day-pb(14))) 
!        sd14=sin(dr*(day-pb(14)))                                      
      endif 
      if(day.ne.dayl.or.pb(18).ne.pb18) cd18=cos(2.*dr*(day-pb(18))) 
      dayl=day 
      pb14=pb(14) 
      pb18=pb(18) 
!JFM  if(xll.ne.long) then 
        slong=sin(dgtr*long) 
        clong=cos(dgtr*long) 
        s2long=sin(2.*dgtr*long) 
        c2long=cos(2.*dgtr*long) 
        xll=long 
        ngvl=2 
!JFM  endif 
!       f10.7 effect                                                    
      df=f107-f107a 
      dfa=f107a-150. 
      dfc=dfa+pb(20)*df 
!       time independent                                                
      f1b=1.+pb(22)*dfc*swc(1) 
      if(ww(1).ne.9898) then 
       wb(1,2)=(pb(2)*bt(3,1)+pb(3)*bt(5,1)+pb(23)*bt(7,1))*f1b 
      endif 
      wb(2,2)=0. 
      f1c=1.+pc(22)*dfc*swc(1) 
      wc(1,2)=0. 
      if(ww(2).ne.9898) then 
       wc(2,2)=-(pc(2)*bt(2,1)+pc(3)*bt(4,1)+pc(23)*bt(6,1))*f1c        &
     & -(pc(27)*bt(3,1)+pc(15)*bt(5,1)+pc(60)*bt(7,1)                   &
     & +pc(161)*bt(9,1)+pc(162)*bt(11,1)+pc(163)*bt(13,1))*f1c          
      endif 
!       symmetrical annual                                              
!       symmetrical semiannual                                          
      if(ww(1).ne.9898) then 
       wb(1,4)=(pb(17)*bt(3,1)+pb(31)*bt(5,1))*cd18 
      endif 
      wb(2,4)=0 
      wc(1,4)=0 
      if(ww(2).ne.9898) then 
       wc(2,4)=-(pc(17)*bt(2,1)+pc(31)*bt(4,1))*cd18 
      endif 
!       asymmetrical annual                                             
      f5b=1.+pb(48)*dfc*swc(1) 
      if(ww(1).ne.9898) then 
       wb(1,5)=(pb(10)*bt(2,1)+pb(11)*bt(4,1))*cd14*f5b 
      endif 
      wb(2,5)=0 
      f5c=1.+pc(48)*dfc*swc(1) 
      wc(1,5)=0 
      if(ww(2).ne.9898) then 
       wc(2,5)=-(pc(10)*bt(3,1)+pc(11)*bt(5,1))*cd14*f5c 
      endif 
!       asymmetrical semiannual                                         
!         none                                                          
!       diurnal                                                         
      if(sw(7).eq.0) goto 200 
      f7b=1.+pb(50)*dfc*swc(1) 
      f75b=1.+pb(83)*dfc*swc(1) 
      if(ww(1).ne.9898) then 
       wb(1,7)=(pb(7)*bt(2,2)+pb(8)*bt(4,2)+pb(29)*bt(6,2)              &
     & +pb(142)*bt(8,2)+pb(144)*bt(10,2)                                &
     &  +pb(182)*bt(3,2)+pb(184)*bt(5,2)                                &
     &  )*sstl*f7b                                                      &
     & +(pb(13)*bt(3,2)+pb(146)*bt(5,2))                                &
     &    *cd14*sstl*f75b*swc(5)                                        &
     & +(pb(171)*bt(2,2)+pb(173)*bt(4,2))                               &
     &    *cd18*sstl*f75b*swc(4)                                        &
     & + (pb(4)*bt(2,2)+pb(5)*bt(4,2)+pb(28)*bt(6,2)                    &
     & +pb(141)*bt(8,2)+pb(143)*bt(10,2)                                &
     &  +pb(181)*bt(3,2)+pb(183)*bt(5,2)                                &
     &  )*cstl*f7b                                                      &
     & +(pb(12)*bt(3,2)+pb(145)*bt(5,2))                                &
     &      *cd14*cstl*f75b*swc(5)                                      &
     & +(pb(170)*bt(2,2)+pb(172)*bt(4,2))                               &
     &    *cd18*cstl*f75b*swc(4)                                        
      endif 
      if(ww(2).ne.9898) then 
       wb(2,7)=-(pb(4)*bp(2,2)+pb(5)*bp(4,2)+pb(28)*bp(6,2)             &
     &   +pb(141)*bp(8,2)+pb(143)*bp(10,2)                              &
     &   +pb(181)*bp(3,2)+pb(183)*bp(5,2)                               &
     &  )*sstl*f7b                                                      &
     & -(pb(12)*bp(3,2)+pb(145)*bp(5,2))                                &
     &    *cd14*sstl*f75b*swc(5)                                        &
     & -(pb(170)*bp(2,2)+pb(172)*bp(4,2))                               &
     &    *cd18*sstl*f75b*swc(4)                                        &
     & + (pb(7)*bp(2,2)+pb(8)*bp(4,2)+pb(29)*bp(6,2)                    &
     &   +pb(142)*bp(8,2)+pb(144)*bp(10,2)                              &
     &   +pb(182)*bp(3,2)+pb(184)*bp(5,2)                               &
     &  )*cstl*f7b                                                      &
     & +(pb(13)*bp(3,2)+pb(146)*bp(5,2))                                &
     &    *cd14*cstl*f75b*swc(5)                                        &
     & +(pb(171)*bp(2,2)+pb(173)*bp(4,2))                               &
     &    *cd18*cstl*f75b*swc(4)                                        
      endif 
      f7c=1.+pc(50)*dfc*swc(1) 
      f75c=1.+pc(83)*dfc*swc(1) 
      if(ww(1).ne.9898) then 
       wc(1,7)=-(pc(4)*bp(3,2)+pc(5)*bp(5,2)+pc(28)*bp(7,2)             &
     &   +pc(141)*bp(9,2)+pc(143)*bp(11,2)                              &
     &   +pc(181)*bp(2,2)+pc(183)*bp(4,2)+pc(185)*bp(6,2)               &
     &   +pc(187)*bp(8,2)+pc(189)*bp(10,2)                              &
     &  )*sstl*f7c                                                      &
     & -(pc(12)*bp(2,2)+pc(145)*bp(4,2))                                &
     &    *cd14*sstl*f75c*swc(5)                                        &
     & -(pc(170)*bp(3,2)+pc(172)*bp(5,2))                               &
     &    *cd18*sstl*f75c*swc(4)                                        &
     & +(pc(7)*bp(3,2)+pc(8)*bp(5,2)+pc(29)*bp(7,2)                     &
     & +pc(142)*bp(9,2)+pc(144)*bp(11,2)                                &
     & +pc(182)*bp(2,2)+pc(184)*bp(4,2)+pc(186)*bp(6,2)                 &
     & +pc(188)*bp(8,2)+pc(190)*bp(10,2)                                &
     &  )*cstl*f7c                                                      &
     & +(pc(13)*bp(2,2)+pc(146)*bp(4,2))                                &
     &     *cd14*cstl*f75c*swc(5)                                       &
     & +(pc(171)*bp(3,2)+pc(173)*bp(5,2))                               &
     &    *cd18*cstl*f75c*swc(4)                                        
      endif 
      if(ww(2).ne.9898) then 
       wc(2,7)=-(pc(7)*bt(3,2)+pc(8)*bt(5,2)+pc(29)*bt(7,2)             &
     & +pc(142)*bt(9,2)+pc(144)*bt(11,2)                                &
     & +pc(182)*bt(2,2)+pc(184)*bt(4,2)+pc(186)*bt(6,2)                 &
     & +pc(188)*bt(8,2)+pc(190)*bt(10,2)                                &
     &  )*sstl*f7c                                                      &
     & -(pc(13)*bt(2,2)+pc(146)*bt(4,2))                                &
     &    *cd14*sstl*f75c*swc(5)                                        &
     & -(pc(171)*bt(3,2)+pc(173)*bt(5,2))                               &
     &    *cd18*sstl*f75c*swc(4)                                        &
     & -(pc(4)*bt(3,2)+pc(5)*bt(5,2)+pc(28)*bt(7,2)                     &
     & +pc(141)*bt(9,2)+pc(143)*bt(11,2)                                &
     & +pc(181)*bt(2,2)+pc(183)*bt(4,2)+pc(185)*bt(6,2)                 &
     & +pc(187)*bt(8,2)+pc(189)*bt(10,2)                                &
     &  )*cstl*f7c                                                      &
     & -(pc(12)*bt(2,2)+pc(145)*bt(4,2))                                &
     &    *cd14*cstl*f75c*swc(5)                                        &
     & -(pc(170)*bt(3,2)+pc(172)*bt(5,2))                               &
     &    *cd18*cstl*f75c*swc(4)                                        
      endif 
  200 continue 
!       semidiurnal                                                     
      if(sw(8).eq.0) goto 210 
      f8b=1.+pb(90)*dfc*swc(1) 
      if(ww(1).ne.9898) then 
       wb(1,8)=(pb(9)*bt(3,3)+pb(43)*bt(5,3)                            &
     &   +pb(111)*bt(7,3)                                               &
     &   +(pb(34)*bt(4,3)+pb(148)*bt(6,3))*cd14*swc(5)                  &
     &   +(pb(134)*bt(3,3))*cd18*swc(4)                                 &
     &   +pb(152)*bt(4,3)+pb(154)*bt(6,3)+pb(156)*bt(8,3)               &
     &   +pb(158)*bt(10,3)                                              &
     &  )*s2stl*f8b                                                     &
     & +(pb(6)*bt(3,3)+pb(42)*bt(5,3)                                   &
     &   +pb(110)*bt(7,3)                                               &
     &   +(pb(24)*bt(4,3)+pb(147)*bt(6,3))*cd14*swc(5)                  &
     &   +(pb(135)*bt(3,3))*cd18*swc(4)                                 &
     &   +pb(151)*bt(4,3)+pb(153)*bt(6,3)+pb(155)*bt(8,3)               &
     &   +pb(157)*bt(10,3)                                              &
     &  )*c2stl*f8b                                                     
      endif 
      if(ww(2).ne.9898) then 
       wb(2,8)=-(pb(6)*bp(3,3)+pb(42)*bp(5,3)                           &
     &   +pb(110)*bp(7,3)                                               &
     &   +(pb(24)*bp(4,3)+pb(147)*bp(6,3))*cd14*swc(5)                  &
     &   +(pb(135)*bp(3,3))*cd18*swc(4)                                 &
     &   +pb(151)*bp(4,3)+pb(153)*bp(6,3)+pb(155)*bp(8,3)               &
     &   +pb(157)*bp(10,3)                                              &
     &  )*s2stl*f8b                                                     &
     &   + (pb(9)*bp(3,3)+pb(43)*bp(5,3)                                &
     &   +pb(111)*bp(7,3)                                               &
     &   +(pb(34)*bp(4,3)+pb(148)*bp(6,3))*cd14*swc(5)                  &
     &   +(pb(134)*bp(3,3))*cd18*swc(4)                                 &
     &   +pb(152)*bp(4,3)+pb(154)*bp(6,3)+pb(156)*bp(8,3)               &
     &   +pb(158)*bp(10,3)                                              &
     &  )*c2stl*f8b                                                     
      endif 
      f8c=1.+pc(90)*dfc*swc(1) 
      if(ww(1).ne.9898) then 
       wc(1,8)=-(pc(6)*bp(4,3)+pc(42)*bp(6,3)                           &
     &   +pc(110)*bp(8,3)                                               &
     &   +(pc(24)*bp(3,3)+pc(147)*bp(5,3))*cd14*swc(5)                  &
     &   +(pc(135)*bp(4,3))*cd18*swc(4)                                 &
     &   +pc(151)*bp(3,3)+pc(153)*bp(5,3)+pc(155)*bp(7,3)               &
     &   +pc(157)*bp(9,3)                                               &
     &  )*s2stl*f8c                                                     &
     & +(pc(9)*bp(4,3)+pc(43)*bp(6,3)                                   &
     &   +pc(111)*bp(8,3)                                               &
     &   +(pc(34)*bp(3,3)+pc(148)*bp(5,3))*cd14*swc(5)                  &
     &   +(pc(134)*bp(4,3))*cd18*swc(4)                                 &
     &   +pc(152)*bp(3,3)+pc(154)*bp(5,3)+pc(156)*bp(7,3)               &
     &   +pc(158)*bp(9,3)                                               &
     &  )*c2stl*f8c                                                     
      endif 
      if(ww(2).ne.9898) then 
       wc(2,8)=-(pc(9)*bt(4,3)+pc(43)*bt(6,3)                           &
     &   +pc(111)*bt(8,3)                                               &
     &   +(pc(34)*bt(3,3)+pc(148)*bt(5,3))*cd14*swc(5)                  &
     &   +(pc(134)*bt(4,3))*cd18*swc(4)                                 &
     &   +pc(152)*bt(3,3)+pc(154)*bt(5,3)+pc(156)*bt(7,3)               &
     &   +pc(158)*bt(9,3)                                               &
     &  )*s2stl*f8c                                                     &
     & - (pc(6)*bt(4,3)+pc(42)*bt(6,3)                                  &
     &   +pc(110)*bt(8,3)                                               &
     &   +(pc(24)*bt(3,3)+pc(147)*bt(5,3))*cd14*swc(5)                  &
     &   +(pc(135)*bt(4,3))*cd18*swc(4)                                 &
     &   +pc(151)*bt(3,3)+pc(153)*bt(5,3)+pc(155)*bt(7,3)               &
     &   +pc(157)*bt(9,3)                                               &
     &  )*c2stl*f8c                                                     
      endif 
  210 continue 
!        terdiurnal                                                     
      if(sw(14).eq.0) goto 220 
      f14b=1. 
      if(ww(1).ne.9898) then 
       wb(1,14)=(pb(40)*bt(4,4)+pb(149)*bt(6,4)                         &
     &   +pb(114)*bt(8,4)                                               &
     &   +(pb(94)*bt(5,4)+pb(47)*bt(7,4))*cd14*swc(5)                   &
     &  )*s3stl*f14b                                                    &
     & + (pb(41)*bt(4,4)+pb(150)*bt(6,4)                                &
     &   +pb(115)*bt(8,4)                                               &
     &   +(pb(95)*bt(5,4)+pb(49)*bt(7,4))*cd14*swc(5)                   &
     &  )*c3stl*f14b                                                    
      endif 
      if(ww(2).ne.9898) then 
       wb(2,14)=-(pb(41)*bp(4,4)+pb(150)*bp(6,4)                        &
     &   +pb(115)*bp(8,4)                                               &
     &   +(pb(95)*bp(5,4)+pb(49)*bp(7,4))*cd14*swc(5)                   &
     &  )*s3stl*f14b                                                    &
     & + (pb(40)*bp(4,4)+pb(149)*bp(6,4)                                &
     &   +pb(114)*bp(8,4)                                               &
     &   +(pb(94)*bp(5,4)+pb(47)*bp(7,4))*cd14*swc(5)                   &
     &  )*c3stl*f14b                                                    
      endif 
      f14c=1. 
      if(ww(1).ne.9898) then 
       wc(1,14)=-(pc(41)*bp(5,4)+pc(150)*bp(7,4)                        &
     &   +pc(115)*bp(9,4)                                               &
     &   +(pc(95)*bp(4,4)+pc(49)*bp(6,4))*cd14*swc(5)                   &
     &  )*s3stl*f14c                                                    &
     & + (pc(40)*bp(5,4)+pc(149)*bp(7,4)                                &
     &   +pc(114)*bp(9,4)                                               &
     &   +(pc(94)*bp(4,4)+pc(47)*bp(6,4))*cd14*swc(5)                   &
     &  )*c3stl*f14c                                                    
      endif 
      if(ww(2).ne.9898) then 
       wc(2,14)=-(pc(40)*bt(5,4)+pc(149)*bt(7,4)                        &
     &   +pc(114)*bt(9,4)                                               &
     &   +(pc(94)*bt(4,4)+pc(47)*bt(6,4))*cd14*swc(5)                   &
     &  )*s3stl*f14c                                                    &
     & - (pc(41)*bt(5,4)+pc(150)*bt(7,4)                                &
     &   +pc(115)*bt(9,4)                                               &
     &   +(pc(95)*bt(4,4)+pc(49)*bt(6,4))*cd14*swc(5)                   &
     &  )*c3stl*f14c                                                    
      endif 
  220 continue 
!        magnetic activity                                              
      if(sw(9).eq.0.) goto 40 
      if(sw9.eq.-1.) goto 30 
!           daily ap                                                    
      apd=ap(1)-4. 
      apdf=(apd+(pb(45)-1.)*(apd+(exp(-pb(44)*apd)-1.)/pb(44))) 
!      apdfc=(apd+(pc(45)-1.)*(apd+(exp(-pc(44)*apd)-1.)/pc(44)))       
      apdfc=apdf 
      if(apd.eq.0.) goto 40 
      if(ww(1).ne.9898) then 
       wb(1,9)=(pb(46)*bt(3,1)+pb(35)*bt(5,1)+pb(33)*bt(7,1))*apdf      &
     &  +(pb(175)*bt(3,3)+pb(177)*bt(5,3))*s2stl*apdf                   &
     &  +(pb(174)*bt(3,3)+pb(176)*bt(5,3))*c2stl*apdf                   
      endif 
      if(ww(2).ne.9898) then 
       wb(2,9)=0                                                        &
     &  -(pb(174)*bp(3,3)+pb(176)*bp(5,3))*s2stl*apdf                   &
     &  +(pb(175)*bp(3,3)+pb(177)*bp(5,3))*c2stl*apdf                   
      endif 
      if(ww(1).ne.9898) then 
       wc(1,9)=swc(7)*wc(1,7)*pc(122)*apdfc                             &
     &  -(pc(174)*bp(4,3)+pc(176)*bp(6,3))*s2stl*apdfc                  &
     &  +(pc(175)*bp(4,3)+pc(177)*bp(6,3))*c2stl*apdfc                  
      endif 
      if(ww(2).ne.9898) then 
       wc(2,9)=-(pc(46)*bt(2,1)+pc(35)*bt(4,1)+pc(33)*bt(6,1))*apdfc    &
     & +swc(7)*wc(2,7)*pc(122)*apdfc                                    &
     & -(pc(175)*bt(4,3)+pc(177)*bt(6,3))*s2stl*apdfc                   &
     & -(pc(174)*bt(4,3)+pc(176)*bt(6,3))*c2stl*apdfc                   
      endif 
      go to 40 
   30 continue 
      if(pb(25).lt.1.e-4) pb(25)=1.e-4 
      apt=g0(ap(2)) 
      if(apt.eq.0) goto 40 
      if(ww(1).ne.9898) then 
       wb(1,9)=(pb(97)*bt(3,1)+pb(55)*bt(5,1)+pb(51)*bt(7,1))*apt       &
     &  +(pb(160)*bt(3,3)+pb(179)*bt(5,3))*s2stl*apt                    &
     &  +(pb(159)*bt(3,3)+pb(178)*bt(5,3))*c2stl*apt                    
      endif 
      if(ww(2).ne.9898) then 
       wb(2,9)=0                                                        &
     &  -(pb(159)*bp(3,3)+pb(178)*bp(5,3))*s2stl*apt                    &
     &  +(pb(160)*bp(3,3)+pb(179)*bp(5,3))*c2stl*apt                    
      endif 
      if(ww(1).ne.9898) then 
       wc(1,9)=swc(7)*wc(1,7)*pc(129)*apt                               &
     &  -(pc(159)*bp(4,3)+pc(178)*bp(6,3))*s2stl*apt                    &
     &  +(pc(160)*bp(4,3)+pc(179)*bp(6,3))*c2stl*apt                    
      endif 
      if(ww(2).ne.9898) then 
      wc(2,9)=-(pc(97)*bt(2,1)+pc(55)*bt(4,1)+pc(51)*bt(6,1))*apt       &
     & +swc(7)*wc(2,7)*pc(129)*apt                                      &
     & -(pc(160)*bt(4,3)+pc(179)*bt(6,3))*s2stl*apt                     &
     & -(pc(159)*bt(4,3)+pc(178)*bt(6,3))*c2stl*apt                     
      endif 
   40 continue 
      if(sw(10).eq.0) goto 49 
!        longitudinal                                                   
      dbasy1=1.+pb(199)*slat 
      dbasy2=1.+pb(200)*slat 
      f11b=1.+pb(81)*dfc*swc(1) 
      if(sw(11).eq.0) goto 230 
      if(ww(1).ne.9898) then 
       wb(1,11)=(pb(91)*bt(3,2)+pb(92)*bt(5,2)+pb(93)*bt(7,2))          &
     &  *slong*dbasy1*f11b                                              &
     & + (pb(65)*bt(3,2)+pb(66)*bt(5,2)+pb(67)*bt(7,2))                 &
     &  *clong*dbasy1*f11b                                              &
     &  +(pb(191)*bt(3,3)+pb(193)*bt(5,3)+pb(195)*bt(7,3)               &
     &   +pb(197)*bt(9,3)                                               &
     &  )*s2long*dbasy2*f11b                                            &
     & + (pb(192)*bt(3,3)+pb(194)*bt(5,3)+pb(196)*bt(7,3)               &
     &    +pb(198)*bt(9,3)                                              &
     &  )*c2long*dbasy2*f11b                                            
      endif 
      if(ww(2).ne.9898) then 
       wb(2,11)=-(pb(65)*bp(3,2)+pb(66)*bp(5,2)+pb(67)*bp(7,2))         &
     &  *slong*dbasy1*f11b                                              &
     & + (pb(91)*bp(3,2)+pb(92)*bp(5,2)+pb(93)*bp(7,2))                 &
     &  *clong*dbasy1*f11b                                              &
     & -(pb(192)*bp(3,3)+pb(194)*bp(5,3)+pb(196)*bp(7,3)                &
     &   +pb(198)*bp(9,3)                                               &
     &  )*s2long*dbasy2*f11b                                            &
     & + (pb(191)*bp(3,3)+pb(193)*bp(5,3)+pb(195)*bp(7,3)               &
     &    +pb(197)*bp(9,3)                                              &
     &  )*c2long*dbasy2*f11b                                            
      endif 
      dcasy1=1.+pc(199)*slat 
      dcasy2=1.+pc(200)*slat 
      f11c=1.+pc(81)*dfc*swc(1) 
      if(ww(1).ne.9898) then 
       wc(1,11)=-(pc(65)*bp(2,2)+pc(66)*bp(4,2)+pc(67)*bp(6,2)          &
     & +pc(73)*bp(8,2)+pc(74)*bp(10,2)                                  &
     &  )*slong*dcasy1*f11c                                             &
     & + (pc(91)*bp(2,2)+pc(92)*bp(4,2)+pc(93)*bp(6,2)                  &
     & +pc(87)*bp(8,2)+pc(88)*bp(10,2)                                  &
     &  )*clong*dcasy1*f11c                                             &
     &  -(pc(192)*bp(4,3)+pc(194)*bp(6,3)+pc(196)*bp(8,3)               &
     & +pc(198)*bp(10,3)                                                &
     &  )*s2long*dcasy2*f11c                                            &
     & + (pc(191)*bp(4,3)+pc(193)*bp(6,3)+pc(195)*bp(8,3)               &
     & +pc(197)*bp(10,3)                                                &
     &  )*c2long*dcasy2*f11c                                            
      endif 
      if(ww(2).ne.9898) then 
       wc(2,11)=-(pc(91)*bt(2,2)+pc(92)*bt(4,2)+pc(93)*bt(6,2)          &
     & +pc(87)*bt(8,2)+pc(88)*bt(10,2)                                  &
     &  )*slong*dcasy1*f11c                                             &
     & - (pc(65)*bt(2,2)+pc(66)*bt(4,2)+pc(67)*bt(6,2)                  &
     & +pc(73)*bt(8,2)+pc(74)*bt(10,2)                                  &
     &  )*clong*dcasy1*f11c                                             &
     &  -(pc(191)*bt(4,3)+pc(193)*bt(6,3)+pc(195)*bt(8,3)               &
     & +pc(197)*bt(10,3)                                                &
     &  )*s2long*dcasy2*f11c                                            &
     & - (pc(192)*bt(4,3)+pc(194)*bt(6,3)+pc(196)*bt(8,3)               &
     & +pc(198)*bt(10,3)                                                &
     &  )*c2long*dcasy2*f11c                                            
      endif 
  230 continue 
!       ut & mixed ut/long                                              
      utbasy=1. 
      f12b=1.+pb(82)*dfc*swc(1) 
      if(sw(12).eq.0) goto 240 
      if(ww(1).ne.9898) then 
       wb(1,12)=(pb(69)*bt(2,1)+pb(70)*bt(4,1)+pb(71)*bt(6,1)           &
     & +pb(116)*bt(8,1)+pb(117)*bt(10,1)+pb(118)*bt(12,1)               &
     &  )*cos(sr*(sec-pb(72)))*utbasy*f12b                              &
     & + (pb(77)*bt(4,3)+pb(78)*bt(6,3)+pb(79)*bt(8,3))                 &
     &  *cos(sr*(sec-pb(80))+2.*dgtr*long)*utbasy*f12b*swc(11)          
      endif 
      if(ww(2).ne.9898) then 
       wb(2,12)=(pb(77)*bp(4,3)+pb(78)*bp(6,3)+pb(79)*bp(8,3))          &
     &  *cos(sr*(sec-pb(80)+21600.)+2.*dgtr*long)                       &
     &    *utbasy*f12b*swc(11)                                          
      endif 
      utcasy=1. 
      f12c=1.+pc(82)*dfc*swc(1) 
      if(ww(1).ne.9898) then 
       wc(1,12)=(pc(77)*bp(3,3)+pc(78)*bp(5,3)+pc(79)*bp(7,3)           &
     & +pc(165)*bp(9,3)+pc(166)*bp(11,3)+pc(167)*bp(13,3)               &
     &  )*cos(sr*(sec-pc(80))+2.*dgtr*long)*utcasy*f12c*swc(11)         
      endif 
      if(ww(2).ne.9898) then 
       wc(2,12)=-(pc(69)*bt(3,1)+pc(70)*bt(5,1)+pc(71)*bt(7,1)          &
     & +pc(116)*bt(9,1)+pc(117)*bt(11,1)+pc(118)*bt(13,1)               &
     &  )*cos(sr*(sec-pc(72)))*utcasy*f12c                              &
     & + (pc(77)*bt(3,3)+pc(78)*bt(5,3)+pc(79)*bt(7,3)                  &
     & +pc(165)*bt(9,3)+pc(166)*bt(11,3)+pc(167)*bt(13,3)               &
     &  )*cos(sr*(sec-pc(80)+21600.)+2.*dgtr*long)                      &
     &   *utcasy*f12c*swc(11)                                           
      endif 
  240 continue 
!       mixed long,ut,ap                                                
      if(sw(13).eq.0) goto 48 
      if(sw9.eq.-1.) go to 45 
      if(apd.eq.0) goto 48 
      if(ww(1).ne.9898) then 
       wb(1,13)=                                                        &
     & (pb(61)*bt(3,2)+pb(62)*bt(5,2)+pb(63)*bt(7,2))                   &
     &  *cos(dgtr*(long-pb(64)))*apdf*swc(11)+                          &
     &  (pb(84)*bt(2,1)+pb(85)*bt(4,1)+pb(86)*bt(6,1))                  &
     &  *cos(sr*(sec-pb(76)))*apdf*swc(12)                              
      endif 
      if(ww(2).ne.9898) then 
       wb(2,13)=(pb(61)*bp(3,2)+pb(62)*bp(5,2)+pb(63)*bp(7,2))          &
     &  *cos(dgtr*(long-pb(64)+90.))*apdf*swc(11)                       
      endif 
      if(ww(1).ne.9898) then 
       wc(1,13)=swc(11)*wc(1,11)*pc(61)*apdfc                           &
     & +swc(12)*wc(1,12)*pc(84)*apdfc                                   
      endif 
      if(ww(2).ne.9898) then 
       wc(2,13)=swc(11)*wc(2,11)*pc(61)*apdfc                           &
     & +swc(12)*wc(2,12)*pc(84)*apdfc                                   
      endif 
      goto 48 
   45 continue 
      if(apt.eq.0) goto 48 
      if(ww(1).ne.9898) then 
       wb(1,13)=                                                        &
     &  (pb(53)*bt(3,2)+pb(99)*bt(5,2)+pb(68)*bt(7,2))                  &
     &  *cos(dgtr*(long-pb(98)))*apt*swc(11)+                           &
     &  (pb(56)*bt(2,1)+pb(57)*bt(4,1)+pb(58)*bt(6,1))                  &
     &  *cos(sr*(sec-pb(59)))*apt*swc(12)                               
      endif 
      if(ww(2).ne.9898) then 
       wb(2,13)=(pb(53)*bp(3,2)+pb(99)*bp(5,2)+pb(68)*bp(7,2))          &
     &  *cos(dgtr*(long-pb(98)+90.))*apt*swc(11)                        
      endif 
      if(ww(1).ne.9898) then 
       wc(1,13)=swc(11)*wc(1,11)*pc(53)*apt                             &
     & +swc(12)*wc(1,12)*pc(56)*apt                                     
      endif 
      if(ww(2).ne.9898) then 
       wc(2,13)=swc(11)*wc(2,11)*pc(53)*apt                             &
     & +swc(12)*wc(2,12)*pc(56)*apt                                     
      endif 
   48 continue 
   49 continue 
      wbt(1)=0 
      wbt(2)=0 
      wct(1)=0 
      wct(2)=0 
!       sum winds and change meridional sign to + north                 
      do 50 k=1,nsw 
        wbt(1)=wbt(1)-abs(sw(k))*wb(1,k) 
        wct(1)=wct(1)-abs(sw(k))*wc(1,k) 
        wbt(2)=wbt(2)+abs(sw(k))*wb(2,k) 
        wct(2)=wct(2)+abs(sw(k))*wc(2,k) 
   50 end do 
      if(ww(1).ne.9898) ww(1)=wbt(1)*sw(24)+wct(1)*sw(25) 
      if(ww(2).ne.9898) ww(2)=wbt(2)*sw(24)+wct(2)*sw(25) 
      return 
      end                                           
!-----------------------------------------------------------------------
      subroutine glbw5m(yrd,sec,lat,long,stl,f107a,f107,ap,pb,pc,ww) 
      real lat,long 
      dimension wb(2,15),wc(2,15),pb(150),pc(150),ww(2) 
      dimension ap(2) 
      common/csw/sw(25),isw,swc(25) 
      common/hwmc/wbt(2),wct(2) 
!      common/vpoly/bt(20,20),bp(20,20),cstl,sstl,c2stl,s2stl,          
!     $ c3stl,s3stl,iyr,day,df,dfa,dfc,apd,apdf,apdfc,apt,stl           
      common/vpoly2/xvl,lvl,mvl,clat,slat,bt(20,20),bp(20,20) 
      common/ltcomp/tll,nsvl,cstl,sstl,c2stl,s2stl,c3stl,s3stl 
      common/lgcomp/xll,ngvl,clong,slong,c2long,s2long 
      save 
      data dgtr/.017453/,sr/7.2722e-5/,hr/.2618/,dr/1.72142e-2/ 
      data pb14/-1./,pb18/-1./ 
      data nsw/14/,wb/30*0/,wc/30*0/ 
      data sw9/1./,lv/10/,mv/2/,nsv/2/,pset/4./ 
      g0(a)=(a-4.+(pb(26)-1.)*(a-4.+(exp(-abs(pb(25))*(a-4.))-1.)/      &
     & abs(pb(25))))                                                    
!       confirm parameter set                                           
      if(pb(100).eq.0) pb(100)=pset 
      if(pb(100).ne.pset) then 
        write(6,900) pset,pb(100),pc(100) 
  900   format(1x,'wrong parameter set for glbw5m',3f10.1) 
        stop 
      endif 
!                                                                       
      do 10 j=1,nsw 
        wb(1,j)=0 
        wb(2,j)=0 
        wc(1,j)=0 
        wc(2,j)=0 
   10 end do 
      if(sw(9).gt.0) sw9=1. 
      if(sw(9).lt.0) sw9=-1. 
      iyr = yrd/1000. 
      day = yrd - iyr*1000. 
      if(xvl.ne.lat.or.lv.gt.lvl.or.mv.gt.mvl) then 
        slat=sin(dgtr*lat) 
        clat=cos(dgtr*lat) 
        call vsphr1(slat,clat,lv,mv,bt,bp,20) 
        xvl=lat 
        lvl=lv 
        mvl=mv 
      endif 
      if(tll.ne.stl.or.nsv.gt.nsvl)  then 
        sstl = sin(hr*stl) 
        cstl = cos(hr*stl) 
        s2stl = sin(2.*hr*stl) 
        c2stl = cos(2.*hr*stl) 
        tll = stl 
        nsvl=nsv 
      endif 
      if(day.ne.dayl.or.pb(14).ne.pb14) cd14=cos(dr*(day-pb(14))) 
      if(day.ne.dayl.or.pb(18).ne.pb18) cd18=cos(2.*dr*(day-pb(18))) 
      if(day.ne.dayl.or.pb(19).ne.pb19) cd19b=cos(2.*dr*(day-pb(19))) 
      dayl=day 
      pb14=pb(14) 
      pb18=pb(18) 
      pb19=pb(19) 
!       f10.7 effect                                                    
      df=f107-f107a 
      dfa=f107a-150. 
      dfc=dfa+pb(20)*df 
!       time independent                                                
      f1b=1. 
      if(ww(1).ne.9898) then 
       wb(1,2)=(pb(2)*bt(3,1)+pb(3)*bt(5,1)+pb(23)*bt(7,1))*f1b 
      endif 
      wb(2,2)=0. 
      f1c=1. 
      wc(1,2)=0. 
      if(ww(2).ne.9898) then 
       wc(2,2)=-(pc(2)*bt(2,1)+pc(3)*bt(4,1)+pc(23)*bt(6,1))*f1c        &
     & -(pc(27)*bt(3,1)+pc(15)*bt(5,1)+pc(60)*bt(7,1))*f1c              
      endif 
!       symmetrical annual                                              
!       symmetrical semiannual                                          
      if(ww(1).ne.9898) then 
       wb(1,4)=(pb(17)*bt(3,1)+pb(31)*bt(5,1))*cd18 
      endif 
      wb(2,4)=0 
      wc(1,4)=0 
      if(ww(2).ne.9898) then 
       wc(2,4)=-(pc(17)*bt(2,1)+pc(31)*bt(4,1))*cd18 
      endif 
!       asymmetrical annual                                             
      f5b=1. 
      if(ww(1).ne.9898) then 
       wb(1,5)=(pb(10)*bt(2,1)+pb(11)*bt(4,1))*cd14*f5b 
      endif 
      wb(2,5)=0 
      f5c=1. 
      wc(1,5)=0 
      if(ww(2).ne.9898) then 
       wc(2,5)=-(pc(10)*bt(3,1)+pc(11)*bt(5,1))*cd14*f5c 
      endif 
!       asymmetrical semiannual                                         
!       diurnal                                                         
      if(sw(7).eq.0) goto 200 
      f7b=1. 
      f75b=1. 
      if(ww(1).ne.9898) then 
       wb(1,7)=(pb(7)*bt(2,2)+pb(8)*bt(4,2)+pb(29)*bt(6,2)              &
     &         +pb(89)*bt(3,2)                                          &
     &  )*sstl*f7b                                                      &
     & +(pb(13)*bt(3,2)+pb(146)*bt(5,2))                                &
     &    *cd14*sstl*f75b*swc(5)                                        &
     & + (pb(4)*bt(2,2)+pb(5)*bt(4,2)+pb(28)*bt(6,2)                    &
     &         +pb(88)*bt(3,2)                                          &
     &  )*cstl*f7b                                                      &
     & +(pb(12)*bt(3,2)+pb(145)*bt(5,2))                                &
     &      *cd14*cstl*f75b*swc(5)                                      
      endif 
      if(ww(2).ne.9898) then 
       wb(2,7)=-(pb(4)*bp(2,2)+pb(5)*bp(4,2)+pb(28)*bp(6,2)             &
     &         +pb(88)*bp(3,2)                                          &
     &  )*sstl*f7b                                                      &
     & -(pb(12)*bp(3,2)+pb(145)*bp(5,2))                                &
     &    *cd14*sstl*f75b*swc(5)                                        &
     & + (pb(7)*bp(2,2)+pb(8)*bp(4,2)+pb(29)*bp(6,2)                    &
     &         +pb(89)*bp(3,2)                                          &
     &  )*cstl*f7b                                                      &
     & +(pb(13)*bp(3,2)+pb(146)*bp(5,2))                                &
     &    *cd14*cstl*f75b*swc(5)                                        
      endif 
      f7c=1. 
      f75c=1. 
      if(ww(1).ne.9898) then 
       wc(1,7)=-(pc(4)*bp(3,2)+pc(5)*bp(5,2)+pc(28)*bp(7,2)             &
     &         +pc(88)*bp(2,2)                                          &
     &   +pc(141)*bp(9,2)+pc(143)*bp(11,2)                              &
     &  )*sstl*f7c                                                      &
     & -(pc(12)*bp(2,2)+pc(145)*bp(4,2))                                &
     &    *cd14*sstl                                                    &
     &   *f75c*swc(5)                                                   &
     & +(pc(7)*bp(3,2)+pc(8)*bp(5,2)+pc(29)*bp(7,2)                     &
     &         +pc(89)*bp(2,2)                                          &
     & +pc(142)*bp(9,2)+pc(144)*bp(11,2)                                &
     &  )*cstl*f7c                                                      &
     & +(pc(13)*bp(2,2)+pc(146)*bp(4,2))                                &
     &     *cd14*cstl                                                   &
     &   *f75c*swc(5)                                                   
      endif 
      if(ww(2).ne.9898) then 
       wc(2,7)=-(pc(7)*bt(3,2)+pc(8)*bt(5,2)+pc(29)*bt(7,2)             &
     &         +pc(89)*bt(2,2)                                          &
     & +pc(142)*bt(9,2)+pc(144)*bt(11,2)                                &
     &  )*sstl*f7c                                                      &
     & -(pc(13)*bt(2,2)+pc(146)*bt(4,2))                                &
     &    *cd14*sstl                                                    &
     &   *f75c*swc(5)                                                   &
     & -(pc(4)*bt(3,2)+pc(5)*bt(5,2)+pc(28)*bt(7,2)                     &
     &         +pc(88)*bt(2,2)                                          &
     & +pc(141)*bt(9,2)+pc(143)*bt(11,2)                                &
     &  )*cstl*f7c                                                      &
     & -(pc(12)*bt(2,2)+pc(145)*bt(4,2))                                &
     &    *cd14*cstl                                                    &
     &   *f75c*swc(5)                                                   
      endif 
  200 continue 
!       semidiurnal                                                     
      if(sw(8).eq.0) goto 210 
      f8b=1.+pb(90)*dfc*swc(1) 
      if(ww(1).ne.9898) then 
       wb(1,8)=(pb(9)*bt(3,3)+pb(43)*bt(5,3)+pb(111)*bt(7,3)            &
     &         +pb(98)*bt(4,3)                                          &
     &   +(pb(34)*bt(4,3)+pb(148)*bt(6,3))*cd14*swc(5)                  &
     &   +(pb(37)*bt(4,3))*cd19b*swc(6)                                 &
     &  )*s2stl*f8b                                                     &
     & +(pb(6)*bt(3,3)+pb(42)*bt(5,3)+pb(110)*bt(7,3)                   &
     &         +pb(96)*bt(4,3)                                          &
     &   +(pb(24)*bt(4,3)+pb(147)*bt(6,3))*cd14*swc(5)                  &
     &   +(pb(36)*bt(4,3))*cd19b*swc(6)                                 &
     &  )*c2stl*f8b                                                     
      endif 
      if(ww(2).ne.9898) then 
       wb(2,8)=-(pb(6)*bp(3,3)+pb(42)*bp(5,3)+pb(110)*bp(7,3)           &
     &          +pb(96)*bp(4,3)                                         &
     &   +(pb(24)*bp(4,3)+pb(147)*bp(6,3))*cd14*swc(5)                  &
     &   +(pb(36)*bp(4,3))*cd19b*swc(6)                                 &
     &  )*s2stl*f8b                                                     &
     &   + (pb(9)*bp(3,3)+pb(43)*bp(5,3)+pb(111)*bp(7,3)                &
     &          +pb(98)*bp(4,3)                                         &
     &   +(pb(34)*bp(4,3)+pb(148)*bp(6,3))*cd14*swc(5)                  &
     &   +(pb(37)*bp(4,3))*cd19b*swc(6)                                 &
     &  )*c2stl*f8b                                                     
      endif 
      f8c=1.+pc(90)*dfc*swc(1) 
      if(ww(1).ne.9898) then 
       wc(1,8)=-(pc(6)*bp(4,3)+pc(42)*bp(6,3)+pc(110)*bp(8,3)           &
     &          +pc(96)*bp(3,3)                                         &
     &   +(pc(24)*bp(3,3)+pc(147)*bp(5,3))*cd14*swc(5)                  &
     &   +(pc(36)*bp(3,3))*cd19b*swc(6)                                 &
     &  )*s2stl*f8c                                                     &
     & +(pc(9)*bp(4,3)+pc(43)*bp(6,3)+pc(111)*bp(8,3)                   &
     &          +pc(98)*bp(3,3)                                         &
     &   +(pc(34)*bp(3,3)+pc(148)*bp(5,3))*cd14*swc(5)                  &
     &   +(pc(37)*bp(3,3))*cd19b*swc(6)                                 &
     &  )*c2stl*f8c                                                     
      endif 
      if(ww(2).ne.9898) then 
       wc(2,8)=-(pc(9)*bt(4,3)+pc(43)*bt(6,3)+pc(111)*bt(8,3)           &
     &          +pc(98)*bt(3,3)                                         &
     &   +(pc(34)*bt(3,3)+pc(148)*bt(5,3))*cd14*swc(5)                  &
     &   +(pc(37)*bt(3,3))*cd19b*swc(6)                                 &
     &  )*s2stl*f8c                                                     &
     & - (pc(6)*bt(4,3)+pc(42)*bt(6,3)                                  &
     &          +pc(96)*bt(3,3)                                         &
     &   +pc(110)*bt(8,3)                                               &
     &   +(pc(24)*bt(3,3)+pc(147)*bt(5,3))*cd14*swc(5)                  &
     &   +(pc(36)*bt(3,3))*cd19b*swc(6)                                 &
     &  )*c2stl*f8c                                                     
      endif 
  210 continue 
!        terdiurnal                                                     
!        magnetic activity                                              
      if(sw(9).eq.0) goto 40 
      if(sw9.eq.-1.) goto 30 
!           daily ap                                                    
      apd=ap(1)-4. 
      apdf=(apd+(pb(45)-1.)*(apd+(exp(-pb(44)*apd)-1.)/pb(44))) 
!      apdfc=(apd+(pc(45)-1.)*(apd+(exp(-pc(44)*apd)-1.)/pc(44)))       
      apdfc=apdf 
      if(apd.eq.0) goto 40 
      if(ww(1).ne.9898) then 
       wb(1,9)=(pb(46)*bt(3,1)+pb(35)*bt(5,1))*apdf                     &
     &    +(pb(122)*bt(2,2)+pb(123)*bt(4,2)+pb(124)*bt(6,2)             &
     &       )*cos(hr*(stl-pb(125)))*apdf*swc(7)                        
      endif 
      if(ww(2).ne.9898) then 
       wb(2,9)=                                                         &
     &   (pb(122)*bp(2,2)+pb(123)*bp(4,2)+pb(124)*bp(6,2)               &
     &     )*cos(hr*(stl-pb(125)+6.))*apdf*swc(7)                       
      endif 
      if(ww(1).ne.9898) then 
       wc(1,9)=                                                         &
     &   (pc(122)*bp(3,2)+pc(123)*bp(5,2)+pc(124)*bp(7,2)               &
     &       )*cos(hr*(stl-pc(125)))*apdfc*swc(7)                       
      endif 
      if(ww(2).ne.9898) then 
       wc(2,9)=-(pc(46)*bt(2,1)+pc(35)*bt(4,1))*apdfc                   &
     &  +(pc(122)*bt(3,2)+pc(123)*bt(5,2)+pc(124)*bt(7,2)               &
     &       )*cos(hr*(stl-pc(125)+6.))*apdfc*swc(7)                    
      endif 
      go to 40 
   30 continue 
      if(pb(25).lt.1.e-4) pb(25)=1.e-4 
      apt=g0(ap(2)) 
      if(apt.eq.0) goto 40 
      if(ww(1).ne.9898) then 
       wb(1,9)=(pb(97)*bt(3,1)+pb(55)*bt(5,1))*apt                      &
     &    +(pb(129)*bt(2,2)+pb(130)*bt(4,2)+pb(131)*bt(6,2)             &
     &       )*cos(hr*(stl-pb(132)))*apt*swc(7)                         
      endif 
      if(ww(2).ne.9898) then 
       wb(2,9)=                                                         &
     &   (pb(129)*bp(2,2)+pb(130)*bp(4,2)+pb(131)*bp(6,2)               &
     &     )*cos(hr*(stl-pb(132)+6.))*apt*swc(7)                        
      endif 
      if(ww(1).ne.9898) then 
       wc(1,9)=                                                         &
     &   (pc(129)*bp(3,2)+pc(130)*bp(5,2)+pc(131)*bp(7,2)               &
     &       )*cos(hr*(stl-pc(132)))*apt*swc(7)                         
      endif 
      if(ww(2).ne.9898) then 
       wc(2,9)=-(pc(97)*bt(2,1)+pc(55)*bt(4,1))*apt                     &
     &  +(pc(129)*bt(3,2)+pc(130)*bt(5,2)+pc(131)*bt(7,2)               &
     &       )*cos(hr*(stl-pc(132)+6.))*apt*swc(7)                      
      endif 
   40 continue 
      wbt(1)=0 
      wbt(2)=0 
      wct(1)=0 
      wct(2)=0 
!       sum winds and change meridional sign to + north                 
      do 50 k=1,nsw 
        wbt(1)=wbt(1)-abs(sw(k))*wb(1,k) 
        wct(1)=wct(1)-abs(sw(k))*wc(1,k) 
        wbt(2)=wbt(2)+abs(sw(k))*wb(2,k) 
        wct(2)=wct(2)+abs(sw(k))*wc(2,k) 
   50 end do 
      if(ww(1).ne.9898) ww(1)=wbt(1)*sw(24)+wct(1)*sw(25) 
      if(ww(2).ne.9898) ww(2)=wbt(2)*sw(24)+wct(2)*sw(25) 
      return 
      end                                           
!-----------------------------------------------------------------------
      subroutine glbw5s(iyd,lat,long,stl,pb,pc,ww) 
      real lat,long 
      dimension wb(2,15),wc(2,15),pb(100),pc(100),ww(2) 
      common/csw/sw(25),isw,swc(25) 
      common/hwmc/wbt(2),wct(2) 
      common/vpoly2/xvl,lvl,mvl,clat,slat,bt(20,20),bp(20,20) 
      common/ltcomp/tll,nsvl,cstl,sstl,c2stl,s2stl,c3stl,s3stl 
      common/lgcomp/xll,ngvl,clong,slong,c2long,s2long 
      save 
      data dgtr/.017453/,sr/7.2722e-5/,hr/.2618/,dr/1.72142e-2/ 
      data pb14/-1./,pb18/-1./,pc14/-1./,pc18/-1./,pset/5./ 
      data nsw/14/,wb/30*0/,wc/30*0/ 
!       confirm parameter set                                           
      if(pb(100).eq.0) pb(100)=pset 
      if(pb(100).ne.pset) then 
        write(6,900) pset,pb(100),pc(100) 
  900   format(1x,'wrong parameter set for glbw5s',3f10.1) 
        stop 
      endif 
!                                                                       
      do 10 j=1,nsw 
        wb(1,j)=0 
        wb(2,j)=0 
        wc(1,j)=0 
        wc(2,j)=0 
   10 end do 
      iyr = iyd/1000 
      day = iyd - iyr*1000 
!                                                                       
      lv=7 
      mv=2 
      if(xvl.ne.lat.or.lv.gt.lvl.or.mv.gt.mvl) then 
        slat=sin(dgtr*lat) 
        clat=cos(dgtr*lat) 
        call vsphr1(slat,clat,lv,mv,bt,bp,20) 
        plg10=slat 
        plg30=(5.*slat*slat-3.)*slat/2. 
        xvl=lat 
        lvl=lv 
        mvl=mv 
      endif 
!                                                                       
      nsv=2 
      if(tll.ne.stl.or.nsv.gt.nsvl)  then 
        sstl = sin(hr*stl) 
        cstl = cos(hr*stl) 
        s2stl = sin(2.*hr*stl) 
        c2stl = cos(2.*hr*stl) 
        tll = stl 
        nsvl=nsv 
      endif 
      if(day.ne.dayl.or.pb(14).ne.pb14) cd14b=cos(dr*(day-pb(14))) 
      if(day.ne.dayl.or.pc(14).ne.pc14) cd14c=cos(dr*(day-pc(14))) 
      if(day.ne.dayl.or.pb(18).ne.pb18) cd18b=cos(2.*dr*(day-pb(18))) 
      if(day.ne.dayl.or.pc(18).ne.pc18) cd18c=cos(2.*dr*(day-pc(18))) 
      if(day.ne.dayl.or.pb(19).ne.pb19) cd19b=cos(2.*dr*(day-pb(19))) 
      if(day.ne.dayl.or.pb(25).ne.pb25) cd25b=cos(dr*(day-pb(25))) 
!      if(day.ne.dayl.or.pc(25).ne.pc25) cd25c=cos(dr*(day-pc(25)))     
      if(day.ne.dayl.or.pb(26).ne.pb26) cd26b=cos(dr*(day-pb(26))) 
!      if(day.ne.dayl.or.pc(26).ne.pc26) cd26c=cos(dr*(day-pc(26)))     
      if(day.ne.dayl.or.pc(32).ne.pc32) cd32c=cos(dr*(day-pc(32))) 
      if(day.ne.dayl.or.pc(39).ne.pc39) cd39c=cos(2.*dr*(day-pc(39))) 
      if(day.ne.dayl.or.pc(64).ne.pc64) cd64c=cos(dr*(day-pc(64))) 
      if(day.ne.dayl.or.pc(87).ne.pc87) cd87c=cos(2.*dr*(day-pc(87))) 
      dayl=day 
      pb14=pb(14) 
      pc14=pc(14) 
      pb18=pb(18) 
      pc18=pc(18) 
      pb19=pb(19) 
      pb25=pb(25) 
      pc25=pc(25) 
      pb26=pb(26) 
      pc26=pc(26) 
      pc32=pc(32) 
      pc39=pc(39) 
      pc64=pc(64) 
      pc87=pc(87) 
!                                                                       
      ngv=1 
!JFM  if(xll.ne.long.or.ngv.gt.ngvl) then 
        slong=sin(dgtr*long) 
        clong=cos(dgtr*long) 
        xll=long 
        ngvl=ngv 
!JFM  endif 
!       time independent                                                
      f1b=1. 
      if(ww(1).ne.9898) then 
       wb(1,2)=(pb(2)*bt(3,1)+pb(3)*bt(5,1)+pb(23)*bt(7,1))*f1b 
      endif 
      wb(2,2)=0. 
      f1c=1. 
      wc(1,2)=0. 
      if(ww(2).ne.9898) then 
       wc(2,2)=-(pc(2)*bt(2,1)+pc(3)*bt(4,1)+pc(23)*bt(6,1))*f1c        &
     & -(pc(27)*bt(3,1)+pc(15)*bt(5,1)+pc(60)*bt(7,1))*f1c              
      endif 
!       symmetrical annual                                              
      if(ww(2).ne.9898) then 
       wc(2,3)=-(pc(48)*bt(2,1)+pc(30)*bt(4,1))*cd32c 
      endif 
!       symmetrical semiannual                                          
      if(ww(1).ne.9898) then 
       wb(1,4)=(pb(17)*bt(3,1)+pb(31)*bt(5,1))*cd18b 
      endif 
      wb(2,4)=0 
      wc(1,4)=0 
      if(ww(2).ne.9898) then 
       wc(2,4)=-(pc(17)*bt(2,1)+pc(31)*bt(4,1)+pc(50)*bt(6,1))*cd18c 
      endif 
!       asymmetrical annual                                             
      f5b=1. 
      if(ww(1).ne.9898) then 
       wb(1,5)=(pb(10)*bt(2,1)+pb(11)*bt(4,1))*cd14b*f5b 
      endif 
      wb(2,5)=0 
      f5c=1. 
      wc(1,5)=0 
      if(ww(2).ne.9898) then 
       wc(2,5)=-(pc(10)*bt(3,1)+pc(11)*bt(5,1)+pc(21)*bt(7,1))*cd14c*f5c 
      endif 
!       asymmetrical semiannual                                         
      if(ww(2).ne.9898) then 
       wc(2,6)=-(pc(38)*bt(3,1)+pc(99)*bt(5,1))*cd39c 
      endif 
!       diurnal                                                         
      if(sw(7).eq.0) goto 200 
      f7b=1. 
      f75b=1. 
      if(ww(1).ne.9898) then 
       wb(1,7)=(pb(7)*bt(2,2)+pb(8)*bt(4,2)+pb(29)*bt(6,2)              &
     &         +pb(89)*bt(3,2)                                          &
     &  )*sstl*f7b                                                      &
     & +(pb(13)*bt(3,2))                                                &
     &    *cd25b*sstl*f75b*swc(5)                                       &
     & + (pb(4)*bt(2,2)+pb(5)*bt(4,2)+pb(28)*bt(6,2)                    &
     &         +pb(88)*bt(3,2)                                          &
     &  )*cstl*f7b                                                      &
     & +(pb(12)*bt(3,2))                                                &
     &      *cd25b*cstl*f75b*swc(5)                                     
      endif 
      if(ww(2).ne.9898) then 
       wb(2,7)=-(pb(4)*bp(2,2)+pb(5)*bp(4,2)+pb(28)*bp(6,2)             &
     &         +pb(88)*bp(3,2)                                          &
     &  )*sstl*f7b                                                      &
     & -(pb(12)*bp(3,2))                                                &
     &    *cd25b*sstl*f75b*swc(5)                                       &
     & + (pb(7)*bp(2,2)+pb(8)*bp(4,2)+pb(29)*bp(6,2)                    &
     &         +pb(89)*bp(3,2)                                          &
     &  )*cstl*f7b                                                      &
     & +(pb(13)*bp(3,2))                                                &
     &    *cd25b*cstl*f75b*swc(5)                                       
      endif 
      f7c=1. 
      f75c=1. 
      if(ww(1).ne.9898) then 
       wc(1,7)=-(pc(4)*bp(3,2)+pc(5)*bp(5,2)+pc(28)*bp(7,2)             &
     &         +pc(88)*bp(2,2)                                          &
     &  )*sstl*f7c                                                      &
     & -(pc(12)*bp(2,2))                                                &
     &    *cd25b*sstl                                                   &
     &   *f75c*swc(5)                                                   &
     & +(pc(7)*bp(3,2)+pc(8)*bp(5,2)+pc(29)*bp(7,2)                     &
     &         +pc(89)*bp(2,2)                                          &
     &  )*cstl*f7c                                                      &
     & +(pc(13)*bp(2,2))                                                &
     &     *cd25b*cstl                                                  &
     &   *f75c*swc(5)                                                   
      endif 
      if(ww(2).ne.9898) then 
       wc(2,7)=-(pc(7)*bt(3,2)+pc(8)*bt(5,2)+pc(29)*bt(7,2)             &
     &         +pc(89)*bt(2,2)                                          &
     &  )*sstl*f7c                                                      &
     & -(pc(13)*bt(2,2))                                                &
     &    *cd25b*sstl                                                   &
     &   *f75c*swc(5)                                                   &
     & -(pc(4)*bt(3,2)+pc(5)*bt(5,2)+pc(28)*bt(7,2)                     &
     &         +pc(88)*bt(2,2)                                          &
     &  )*cstl*f7c                                                      &
     & -(pc(12)*bt(2,2))                                                &
     &    *cd25b*cstl                                                   &
     &   *f75c*swc(5)                                                   
      endif 
  200 continue 
!       semidiurnal                                                     
      if(sw(8).eq.0) goto 210 
      f8b=1. 
      if(ww(1).ne.9898) then 
       wb(1,8)=(pb(9)*bt(3,3)+pb(43)*bt(5,3)+pb(35)*bt(7,3)             &
     &         +pb(98)*bt(4,3)                                          &
     &   +(pb(34)*bt(4,3))*cd26b*swc(5)                                 &
     &   +(pb(37)*bt(4,3))*cd19b*swc(6)                                 &
     &  )*s2stl*f8b                                                     &
     & +(pb(6)*bt(3,3)+pb(42)*bt(5,3)+pb(33)*bt(7,3)                    &
     &         +pb(96)*bt(4,3)                                          &
     &   +(pb(24)*bt(4,3))*cd26b*swc(5)                                 &
     &   +(pb(36)*bt(4,3))*cd19b*swc(6)                                 &
     &  )*c2stl*f8b                                                     
      endif 
      if(ww(2).ne.9898) then 
       wb(2,8)=-(pb(6)*bp(3,3)+pb(42)*bp(5,3)+pb(33)*bp(7,3)            &
     &          +pb(96)*bp(4,3)                                         &
     &   +(pb(24)*bp(4,3))*cd26b*swc(5)                                 &
     &   +(pb(36)*bp(4,3))*cd19b*swc(6)                                 &
     &  )*s2stl*f8b                                                     &
     &   + (pb(9)*bp(3,3)+pb(43)*bp(5,3)+pb(35)*bp(7,3)                 &
     &          +pb(98)*bp(4,3)                                         &
     &   +(pb(34)*bp(4,3))*cd26b*swc(5)                                 &
     &   +(pb(37)*bp(4,3))*cd19b*swc(6)                                 &
     &  )*c2stl*f8b                                                     
      endif 
      f8c=1. 
      if(ww(1).ne.9898) then 
       wc(1,8)=-(pc(6)*bp(4,3)+pc(42)*bp(6,3)+pc(33)*bp(8,3)            &
     &          +pc(96)*bp(3,3)                                         &
     &   +(pc(24)*bp(3,3))*cd26b*swc(5)                                 &
     &   +(pc(36)*bp(3,3))*cd19b*swc(6)                                 &
     &  )*s2stl*f8c                                                     &
     & +(pc(9)*bp(4,3)+pc(43)*bp(6,3)+pc(35)*bp(8,3)                    &
     &          +pc(98)*bp(3,3)                                         &
     &   +(pc(34)*bp(3,3))*cd26b*swc(5)                                 &
     &   +(pc(37)*bp(3,3))*cd19b*swc(6)                                 &
     &  )*c2stl*f8c                                                     
      endif 
      if(ww(2).ne.9898) then 
       wc(2,8)=-(pc(9)*bt(4,3)+pc(43)*bt(6,3)+pc(35)*bt(8,3)            &
     &          +pc(98)*bt(3,3)                                         &
     &   +(pc(34)*bt(3,3))*cd26b*swc(5)                                 &
     &   +(pc(37)*bt(3,3))*cd19b*swc(6)                                 &
     &  )*s2stl*f8c                                                     &
     & - (pc(6)*bt(4,3)+pc(42)*bt(6,3)+pc(33)*bt(8,3)                   &
     &          +pc(96)*bt(3,3)                                         &
     &   +(pc(24)*bt(3,3))*cd26b*swc(5)                                 &
     &   +(pc(36)*bt(3,3))*cd19b*swc(6)                                 &
     &  )*c2stl*f8c                                                     
      endif 
  210 continue 
!        longitudinal                                                   
      if(sw(10).eq.0.or.sw(11).eq.0) goto 230 
      if(ww(1).ne.9898) then 
       wc(1,11)=                                                        &
     & - (pc(65)*bp(2,2)+pc(66)*bp(4,2)+pc(67)*bp(6,2)                  &
     &   +pc(75)*bp(3,2)+pc(76)*bp(5,2)+ pc(77)*bp(7,2)                 &
     &   +(pc(57)*bp(2,2)+pc(59)*bp(4,2)+pc(62)*bp(6,2)                 &
     &    +pc(51)*bp(3,2)+pc(53)*bp(5,2)+pc(55)*bp(7,2))                &
     &     *cd64c*swc(3)                                                &
     &   +(pc(74)*bp(2,2)+pc(82)*bp(4,2)+pc(85)*bp(6,2)                 &
     &    +pc(68)*bp(3,2)+pc(70)*bp(5,2)+pc(72)*bp(7,2))                &
     &     *cd87c*swc(4)                                                &
     &  )*slong                                                         &
     & + (pc(91)*bp(2,2)+pc(92)*bp(4,2)+pc(93)*bp(6,2)                  &
     &   +pc(78)*bp(3,2)+pc(79)*bp(5,2)+pc(80)*bp(7,2)                  &
     &   +(pc(58)*bp(2,2)+pc(61)*bp(4,2)+pc(63)*bp(6,2)                 &
     &    +pc(52)*bp(3,2)+pc(54)*bp(5,2)+pc(56)*bp(7,2))                &
     &     *cd64c*swc(3)                                                &
     &   +(pc(81)*bp(2,2)+pc(84)*bp(4,2)+pc(86)*bp(6,2)                 &
     &    +pc(69)*bp(3,2)+pc(71)*bp(5,2)+pc(73)*bp(7,2))                &
     &     *cd87c*swc(4)                                                &
     &  )*clong                                                         
      endif 
      if(ww(2).ne.9898) then 
       wc(2,11)=                                                        &
     & - (pc(91)*bt(2,2)+pc(92)*bt(4,2)+pc(93)*bt(6,2)                  &
     &   +pc(78)*bt(3,2)+pc(79)*bt(5,2)+pc(80)*bt(7,2)                  &
     &   +(pc(58)*bt(2,2)+pc(61)*bt(4,2)+pc(63)*bt(6,2)                 &
     &    +pc(52)*bt(3,2)+pc(54)*bt(5,2)+pc(56)*bt(7,2))                &
     &     *cd64c*swc(3)                                                &
     &   +(pc(81)*bt(2,2)+pc(84)*bt(4,2)+pc(86)*bt(6,2)                 &
     &    +pc(69)*bt(3,2)+pc(71)*bt(5,2)+pc(73)*bt(7,2))                &
     &     *cd87c*swc(4)                                                &
     &  )*slong                                                         &
     & - (pc(65)*bt(2,2)+pc(66)*bt(4,2)+pc(67)*bt(6,2)                  &
     &   +pc(75)*bt(3,2)+pc(76)*bt(5,2)+pc(77)*bt(7,2)                  &
     &   +(pc(57)*bt(2,2)+pc(59)*bt(4,2)+pc(62)*bt(6,2)                 &
     &    +pc(51)*bt(3,2)+pc(53)*bt(5,2)+pc(55)*bt(7,2))                &
     &     *cd64c*swc(3)                                                &
     &   +(pc(74)*bt(2,2)+pc(82)*bt(4,2)+pc(85)*bt(6,2)                 &
     &    +pc(68)*bt(3,2)+pc(70)*bt(5,2)+pc(72)*bt(7,2))                &
     &     *cd87c*swc(4)                                                &
     &  )*clong                                                         
      endif 
  230 continue 
      wbt(1)=0 
      wbt(2)=0 
      wct(1)=0 
      wct(2)=0 
!       sum winds and change meridional sign to + north                 
      do 50 k=1,nsw 
        wbt(1)=wbt(1)-abs(sw(k))*wb(1,k) 
        wct(1)=wct(1)-abs(sw(k))*wc(1,k) 
        wbt(2)=wbt(2)+abs(sw(k))*wb(2,k) 
        wct(2)=wct(2)+abs(sw(k))*wc(2,k) 
   50 end do 
      if(ww(1).ne.9898) ww(1)=wbt(1)*sw(24)+wct(1)*sw(25) 
      if(ww(2).ne.9898) ww(2)=wbt(2)*sw(24)+wct(2)*sw(25) 
      return 
      end                                           
!-----------------------------------------------------------------------
      subroutine tselec_hwm(sv) 
!        set switches                                                   
!        sw for main terms, swc for cross terms                         
      dimension sv(1),sav(25),svv(1) 
      common/csw/sw(25),isw,swc(25) 
      do 100 i = 1,25 
        sav(i)=sv(i) 
        sw(i)=amod(sv(i),2.) 
        if(abs(sv(i)).eq.1.or.abs(sv(i)).eq.2.) then 
          swc(i)=1. 
        else 
          swc(i)=0. 
        endif 
  100 end do 
      isw=64999 
      return 
      entry tretrv_hwm(svv) 
      do 200 i=1,25 
        svv(i)=sav(i) 
  200 end do 
      end                                           
!-----------------------------------------------------------------------
      subroutine vsphr1(c,s,l,m,bt,bp,lmax) 
!      calculate vector spherical harmonic b field theta and phi        
!      functions bt,bp through order l,m for colatitude (theta)         
!      with cosine c and sine s of colatitude                           
!      bt(l+1,m+1)= [(l-m+1) p(l+1,m) - (l+1) p(l,m) cos(theta)] /      
!                [sqrt(l(l+1)) sin(theta)]                              
!      bp(l+1,m+1)= m p(l,m) /[sqrt(l(l+1)) sin(theta)]                 
!       result for given l,m saved in bt and bp at one higher index num 
      dimension bt(lmax,1),bp(lmax,1),plg(20,20) 
      save 
      data dgtr/1.74533e-2/ 
      if(m.gt.l.or.l.gt.lmax-1) then 
        write(6,100) l,m,lmax 
  100   format('illegal indicies to vspher',3i6) 
        return 
      endif 
      bt(1,1)=0 
      bp(1,1)=0 
      if(l.eq.0.and.m.eq.0) return 
      call legpl1(c,s,l+1,m,plg,20) 
      if(abs(s).lt.1.e-5) then 
        ic=sign(1.,s) 
        s=0 
      endif 
      do 20 ll=1,l 
        sqt=sqrt(float(ll)*(float(ll)+1)) 
        lmx=min(ll,m) 
        do 15 mm=0,lmx 
          if(s.eq.0) then 
            if(mm.ne.1) then 
              bt(ll+1,mm+1)=0 
              bp(ll+1,mm+1)=0 
            else 
              bt(ll+1,mm+1)=(ll*(ll+1)*(ll+2)*.5*(ic)**(ll+2)           &
     &           -(ll+1)*c*ll*(ll+1)*.5*(ic)**(ll+1))/sqt               
              bp(ll+1,mm+1)=mm*ll*(ll+1)*.5*(ic)**(ll+1)/sqt 
            endif 
          else 
            bt(ll+1,mm+1)=((ll-mm+1)*plg(ll+2,mm+1)                     &
     &      -(ll+1)*c*plg(ll+1,mm+1))/(s*sqt)                           
            bp(ll+1,mm+1)=mm*plg(ll+1,mm+1)/(s*sqt) 
          endif 
   15   continue 
   20 end do 
      end                                           
!-----------------------------------------------------------------------
      subroutine legpl1(c,s,l,m,plg,lmax) 
!      calculate legendre polynomials plg(l+1,m+1) through order l,m    
!      for cosine c and sine s of colatitude                            
      dimension plg(lmax,1) 
      save 
      data dgtr/1.74533e-2/ 
      if(m.gt.l.or.l.gt.lmax-1) then 
        write(6,99) l,m,lmax 
   99 format(1x,'illegal indicies to legpol',3i5) 
        return 
      endif 
      plg(1,1)=1. 
      if(l.eq.0.and.m.eq.0) return 
!      calculate l=m case and l=m+1                                     
      do 10 mm=0,m 
        if(mm.gt.0) plg(mm+1,mm+1)=plg(mm,mm)*(2.*mm-1.)*s 
        if(l.gt.mm) plg(mm+2,mm+1)=plg(mm+1,mm+1)*(2.*mm+1)*c 
   10 end do 
      if(l.eq.1) return 
      mmx=min(m,l-2) 
      do 30 mm=0,mmx 
        do 20 ll=mm+2,l 
          plg(ll+1,mm+1)=((2.*ll-1.)*c*plg(ll,mm+1)-                    &
     &     (ll+mm-1.)*plg(ll-1,mm+1))/(ll-mm)                           
   20   continue 
   30 end do 
      return 
      end                                           
!-----------------------------------------------------------------------
      subroutine spline_hwm(x,y,n,yp1,ypn,y2) 
!        calculate 2nd derivatives of cubic spline interp function      
!        x,y: arrays of tabulated function in ascending order by x      
!        n: size of arrays x,y                                          
!        yp1,ypn: specified derivatives at x(1) and x(n); values        
!                 >= 1e30 signal signal second derivative zero          
!        y2: output array of second derivatives                         
      parameter (nmax=100) 
      dimension x(n),y(n),y2(n),u(nmax) 
      save 
      if(yp1.gt..99e30) then 
        y2(1)=0 
        u(1)=0 
      else 
        y2(1)=-.5 
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1) 
      endif 
      do 11 i=2,n-1 
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1)) 
        p=sig*y2(i-1)+2. 
        y2(i)=(sig-1.)/p 
        u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))             &
     &    /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p                 
   11 end do 
      if(ypn.gt..99e30) then 
        qn=0 
        un=0 
      else 
        qn=.5 
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1))) 
      endif 
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.) 
      do 12 k=n-1,1,-1 
        y2(k)=y2(k)*y2(k+1)+u(k) 
   12 end do 
      return 
      end                                           
!-----------------------------------------------------------------------
      subroutine splint_hwm(xa,ya,y2a,n,x,y) 
!        calculate cubic spline interp value                            
!        xa,ya: arrays of tabulated function in ascending order by x    
!        y2a: array of second derivatives                               
!        n: size of arrays xa,ya,y2a                                    
!        x: abscissa for interpolation                                  
!        y: output value                                                
      dimension xa(n),ya(n),y2a(n) 
      save 
      klo=1 
      khi=n 
    1 continue 
      if(khi-klo.gt.1) then 
        k=(khi+klo)/2 
        if(xa(k).gt.x) then 
          khi=k 
        else 
          klo=k 
        endif 
        goto 1 
      endif 
      h=xa(khi)-xa(klo) 
      if(h.eq.0) write(6,*) 'bad xa input to splint_hwm' 
      a=(xa(khi)-x)/h 
      b=(x-xa(klo))/h 
      y=a*ya(klo)+b*ya(khi)+                                            &
     &  ((a*a*a-a)*y2a(klo)+(b*b*b-b)*y2a(khi))*h*h/6.                  
      return 
      end                                           
!-----------------------------------------------------------------------
      block data initw5 
!       for wind model gws                                              
      common/csw/sw(25),isw,swc(25) 
      common/vpoly2/xvl,lvl,mvl,clat,slat,bt(20,20),bp(20,20) 
      common/ltcomp/tll,nsvl,cstl,sstl,c2stl,s2stl,c3stl,s3stl 
      common/lgcomp/xll,ngvl,clong,slong,c2long,s2long 
      data isw/0/ 
      data xvl/-999./,lvl/-1/,mvl/-1/ 
      data tll/-999./,nsvl/-1/ 
      data xll/-999./,ngvl/-1/ 
      end                                           
!-----------------------------------------------------------------------
      block data gwsbk5 
!          hwm93    28-jan-93                                           
      common/parmw5/pba1(50),pba2(50),pba3(50),pba4(50),                &
     &pca1(50),pca2(50),pca3(50),pca4(50),                              &
     &pbb1(50),pbb2(50),pbb3(50),pcb1(50),pcb2(50),pcb3(50),            &
     &pbc1(50),pbc2(50),pbc3(50),pcc1(50),pcc2(50),pcc3(50),            &
     &pbd1(50),pbd2(50),pbd3(50),pcd1(50),pcd2(50),pcd3(50),            &
     &pbe1(50),pbe2(50),pbe3(50),pce1(50),pce2(50),pce3(50),            &
     &pbf1(50),pbf2(50),pbf3(50),pcf1(50),pcf2(50),pcf3(50),            &
     &pbg1(50),pbg2(50),pbg3(50),pcg1(50),pcg2(50),pcg3(50),            &
     &pbh1(50),pbh2(50),pbh3(50),pch1(50),pch2(50),pch3(50),            &
     &pbi1(50),pbi2(50),pci1(50),pci2(50),pbj1(50),pbj2(50),            &
     &pcj1(50),pcj2(50),pbk1(50),pbk2(50),pck1(50),pck2(50),            &
     &pbl1(50),pbl2(50),pcl1(50),pcl2(50),pbm1(50),pbm2(50),            &
     &pcm1(50),pcm2(50),pbn1(50),pbn2(50),pcn1(50),pcn2(50),            &
     &pbo1(50),pbo2(50),pco1(50),pco2(50),pbp1(50),pbp2(50),            &
     &pcp1(50),pcp2(50),pbq1(50),pbq2(50),pcq1(50),pcq2(50),            &
     &pbr1(50),pbr2(50),pcr1(50),pcr2(50),pbs1(50),pbs2(50),            &
     &pcs1(50),pcs2(50),pbt1(50),pbt2(50),pct1(50),pct2(50),            &
     &pbu1(50),pbu2(50),pcu1(50),pcu2(50)                               
      common/datw/isdate(3),istime(2),name(2) 
!
!nm101910:      data isdate/'28-j','an-9','3   '/,istime/'20:3','5:39'/ 
      character(len=4):: isdate=(/'28-j','an-9','3   '/)
      character(len=4):: istime=(/'20:3','5:39'/)
!nm101910:      data name/'hwm9','3   '/ 
      character(len=4):: name=(/'hwm9','3   '/)
!
!         winf                                                          
      data pba1/                                                        &
     &  0.00000e+00,-1.31640e+01,-1.52352e+01, 1.00718e+02, 3.94962e+00,&
     &  2.19452e-01, 8.03296e+01,-1.02032e+00,-2.02149e-01, 5.67263e+01,&
     &  0.00000e+00,-6.05459e+00, 6.68106e+00,-8.49486e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 8.39399e+01, 0.00000e+00, 9.96285e-02,&
     &  0.00000e+00,-2.66243e-02, 0.00000e+00,-1.32373e+00, 1.39396e-02,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 3.36523e+01,-7.42795e-01,-3.89352e+00,-7.81354e-01,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 3.76631e+00,-1.22024e+00,&
     & -5.47580e-01, 1.09146e+00, 9.06245e-01, 2.21119e-02, 0.00000e+00,&
     &  7.73919e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pba2/                                                        &
     & -3.82415e-01, 0.00000e+00, 1.76202e-01, 0.00000e+00,-6.77651e-01,&
     &  1.10357e+00, 2.25732e+00, 0.00000e+00, 1.54237e+04, 0.00000e+00,&
     &  1.27411e-01,-2.84314e-03, 4.62562e-01,-5.34596e+01,-7.23808e+00,&
     &  0.00000e+00, 0.00000e+00, 4.52770e-01,-8.50922e+00,-2.85389e-01,&
     &  2.12000e+01, 6.80171e+02, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     & -2.72552e+04, 0.00000e+00, 0.00000e+00, 0.00000e+00, 2.64109e+03,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-1.47320e+00,-2.98179e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 1.05412e-02,&
     &  4.93452e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 7.98332e-02,-5.30954e+01, 2.10211e-02, 3.00000e+00/
      data pba3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,-2.79843e-01,&
     &  1.81152e-01, 0.00000e+00, 0.00000e+00,-6.24673e-02,-5.37589e-02,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-8.94418e-02, 3.70413e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,-4.84645e+00,&
     &  4.24178e-01, 0.00000e+00, 0.00000e+00, 1.86494e-01,-9.56931e-02/
      data pba4/                                                        &
     &  2.08426e+00, 1.53714e+00,-2.87496e-01, 4.06380e-01,-3.59788e-01,&
     & -1.87814e-01, 0.00000e+00, 0.00000e+00, 2.01362e-01,-1.21604e-01,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 7.86304e+00,&
     &  2.51878e+00, 2.91455e+00, 4.32308e+00, 6.77054e-02,-2.39125e-01,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  1.57976e+00,-5.44598e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     & -5.30593e-01,-5.02237e-01,-2.05258e-01, 2.62263e-01,-2.50195e-01,&
     &  4.28151e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!         winf                                                          
      data pca1/                                                        &
     &  0.00000e+00, 1.31026e+01,-4.93171e+01, 2.51045e+01,-1.30531e+01,&
     &  6.56421e-01, 2.75633e+01, 4.36433e+00, 1.04638e+00, 5.77365e+01,&
     &  0.00000e+00,-6.27766e+00, 2.33010e+00,-1.41351e+01, 2.49653e-01,&
     &  0.00000e+00, 0.00000e+00, 8.00000e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 1.03817e-02,-1.70950e+01,-1.92295e+00, 0.00000e+00,&
     &  0.00000e+00,-1.17490e+01,-7.14788e-01, 6.72649e+00, 0.00000e+00,&
     &  0.00000e+00,-1.57793e+02,-1.70815e+00,-7.92416e+00,-1.67372e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 1.87973e-01,&
     & -1.61602e-01,-1.13832e-01,-7.22447e-01, 2.21119e-02, 0.00000e+00,&
     & -3.01967e+00,-1.72798e-01,-5.15055e-03,-1.23477e-02, 3.60805e-03/
      data pca2/                                                        &
     & -1.36730e+00, 0.00000e+00, 1.24390e-02, 0.00000e+00,-1.36577e+00,&
     &  3.18101e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00,-1.39334e+01,&
     &  1.42088e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00,-4.72219e+00,&
     & -7.47970e+00,-4.96528e+00, 0.00000e+00, 1.24712e+00,-2.56833e+01,&
     & -4.26630e+01, 3.92431e+04,-2.57155e+00,-4.35589e-02, 0.00000e+00,&
     &  0.00000e+00, 2.02425e+00,-1.48131e+00,-7.72242e-01, 2.99008e+04,&
     &  4.50148e-03, 5.29718e-03,-1.26697e-02, 3.20909e-02, 0.00000e+00,&
     &  0.00000e+00, 7.01739e+00, 3.11204e+00, 0.00000e+00, 0.00000e+00,&
     & -2.13088e+00, 1.32789e+01, 5.07958e+00, 7.26537e-02, 2.87495e-01,&
     &  9.97311e-03,-2.56440e+00, 0.00000e+00, 0.00000e+00, 3.00000e+00/
      data pca3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-9.90073e-03,-3.27333e-02,&
     & -4.30379e+01,-2.87643e+01,-5.91793e+00,-1.50460e+02, 0.00000e+00,&
     &  0.00000e+00, 6.55038e-03, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 6.18051e-03, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  1.40484e+00, 5.54554e+00, 0.00000e+00, 0.00000e+00, 7.93810e+00,&
     &  1.57192e+00, 1.03971e+00, 9.88279e-01,-4.37662e-02,-2.15763e-02/
      data pca4/                                                        &
     & -2.31583e+00, 4.32633e+00,-1.12716e+00, 3.38459e-01, 4.66956e-01,&
     &  7.18403e-01, 5.80836e-02, 4.12653e-01, 1.04111e-01,-8.30672e-02,&
     & -5.55541e+00,-4.97473e+00,-2.03007e+01, 0.00000e+00,-6.06235e-01,&
     & -1.73121e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 9.29850e-02,-6.38131e-02,&
     &  3.93037e-02, 5.21942e-02, 2.26578e-02, 4.13157e-02, 0.00000e+00,&
     &  6.28524e+00, 4.43721e+00,-4.31270e+00, 2.32787e+00, 2.55591e-01,&
     &  1.60098e+00,-1.20649e+00, 3.05042e+00,-1.88944e+00, 5.35561e+00,&
     &  2.02391e-01, 4.62950e-02, 3.39155e-01, 7.94007e-02, 6.30345e-01,&
     &  1.93554e-01, 3.93238e-01, 1.76254e-01,-2.51359e-01,-7.06879e-01/
!       ugn1(1)                                                         
      data pbb1/                                                        &
     &  6.22831e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  5.90566e+00, 0.00000e+00, 0.00000e+00,-3.20571e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-8.30368e-01, 1.39396e-02,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 2.40657e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-4.80790e+00,-1.62744e+00, 2.21119e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbb2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pbb3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 2.10531e-01,&
     & -8.94829e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!         ugn1(1)                                                       
      data pcb1/                                                        &
     &  5.45009e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     & -3.60304e+00, 0.00000e+00, 0.00000e+00,-5.04071e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 5.62113e-01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.14657e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 4.65483e-01, 1.73636e+00, 2.21119e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pcb2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pcb3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,-8.30769e-01,&
     &  7.73649e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!         un1(1)                                                        
      data pbc1/                                                        &
     &  6.09940e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 1.39396e-02,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 2.21119e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbc2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pbc3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!        un1(1)                                                         
      data pcc1/                                                        &
     &  5.46739e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 2.21119e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pcc2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pcc3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!        un1(2)                                                         
      data pbd1/                                                        &
     &  4.99007e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  2.59994e+00, 0.00000e+00, 0.00000e+00,-1.78418e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-5.24986e+00, 1.39396e-02,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 2.77918e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 2.21119e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbd2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pbd3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.68996e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!         un1(2)                                                        
      data pcd1/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     & -7.26156e+00, 0.00000e+00, 0.00000e+00,-4.12416e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-2.88934e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 3.65720e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 2.21119e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pcd2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pcd3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 2.01835e-01,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!        un1(3)                                                         
      data pbe1/                                                        &
     &  0.00000e+00,-1.37217e+01, 0.00000e+00, 2.38712e-01,-3.92230e+00,&
     &  6.11035e+00,-1.57794e+00,-5.87709e-01, 1.21178e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 5.23202e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-2.22836e+03, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-3.94006e+00, 1.39396e-02,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 3.99844e-01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-1.38936e+00, 2.22534e+00, 2.21119e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbe2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pbe3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 4.35518e-01, 8.40051e-01, 0.00000e+00,-8.88181e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 6.81729e-01, 9.67536e-01,&
     &  0.00000e+00,-9.67836e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!          un1(3)                                                       
      data pce1/                                                        &
     &  0.00000e+00,-2.75655e+01,-6.61134e+00, 4.85118e+00, 8.15375e-01,&
     & -2.62856e+00, 2.99508e-02,-2.00532e-01,-9.35618e+00, 1.17196e+01,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-2.43848e+00, 1.90065e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-3.37525e-01, 1.76471e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pce2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pce3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-9.23682e-01,-8.84150e-02, 0.00000e+00,-9.88578e-01,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-1.00747e+00,-1.07468e-02,&
     &  0.00000e+00,-3.66376e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!           un1(4)                                                      
      data pbf1/                                                        &
     &  0.00000e+00, 1.02709e+01, 0.00000e+00,-1.42016e+00,-4.90438e+00,&
     & -9.11544e+00,-3.80570e+00,-2.09013e+00, 1.32939e+01,-1.28062e+01,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.23024e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 3.92126e+02, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 1.39396e-02,&
     &  0.00000e+00, 0.00000e+00,-5.56532e+00,-1.27046e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-3.03553e+00,-9.09832e-01, 2.21119e-02, 0.00000e+00,&
     &  8.89965e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbf2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 9.19210e-01, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pbf3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-2.46693e-01, 7.44650e-02, 3.84661e-01, 9.44052e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-2.25083e-01, 1.54206e-01,&
     &  4.41303e-01, 8.74742e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!           un1(4)                                                      
      data pcf1/                                                        &
     &  0.00000e+00, 3.61143e+00,-8.24679e+00, 1.70751e+00, 1.16676e+00,&
     &  6.24821e+00,-5.68968e-01, 8.53046e-01,-6.94168e+00, 1.04152e+01,&
     & -3.70861e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-1.23336e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 5.33958e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-6.43682e-01,-1.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-1.00000e+00, 0.00000e+00,-5.47300e-01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-8.58764e-01, 4.72310e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pcf2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pcf3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 3.37325e-01,-3.57698e-02,-6.97393e-01, 1.35387e+01,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 2.78162e-01,-2.33383e-01,&
     & -7.12994e-01, 1.29234e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!         un1(5)                                                        
      data pbg1/                                                        &
     &  0.00000e+00,-1.71856e+00, 5.32877e+00, 5.33548e-01,-2.66034e+00,&
     &  6.76192e-01, 2.25618e+00,-5.78954e-01,-2.69685e+00, 1.21933e+00,&
     & -6.13650e+00, 7.79531e-01, 1.63652e+00, 3.63835e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 7.51539e+00,-5.27337e-01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.06625e-01, 1.39396e-02,&
     &  0.00000e+00, 0.00000e+00,-1.07240e+00,-8.31257e-01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 7.04016e-01, 0.00000e+00,&
     &  7.56158e-01,-4.21268e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 1.02843e+00, 5.21034e-01, 2.21119e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbg2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 4.12504e+00, 1.08459e-01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     & -3.16261e-01, 0.00000e+00,-1.44288e-01, 0.00000e+00, 4.00000e+00/
      data pbg3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,-2.36181e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!         un1(5)                                                        
      data pcg1/                                                        &
     &  0.00000e+00, 3.47155e+00, 1.76102e+01, 2.80371e+00,-2.08556e+00,&
     &  1.10473e+00, 6.74582e+00,-5.75988e-01, 1.02708e+00,-2.23381e+01,&
     &  8.60171e+00, 5.12046e-01,-8.12861e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 9.11036e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 3.89742e+00, 2.01725e-01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 5.06308e-01, 2.04596e-01, 0.00000e+00,&
     &  4.40377e+00, 0.00000e+00, 0.00000e+00, 2.20760e+00, 0.00000e+00,&
     & -1.36478e+00, 2.38097e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-7.08949e-02,-1.61277e-01, 2.21119e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pcg2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-2.16898e+00,-5.31596e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  2.53060e+00, 0.00000e+00,-7.17287e-01, 0.00000e+00, 4.00000e+00/
      data pcg3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,-1.91762e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!          ugn1(2                                                       
      data pbh1/                                                        &
     &  0.00000e+00,-7.70936e-01, 1.58158e+00, 3.61790e+00,-1.51748e+00,&
     & -5.66098e-01, 1.69393e+00,-4.60489e-01,-8.31527e-01,-4.66437e-01,&
     & -1.21750e+00, 0.00000e+00, 0.00000e+00, 1.56505e+02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-5.19321e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 1.39396e-02,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 3.09223e-01, 1.33715e-01, 2.21119e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbh2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pbh3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!          ugn1(2)                                                      
      data pch1/                                                        &
     &  0.00000e+00, 1.72324e-01, 3.08033e-01, 4.55771e-01, 1.46516e-01,&
     &  1.97176e-01,-1.53329e-01, 6.91877e-02,-3.07184e-01, 2.65686e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-2.24369e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 4.04079e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  4.99627e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-7.83317e-03,-6.88967e-02, 2.21119e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pch2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.00000e+00/
      data pch3/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
!          un2(2)                                                       
      data pbi1/                                                        &
     &  0.00000e+00,-7.99767e-01,-3.24774e-01, 7.70975e-01, 6.71796e-01,&
     &  5.65483e-01,-2.99727e+00, 3.32448e+00,-9.15018e-01, 5.97656e+00,&
     &  0.00000e+00,-1.19515e+00,-8.30457e-01, 3.26074e+00, 0.00000e+00,&
     &  0.00000e+00,-1.58365e+00, 7.44825e-02, 5.91372e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-1.41511e-01,-3.01048e+00,&
     &  2.35960e+01, 0.00000e+00,-1.70352e+00,-2.39746e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.30488e+00, 0.00000e+00,&
     &  5.95132e-01, 5.64301e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 5.30317e-01, 5.66569e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbi2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 5.72367e+00, 1.58411e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  1.04557e-01, 0.00000e+00,-2.04710e-01, 0.00000e+00, 5.00000e+00/
!          un2(2)                                                       
      data pci1/                                                        &
     &  0.00000e+00, 6.34487e+00, 9.84162e+00, 3.42136e+00,-5.10607e+00,&
     & -8.58745e-02, 3.11501e+00, 5.34570e-01, 1.18027e+00, 4.28027e+00,&
     &  4.75123e+00, 6.40947e-01,-4.15165e+00,-1.38154e+01, 0.00000e+00,&
     &  0.00000e+00, 1.13145e+01,-5.15954e+00, 0.00000e+00, 0.00000e+00,&
     &  1.35576e+01, 0.00000e+00,-5.78982e+00,-2.22043e+00, 3.36776e+00,&
     &  3.04791e+01, 0.00000e+00, 2.94709e+00,-4.17536e-01,-1.59855e+00,&
     & -2.18320e+00, 1.68269e+01, 0.00000e+00, 1.00829e+00, 0.00000e+00,&
     & -6.85096e-01, 2.07822e-01, 3.50168e-01,-3.03662e+01, 0.00000e+00,&
     &  0.00000e+00,-1.65726e-01,-8.97831e-02, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-5.24159e+00, 0.00000e+00,-3.52218e+00/
      data pci2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 5.69093e-01,-7.44918e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  2.10865e+00, 0.00000e+00, 1.76776e-01, 1.54755e+00, 5.00000e+00/
!          un2(3)                                                       
      data pbj1/                                                        &
     &  0.00000e+00, 2.28657e+00, 4.96548e-01, 6.99915e+00,-2.31540e+00,&
     & -1.82163e-01,-5.00779e-01, 3.18199e-01,-6.14645e-01, 6.34816e+00,&
     &  0.00000e+00, 7.94635e-01,-5.55565e-01, 3.85494e+00, 0.00000e+00,&
     &  0.00000e+00,-3.96109e+00, 1.90775e-01, 4.51396e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-5.04618e-01,-4.14385e+00,&
     &  2.30244e+01, 0.00000e+00, 1.00689e+00, 5.75680e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 8.56741e-01, 0.00000e+00,&
     &  9.54921e-02, 5.56659e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 1.38503e-01, 4.50415e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbj2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 2.22813e-01,-8.63549e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  1.37970e-01, 0.00000e+00,-3.25612e-01, 0.00000e+00, 5.00000e+00/
!          un2(3)                                                       
      data pcj1/                                                        &
     &  0.00000e+00, 5.07608e+00, 3.31479e+00, 3.01548e-01,-1.12100e+00,&
     & -7.63711e-02, 2.29748e+00,-1.36699e+00, 7.53433e-01, 3.60702e+01,&
     & -1.55266e+00, 1.47382e+00,-2.53895e+00,-1.47720e+01, 0.00000e+00,&
     &  0.00000e+00, 1.11787e+01,-1.06256e+01, 0.00000e+00, 0.00000e+00,&
     &  7.86391e+00, 0.00000e+00,-8.61020e+00,-1.59313e+00,-5.17013e+00,&
     &  1.20468e+00, 0.00000e+00, 5.76509e-01, 9.96195e-01,-1.45539e+00,&
     & -1.79950e+01, 8.76957e+00, 0.00000e+00,-1.22863e+00, 0.00000e+00,&
     & -6.19019e-01,-1.09571e-01,-4.31325e-02,-4.21981e+01, 0.00000e+00,&
     &  0.00000e+00,-1.51519e-01,-1.24067e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-6.39248e+00, 0.00000e+00, 6.64508e-01/
      data pcj2/                                                        &
     & -7.33184e-01,-9.72031e-03, 1.36789e+00,-8.62311e-01,-3.06395e-03,&
     &  2.53354e-01,-2.40918e-01,-4.06932e-02,-5.82223e-01, 0.00000e+00,&
     & -8.70285e-01, 7.72318e-01,-6.54213e-01,-2.19231e+01,-1.56509e-01,&
     &  2.71745e-01, 5.93538e-01, 2.27757e-01,-5.98215e-01, 3.96457e-01,&
     &  2.98705e-01, 1.78618e-01,-5.24538e-01, 1.16439e-01, 7.56829e-02,&
     & -4.26809e-01, 5.77187e-01, 8.65450e-01,-7.53614e-01, 1.38381e-01,&
     & -1.82265e-01, 2.85263e-01, 4.51322e-01, 1.02775e-01, 3.55731e-01,&
     & -4.60896e-01,-3.13037e+01,-2.70818e+00,-7.84847e-01, 0.00000e+00,&
     & -1.03473e-01,-3.87649e-01,-1.22613e-01, 0.00000e+00, 0.00000e+00,&
     &  8.91325e-01, 0.00000e+00, 1.06189e-01, 9.13030e-02, 5.00000e+00/
!          un2(4)                                                       
      data pbk1/                                                        &
     &  0.00000e+00, 2.94921e+00, 2.79238e+00, 2.58949e+00, 3.56459e-01,&
     &  3.12952e-01, 3.34337e+00,-2.83209e+00,-1.05979e+00, 3.92313e+00,&
     &  0.00000e+00, 1.73703e-01,-3.23441e-01, 4.15836e+00, 0.00000e+00,&
     &  0.00000e+00,-1.77156e+00, 6.44113e-01, 1.88743e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-4.64778e-01,-4.23560e+00,&
     &  2.27271e+01, 0.00000e+00,-4.89468e-01, 1.82689e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 4.38217e-02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 8.62449e-02, 4.46041e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbk2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-1.40393e-01, 1.01821e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(4)                                                       
      data pck1/                                                        &
     &  0.00000e+00, 6.04465e+00, 4.50924e+00, 3.84425e-02,-8.70772e-01,&
     & -9.55408e-02, 2.28287e+00,-4.37834e-01, 3.57839e-01, 7.20721e+01,&
     & -4.41757e+00,-9.13648e-01,-8.71866e-01,-6.26173e+00, 0.00000e+00,&
     &  0.00000e+00, 5.92817e+00, 6.15853e+00, 0.00000e+00, 0.00000e+00,&
     & -4.89060e+00, 0.00000e+00,-8.30378e+00, 1.07462e-01, 1.08471e+02,&
     &  3.39150e+01,-4.57863e+00,-7.18349e-02,-2.71703e-01,-8.96297e+00,&
     & -2.37986e+01, 4.11880e+00, 0.00000e+00,-9.95820e-01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-8.91622e+00,-6.85950e+01, 0.00000e+00,&
     &  0.00000e+00,-3.62769e-02,-1.65893e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-2.94563e+00, 0.00000e+00, 1.23581e+00/
      data pck2/                                                        &
     & -6.06026e-01,-6.50229e-01, 1.91330e+00,-1.00314e+00, 1.13346e-01,&
     &  4.21885e-01,-3.97688e-01,-2.77437e-01,-6.65893e-01, 0.00000e+00,&
     & -1.37646e+00, 1.35171e+00,-9.55595e-01,-1.96450e+01,-2.50039e-01,&
     &  5.93389e-01, 9.87131e-01, 5.43559e-01,-1.04322e+00, 6.32546e-01,&
     &  3.73259e-01, 5.22657e-01,-5.81400e-01,-1.26425e-01,-1.29843e-01,&
     & -5.36598e-01, 8.02402e-01, 9.04347e-01,-1.10799e+00, 1.24800e-01,&
     &  1.62487e-02, 2.84237e-01,-1.68866e+00, 5.07723e-01, 5.14161e-01,&
     & -4.71292e-01,-3.03487e+01, 4.17455e-01,-1.12591e+00, 0.00000e+00,&
     & -3.03544e-01,-6.60313e-01,-1.48606e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.00607e+01, 5.00000e+00/
!          un2(5)                                                       
      data pbl1/                                                        &
     &  0.00000e+00, 2.52207e+00, 3.84550e+00, 1.68023e+00, 7.93489e-01,&
     &  3.93725e-02,-2.79707e+00,-4.76621e-01,-1.19972e-01, 3.20454e-01,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 4.17146e+00, 0.00000e+00,&
     &  0.00000e+00,-5.30699e-01, 9.14373e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-4.84434e-02, 1.85902e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbl2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(5)                                                       
      data pcl1/                                                        &
     &  0.00000e+00, 1.55386e+01, 4.21418e+00,-9.70151e-01,-8.77326e-01,&
     &  2.65813e-02, 1.40164e+00,-9.03874e-01, 3.17281e-03, 9.26891e+01,&
     & -4.96004e+00, 0.00000e+00, 0.00000e+00,-4.17851e+00, 0.00000e+00,&
     &  0.00000e+00,-1.14760e+01, 2.67744e+00, 0.00000e+00, 0.00000e+00,&
     & -1.60056e+01, 0.00000e+00,-7.14647e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-2.89639e+00, 0.00000e+00, 0.00000e+00,-3.88601e+00,&
     & -1.65784e+01, 8.44796e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-3.75324e+00,-6.24047e+01, 0.00000e+00,&
     &  0.00000e+00,-2.86808e-02,-1.95891e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-3.10534e-01, 0.00000e+00,-3.37448e+00/
      data pcl2/                                                        &
     &  1.63964e-02,-1.45191e+00, 1.85618e+00,-9.61979e-01, 3.93783e-01,&
     &  4.21681e-01,-5.30254e-01,-2.96232e-01,-7.55211e-01, 0.00000e+00,&
     & -1.85443e+00, 1.88047e+00,-1.07818e+00,-1.35373e+01,-3.05785e-01,&
     &  7.82159e-01, 1.32586e+00, 2.34413e-01,-7.47152e-01, 9.92893e-01,&
     & -2.80110e-02, 3.61747e-01,-4.16280e-01,-3.46427e-01,-5.76431e-01,&
     & -2.13906e-01, 9.51184e-01, 3.69403e-01,-1.35563e+00, 6.59534e-02,&
     &  1.39764e-01, 4.50687e-01,-1.22025e+00, 5.73280e-02, 7.49303e-01,&
     & -8.37947e-01,-3.01332e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     & -4.36697e-01,-7.76068e-01,-1.41680e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.21958e+01, 5.00000e+00/
!          un2(6)                                                       
      data pbm1/                                                        &
     &  0.00000e+00, 3.13842e+00,-8.20417e-01, 3.72282e+00,-5.20477e-01,&
     & -3.61867e-01,-2.92604e+00, 3.13013e-01,-1.38865e-01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.30060e+01, 0.00000e+00,&
     &  0.00000e+00, 1.67696e+00, 9.85990e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-8.46922e-02, 5.59429e-03, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbm2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(6)                                                       
      data pcm1/                                                        &
     &  0.00000e+00, 1.78539e+01, 1.07314e+01,-1.13212e+00, 1.59867e-02,&
     &  1.53736e-01, 2.25710e+00,-9.39080e-01,-9.72620e-02, 9.89789e+01,&
     & -5.17469e+00, 0.00000e+00, 0.00000e+00,-2.98597e+00, 0.00000e+00,&
     &  0.00000e+00,-2.04707e+01, 4.92899e+00, 0.00000e+00, 0.00000e+00,&
     & -1.44316e+01, 0.00000e+00,-3.31557e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-6.22743e+00, 0.00000e+00, 0.00000e+00,-4.34344e+00,&
     & -8.29640e+00,-3.03800e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 2.79387e+00,-5.23752e+01, 0.00000e+00,&
     &  0.00000e+00,-2.59963e-02,-1.73426e-02, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-5.37220e+00, 0.00000e+00,-6.53478e-01/
      data pcm2/                                                        &
     &  3.48181e-01,-1.88980e+00, 1.47787e+00,-7.92670e-01, 6.49224e-01,&
     &  5.96079e-01,-1.04901e+00,-5.24003e-01,-6.77311e-01, 0.00000e+00,&
     & -2.26873e+00, 2.80910e+00,-9.84994e-01,-6.79661e+00,-3.71975e-01,&
     &  1.13310e+00, 1.57164e+00, 2.15176e-01,-5.58583e-01, 1.16045e+00,&
     &  2.05395e-02, 2.27714e-01, 1.41203e-01,-3.92231e-01,-8.82859e-01,&
     &  4.90400e-01, 1.14013e+00,-2.25250e-01,-1.64930e+00, 5.73434e-02,&
     &  1.89857e-01, 4.31221e-01,-1.35345e+00,-2.94189e-01, 6.87530e-01,&
     & -7.78284e-01,-2.88975e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     & -3.98115e-01,-7.40699e-01,-8.28264e-02, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 2.02069e+00, 5.00000e+00/
!          un2(7)                                                       
      data pbn1/                                                        &
     &  0.00000e+00, 2.08818e+00,-1.96235e+00, 4.55317e+00,-1.76012e+00,&
     & -4.75258e-01,-1.44220e+00,-3.28566e-01,-1.41177e-01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.49146e+01, 0.00000e+00,&
     &  0.00000e+00, 1.73222e+00, 9.91286e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-1.35468e-01, 1.91833e-02, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbn2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(7)                                                       
      data pcn1/                                                        &
     &  0.00000e+00, 1.25645e+01, 2.43937e+01,-4.89691e-01,-5.46437e-01,&
     &  1.22200e-01, 2.89309e+00,-2.85509e-01,-2.27122e-01, 9.54192e+01,&
     & -4.07394e+00, 0.00000e+00, 0.00000e+00,-3.04354e+00, 0.00000e+00,&
     &  0.00000e+00,-2.36547e+01, 1.04903e+01, 0.00000e+00, 0.00000e+00,&
     & -8.32274e+00, 0.00000e+00,-3.34712e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-7.95953e+00, 0.00000e+00, 0.00000e+00,-5.83474e+00,&
     & -1.48074e+00, 1.02268e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 6.19470e+00,-3.90767e+01, 0.00000e+00,&
     &  0.00000e+00,-3.58136e-03, 1.22289e-03, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-8.49787e+00, 0.00000e+00,-3.97498e+00/
      data pcn2/                                                        &
     &  3.79580e-01,-1.93595e+00, 2.89114e+00,-4.73457e-01, 7.67548e-01,&
     &  5.66859e-01,-1.28683e+00,-8.37174e-01,-3.48022e-01, 0.00000e+00,&
     & -2.62865e+00, 3.50575e+00,-7.93257e-01,-8.10692e-01,-4.99450e-01,&
     &  1.56654e+00, 1.63039e+00, 7.58900e-02,-4.30952e-01, 1.23068e+00,&
     &  1.06404e-01, 4.73870e-02, 5.50559e-01,-4.11375e-01,-9.94162e-01,&
     &  1.35025e+00, 1.26053e+00,-7.34502e-01,-2.01952e+00, 2.05398e-01,&
     & -4.77248e-02, 2.41549e-01,-9.32522e-01,-5.63663e-01, 5.34833e-01,&
     & -5.77563e-01,-2.65033e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     & -2.42317e-01,-7.33679e-01,-7.85537e-02, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.56842e-01, 5.00000e+00/
!          un2(8)                                                       
      data pbo1/                                                        &
     &  0.00000e+00, 7.00409e-01,-4.17017e-01, 3.24757e+00,-1.28352e+00,&
     & -4.23875e-01, 1.64346e+00,-1.20855e+00,-7.65316e-01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-3.39417e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 2.68534e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-1.56444e-01,-4.60043e-02, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbo2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(8)                                                       
      data pco1/                                                        &
     &  0.00000e+00, 7.30129e+00, 3.14811e+01,-7.06834e-02,-2.96193e-01,&
     &  1.73817e-01, 1.62127e+00,-2.71556e-01,-2.05844e-01, 8.02088e+01,&
     & -1.86956e-01, 0.00000e+00, 0.00000e+00,-9.43641e-01,-3.24716e+00,&
     &  0.00000e+00,-2.32748e+01, 1.96724e+01, 0.00000e+00, 0.00000e+00,&
     & -3.95949e+00, 0.00000e+00, 5.44787e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-1.00161e+01, 0.00000e+00, 0.00000e+00,-4.57422e+00,&
     &  4.31304e+00, 1.49868e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 5.99489e+00,-2.82120e+01, 0.00000e+00,&
     &  0.00000e+00, 4.03624e-02, 1.19463e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-1.39050e+01, 0.00000e+00,-2.65634e+00/
      data pco2/                                                        &
     &  6.37036e-01,-1.77461e+00, 3.03103e+00,-1.49839e-01, 7.02027e-01,&
     &  6.08841e-01,-9.27289e-01,-8.52362e-01, 5.61723e-01, 0.00000e+00,&
     & -2.72061e+00, 3.66183e+00,-2.54943e-01, 2.94668e+00,-3.57898e-01,&
     &  1.71858e+00, 1.58782e+00,-2.42995e-01,-3.57783e-01, 1.20157e+00,&
     &  2.58895e-01,-1.05773e-01, 5.79397e-01,-3.30395e-01,-4.03569e-01,&
     &  1.99175e+00, 1.21688e+00,-8.64350e-01,-1.95569e+00, 4.61136e-01,&
     & -8.61382e-02, 3.38859e-01, 0.00000e+00,-5.78864e-01, 4.46659e-01,&
     & -4.57428e-01,-1.99920e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     & -1.19841e-01,-4.56968e-01, 2.00180e-02, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-1.07368e+00, 5.00000e+00/
!          un2(9)                                                       
      data pbp1/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.75863e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 3.18522e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbp2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(9)                                                       
      data pcp1/                                                        &
     &  0.00000e+00, 4.61019e-02, 3.50615e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 6.15349e+01,&
     &  4.28634e+00, 0.00000e+00, 0.00000e+00, 6.03982e+00,-4.72305e+00,&
     &  0.00000e+00,-1.43678e+01, 3.62580e+01, 0.00000e+00, 0.00000e+00,&
     &  1.26574e+00, 0.00000e+00,-2.77285e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-1.14802e+01, 0.00000e+00, 0.00000e+00,-1.11940e+01,&
     & -1.39535e+00, 2.63070e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-1.53024e+00,-2.14609e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-1.26956e+01, 0.00000e+00, 5.49926e+00/
      data pcp2/                                                        &
     &  9.80142e-01,-1.19016e+00, 2.75110e+00, 4.23423e-01, 5.89893e-01,&
     &  4.94288e-01,-5.25954e-01,-8.51760e-01, 1.62676e+00, 0.00000e+00,&
     & -1.90027e+00, 3.19950e+00, 4.72739e-01, 7.04179e+00,-1.43685e-03,&
     &  1.43219e+00, 1.32136e+00,-2.92744e-03,-3.43680e-01, 7.75735e-01,&
     &  6.92202e-01,-1.45519e-01, 6.97813e-02,-3.11588e-01, 6.65750e-01,&
     &  2.33809e+00, 1.06694e+00,-5.77590e-01,-1.33717e+00, 8.13367e-01,&
     & -5.05737e-01, 5.99169e-01,-8.83386e-01,-4.38123e-01, 2.63649e-01,&
     & -3.03448e-01,-1.28190e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  1.45478e-02, 1.45491e-01, 2.40080e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-3.86910e+00, 5.00000e+00/
!          un2(10)                                                      
      data pbq1/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.10647e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 3.13252e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbq2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(10)                                                      
      data pcq1/                                                        &
     &  0.00000e+00,-3.03260e+00, 3.15488e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.42798e+01,&
     &  7.08849e+00, 0.00000e+00, 0.00000e+00, 1.64773e+01,-6.86505e+00,&
     &  0.00000e+00,-6.27112e+00, 3.78373e+01, 0.00000e+00, 0.00000e+00,&
     &  2.97763e+00, 0.00000e+00,-3.44134e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-1.19424e+01, 0.00000e+00, 0.00000e+00,-1.64645e+01,&
     & -2.27053e+00, 3.82330e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 1.33140e-01,-2.08131e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-7.04687e+00, 0.00000e+00, 6.52184e+00/
      data pcq2/                                                        &
     &  7.31799e-01,-2.75395e-01, 1.92467e+00, 8.71269e-01, 3.72836e-01,&
     &  3.04967e-01, 7.72480e-02,-5.08596e-01, 1.99828e+00, 0.00000e+00,&
     & -5.51169e-01, 2.12420e+00, 8.96069e-01, 1.12092e+01,-4.30438e-02,&
     &  7.38391e-01, 6.12050e-01, 3.62981e-02,-1.02054e-01, 1.82404e-01,&
     &  3.70643e-01,-1.68899e-01,-1.79628e-01,-1.21117e-01, 1.45823e+00,&
     &  2.04352e+00, 7.83711e-01,-3.42979e-02,-2.31363e-01, 7.11253e-01,&
     & -3.16353e-01, 6.21069e-01,-1.05676e+00,-4.03488e-01, 4.11595e-01,&
     & -2.12535e-01,-6.51453e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  1.48238e-01, 6.38716e-01, 2.99311e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-1.01846e+00, 5.00000e+00/
!          un2(11)                                                      
      data pbr1/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 2.21764e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 6.77475e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbr2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(11)                                                      
      data pcr1/                                                        &
     &  0.00000e+00,-1.74115e+00, 2.66621e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 3.13017e+01,&
     &  6.86985e+00, 0.00000e+00, 0.00000e+00, 2.08835e+01,-7.86030e+00,&
     &  0.00000e+00,-3.77141e+00, 3.87788e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 1.31580e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-9.98927e+00, 0.00000e+00, 0.00000e+00,-1.71002e+01,&
     & -9.88358e-01, 4.47756e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 5.95029e-01,-2.11313e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-3.84164e+00, 0.00000e+00, 0.00000e+00/
      data pcr2/                                                        &
     &  3.07191e-01, 4.79094e-02, 6.72159e-01, 5.54185e-01, 1.82847e-01,&
     & -1.23768e-02, 1.91637e-01,-2.89429e-02, 1.18297e+00, 0.00000e+00,&
     &  2.37450e-01, 9.23551e-01, 6.05670e-01, 1.35990e+01,-1.64210e-01,&
     &  5.38355e-03,-4.91246e-02,-1.06966e-01,-2.09635e-01,-3.23023e-02,&
     & -3.41663e-02,-3.48871e-02,-2.62450e-01, 2.21492e-01, 1.43749e+00,&
     &  1.08677e+00, 3.97778e-01, 3.61526e-01, 5.55950e-01, 3.53058e-01,&
     & -5.93339e-02, 4.14203e-01,-6.05024e-01,-1.38714e-01, 2.78897e-01,&
     & -8.92889e-02,-3.59033e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  9.90623e-02, 4.36170e-01, 7.95418e-02, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00,-1.11426e+00, 5.00000e+00/
!          un2(12)                                                      
      data pbs1/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 3.07320e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 1.60738e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbs2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(12)                                                      
      data pcs1/                                                        &
     &  0.00000e+00, 1.26217e+01, 2.30787e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 2.00029e+01,&
     & -2.88682e+00, 0.00000e+00, 0.00000e+00, 2.09439e+01,-4.56923e+00,&
     &  0.00000e+00,-2.15929e+00, 3.87149e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-7.98039e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-6.63423e+00, 0.00000e+00, 0.00000e+00,-5.84850e+00,&
     &  3.72111e+00, 4.52300e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 3.21872e-01, 0.00000e+00, 0.00000e+00/
      data pcs2/                                                        &
     &  1.09405e-02,-4.35341e-02, 8.00586e-02, 1.48577e-01, 1.01602e-01,&
     & -1.01104e-01,-1.98993e-02, 3.51174e-02, 2.41112e-01, 0.00000e+00,&
     &  2.76479e-01, 1.97043e-01, 2.68708e-01, 1.39832e+01,-1.56638e-01,&
     & -2.39101e-01,-1.50605e-01,-2.17139e-01,-2.59057e-01,-4.36362e-01,&
     & -1.43496e-01, 7.51305e-02,-2.40850e-01, 1.34858e-01, 7.59193e-01,&
     &  3.52708e-01, 1.29922e-01, 3.27957e-01, 5.35491e-01, 1.19120e-01,&
     & -2.94029e-02, 1.76113e-01,-6.51597e-01, 3.61575e-02, 4.26836e-02,&
     & -2.29297e-02,-4.27373e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     & -2.78548e-02, 5.77322e-02,-1.02411e-01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(13)                                                      
      data pbt1/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 3.69447e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 2.34073e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbt2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(13)                                                      
      data pct1/                                                        &
     &  0.00000e+00, 1.22096e+01, 1.92342e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 8.13667e+00,&
     & -6.19078e+00, 0.00000e+00, 0.00000e+00, 2.37009e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-7.87365e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-1.12371e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-2.76047e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 1.85864e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pct2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(14)                                                      
      data pbu1/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.01008e+01, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 2.21469e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pbu2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
!          un2(14)                                                      
      data pcu1/                                                        &
     &  0.00000e+00,-1.40697e+00, 6.88709e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 3.67624e+02, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 1.58312e+01, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00,-2.46486e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00,-1.90327e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 1.13248e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pcu2/                                                        &
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,&
     &  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.00000e+00/
      end                                           

!end module hwm93
