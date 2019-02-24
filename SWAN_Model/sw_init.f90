
      SUBROUTINE SWAN_INITIALIZATION
      !use mod_kinds, only : int4,sp,dp                                            
      USE SWAN_COMMON   
      IMPLICIT NONE
      CHARACTER PTYPE, PNAME *8, COMPUT *4, DTTIWR*18  
                                          

!     --- initialize various data
!TIMG
!TIMG      DCUMTM(:,1:2) = 0D0                                                 40.23
!TIMG      NCUMTM(:)     = 0                                                   40.23
!TIMG      CALL SWTSTA(1)                                                      40.23

      LEVERR=0                                                                 !40.23
      MAXERR=1                                                                ! 34.01
      ITRACE=0                                                                 !40.23
      INERR =0                                                                ! 40.31

!TIMG      CALL SWTSTA(2)                                                      40.23
      CALL SWINIT (INERR)                                                     ! 40.31 34.01
!TIMG      CALL SWTSTO(2)                                                      40.23
      IF (INERR.GT.0) RETURN                                                  ! 34.01
!      IF (STPNOW()) RETURN                                                     34.01
!
      COMPUT = '    '

!     --- repeat

!      DO

!       --- read and process user commands

!TIMG        CALL SWTSTA(3)                                                    40.23
        CALL SWREAD (COMPUT)                                                 ! 40.31 30.90
!TIMG        CALL SWTSTO(3)                                                    40.23
!        IF (STPNOW()) RETURN                                                  34.01

!       --- if last command was STOP then exit from repeat

        IF (COMPUT.EQ.'STOP') THEN                                        !40.13
          IUNIT  = 0                                                      !40.13
          IOSTAT = 0                                                      !40.13
          FILENM = 'norm_end'                                             !40.13
          CALL FOR (IUNIT, FILENM, 'UF', IOSTAT)                          !40.13
          WRITE (IUNIT, *) ' Normal end of run ', PROJNR                  !40.13
          GOTO 900                                                        !40.13
        ENDIF                                                             !40.13

!       --- allocate some arrays meant for computation                    40.31

        IF (NUMOBS .GT. 0) THEN
           IF (.NOT.ALLOCATED(CROSS)) ALLOCATE(CROSS(2,MCGRD))            !40.31
        ELSE
           IF (.NOT.ALLOCATED(CROSS)) ALLOCATE(CROSS(0,0))                !40.31
        ENDIF                                                             !34.01
        IF (.NOT.ALLOCATED(BSPECS)) ALLOCATE(BSPECS(MDC,MSC,NBSPEC,2))    !40.31
        IF (.NOT.ALLOCATED(BGRIDP)) ALLOCATE(BGRIDP(6*NBGRPT))            !40.31

!       --- do some preparations before computation                       40.31

!TIMG        CALL SWTSTA(4)                                                    40.23
        CALL SWPREP ( BSPECS, BGRIDP, CROSS , XCGRID, YCGRID, KGRPNT,  &  ! 40.31
                     KGRBND, SPCDIR, SPCSIG )                            !40.31
!TIMG        CALL SWTSTO(4)                                                    40.23

!       --- check all possible flags and if necessary change
!           if option is not correct

        CALL ERRCHK                                                       !30.60
!        IF (STPNOW()) RETURN                                              34.01

!       --- initialisation of necessary grids for depth,
!           current, wind and friction

        IF(.NOT.ALLOCATED(COMPDA))                   &
                            ALLOCATE(COMPDA(MCGRD,MCMVAR),STAT=ISTAT)   !40.41 40.31
!        IF ( ISTAT.NE.0 ) THEN                                            40.41
!           CHARS(1) = NUMSTR(ISTAT,RNAN,'(I6)')                           40.41
!           CALL TXPBLA(CHARS(1),IF1,IL1)                                  40.41
!           MSGSTR =                                                       40.41
!     &         'Allocation problem: array COMPDA and return code is '//   40.41
!     &         CHARS(1)(IF1:IL1)                                          40.41
!           CALL MSGERR ( 4, MSGSTR )                                      40.41
!           RETURN                                                         40.41
!        END IF                                                            40.41

!TIMG        CALL SWTSTA(5)                                                    40.23
        CALL SWRBC(COMPDA)                                                !40.31
!TIMG        CALL SWTSTO(5)                                                    40.23

!       --- allocate AC1 in case of non-stationary situation or in case   40.31
!           of using the S&L scheme                                       40.31

        IF ( NSTATM.EQ.1 .AND. MXITNS.GT.1 .OR. PROPSC.EQ.3 ) THEN        !40.31
           IF (.NOT.ALLOCATED(AC1)) THEN                                  !40.41 40.31
              ALLOCATE(AC1(MDC,MSC,MCGRD),STAT=ISTAT)                     !40.41
           ELSE IF (SIZE(AC1).EQ.0) THEN                                  !40.41
              DEALLOCATE(AC1)                                             !40.41
              ALLOCATE(AC1(MDC,MSC,MCGRD),STAT=ISTAT)                     !40.41
           END IF                                                         !40.41
!           IF ( ISTAT.NE.0 ) THEN                                         40.41
!              CHARS(1) = NUMSTR(ISTAT,RNAN,'(I6)')                        40.41
!              CALL TXPBLA(CHARS(1),IF1,IL1)                               40.41
!              MSGSTR =                                                    40.41
!     &            'Allocation problem: array AC1 and return code is '//   40.41
!     &            CHARS(1)(IF1:IL1)                                       40.41
!              CALL MSGERR ( 4, MSGSTR )                                   40.41
!              RETURN                                                      40.41
!           END IF                                                         40.41
           AC1 = 0.                                                       !40.31
        ELSE                                                             ! 40.31
           IF(.NOT.ALLOCATED(AC1)) ALLOCATE(AC1(0,0,0))                  ! 40.31
        ENDIF

        IF (LEVERR.GT.MAXERR) THEN                                        !40.00

          WRITE (PRINTF, 6010) LEVERR
          IF (LEVERR.LT.4) WRITE (PRINTF, 6011)         &                  !30.72
 6010     FORMAT(' ** No start of computation because of error level:'
           ,I3)
 6011     FORMAT(' ** To ignore this error, change [maxerr] with the',  &  !30.72
               ' SET command')                                          !30.72

        ELSE
!
          IF (ITEST.GE.40) THEN                                           !40.00
            IF (NSTATC.EQ.1) THEN                                         !33.08
              WRITE (PRINTF, '(" Type of computation: dynamic")')         !32.02
            ELSE                                                         ! 32.02
              IF (ONED) THEN                                             ! 32.02
                WRITE (PRINTF, '(" Type of computation: static 1-D")')   ! 32.02
              ELSE                                                        !32.02
                WRITE (PRINTF, '(" Type of computation: static 2-D")')    !32.02
              ENDIF                                                       !32.02
            ENDIF                                                         !32.02
          ENDIF
!
          IF (NSTATC.EQ.1) THEN                                           !40.00
            IT0 = 0                                                       !40.00
            IF (ICOND.EQ.1) THEN                                          !40.00
!
!             --- compute default initial conditions
!
!TIMG              CALL SWTSTA(6)                                              40.23
              CALL SWINCO ( AC2   , COMPDA, XCGRID, YCGRID,     &          !40.31
                           KGRPNT, SPCDIR, SPCSIG, XYTST )               !40.31
!TIMG              CALL SWTSTO(6)                                              40.23
!
!             --- reset ICOND to prevent second computation of
!                 initial condition
              ICOND = 0                                                   !40.00

            ENDIF
          ELSE
            IT0 = 1
          ENDIF

!         --- synchronize nodes                                           40.30

          CALL SWSYNC                                                     !40.30
!          IF (STPNOW()) RETURN                                            40.30

       ENDIF ! maxerr
!       ENDDO

900      CONTINUE

          RETURN

          END






