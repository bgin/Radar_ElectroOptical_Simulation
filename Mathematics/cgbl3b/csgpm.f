      PROGRAM CSGPM
*
*  CSGPM re-writes GEMM-Based Level 3 BLAS source files replacing lines
*  containing old PARAMETER statements for user specified parameters,
*  with lines containing new PARAMETER statements given in an input
*  file. The user can conveniently assign new values to the PARAMETER
*  statements in the input file, and then run CSGPM to distribute these
*  values to the GEMM-based routines. An input file consists of three
*  different types of lines, except for empty lines.
*
*  o  Comment lines starting with the character '*'.
*
*  o  Lines containing single file-names for GEMM-based source files.
*
*  o  Lines containing PARAMETER statements that replaces the
*     corresponding lines in the GEMM-based routines.
*
*  The lines with single filenames are followed by lines containing the
*  new PARAMETER statements for that particular file (see the input file
*  'sgpm.in'). Read the file INSTALL for further instructions.
*
*
*  -- Written in May-1994.
*     GEMM-Based Level 3 BLAS.
*     Per Ling, Institute of Information Processing,
*     University of Umea, Sweden.
*
*
*     .. Local Scalars ..
      INTEGER            I, IB, IE, JB, JE, KB, KE, NAM, NXTLN
      LOGICAL            PMEOF
*     .. External Functions ..
      LOGICAL            LNCMP, GETWRD
      INTEGER            EOLN
      EXTERNAL           LNCMP, GETWRD, EOLN
*     .. Parameters ..
      INTEGER            NPM, NGB, NTMP, NERR
      PARAMETER        ( NPM = 5, NERR = 6, NGB = 10, NTMP = 12 )
      INTEGER            NLNS, LLN
      PARAMETER        ( NLNS = 14, LLN = 80 )
      CHARACTER*(LLN)    TMPNAM
      PARAMETER        ( TMPNAM = 'tmpgb.tmp' )
*     .. Local Arrays ..
      CHARACTER*(LLN)    PMLN, GBLN, GBNAM, STRS( NLNS, 2 )
*     .. Data statements ..
      DATA               STRS/
     $'csymm.f'   ,'chemm.f'   ,'csyrk.f'   ,'cherk.f'   ,
     $'csyr2k.f'  ,'cher2k.f'  ,'ctrmm.f'   ,'ctrsm.f'   ,
     $'cbigp.f'   ,'          ','          ','          ',
     $'ccld.f'    ,'          ',
     $'PARAMETER ( RCB = $$ , CB = $$ )',
     $'PARAMETER ( RCB = $$ , CB = $$ )',
     $'PARAMETER ( RCB = $$ , RB = $$ , CB = $$ )',
     $'PARAMETER ( RCB = $$ , RB = $$ , CB = $$ )',
     $'PARAMETER ( RCB = $$ , CB = $$ )',
     $'PARAMETER ( RCB = $$ , CB = $$ )',
     $'PARAMETER ( RCB = $$ , RB = $$ , CB = $$ )',
     $'PARAMETER ( RCB = $$ , RB = $$ , CB = $$ )',
     $'PARAMETER ( CIP41 = $$ , CIP42 = $$ ,',
     $'$ CIP51 = $$ , CIP52 = $$ ,',
     $'$ CIP81 = $$ , CIP82 = $$ , CIP83 = $$ ,',
     $'$ CIP91 = $$ , CIP92 = $$ , CIP93 = $$ )',
     $'PARAMETER ( LNSZ = $$ , NPRT = $$ , PRTSZ = $$ ,',
     $'$ LOLIM = $$ , CP = $$ )' /
*     ..
*     .. Executable Statements ..
*
*     Read the next non-blank/non-comment line from the input parameter
*     file.
*
   10 READ( NPM, FMT = 9000, END = 110 ) GBNAM
      IF( .NOT.GETWRD( GBNAM, LLN, IB, IE ).OR.
     $                                     ( GBNAM( 1:1 ).EQ.'*' ) )THEN
         GO TO 10
      END IF
*
*     Check if the first word on the line is the name of a file that is
*     due to be changed.
*
   20 NAM = -1
      PMEOF = .FALSE.
      DO 30, I = 1, NLNS
         IF( GBNAM( IB:IE ).EQ.STRS( I, 1 ) )THEN
            NAM = I
         END IF
   30 CONTINUE
      IF( NAM.EQ.-1 )THEN
         WRITE( NERR, FMT = * )'Unknown routine name: ', GBNAM( IB:IE )
         STOP
      END IF
*
*     Read the next non-blank/non-comment line from the input parameter
*     file.
*
   40 READ( NPM, FMT = 9000, END = 110 ) PMLN
      IF( .NOT.GETWRD( PMLN, LLN, JB, JE ).OR.
     $                                     ( PMLN( 1:1 ).EQ.'*' ) )THEN
         GO TO 40
      END IF
*
*     Copy each line of the GEMM-Based file, except for the lines that
*     are due to be changed, to the temporary file TMPNAM. Copy the
*     lines that should be changed from the input parameter file. Check
*     that the lines in the parameter file are correct compared to STRS.
*
      NXTLN = NAM
      IF( LNCMP( PMLN, LLN, STRS( NXTLN, 2 ), LLN ) )THEN
         OPEN( NGB, FILE = GBNAM( IB:IE ), STATUS = 'OLD' )
         OPEN( NTMP, FILE = TMPNAM, STATUS = 'NEW' )
   50    READ( NGB, FMT = 9000, END = 80 ) GBLN
         IF( LNCMP( GBLN, LLN, STRS( NXTLN, 2 ), LLN ) )THEN
            WRITE( NTMP, FMT = 9010 ) PMLN( 1:EOLN( PMLN, LLN ) )
   60       READ( NPM, FMT = 9000, END = 70 ) PMLN
            IF( .NOT.GETWRD( PMLN, LLN, JB, JE ).OR.
     $                                      ( PMLN( 1:1 ).EQ.'*' ) )THEN
               GO TO 60
            END IF
            IF( .NOT.GETWRD( STRS( NXTLN+1, 1 ), LLN, KB, KE ).AND.
     $             ( LNCMP( PMLN, LLN, STRS( NXTLN+1, 2 ), LLN ) ) )THEN
               NXTLN = NXTLN + 1
            END IF
         ELSE
            WRITE( NTMP, FMT = 9010 ) GBLN( 1:EOLN( GBLN, LLN ) )
         END IF
         GO TO 50
   70    PMEOF = .TRUE.
         GO TO 50
   80    CLOSE( NGB, STATUS = 'DELETE' )
         CLOSE( NTMP, STATUS = 'KEEP' )
      ELSE
         WRITE( NERR, FMT = * )'Error in parameter file: '
         WRITE( NERR, FMT = * ) PMLN
         STOP
      END IF
*
*     Write back the temporary file TMPNAM to the GEMM-Based file and
*     remove the temporary file.
*
      OPEN( NTMP, FILE = TMPNAM, STATUS = 'OLD' )
      OPEN( NGB, FILE = GBNAM( IB:IE ), STATUS = 'NEW' )
   90 READ( NTMP, FMT = 9000, END = 100 ) GBLN
      WRITE( NGB, FMT = 9010 ) GBLN( 1:EOLN( GBLN, LLN ) )
      GO TO 90
  100 CONTINUE
      CLOSE( NTMP, STATUS = 'DELETE' )
      CLOSE( NGB, STATUS = 'KEEP' )
      GBNAM = PMLN
      IB = JB
      IE = JE
*
      IF( .NOT.PMEOF )THEN
         GO TO 20
      END IF
  110 CONTINUE
*
      STOP
*
 9000 FORMAT( A )
 9010 FORMAT( A )
*
*     End of SSGPM.
*
      END
      LOGICAL FUNCTION LNCMP( LN1, LEN1, LN2, LEN2 )
*     .. Scalar Arguments ..
      INTEGER            LEN1, LEN2
*     .. Array Arguments ..
      CHARACTER          LN1( LEN1 ), LN2( LEN2 )
*
*  Compare the character strings LN1 and LN2. Return .TRUE. if the
*  strings are identical except from wild cards ($$) corresponding
*  to positive integers and except from a different number of
*  consecutive blanks between tokens.
*
*
*  -- Written in December-1993.
*     GEMM-Based Level 3 BLAS.
*     Per Ling, Institute of Information Processing,
*     University of Umea, Sweden.
*
*
*     .. Local Scalars ..
      INTEGER            I, J
      LOGICAL            MATCH
*     .. Intrinsic Functions ..
      INTRINSIC          LGE, LLE
      LOGICAL            LGE, LLE
*     ..
*     .. Executable Statements ..
*
*     Find the beginning of the next tokens in LN1 and LN2.
*
      I = 1
      J = 1
   10 IF( ( LN1( I ).EQ.' ' ).AND.( I.LT.LEN1 ) )THEN
         I = I + 1
         GO TO 10
      END IF
   20 IF( ( LN2( J ).EQ.' ' ).AND.( J.LT.LEN2 ) )THEN
         J = J + 1
         GO TO 20
      END IF
*
*     Compare the tokens.
*
      IF( ( LN1( I ).EQ.LN2( J ) ).AND.( I.LT.LEN1 ).AND.
     $                                               ( J.LT.LEN2 ) )THEN
         I = I + 1
         J = J + 1
         GO TO 10
      ELSE IF( ( LN1( I ).EQ.LN2( J ) ).AND.( I.EQ.LEN1 ).AND.
     $                                               ( J.EQ.LEN2 ) )THEN
         LNCMP = .TRUE.
         RETURN
      ELSE IF( ( I.EQ.LEN1 ).AND.( J.EQ.LEN2 ) )THEN
         LNCMP = .FALSE.
         RETURN
      ELSE IF( LN1( I ).EQ.'$' )THEN
         IF( I.LT.LEN1-1 )THEN
            IF( LN1( I+1 ).EQ.'$' )THEN
               I = I + 2
               MATCH = .FALSE.
   30          IF( ( LGE( LN2( J ), '0' ).AND.LLE( LN2( J ), '9' ) )
     $                                          .AND.( J.LT.LEN2 ) )THEN
                  J = J + 1
                  MATCH = .TRUE.
                  GO TO 30
               ELSE IF( .NOT.MATCH )THEN
                  LNCMP = .FALSE.
                  RETURN
               END IF
            ELSE
               LNCMP = .FALSE.
               RETURN
            END IF
         ELSE
            LNCMP = .FALSE.
            RETURN
         END IF
         GO TO 10
      ELSE IF( LN2( J ).EQ.'$' )THEN
         IF( J.LT.LEN2-1 )THEN
            IF( LN2( J+1 ).EQ.'$' )THEN
               J = J + 2
               MATCH = .FALSE.
   40          IF( ( LGE( LN1( I ), '0' ).AND.LLE( LN1( I ), '9' ) )
     $                                          .AND.( I.LT.LEN1 ) )THEN
                  I = I + 1
                  MATCH = .TRUE.
                  GO TO 40
               ELSE IF( .NOT.MATCH )THEN
                  LNCMP = .FALSE.
                  RETURN
               END IF
            ELSE
               LNCMP = .FALSE.
               RETURN
            END IF
         ELSE
            LNCMP = .FALSE.
            RETURN
         END IF
         GO TO 10
      END IF
*
      LNCMP = .FALSE.
      RETURN
*
*     End of LNCMP.
*
      END
