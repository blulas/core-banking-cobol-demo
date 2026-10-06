      *================================================================*
      * PROGRAM  : LDEUR                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE EURO CONVERSION MASTER (EURMST) FROM A    *
      *            PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#' ARE*
      *            COMMENTS.                                           *
      * INPUT    : EURSEED  PIPE-DELIMITED, 49 FIELDS                  *
      * OUTPUT   : EURMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-06-08 DPS  ORIGINAL                                       *
      * 1996-05-23 SKN  CONVERTED RERUN PROTECTION REQ8239             *
      * 1997-08-11 EWS  CONVERTED NOTICE TEXT PER LEGAL CHG5707        *
      * 1997-11-21 EWS  PERFORMANCE - EURO READ AHEAD CHG7421          *
      * 1998-05-28 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-07-23 MKL  FEE FEE SCHEDULE UPDATE PRB5878                *
      * 2000-08-02 EWS  NATIONAL LIMIT REVISED INC7819                 *
      * 2000-08-20 JLM  ROUNDING CUTOFF MOVED INC9628                  *
      * 2001-02-17 PQR  ROUNDING CALCULATION CORRECTED PRB0713         *
      * 2002-05-26 BHS  ROUNDING STATUS CODES ADDED REQ4228            *
      * 2003-06-25 BHS  EURO FEE SCHEDULE UPDATE CHG4078               *
      * 2009-10-06 PQR  TRIANGULAR CUTOFF MOVED CHG5753                *
      * 2010-10-20 PQR  AUDIT FINDING - NATIONAL CONTROL TOTALS INC7014*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDEUR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "EURSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "EURMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS EURM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY EURMST.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-MST-STATUS                   PIC X(02).
       01  WS-EOF                          PIC X(01) VALUE 'N'.
       01  WS-LOADED                       PIC 9(07) VALUE 0.
       01  WS-SKIPPED                      PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F OCCURS 49 TIMES PIC X(40).

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT SEED-FILE
           OPEN OUTPUT MASTER-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ SEED-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM 1000-LOAD-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE MASTER-FILE
           DISPLAY 'LDEUR LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE EUR-MASTER-REC
           UNSTRING SEED-LINE DELIMITED BY '|'
               INTO WS-F(1)
                    WS-F(2)
                    WS-F(3)
                    WS-F(4)
                    WS-F(5)
                    WS-F(6)
                    WS-F(7)
                    WS-F(8)
                    WS-F(9)
                    WS-F(10)
                    WS-F(11)
                    WS-F(12)
                    WS-F(13)
                    WS-F(14)
                    WS-F(15)
                    WS-F(16)
                    WS-F(17)
                    WS-F(18)
                    WS-F(19)
                    WS-F(20)
                    WS-F(21)
                    WS-F(22)
                    WS-F(23)
                    WS-F(24)
                    WS-F(25)
                    WS-F(26)
                    WS-F(27)
                    WS-F(28)
                    WS-F(29)
                    WS-F(30)
                    WS-F(31)
                    WS-F(32)
                    WS-F(33)
                    WS-F(34)
                    WS-F(35)
                    WS-F(36)
                    WS-F(37)
                    WS-F(38)
                    WS-F(39)
                    WS-F(40)
                    WS-F(41)
                    WS-F(42)
                    WS-F(43)
                    WS-F(44)
                    WS-F(45)
                    WS-F(46)
                    WS-F(47)
                    WS-F(48)
                    WS-F(49)
           END-UNSTRING
           MOVE WS-F(1) TO EURM-KEY
           MOVE WS-F(2) TO EURM-CUST-ID
           MOVE WS-F(3) TO EURM-NAME
           MOVE WS-F(4) TO EURM-DESC
           MOVE WS-F(5) TO EURM-REF-NO
           MOVE WS-F(6) TO EURM-ADDR
           MOVE WS-F(7) TO EURM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE EURM-NATIONAL-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE EURM-NATIONAL-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE EURM-EURO-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE EURM-EURO-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE EURM-ROUNDING-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE EURM-ROUNDING-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE EURM-TRIANGULAR-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE EURM-TRIANGULAR-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE EURM-CONVERTED-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE EURM-CONVERTED-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE EURM-RESIDUAL-AMT =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE EURM-RESIDUAL-YTD =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE EURM-FEE-AMT =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE EURM-FEE-YTD =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE EURM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE EURM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE EURM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE EURM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE EURM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           IF WS-F(27) NOT = SPACES
               COMPUTE EURM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(27))
           END-IF
           IF WS-F(28) NOT = SPACES
               COMPUTE EURM-LAST-STMT-DATE =
                   FUNCTION NUMVAL(WS-F(28))
           END-IF
           IF WS-F(29) NOT = SPACES
               COMPUTE EURM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(29))
           END-IF
           IF WS-F(30) NOT = SPACES
               COMPUTE EURM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(30))
           END-IF
           MOVE WS-F(31) TO EURM-CURRENCY-CD
           MOVE WS-F(32) TO EURM-CONV-METHOD-CD
           MOVE WS-F(33) TO EURM-STATUS-CD
           MOVE WS-F(34) TO EURM-TYPE-CD
           MOVE WS-F(35) TO EURM-CHANNEL-CD
           MOVE WS-F(36) TO EURM-REGION-CD
           MOVE WS-F(37) TO EURM-REVIEW-FLAG
           MOVE WS-F(38) TO EURM-AUTO-RENEW-FLAG
           MOVE WS-F(39) TO EURM-EXEMPT-FLAG
           MOVE WS-F(40) TO EURM-OVERRIDE-FLAG
           MOVE WS-F(41) TO EURM-PREFERRED-FLAG
           MOVE WS-F(42) TO EURM-STOP-FLAG
           IF WS-F(43) NOT = SPACES
               COMPUTE EURM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE EURM-EXCEPT-CNT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE EURM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           IF WS-F(46) NOT = SPACES
               COMPUTE EURM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(46))
           END-IF
           IF WS-F(47) NOT = SPACES
               COMPUTE EURM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(47))
           END-IF
           IF WS-F(48) NOT = SPACES
               COMPUTE EURM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(48))
           END-IF
           IF WS-F(49) NOT = SPACES
               COMPUTE EURM-RATE =
                   FUNCTION NUMVAL(WS-F(49))
           END-IF
           WRITE EUR-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDEUR WRITE FAILED ' WS-MST-STATUS ' '
                       EURM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
