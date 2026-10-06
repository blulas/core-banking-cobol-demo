      *================================================================*
      * PROGRAM  : LDBCD                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE BUMP-UP CERTIFICATES MASTER (BCDMST) FROM *
      *            A PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#'  *
      *            ARE COMMENTS.                                       *
      * INPUT    : BCDSEED  PIPE-DELIMITED, 45 FIELDS                  *
      * OUTPUT   : BCDMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-05-06 EWS  ORIGINAL                                       *
      * 1994-10-04 GAF  BUMP RERUN PROTECTION INC2246                  *
      * 1995-05-07 JLM  ADDON NOTICE TEXT PER LEGAL CHG3993            *
      * 1996-03-16 SKN  AUDIT FINDING - PENALTY CONTROL TOTALS INC4913 *
      * 1996-04-21 SKN  INTEREST NOTICE TEXT PER LEGAL CHG4140         *
      * 1996-06-28 MKL  PENALTY RERUN PROTECTION INC3740               *
      * 1996-08-22 BHS  PRINCIPAL CALCULATION CORRECTED INC6541        *
      * 1999-03-18 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-09-10 DPS  PRINCIPAL FEE SCHEDULE UPDATE CHG4980          *
      * 2004-08-08 BHS  INTEREST LIMIT REVISED PRB9534                 *
      * 2007-11-16 SKN  BUMP CALCULATION CORRECTED INC7708             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDBCD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "BCDSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "BCDMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS BCDM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY BCDMST.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-MST-STATUS                   PIC X(02).
       01  WS-EOF                          PIC X(01) VALUE 'N'.
       01  WS-LOADED                       PIC 9(07) VALUE 0.
       01  WS-SKIPPED                      PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F OCCURS 45 TIMES PIC X(40).

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
           DISPLAY 'LDBCD LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE BCD-MASTER-REC
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
           END-UNSTRING
           MOVE WS-F(1) TO BCDM-KEY
           MOVE WS-F(2) TO BCDM-CUST-ID
           MOVE WS-F(3) TO BCDM-NAME
           MOVE WS-F(4) TO BCDM-DESC
           MOVE WS-F(5) TO BCDM-REF-NO
           MOVE WS-F(6) TO BCDM-ADDR
           MOVE WS-F(7) TO BCDM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE BCDM-PRINCIPAL-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE BCDM-PRINCIPAL-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE BCDM-INTEREST-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE BCDM-INTEREST-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE BCDM-BUMP-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE BCDM-BUMP-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE BCDM-PENALTY-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE BCDM-PENALTY-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE BCDM-ADDON-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE BCDM-ADDON-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE BCDM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE BCDM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE BCDM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE BCDM-LAST-STMT-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE BCDM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE BCDM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE BCDM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE BCDM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE BCDM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           MOVE WS-F(27) TO BCDM-STEP-PLAN-CD
           MOVE WS-F(28) TO BCDM-ELECTION-CD
           MOVE WS-F(29) TO BCDM-CHANNEL-CD
           MOVE WS-F(30) TO BCDM-REGION-CD
           MOVE WS-F(31) TO BCDM-CYCLE-CD
           MOVE WS-F(32) TO BCDM-SOURCE-CD
           MOVE WS-F(33) TO BCDM-HOLD-FLAG
           MOVE WS-F(34) TO BCDM-OVERRIDE-FLAG
           MOVE WS-F(35) TO BCDM-SUSPEND-FLAG
           MOVE WS-F(36) TO BCDM-EXEMPT-FLAG
           MOVE WS-F(37) TO BCDM-REVIEW-FLAG
           MOVE WS-F(38) TO BCDM-STOP-FLAG
           IF WS-F(39) NOT = SPACES
               COMPUTE BCDM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(39))
           END-IF
           IF WS-F(40) NOT = SPACES
               COMPUTE BCDM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(40))
           END-IF
           IF WS-F(41) NOT = SPACES
               COMPUTE BCDM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE BCDM-EXCEPT-CNT =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE BCDM-MARGIN =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE BCDM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE BCDM-RATE =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           WRITE BCD-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDBCD WRITE FAILED ' WS-MST-STATUS ' '
                       BCDM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
