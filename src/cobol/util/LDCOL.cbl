      *================================================================*
      * PROGRAM  : LDCOL                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE COLLECTIONS QUEUE MASTER (COLMST) FROM A  *
      *            PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#' ARE*
      *            COMMENTS.                                           *
      * INPUT    : COLSEED  PIPE-DELIMITED, 45 FIELDS                  *
      * OUTPUT   : COLMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-04 BHS  ORIGINAL                                       *
      * 1995-04-03 LMB  NEW PROMISED REPORT COLUMNS PRB7381            *
      * 1995-07-05 MKL  PAST-DUE TABLE EXPANDED PRB1662                *
      * 1997-07-27 LMB  PERFORMANCE - PAST-DUE READ AHEAD CHG3985      *
      * 1999-08-23 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-10-27 MKL  NEW FEE REPORT COLUMNS PRB6410                 *
      * 1999-03-12 JLM  PERFORMANCE - COLLECTED READ AHEAD REQ8127     *
      * 2000-09-22 DPS  FEE TABLE EXPANDED PRB2866                     *
      * 2001-05-12 GAF  COLLECTED CALCULATION CORRECTED REQ7217        *
      * 2002-12-26 DPS  COLLECTED NOTICE TEXT PER LEGAL REQ8674        *
      * 2003-07-17 SKN  AUDIT FINDING - COLLECTED CONTROL TOTALS REQ102*
      * 2004-03-06 GAF  NEW WRITEOFF REPORT COLUMNS INC9601            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDCOL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "COLSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "COLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS COLM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY COLMST.

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
           DISPLAY 'LDCOL LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE COL-MASTER-REC
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
           MOVE WS-F(1) TO COLM-KEY
           MOVE WS-F(2) TO COLM-CUST-ID
           MOVE WS-F(3) TO COLM-NAME
           MOVE WS-F(4) TO COLM-DESC
           MOVE WS-F(5) TO COLM-REF-NO
           MOVE WS-F(6) TO COLM-ADDR
           MOVE WS-F(7) TO COLM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE COLM-PAST-DUE-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE COLM-PAST-DUE-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE COLM-PROMISED-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE COLM-PROMISED-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE COLM-COLLECTED-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE COLM-COLLECTED-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE COLM-WRITEOFF-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE COLM-WRITEOFF-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE COLM-FEE-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE COLM-FEE-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE COLM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE COLM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE COLM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE COLM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE COLM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE COLM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE COLM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE COLM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE COLM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           MOVE WS-F(27) TO COLM-QUEUE-CD
           MOVE WS-F(28) TO COLM-ACTION-CD
           MOVE WS-F(29) TO COLM-CYCLE-CD
           MOVE WS-F(30) TO COLM-STATUS-CD
           MOVE WS-F(31) TO COLM-REASON-CD
           MOVE WS-F(32) TO COLM-REGION-CD
           MOVE WS-F(33) TO COLM-NOTICE-SENT-FLAG
           MOVE WS-F(34) TO COLM-SUSPEND-FLAG
           MOVE WS-F(35) TO COLM-REVIEW-FLAG
           MOVE WS-F(36) TO COLM-OVERRIDE-FLAG
           MOVE WS-F(37) TO COLM-EXEMPT-FLAG
           MOVE WS-F(38) TO COLM-PREFERRED-FLAG
           IF WS-F(39) NOT = SPACES
               COMPUTE COLM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(39))
           END-IF
           IF WS-F(40) NOT = SPACES
               COMPUTE COLM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(40))
           END-IF
           IF WS-F(41) NOT = SPACES
               COMPUTE COLM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE COLM-NOTICE-CNT =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE COLM-MARGIN =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE COLM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE COLM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           WRITE COL-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDCOL WRITE FAILED ' WS-MST-STATUS ' '
                       COLM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
