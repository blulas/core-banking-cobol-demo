      *================================================================*
      * PROGRAM  : LDATN                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE ATM NETWORK SETTLEMENT MASTER (ATNMST)    *
      *            FROM A PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH *
      *            '#' ARE COMMENTS.                                   *
      * INPUT    : ATNSEED  PIPE-DELIMITED, 47 FIELDS                  *
      * OUTPUT   : ATNMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-08-13 SKN  ORIGINAL                                       *
      * 1992-11-21 DPS  SETTLE FEE SCHEDULE UPDATE REQ6588             *
      * 1996-01-03 RJT  ADJUST CUTOFF MOVED REQ0206                    *
      * 2001-10-09 EWS  NEW DISPENSED REPORT COLUMNS REQ9784           *
      * 2001-11-17 JLM  SURCHG TABLE EXPANDED INC4785                  *
      * 2002-09-16 EWS  ADJUST EXCEPTIONS TO OPS QUEUE REQ5652         *
      * 2006-05-09 RJT  PERFORMANCE - SETTLE READ AHEAD INC8288        *
      * 2010-12-07 MKL  NEW ADJUST REPORT COLUMNS PRB8153              *
      * 2011-07-27 PQR  ADJUST TABLE EXPANDED CHG3266                  *
      * 2011-07-18 CRP  INTERCHG RERUN PROTECTION CHG0667              *
      * 2012-06-10 PQR  AUDIT FINDING - ADJUST CONTROL TOTALS REQ7569  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDATN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "ATNSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "ATNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ATNM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY ATNMST.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-MST-STATUS                   PIC X(02).
       01  WS-EOF                          PIC X(01) VALUE 'N'.
       01  WS-LOADED                       PIC 9(07) VALUE 0.
       01  WS-SKIPPED                      PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F OCCURS 47 TIMES PIC X(40).

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
           DISPLAY 'LDATN LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE ATN-MASTER-REC
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
           END-UNSTRING
           MOVE WS-F(1) TO ATNM-KEY
           MOVE WS-F(2) TO ATNM-CUST-ID
           MOVE WS-F(3) TO ATNM-NAME
           MOVE WS-F(4) TO ATNM-DESC
           MOVE WS-F(5) TO ATNM-REF-NO
           MOVE WS-F(6) TO ATNM-ADDR
           MOVE WS-F(7) TO ATNM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE ATNM-DISPENSED-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE ATNM-DISPENSED-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE ATNM-INTERCHG-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE ATNM-INTERCHG-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE ATNM-SURCHG-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE ATNM-SURCHG-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE ATNM-SETTLE-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE ATNM-SETTLE-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE ATNM-ADJUST-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE ATNM-ADJUST-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE ATNM-CASSETTE-AMT =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE ATNM-CASSETTE-YTD =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE ATNM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE ATNM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE ATNM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE ATNM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE ATNM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE ATNM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE ATNM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           IF WS-F(27) NOT = SPACES
               COMPUTE ATNM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(27))
           END-IF
           IF WS-F(28) NOT = SPACES
               COMPUTE ATNM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(28))
           END-IF
           MOVE WS-F(29) TO ATNM-NETWORK-CD
           MOVE WS-F(30) TO ATNM-TERMINAL-CD
           MOVE WS-F(31) TO ATNM-REASON-CD
           MOVE WS-F(32) TO ATNM-BRANCH-CD
           MOVE WS-F(33) TO ATNM-REGION-CD
           MOVE WS-F(34) TO ATNM-STATUS-CD
           MOVE WS-F(35) TO ATNM-SUSPEND-FLAG
           MOVE WS-F(36) TO ATNM-STOP-FLAG
           MOVE WS-F(37) TO ATNM-EXEMPT-FLAG
           MOVE WS-F(38) TO ATNM-REVIEW-FLAG
           MOVE WS-F(39) TO ATNM-NOTICE-SENT-FLAG
           MOVE WS-F(40) TO ATNM-HOLD-FLAG
           IF WS-F(41) NOT = SPACES
               COMPUTE ATNM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE ATNM-NOTICE-CNT =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE ATNM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE ATNM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE ATNM-DISCOUNT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           IF WS-F(46) NOT = SPACES
               COMPUTE ATNM-RATE =
                   FUNCTION NUMVAL(WS-F(46))
           END-IF
           IF WS-F(47) NOT = SPACES
               COMPUTE ATNM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(47))
           END-IF
           WRITE ATN-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDATN WRITE FAILED ' WS-MST-STATUS ' '
                       ATNM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
