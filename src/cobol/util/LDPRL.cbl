      *================================================================*
      * PROGRAM  : LDPRL                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE PAYROLL SERVICES MASTER (PRLMST) FROM A   *
      *            PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#' ARE*
      *            COMMENTS.                                           *
      * INPUT    : PRLSEED  PIPE-DELIMITED, 47 FIELDS                  *
      * OUTPUT   : PRLMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-01-22 PQR  ORIGINAL                                       *
      * 1989-07-16 TWH  DEDUCT ROUNDING - HALF UP CHG9671              *
      * 1992-06-17 MKL  NET EXCEPTIONS TO OPS QUEUE PRB9896            *
      * 1994-10-06 BHS  FICA FEE SCHEDULE UPDATE CHG2981               *
      * 1995-03-11 PQR  FICA CUTOFF MOVED PRB9375                      *
      * 1995-05-06 CRP  FICA STATUS CODES ADDED REQ4216                *
      * 1996-11-04 DPS  PERFORMANCE - STATE-TAX READ AHEAD PRB4747     *
      * 1997-10-07 RJT  NET CALCULATION CORRECTED REQ4101              *
      * 1998-10-28 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-08-09 MKL  AUDIT FINDING - GROSS CONTROL TOTALS INC6369   *
      * 2011-10-01 PQR  FED-TAX RERUN PROTECTION INC1164               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDPRL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "PRLSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "PRLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS PRLM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY PRLMST.

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
           DISPLAY 'LDPRL LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE PRL-MASTER-REC
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
           MOVE WS-F(1) TO PRLM-KEY
           MOVE WS-F(2) TO PRLM-CUST-ID
           MOVE WS-F(3) TO PRLM-NAME
           MOVE WS-F(4) TO PRLM-DESC
           MOVE WS-F(5) TO PRLM-REF-NO
           MOVE WS-F(6) TO PRLM-ADDR
           MOVE WS-F(7) TO PRLM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE PRLM-GROSS-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE PRLM-GROSS-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE PRLM-NET-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE PRLM-NET-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE PRLM-FED-TAX-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE PRLM-FED-TAX-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE PRLM-STATE-TAX-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE PRLM-STATE-TAX-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE PRLM-FICA-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE PRLM-FICA-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE PRLM-DEDUCT-AMT =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE PRLM-DEDUCT-YTD =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE PRLM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE PRLM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE PRLM-LAST-ACT-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE PRLM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE PRLM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE PRLM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE PRLM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           IF WS-F(27) NOT = SPACES
               COMPUTE PRLM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(27))
           END-IF
           IF WS-F(28) NOT = SPACES
               COMPUTE PRLM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(28))
           END-IF
           MOVE WS-F(29) TO PRLM-PAY-FREQ-CD
           MOVE WS-F(30) TO PRLM-TAX-STATE-CD
           MOVE WS-F(31) TO PRLM-STATUS-CD
           MOVE WS-F(32) TO PRLM-SOURCE-CD
           MOVE WS-F(33) TO PRLM-REGION-CD
           MOVE WS-F(34) TO PRLM-REASON-CD
           MOVE WS-F(35) TO PRLM-PREFERRED-FLAG
           MOVE WS-F(36) TO PRLM-AUTO-RENEW-FLAG
           MOVE WS-F(37) TO PRLM-HOLD-FLAG
           MOVE WS-F(38) TO PRLM-STOP-FLAG
           MOVE WS-F(39) TO PRLM-NOTICE-SENT-FLAG
           MOVE WS-F(40) TO PRLM-SUSPEND-FLAG
           IF WS-F(41) NOT = SPACES
               COMPUTE PRLM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE PRLM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE PRLM-NOTICE-CNT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE PRLM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE PRLM-MARGIN =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           IF WS-F(46) NOT = SPACES
               COMPUTE PRLM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(46))
           END-IF
           IF WS-F(47) NOT = SPACES
               COMPUTE PRLM-RATE =
                   FUNCTION NUMVAL(WS-F(47))
           END-IF
           WRITE PRL-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDPRL WRITE FAILED ' WS-MST-STATUS ' '
                       PRLM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
