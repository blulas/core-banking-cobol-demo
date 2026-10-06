      *================================================================*
      * PROGRAM  : LDXCL                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE CHRISTMAS CLUB SAVINGS MASTER (XCLMST)    *
      *            FROM A PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH *
      *            '#' ARE COMMENTS.                                   *
      * INPUT    : XCLSEED  PIPE-DELIMITED, 51 FIELDS                  *
      * OUTPUT   : XCLMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-08-26 EWS  ORIGINAL                                       *
      * 1994-10-23 BHS  ACCRUED CALCULATION CORRECTED INC7314          *
      * 1994-09-28 SKN  CONTRIB STATUS CODES ADDED INC8412             *
      * 1995-05-23 JLM  WEEKLY FEE SCHEDULE UPDATE PRB2647             *
      * 1997-12-05 TWH  BONUS CUTOFF MOVED PRB2852                     *
      * 1997-04-04 LMB  BONUS LIMIT REVISED REQ7876                    *
      * 1999-05-12 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-07-05 CRP  PENALTY CUTOFF MOVED REQ4448                   *
      * 1999-06-08 TWH  PAYOUT RERUN PROTECTION INC9304                *
      * 2000-11-17 JLM  WEEKLY RERUN PROTECTION REQ6079                *
      * 2000-03-27 JLM  CONTRIB ROUNDING - HALF UP INC2966             *
      * 2003-10-14 DPS  CONTRIB ROUNDING - HALF UP CHG6626             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDXCL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "XCLSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "XCLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS XCLM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY XCLMST.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-MST-STATUS                   PIC X(02).
       01  WS-EOF                          PIC X(01) VALUE 'N'.
       01  WS-LOADED                       PIC 9(07) VALUE 0.
       01  WS-SKIPPED                      PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F OCCURS 51 TIMES PIC X(40).

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
           DISPLAY 'LDXCL LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE XCL-MASTER-REC
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
                    WS-F(50)
                    WS-F(51)
           END-UNSTRING
           MOVE WS-F(1) TO XCLM-KEY
           MOVE WS-F(2) TO XCLM-CUST-ID
           MOVE WS-F(3) TO XCLM-NAME
           MOVE WS-F(4) TO XCLM-DESC
           MOVE WS-F(5) TO XCLM-REF-NO
           MOVE WS-F(6) TO XCLM-ADDR
           MOVE WS-F(7) TO XCLM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE XCLM-GOAL-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE XCLM-GOAL-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE XCLM-WEEKLY-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE XCLM-WEEKLY-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE XCLM-PAYOUT-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE XCLM-PAYOUT-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE XCLM-BONUS-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE XCLM-BONUS-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE XCLM-PENALTY-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE XCLM-PENALTY-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE XCLM-ACCRUED-AMT =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE XCLM-ACCRUED-YTD =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE XCLM-CONTRIB-AMT =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE XCLM-CONTRIB-YTD =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE XCLM-MISSED-AMT =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE XCLM-MISSED-YTD =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE XCLM-LAST-ACT-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE XCLM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE XCLM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           IF WS-F(27) NOT = SPACES
               COMPUTE XCLM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(27))
           END-IF
           IF WS-F(28) NOT = SPACES
               COMPUTE XCLM-LAST-STMT-DATE =
                   FUNCTION NUMVAL(WS-F(28))
           END-IF
           IF WS-F(29) NOT = SPACES
               COMPUTE XCLM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(29))
           END-IF
           IF WS-F(30) NOT = SPACES
               COMPUTE XCLM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(30))
           END-IF
           IF WS-F(31) NOT = SPACES
               COMPUTE XCLM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(31))
           END-IF
           IF WS-F(32) NOT = SPACES
               COMPUTE XCLM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(32))
           END-IF
           MOVE WS-F(33) TO XCLM-PLAN-CD
           MOVE WS-F(34) TO XCLM-PAYOUT-METHOD-CD
           MOVE WS-F(35) TO XCLM-TYPE-CD
           MOVE WS-F(36) TO XCLM-REASON-CD
           MOVE WS-F(37) TO XCLM-CYCLE-CD
           MOVE WS-F(38) TO XCLM-REGION-CD
           MOVE WS-F(39) TO XCLM-EXEMPT-FLAG
           MOVE WS-F(40) TO XCLM-PREFERRED-FLAG
           MOVE WS-F(41) TO XCLM-AUTO-RENEW-FLAG
           MOVE WS-F(42) TO XCLM-STOP-FLAG
           MOVE WS-F(43) TO XCLM-OVERRIDE-FLAG
           MOVE WS-F(44) TO XCLM-SUSPEND-FLAG
           IF WS-F(45) NOT = SPACES
               COMPUTE XCLM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           IF WS-F(46) NOT = SPACES
               COMPUTE XCLM-NOTICE-CNT =
                   FUNCTION NUMVAL(WS-F(46))
           END-IF
           IF WS-F(47) NOT = SPACES
               COMPUTE XCLM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(47))
           END-IF
           IF WS-F(48) NOT = SPACES
               COMPUTE XCLM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(48))
           END-IF
           IF WS-F(49) NOT = SPACES
               COMPUTE XCLM-DISCOUNT =
                   FUNCTION NUMVAL(WS-F(49))
           END-IF
           IF WS-F(50) NOT = SPACES
               COMPUTE XCLM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(50))
           END-IF
           IF WS-F(51) NOT = SPACES
               COMPUTE XCLM-RATE =
                   FUNCTION NUMVAL(WS-F(51))
           END-IF
           WRITE XCL-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDXCL WRITE FAILED ' WS-MST-STATUS ' '
                       XCLM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
