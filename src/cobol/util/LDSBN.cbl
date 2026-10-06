      *================================================================*
      * PROGRAM  : LDSBN                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE SAVINGS BOND SERVICES MASTER (SBNMST) FROM*
      *            A PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#'  *
      *            ARE COMMENTS.                                       *
      * INPUT    : SBNSEED  PIPE-DELIMITED, 49 FIELDS                  *
      * OUTPUT   : SBNMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-07-15 JLM  ORIGINAL                                       *
      * 2003-12-14 BHS  FACE CALCULATION CORRECTED REQ1547             *
      * 2005-03-17 BHS  FACE ROUNDING - HALF UP REQ3980                *
      * 2007-10-20 TWH  INTEREST RERUN PROTECTION CHG9240              *
      * 2007-04-13 BHS  PURCHASE CUTOFF MOVED REQ5280                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDSBN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "SBNSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "SBNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS SBNM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY SBNMST.

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
           DISPLAY 'LDSBN LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE SBN-MASTER-REC
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
           MOVE WS-F(1) TO SBNM-KEY
           MOVE WS-F(2) TO SBNM-CUST-ID
           MOVE WS-F(3) TO SBNM-NAME
           MOVE WS-F(4) TO SBNM-DESC
           MOVE WS-F(5) TO SBNM-REF-NO
           MOVE WS-F(6) TO SBNM-ADDR
           MOVE WS-F(7) TO SBNM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE SBNM-FACE-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE SBNM-FACE-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE SBNM-REDEEM-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE SBNM-REDEEM-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE SBNM-INTEREST-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE SBNM-INTEREST-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE SBNM-PURCHASE-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE SBNM-PURCHASE-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE SBNM-ACCRUAL-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE SBNM-ACCRUAL-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE SBNM-TAXABLE-AMT =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE SBNM-TAXABLE-YTD =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE SBNM-ISSUE-PRICE-AMT =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE SBNM-ISSUE-PRICE-YTD =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE SBNM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE SBNM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE SBNM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE SBNM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE SBNM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           IF WS-F(27) NOT = SPACES
               COMPUTE SBNM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(27))
           END-IF
           IF WS-F(28) NOT = SPACES
               COMPUTE SBNM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(28))
           END-IF
           IF WS-F(29) NOT = SPACES
               COMPUTE SBNM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(29))
           END-IF
           IF WS-F(30) NOT = SPACES
               COMPUTE SBNM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(30))
           END-IF
           MOVE WS-F(31) TO SBNM-SERIES-CD
           MOVE WS-F(32) TO SBNM-ISSUE-TYPE-CD
           MOVE WS-F(33) TO SBNM-SOURCE-CD
           MOVE WS-F(34) TO SBNM-STATUS-CD
           MOVE WS-F(35) TO SBNM-REASON-CD
           MOVE WS-F(36) TO SBNM-BRANCH-CD
           MOVE WS-F(37) TO SBNM-AUTO-RENEW-FLAG
           MOVE WS-F(38) TO SBNM-NOTICE-SENT-FLAG
           MOVE WS-F(39) TO SBNM-PREFERRED-FLAG
           MOVE WS-F(40) TO SBNM-OVERRIDE-FLAG
           MOVE WS-F(41) TO SBNM-HOLD-FLAG
           MOVE WS-F(42) TO SBNM-REVIEW-FLAG
           IF WS-F(43) NOT = SPACES
               COMPUTE SBNM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE SBNM-EXCEPT-CNT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE SBNM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           IF WS-F(46) NOT = SPACES
               COMPUTE SBNM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(46))
           END-IF
           IF WS-F(47) NOT = SPACES
               COMPUTE SBNM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(47))
           END-IF
           IF WS-F(48) NOT = SPACES
               COMPUTE SBNM-MARGIN =
                   FUNCTION NUMVAL(WS-F(48))
           END-IF
           IF WS-F(49) NOT = SPACES
               COMPUTE SBNM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(49))
           END-IF
           WRITE SBN-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDSBN WRITE FAILED ' WS-MST-STATUS ' '
                       SBNM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
