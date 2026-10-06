      *================================================================*
      * PROGRAM  : LDCRC                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE PROPRIETARY CREDIT CARD MASTER (CRCMST)   *
      *            FROM A PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH *
      *            '#' ARE COMMENTS.                                   *
      * INPUT    : CRCSEED  PIPE-DELIMITED, 49 FIELDS                  *
      * OUTPUT   : CRCMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-02-27 GAF  ORIGINAL                                       *
      * 1996-07-26 TWH  LIMIT ROUNDING - HALF UP PRB3447               *
      * 1998-02-24 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-12-20 LMB  AUDIT FINDING - LIMIT CONTROL TOTALS CHG4874   *
      * 1999-02-22 CRP  LIMIT EXCEPTIONS TO OPS QUEUE CHG0673          *
      * 2000-11-11 EWS  LIMIT RERUN PROTECTION CHG3045                 *
      * 2001-06-07 CRP  NEW PURCHASE REPORT COLUMNS PRB3761            *
      * 2002-06-16 LMB  PURCHASE FEE SCHEDULE UPDATE INC5461           *
      * 2003-11-04 EWS  PURCHASE STATUS CODES ADDED PRB0279            *
      * 2003-09-15 JLM  PAYMENT STATUS CODES ADDED REQ3951             *
      * 2005-06-27 RJT  CASH-ADV CUTOFF MOVED INC6808                  *
      * 2007-04-07 DPS  MIN-DUE CALCULATION CORRECTED PRB4485          *
      * 2007-11-04 PQR  CASH-ADV FEE SCHEDULE UPDATE PRB7754           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDCRC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "CRCSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "CRCMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CRCM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY CRCMST.

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
           DISPLAY 'LDCRC LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE CRC-MASTER-REC
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
           MOVE WS-F(1) TO CRCM-KEY
           MOVE WS-F(2) TO CRCM-CUST-ID
           MOVE WS-F(3) TO CRCM-NAME
           MOVE WS-F(4) TO CRCM-DESC
           MOVE WS-F(5) TO CRCM-REF-NO
           MOVE WS-F(6) TO CRCM-ADDR
           MOVE WS-F(7) TO CRCM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE CRCM-LIMIT-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE CRCM-LIMIT-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE CRCM-PURCHASE-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE CRCM-PURCHASE-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE CRCM-CASH-ADV-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE CRCM-CASH-ADV-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE CRCM-FINANCE-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE CRCM-FINANCE-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE CRCM-MIN-DUE-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE CRCM-MIN-DUE-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE CRCM-LATE-FEE-AMT =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE CRCM-LATE-FEE-YTD =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE CRCM-PAYMENT-AMT =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE CRCM-PAYMENT-YTD =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE CRCM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE CRCM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE CRCM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE CRCM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE CRCM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           IF WS-F(27) NOT = SPACES
               COMPUTE CRCM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(27))
           END-IF
           IF WS-F(28) NOT = SPACES
               COMPUTE CRCM-LAST-ACT-DATE =
                   FUNCTION NUMVAL(WS-F(28))
           END-IF
           IF WS-F(29) NOT = SPACES
               COMPUTE CRCM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(29))
           END-IF
           IF WS-F(30) NOT = SPACES
               COMPUTE CRCM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(30))
           END-IF
           MOVE WS-F(31) TO CRCM-CARD-TYPE-CD
           MOVE WS-F(32) TO CRCM-BILL-CYCLE-CD
           MOVE WS-F(33) TO CRCM-TYPE-CD
           MOVE WS-F(34) TO CRCM-SOURCE-CD
           MOVE WS-F(35) TO CRCM-STATUS-CD
           MOVE WS-F(36) TO CRCM-BRANCH-CD
           MOVE WS-F(37) TO CRCM-EXEMPT-FLAG
           MOVE WS-F(38) TO CRCM-STOP-FLAG
           MOVE WS-F(39) TO CRCM-NOTICE-SENT-FLAG
           MOVE WS-F(40) TO CRCM-OVERRIDE-FLAG
           MOVE WS-F(41) TO CRCM-AUTO-RENEW-FLAG
           MOVE WS-F(42) TO CRCM-PREFERRED-FLAG
           IF WS-F(43) NOT = SPACES
               COMPUTE CRCM-NOTICE-CNT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE CRCM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE CRCM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           IF WS-F(46) NOT = SPACES
               COMPUTE CRCM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(46))
           END-IF
           IF WS-F(47) NOT = SPACES
               COMPUTE CRCM-DISCOUNT =
                   FUNCTION NUMVAL(WS-F(47))
           END-IF
           IF WS-F(48) NOT = SPACES
               COMPUTE CRCM-RATE =
                   FUNCTION NUMVAL(WS-F(48))
           END-IF
           IF WS-F(49) NOT = SPACES
               COMPUTE CRCM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(49))
           END-IF
           WRITE CRC-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDCRC WRITE FAILED ' WS-MST-STATUS ' '
                       CRCM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
