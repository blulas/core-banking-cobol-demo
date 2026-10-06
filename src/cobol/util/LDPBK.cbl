      *================================================================*
      * PROGRAM  : LDPBK                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE PASSBOOK PRINTING MASTER (PBKMST) FROM A  *
      *            PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#' ARE*
      *            COMMENTS.                                           *
      * INPUT    : PBKSEED  PIPE-DELIMITED, 47 FIELDS                  *
      * OUTPUT   : PBKMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-01-18 BHS  ORIGINAL                                       *
      * 1997-11-22 TWH  UNPOSTED CALCULATION CORRECTED INC2325         *
      * 1999-06-03 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-18 SKN  LINE ROUNDING - HALF UP CHG9850                *
      * 2003-05-04 TWH  INTEREST FEE SCHEDULE UPDATE PRB4746           *
      * 2005-02-18 EWS  CARRYFWD RERUN PROTECTION REQ3528              *
      * 2008-01-13 SKN  INTEREST NOTICE TEXT PER LEGAL INC7253         *
      * 2011-11-08 BHS  INTEREST RERUN PROTECTION CHG0264              *
      * 2012-06-19 CRP  UNPOSTED EXCEPTIONS TO OPS QUEUE PRB2688       *
      * 2013-12-11 GAF  AUDIT FINDING - UNPOSTED CONTROL TOTALS REQ9846*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDPBK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "PBKSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "PBKMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS PBKM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY PBKMST.

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
           DISPLAY 'LDPBK LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE PBK-MASTER-REC
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
           MOVE WS-F(1) TO PBKM-KEY
           MOVE WS-F(2) TO PBKM-CUST-ID
           MOVE WS-F(3) TO PBKM-NAME
           MOVE WS-F(4) TO PBKM-DESC
           MOVE WS-F(5) TO PBKM-REF-NO
           MOVE WS-F(6) TO PBKM-ADDR
           MOVE WS-F(7) TO PBKM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE PBKM-LINE-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE PBKM-LINE-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE PBKM-UNPOSTED-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE PBKM-UNPOSTED-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE PBKM-PRINTED-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE PBKM-PRINTED-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE PBKM-CARRYFWD-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE PBKM-CARRYFWD-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE PBKM-INTEREST-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE PBKM-INTEREST-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE PBKM-BALANCE-AMT =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE PBKM-BALANCE-YTD =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE PBKM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE PBKM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE PBKM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE PBKM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE PBKM-LAST-ACT-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE PBKM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE PBKM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           IF WS-F(27) NOT = SPACES
               COMPUTE PBKM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(27))
           END-IF
           IF WS-F(28) NOT = SPACES
               COMPUTE PBKM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(28))
           END-IF
           MOVE WS-F(29) TO PBKM-BOOK-TYPE-CD
           MOVE WS-F(30) TO PBKM-PRINTER-CD
           MOVE WS-F(31) TO PBKM-BRANCH-CD
           MOVE WS-F(32) TO PBKM-SOURCE-CD
           MOVE WS-F(33) TO PBKM-CHANNEL-CD
           MOVE WS-F(34) TO PBKM-REGION-CD
           MOVE WS-F(35) TO PBKM-EXEMPT-FLAG
           MOVE WS-F(36) TO PBKM-NOTICE-SENT-FLAG
           MOVE WS-F(37) TO PBKM-AUTO-RENEW-FLAG
           MOVE WS-F(38) TO PBKM-SUSPEND-FLAG
           MOVE WS-F(39) TO PBKM-HOLD-FLAG
           MOVE WS-F(40) TO PBKM-REVIEW-FLAG
           IF WS-F(41) NOT = SPACES
               COMPUTE PBKM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE PBKM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE PBKM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE PBKM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE PBKM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           IF WS-F(46) NOT = SPACES
               COMPUTE PBKM-MARGIN =
                   FUNCTION NUMVAL(WS-F(46))
           END-IF
           IF WS-F(47) NOT = SPACES
               COMPUTE PBKM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(47))
           END-IF
           WRITE PBK-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDPBK WRITE FAILED ' WS-MST-STATUS ' '
                       PBKM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
