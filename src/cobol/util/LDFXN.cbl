      *================================================================*
      * PROGRAM  : LDFXN                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE FOREIGN BANKNOTES MASTER (FXNMST) FROM A  *
      *            PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#' ARE*
      *            COMMENTS.                                           *
      * INPUT    : FXNSEED  PIPE-DELIMITED, 45 FIELDS                  *
      * OUTPUT   : FXNMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-26 SKN  ORIGINAL                                       *
      * 1997-06-11 LMB  BUYBACK RERUN PROTECTION PRB8861               *
      * 1997-11-17 RJT  SHIPPING CALCULATION CORRECTED INC0596         *
      * 1997-03-13 GAF  BUYBACK TABLE EXPANDED INC4153                 *
      * 1998-12-17 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-04-03 EWS  USD LIMIT REVISED INC7564                      *
      * 2000-12-11 MKL  MARGIN FEE SCHEDULE UPDATE PRB8161             *
      * 2002-11-08 GAF  SHIPPING STATUS CODES ADDED REQ1595            *
      * 2002-09-13 CRP  FOREIGN CUTOFF MOVED CHG3236                   *
      * 2005-08-16 MKL  BUYBACK EXCEPTIONS TO OPS QUEUE REQ9607        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDFXN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "FXNSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "FXNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS FXNM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY FXNMST.

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
           DISPLAY 'LDFXN LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE FXN-MASTER-REC
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
           MOVE WS-F(1) TO FXNM-KEY
           MOVE WS-F(2) TO FXNM-CUST-ID
           MOVE WS-F(3) TO FXNM-NAME
           MOVE WS-F(4) TO FXNM-DESC
           MOVE WS-F(5) TO FXNM-REF-NO
           MOVE WS-F(6) TO FXNM-ADDR
           MOVE WS-F(7) TO FXNM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE FXNM-FOREIGN-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE FXNM-FOREIGN-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE FXNM-USD-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE FXNM-USD-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE FXNM-MARGIN-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE FXNM-MARGIN-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE FXNM-BUYBACK-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE FXNM-BUYBACK-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE FXNM-SHIPPING-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE FXNM-SHIPPING-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE FXNM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE FXNM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE FXNM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE FXNM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE FXNM-LAST-ACT-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE FXNM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE FXNM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE FXNM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE FXNM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           MOVE WS-F(27) TO FXNM-CURRENCY-CD
           MOVE WS-F(28) TO FXNM-ORDER-TYPE-CD
           MOVE WS-F(29) TO FXNM-TYPE-CD
           MOVE WS-F(30) TO FXNM-BRANCH-CD
           MOVE WS-F(31) TO FXNM-STATUS-CD
           MOVE WS-F(32) TO FXNM-SOURCE-CD
           MOVE WS-F(33) TO FXNM-EXEMPT-FLAG
           MOVE WS-F(34) TO FXNM-PREFERRED-FLAG
           MOVE WS-F(35) TO FXNM-AUTO-RENEW-FLAG
           MOVE WS-F(36) TO FXNM-REVIEW-FLAG
           MOVE WS-F(37) TO FXNM-NOTICE-SENT-FLAG
           MOVE WS-F(38) TO FXNM-OVERRIDE-FLAG
           IF WS-F(39) NOT = SPACES
               COMPUTE FXNM-EXCEPT-CNT =
                   FUNCTION NUMVAL(WS-F(39))
           END-IF
           IF WS-F(40) NOT = SPACES
               COMPUTE FXNM-NOTICE-CNT =
                   FUNCTION NUMVAL(WS-F(40))
           END-IF
           IF WS-F(41) NOT = SPACES
               COMPUTE FXNM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE FXNM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE FXNM-RATE =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE FXNM-MARGIN =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE FXNM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           WRITE FXN-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDFXN WRITE FAILED ' WS-MST-STATUS ' '
                       FXNM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
