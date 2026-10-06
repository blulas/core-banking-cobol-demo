      *================================================================*
      * PROGRAM  : LDHEL                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE HOME EQUITY LINES MASTER (HELMST) FROM A  *
      *            PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#' ARE*
      *            COMMENTS.                                           *
      * INPUT    : HELSEED  PIPE-DELIMITED, 49 FIELDS                  *
      * OUTPUT   : HELMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-20 EWS  ORIGINAL                                       *
      * 1992-07-09 CRP  APPRAISE CALCULATION CORRECTED PRB2908         *
      * 1993-05-04 RJT  BALANCE EXCEPTIONS TO OPS QUEUE REQ2134        *
      * 1999-08-11 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-10-19 SKN  DRAW TABLE EXPANDED PRB2496                    *
      * 2001-10-09 GAF  MIN-PAY STATUS CODES ADDED PRB8263             *
      * 2001-04-14 CRP  APPRAISE ROUNDING - HALF UP PRB5451            *
      * 2002-04-28 GAF  APPRAISE STATUS CODES ADDED PRB1460            *
      * 2008-02-04 BHS  MIN-PAY FEE SCHEDULE UPDATE CHG6997            *
      * 2008-11-14 LMB  APPRAISE ROUNDING - HALF UP REQ5558            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDHEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "HELSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "HELMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS HELM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY HELMST.

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
           DISPLAY 'LDHEL LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE HEL-MASTER-REC
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
           MOVE WS-F(1) TO HELM-KEY
           MOVE WS-F(2) TO HELM-CUST-ID
           MOVE WS-F(3) TO HELM-NAME
           MOVE WS-F(4) TO HELM-DESC
           MOVE WS-F(5) TO HELM-REF-NO
           MOVE WS-F(6) TO HELM-ADDR
           MOVE WS-F(7) TO HELM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE HELM-LIMIT-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE HELM-LIMIT-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE HELM-DRAW-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE HELM-DRAW-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE HELM-BALANCE-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE HELM-BALANCE-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE HELM-INTEREST-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE HELM-INTEREST-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE HELM-MIN-PAY-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE HELM-MIN-PAY-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE HELM-ANNUAL-FEE-AMT =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE HELM-ANNUAL-FEE-YTD =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE HELM-APPRAISE-AMT =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE HELM-APPRAISE-YTD =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE HELM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE HELM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE HELM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE HELM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE HELM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           IF WS-F(27) NOT = SPACES
               COMPUTE HELM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(27))
           END-IF
           IF WS-F(28) NOT = SPACES
               COMPUTE HELM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(28))
           END-IF
           IF WS-F(29) NOT = SPACES
               COMPUTE HELM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(29))
           END-IF
           IF WS-F(30) NOT = SPACES
               COMPUTE HELM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(30))
           END-IF
           MOVE WS-F(31) TO HELM-LIEN-POS-CD
           MOVE WS-F(32) TO HELM-DRAW-PERIOD-CD
           MOVE WS-F(33) TO HELM-TYPE-CD
           MOVE WS-F(34) TO HELM-REGION-CD
           MOVE WS-F(35) TO HELM-STATUS-CD
           MOVE WS-F(36) TO HELM-REASON-CD
           MOVE WS-F(37) TO HELM-SUSPEND-FLAG
           MOVE WS-F(38) TO HELM-OVERRIDE-FLAG
           MOVE WS-F(39) TO HELM-NOTICE-SENT-FLAG
           MOVE WS-F(40) TO HELM-HOLD-FLAG
           MOVE WS-F(41) TO HELM-AUTO-RENEW-FLAG
           MOVE WS-F(42) TO HELM-PREFERRED-FLAG
           IF WS-F(43) NOT = SPACES
               COMPUTE HELM-EXCEPT-CNT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE HELM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE HELM-NOTICE-CNT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           IF WS-F(46) NOT = SPACES
               COMPUTE HELM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(46))
           END-IF
           IF WS-F(47) NOT = SPACES
               COMPUTE HELM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(47))
           END-IF
           IF WS-F(48) NOT = SPACES
               COMPUTE HELM-MARGIN =
                   FUNCTION NUMVAL(WS-F(48))
           END-IF
           IF WS-F(49) NOT = SPACES
               COMPUTE HELM-DISCOUNT =
                   FUNCTION NUMVAL(WS-F(49))
           END-IF
           WRITE HEL-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDHEL WRITE FAILED ' WS-MST-STATUS ' '
                       HELM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
