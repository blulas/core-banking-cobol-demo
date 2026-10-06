      *================================================================*
      * PROGRAM  : LDYDB                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE DATE WINDOW BRIDGE MASTER (YDBMST) FROM A *
      *            PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#' ARE*
      *            COMMENTS.                                           *
      * INPUT    : YDBSEED  PIPE-DELIMITED, 41 FIELDS                  *
      * OUTPUT   : YDBMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-20 JLM  ORIGINAL                                       *
      * 1995-12-06 MKL  AMOUNT-2 ROUNDING - HALF UP INC4276            *
      * 1999-11-18 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-02-08 LMB  AUDIT FINDING - AMOUNT-2 CONTROL TOTALS INC1467*
      * 2006-10-25 MKL  CONTROL ROUNDING - HALF UP REQ2278             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDYDB.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "YDBSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "YDBMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS YDBM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY YDBMST.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-MST-STATUS                   PIC X(02).
       01  WS-EOF                          PIC X(01) VALUE 'N'.
       01  WS-LOADED                       PIC 9(07) VALUE 0.
       01  WS-SKIPPED                      PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F OCCURS 41 TIMES PIC X(40).

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
           DISPLAY 'LDYDB LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE YDB-MASTER-REC
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
           END-UNSTRING
           MOVE WS-F(1) TO YDBM-KEY
           MOVE WS-F(2) TO YDBM-CUST-ID
           MOVE WS-F(3) TO YDBM-NAME
           MOVE WS-F(4) TO YDBM-DESC
           MOVE WS-F(5) TO YDBM-REF-NO
           MOVE WS-F(6) TO YDBM-ADDR
           MOVE WS-F(7) TO YDBM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE YDBM-AMOUNT-1-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE YDBM-AMOUNT-1-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE YDBM-AMOUNT-2-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE YDBM-AMOUNT-2-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE YDBM-CONTROL-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE YDBM-CONTROL-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE YDBM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE YDBM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE YDBM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE YDBM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE YDBM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE YDBM-LAST-ACT-DATE =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE YDBM-LAST-STMT-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE YDBM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE YDBM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           MOVE WS-F(23) TO YDBM-WINDOW-RULE-CD
           MOVE WS-F(24) TO YDBM-PIVOT-CD
           MOVE WS-F(25) TO YDBM-REGION-CD
           MOVE WS-F(26) TO YDBM-REASON-CD
           MOVE WS-F(27) TO YDBM-SOURCE-CD
           MOVE WS-F(28) TO YDBM-CYCLE-CD
           MOVE WS-F(29) TO YDBM-REVIEW-FLAG
           MOVE WS-F(30) TO YDBM-NOTICE-SENT-FLAG
           MOVE WS-F(31) TO YDBM-AUTO-RENEW-FLAG
           MOVE WS-F(32) TO YDBM-PREFERRED-FLAG
           MOVE WS-F(33) TO YDBM-OVERRIDE-FLAG
           MOVE WS-F(34) TO YDBM-EXEMPT-FLAG
           IF WS-F(35) NOT = SPACES
               COMPUTE YDBM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(35))
           END-IF
           IF WS-F(36) NOT = SPACES
               COMPUTE YDBM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(36))
           END-IF
           IF WS-F(37) NOT = SPACES
               COMPUTE YDBM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(37))
           END-IF
           IF WS-F(38) NOT = SPACES
               COMPUTE YDBM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(38))
           END-IF
           IF WS-F(39) NOT = SPACES
               COMPUTE YDBM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(39))
           END-IF
           IF WS-F(40) NOT = SPACES
               COMPUTE YDBM-MARGIN =
                   FUNCTION NUMVAL(WS-F(40))
           END-IF
           IF WS-F(41) NOT = SPACES
               COMPUTE YDBM-DISCOUNT =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           WRITE YDB-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDYDB WRITE FAILED ' WS-MST-STATUS ' '
                       YDBM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
