      *================================================================*
      * PROGRAM  : LDBPY                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE BILL PAYMENT SERVICE MASTER (BPYMST) FROM *
      *            A PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#'  *
      *            ARE COMMENTS.                                       *
      * INPUT    : BPYSEED  PIPE-DELIMITED, 43 FIELDS                  *
      * OUTPUT   : BPYMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-03-16 BHS  ORIGINAL                                       *
      * 1993-03-26 RJT  FEE NOTICE TEXT PER LEGAL REQ8628              *
      * 1995-02-06 RJT  PERFORMANCE - FEE READ AHEAD CHG8789           *
      * 1997-01-21 JLM  RETURNED CUTOFF MOVED CHG9523                  *
      * 1998-10-04 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-12-28 MKL  RETURNED FEE SCHEDULE UPDATE REQ1511           *
      * 2001-12-21 RJT  PAYMENT RERUN PROTECTION REQ4445               *
      * 2003-02-12 JLM  RETURNED ROUNDING - HALF UP INC0540            *
      * 2003-01-15 GAF  REFUND LIMIT REVISED INC6062                   *
      * 2004-11-12 JLM  RETURNED NOTICE TEXT PER LEGAL REQ8624         *
      * 2006-12-07 PQR  PERFORMANCE - PAYMENT READ AHEAD CHG9460       *
      * 2007-05-13 GAF  RETURNED EXCEPTIONS TO OPS QUEUE CHG8051       *
      * 2007-07-16 LMB  FEE EXCEPTIONS TO OPS QUEUE PRB1144            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDBPY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "BPYSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "BPYMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS BPYM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY BPYMST.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-MST-STATUS                   PIC X(02).
       01  WS-EOF                          PIC X(01) VALUE 'N'.
       01  WS-LOADED                       PIC 9(07) VALUE 0.
       01  WS-SKIPPED                      PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F OCCURS 43 TIMES PIC X(40).

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
           DISPLAY 'LDBPY LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE BPY-MASTER-REC
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
           END-UNSTRING
           MOVE WS-F(1) TO BPYM-KEY
           MOVE WS-F(2) TO BPYM-CUST-ID
           MOVE WS-F(3) TO BPYM-NAME
           MOVE WS-F(4) TO BPYM-DESC
           MOVE WS-F(5) TO BPYM-REF-NO
           MOVE WS-F(6) TO BPYM-ADDR
           MOVE WS-F(7) TO BPYM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE BPYM-PAYMENT-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE BPYM-PAYMENT-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE BPYM-FEE-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE BPYM-FEE-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE BPYM-RETURNED-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE BPYM-RETURNED-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE BPYM-REFUND-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE BPYM-REFUND-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE BPYM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE BPYM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE BPYM-LAST-STMT-DATE =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE BPYM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE BPYM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE BPYM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE BPYM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE BPYM-LAST-ACT-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE BPYM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           MOVE WS-F(25) TO BPYM-PAYEE-TYPE-CD
           MOVE WS-F(26) TO BPYM-REMIT-METHOD-CD
           MOVE WS-F(27) TO BPYM-REASON-CD
           MOVE WS-F(28) TO BPYM-CYCLE-CD
           MOVE WS-F(29) TO BPYM-STATUS-CD
           MOVE WS-F(30) TO BPYM-SOURCE-CD
           MOVE WS-F(31) TO BPYM-OVERRIDE-FLAG
           MOVE WS-F(32) TO BPYM-AUTO-RENEW-FLAG
           MOVE WS-F(33) TO BPYM-SUSPEND-FLAG
           MOVE WS-F(34) TO BPYM-EXEMPT-FLAG
           MOVE WS-F(35) TO BPYM-PREFERRED-FLAG
           MOVE WS-F(36) TO BPYM-HOLD-FLAG
           IF WS-F(37) NOT = SPACES
               COMPUTE BPYM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(37))
           END-IF
           IF WS-F(38) NOT = SPACES
               COMPUTE BPYM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(38))
           END-IF
           IF WS-F(39) NOT = SPACES
               COMPUTE BPYM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(39))
           END-IF
           IF WS-F(40) NOT = SPACES
               COMPUTE BPYM-EXCEPT-CNT =
                   FUNCTION NUMVAL(WS-F(40))
           END-IF
           IF WS-F(41) NOT = SPACES
               COMPUTE BPYM-RATE =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE BPYM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE BPYM-MARGIN =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           WRITE BPY-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDBPY WRITE FAILED ' WS-MST-STATUS ' '
                       BPYM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
