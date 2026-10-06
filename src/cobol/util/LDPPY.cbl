      *================================================================*
      * PROGRAM  : LDPPY                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE POSITIVE PAY MASTER (PPYMST) FROM A       *
      *            PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#' ARE*
      *            COMMENTS.                                           *
      * INPUT    : PPYSEED  PIPE-DELIMITED, 45 FIELDS                  *
      * OUTPUT   : PPYMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-10-21 JLM  ORIGINAL                                       *
      * 1993-04-25 TWH  AUDIT FINDING - STALE CONTROL TOTALS PRB2237   *
      * 1995-03-25 PQR  PAID STATUS CODES ADDED REQ3124                *
      * 1996-12-18 LMB  NEW VOID REPORT COLUMNS CHG6318                *
      * 1996-12-07 GAF  EXCEPT EXCEPTIONS TO OPS QUEUE CHG6366         *
      * 1996-03-10 MKL  STALE TABLE EXPANDED CHG8317                   *
      * 1997-05-14 PQR  NEW VOID REPORT COLUMNS CHG5912                *
      * 1999-10-04 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-03-13 BHS  VOID EXCEPTIONS TO OPS QUEUE REQ2734           *
      * 2001-03-04 SKN  STALE ROUNDING - HALF UP CHG4097               *
      * 2003-10-15 SKN  ISSUED ROUNDING - HALF UP REQ9793              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDPPY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "PPYSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "PPYMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS PPYM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY PPYMST.

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
           DISPLAY 'LDPPY LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE PPY-MASTER-REC
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
           MOVE WS-F(1) TO PPYM-KEY
           MOVE WS-F(2) TO PPYM-CUST-ID
           MOVE WS-F(3) TO PPYM-NAME
           MOVE WS-F(4) TO PPYM-DESC
           MOVE WS-F(5) TO PPYM-REF-NO
           MOVE WS-F(6) TO PPYM-ADDR
           MOVE WS-F(7) TO PPYM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE PPYM-ISSUED-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE PPYM-ISSUED-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE PPYM-PAID-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE PPYM-PAID-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE PPYM-EXCEPT-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE PPYM-EXCEPT-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE PPYM-STALE-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE PPYM-STALE-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE PPYM-VOID-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE PPYM-VOID-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE PPYM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE PPYM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE PPYM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE PPYM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE PPYM-LAST-ACT-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE PPYM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE PPYM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE PPYM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE PPYM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           MOVE WS-F(27) TO PPYM-MATCH-RULE-CD
           MOVE WS-F(28) TO PPYM-DECISION-CD
           MOVE WS-F(29) TO PPYM-REGION-CD
           MOVE WS-F(30) TO PPYM-STATUS-CD
           MOVE WS-F(31) TO PPYM-CYCLE-CD
           MOVE WS-F(32) TO PPYM-REASON-CD
           MOVE WS-F(33) TO PPYM-OVERRIDE-FLAG
           MOVE WS-F(34) TO PPYM-EXEMPT-FLAG
           MOVE WS-F(35) TO PPYM-AUTO-RENEW-FLAG
           MOVE WS-F(36) TO PPYM-NOTICE-SENT-FLAG
           MOVE WS-F(37) TO PPYM-STOP-FLAG
           MOVE WS-F(38) TO PPYM-HOLD-FLAG
           IF WS-F(39) NOT = SPACES
               COMPUTE PPYM-EXCEPT-CNT =
                   FUNCTION NUMVAL(WS-F(39))
           END-IF
           IF WS-F(40) NOT = SPACES
               COMPUTE PPYM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(40))
           END-IF
           IF WS-F(41) NOT = SPACES
               COMPUTE PPYM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE PPYM-NOTICE-CNT =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE PPYM-RATE =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE PPYM-MARGIN =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE PPYM-DISCOUNT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           WRITE PPY-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDPPY WRITE FAILED ' WS-MST-STATUS ' '
                       PPYM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
