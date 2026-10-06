      *================================================================*
      * PROGRAM  : LDMOR                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE MONEY ORDERS MASTER (MORMST) FROM A       *
      *            PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#' ARE*
      *            COMMENTS.                                           *
      * INPUT    : MORSEED  PIPE-DELIMITED, 47 FIELDS                  *
      * OUTPUT   : MORMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-08 JLM  ORIGINAL                                       *
      * 1993-11-24 EWS  VOIDED ROUNDING - HALF UP INC3001              *
      * 1993-01-11 CRP  OUTSTAND STATUS CODES ADDED REQ6592            *
      * 1993-10-07 GAF  FEE NOTICE TEXT PER LEGAL REQ2215              *
      * 1994-02-01 JLM  FEE TABLE EXPANDED INC5698                     *
      * 1997-02-12 RJT  NEW VOIDED REPORT COLUMNS INC5932              *
      * 1998-05-25 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-12-04 JLM  FEE RERUN PROTECTION PRB8172                   *
      * 2002-09-21 PQR  FACE FEE SCHEDULE UPDATE REQ1456               *
      * 2004-11-23 BHS  VOIDED CUTOFF MOVED REQ8278                    *
      * 2004-10-07 MKL  AUDIT FINDING - FEE CONTROL TOTALS INC7981     *
      * 2005-06-25 TWH  FACE CUTOFF MOVED PRB7269                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDMOR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "MORSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "MORMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS MORM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY MORMST.

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
           DISPLAY 'LDMOR LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE MOR-MASTER-REC
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
           MOVE WS-F(1) TO MORM-KEY
           MOVE WS-F(2) TO MORM-CUST-ID
           MOVE WS-F(3) TO MORM-NAME
           MOVE WS-F(4) TO MORM-DESC
           MOVE WS-F(5) TO MORM-REF-NO
           MOVE WS-F(6) TO MORM-ADDR
           MOVE WS-F(7) TO MORM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE MORM-FACE-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE MORM-FACE-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE MORM-FEE-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE MORM-FEE-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE MORM-PAID-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE MORM-PAID-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE MORM-OUTSTAND-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE MORM-OUTSTAND-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE MORM-ESCHEAT-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE MORM-ESCHEAT-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE MORM-VOIDED-AMT =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE MORM-VOIDED-YTD =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE MORM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE MORM-LAST-STMT-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE MORM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE MORM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE MORM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE MORM-LAST-ACT-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE MORM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           IF WS-F(27) NOT = SPACES
               COMPUTE MORM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(27))
           END-IF
           IF WS-F(28) NOT = SPACES
               COMPUTE MORM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(28))
           END-IF
           MOVE WS-F(29) TO MORM-ITEM-TYPE-CD
           MOVE WS-F(30) TO MORM-PAYEE-TYPE-CD
           MOVE WS-F(31) TO MORM-REASON-CD
           MOVE WS-F(32) TO MORM-TYPE-CD
           MOVE WS-F(33) TO MORM-CHANNEL-CD
           MOVE WS-F(34) TO MORM-CYCLE-CD
           MOVE WS-F(35) TO MORM-PREFERRED-FLAG
           MOVE WS-F(36) TO MORM-OVERRIDE-FLAG
           MOVE WS-F(37) TO MORM-AUTO-RENEW-FLAG
           MOVE WS-F(38) TO MORM-REVIEW-FLAG
           MOVE WS-F(39) TO MORM-EXEMPT-FLAG
           MOVE WS-F(40) TO MORM-STOP-FLAG
           IF WS-F(41) NOT = SPACES
               COMPUTE MORM-NOTICE-CNT =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE MORM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE MORM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE MORM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE MORM-DISCOUNT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           IF WS-F(46) NOT = SPACES
               COMPUTE MORM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(46))
           END-IF
           IF WS-F(47) NOT = SPACES
               COMPUTE MORM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(47))
           END-IF
           WRITE MOR-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDMOR WRITE FAILED ' WS-MST-STATUS ' '
                       MORM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
