      *================================================================*
      * PROGRAM  : LDIRA                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE IRA CUSTODY MASTER (IRAMST) FROM A        *
      *            PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#' ARE*
      *            COMMENTS.                                           *
      * INPUT    : IRASEED  PIPE-DELIMITED, 49 FIELDS                  *
      * OUTPUT   : IRAMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-06-28 GAF  ORIGINAL                                       *
      * 1996-11-17 BHS  FMV FEE SCHEDULE UPDATE INC3311                *
      * 1997-03-24 JLM  EXCESS CALCULATION CORRECTED REQ1152           *
      * 1997-09-01 CRP  PERFORMANCE - DISTRIB READ AHEAD REQ6149       *
      * 1999-06-07 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-06-24 BHS  PERFORMANCE - WITHHOLD READ AHEAD PRB6568      *
      * 2003-11-10 RJT  NEW WITHHOLD REPORT COLUMNS REQ9413            *
      * 2006-06-22 PQR  RMD LIMIT REVISED REQ0810                      *
      * 2007-07-01 EWS  ROLLOVER FEE SCHEDULE UPDATE REQ0728           *
      * 2009-02-20 RJT  FMV LIMIT REVISED PRB9550                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDIRA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "IRASEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "IRAMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS IRAM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY IRAMST.

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
           DISPLAY 'LDIRA LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE IRA-MASTER-REC
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
           MOVE WS-F(1) TO IRAM-KEY
           MOVE WS-F(2) TO IRAM-CUST-ID
           MOVE WS-F(3) TO IRAM-NAME
           MOVE WS-F(4) TO IRAM-DESC
           MOVE WS-F(5) TO IRAM-REF-NO
           MOVE WS-F(6) TO IRAM-ADDR
           MOVE WS-F(7) TO IRAM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE IRAM-CONTRIB-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE IRAM-CONTRIB-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE IRAM-DISTRIB-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE IRAM-DISTRIB-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE IRAM-WITHHOLD-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE IRAM-WITHHOLD-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE IRAM-ROLLOVER-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE IRAM-ROLLOVER-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE IRAM-RMD-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE IRAM-RMD-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE IRAM-FMV-AMT =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE IRAM-FMV-YTD =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE IRAM-EXCESS-AMT =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE IRAM-EXCESS-YTD =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE IRAM-LAST-STMT-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE IRAM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE IRAM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE IRAM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE IRAM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           IF WS-F(27) NOT = SPACES
               COMPUTE IRAM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(27))
           END-IF
           IF WS-F(28) NOT = SPACES
               COMPUTE IRAM-LAST-ACT-DATE =
                   FUNCTION NUMVAL(WS-F(28))
           END-IF
           IF WS-F(29) NOT = SPACES
               COMPUTE IRAM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(29))
           END-IF
           IF WS-F(30) NOT = SPACES
               COMPUTE IRAM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(30))
           END-IF
           MOVE WS-F(31) TO IRAM-IRA-TYPE-CD
           MOVE WS-F(32) TO IRAM-DIST-CODE-CD
           MOVE WS-F(33) TO IRAM-CHANNEL-CD
           MOVE WS-F(34) TO IRAM-TYPE-CD
           MOVE WS-F(35) TO IRAM-REGION-CD
           MOVE WS-F(36) TO IRAM-REASON-CD
           MOVE WS-F(37) TO IRAM-SUSPEND-FLAG
           MOVE WS-F(38) TO IRAM-STOP-FLAG
           MOVE WS-F(39) TO IRAM-OVERRIDE-FLAG
           MOVE WS-F(40) TO IRAM-AUTO-RENEW-FLAG
           MOVE WS-F(41) TO IRAM-REVIEW-FLAG
           MOVE WS-F(42) TO IRAM-HOLD-FLAG
           IF WS-F(43) NOT = SPACES
               COMPUTE IRAM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE IRAM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE IRAM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           IF WS-F(46) NOT = SPACES
               COMPUTE IRAM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(46))
           END-IF
           IF WS-F(47) NOT = SPACES
               COMPUTE IRAM-RATE =
                   FUNCTION NUMVAL(WS-F(47))
           END-IF
           IF WS-F(48) NOT = SPACES
               COMPUTE IRAM-MARGIN =
                   FUNCTION NUMVAL(WS-F(48))
           END-IF
           IF WS-F(49) NOT = SPACES
               COMPUTE IRAM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(49))
           END-IF
           WRITE IRA-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDIRA WRITE FAILED ' WS-MST-STATUS ' '
                       IRAM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
