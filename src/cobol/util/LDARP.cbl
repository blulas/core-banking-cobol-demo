      *================================================================*
      * PROGRAM  : LDARP                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE ACCOUNT RECONCILEMENT MASTER (ARPMST) FROM*
      *            A PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#'  *
      *            ARE COMMENTS.                                       *
      * INPUT    : ARPSEED  PIPE-DELIMITED, 47 FIELDS                  *
      * OUTPUT   : ARPMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-24 DPS  ORIGINAL                                       *
      * 2002-06-18 JLM  ISSUED FEE SCHEDULE UPDATE PRB2928             *
      * 2003-05-09 EWS  OUTSTAND RERUN PROTECTION REQ4156              *
      * 2004-10-12 BHS  PAID RERUN PROTECTION REQ7206                  *
      * 2008-02-08 RJT  VOID TABLE EXPANDED CHG4653                    *
      * 2009-06-13 MKL  VOID LIMIT REVISED PRB1258                     *
      * 2009-03-17 JLM  VOID ROUNDING - HALF UP INC6505                *
      * 2013-05-22 TWH  FEE CUTOFF MOVED INC4380                       *
      * 2014-04-18 SKN  ISSUED STATUS CODES ADDED PRB9797              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDARP.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "ARPSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "ARPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ARPM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY ARPMST.

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
           DISPLAY 'LDARP LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE ARP-MASTER-REC
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
           MOVE WS-F(1) TO ARPM-KEY
           MOVE WS-F(2) TO ARPM-CUST-ID
           MOVE WS-F(3) TO ARPM-NAME
           MOVE WS-F(4) TO ARPM-DESC
           MOVE WS-F(5) TO ARPM-REF-NO
           MOVE WS-F(6) TO ARPM-ADDR
           MOVE WS-F(7) TO ARPM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE ARPM-ISSUED-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE ARPM-ISSUED-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE ARPM-PAID-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE ARPM-PAID-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE ARPM-OUTSTAND-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE ARPM-OUTSTAND-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE ARPM-STOP-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE ARPM-STOP-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE ARPM-VOID-AMT =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE ARPM-VOID-YTD =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE ARPM-FEE-AMT =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE ARPM-FEE-YTD =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE ARPM-LAST-STMT-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE ARPM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE ARPM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE ARPM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE ARPM-LAST-ACT-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           IF WS-F(25) NOT = SPACES
               COMPUTE ARPM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           END-IF
           IF WS-F(26) NOT = SPACES
               COMPUTE ARPM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(26))
           END-IF
           IF WS-F(27) NOT = SPACES
               COMPUTE ARPM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(27))
           END-IF
           IF WS-F(28) NOT = SPACES
               COMPUTE ARPM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(28))
           END-IF
           MOVE WS-F(29) TO ARPM-RECON-TYPE-CD
           MOVE WS-F(30) TO ARPM-CYCLE-CD
           MOVE WS-F(31) TO ARPM-REGION-CD
           MOVE WS-F(32) TO ARPM-BRANCH-CD
           MOVE WS-F(33) TO ARPM-CHANNEL-CD
           MOVE WS-F(34) TO ARPM-SOURCE-CD
           MOVE WS-F(35) TO ARPM-HOLD-FLAG
           MOVE WS-F(36) TO ARPM-OVERRIDE-FLAG
           MOVE WS-F(37) TO ARPM-NOTICE-SENT-FLAG
           MOVE WS-F(38) TO ARPM-EXEMPT-FLAG
           MOVE WS-F(39) TO ARPM-STOP-FLAG
           MOVE WS-F(40) TO ARPM-PREFERRED-FLAG
           IF WS-F(41) NOT = SPACES
               COMPUTE ARPM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE ARPM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE ARPM-EXCEPT-CNT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           IF WS-F(44) NOT = SPACES
               COMPUTE ARPM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(44))
           END-IF
           IF WS-F(45) NOT = SPACES
               COMPUTE ARPM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(45))
           END-IF
           IF WS-F(46) NOT = SPACES
               COMPUTE ARPM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(46))
           END-IF
           IF WS-F(47) NOT = SPACES
               COMPUTE ARPM-DISCOUNT =
                   FUNCTION NUMVAL(WS-F(47))
           END-IF
           WRITE ARP-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDARP WRITE FAILED ' WS-MST-STATUS ' '
                       ARPM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
