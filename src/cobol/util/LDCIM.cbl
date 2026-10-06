      *================================================================*
      * PROGRAM  : LDCIM                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE CHECK IMAGE ARCHIVE MASTER (CIMMST) FROM A*
      *            PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#' ARE*
      *            COMMENTS.                                           *
      * INPUT    : CIMSEED  PIPE-DELIMITED, 43 FIELDS                  *
      * OUTPUT   : CIMMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-11 TWH  ORIGINAL                                       *
      * 1992-02-09 PQR  AUDIT FINDING - ITEM CONTROL TOTALS CHG6963    *
      * 1992-05-12 BHS  NEW RETRIEVE-FEE REPORT COLUMNS INC7354        *
      * 1997-10-21 BHS  ITEM RERUN PROTECTION REQ8652                  *
      * 2000-10-18 MKL  IMAGE-FEE TABLE EXPANDED INC9594               *
      * 2000-08-07 CRP  AUDIT FINDING - RETRIEVE-FEE CONTROL TOTALS CHG*
      * 2001-03-04 RJT  AUDIT FINDING - STORAGE CONTROL TOTALS REQ7065 *
      * 2003-10-16 MKL  ITEM STATUS CODES ADDED PRB3276                *
      * 2005-04-20 TWH  PERFORMANCE - ITEM READ AHEAD CHG5194          *
      * 2005-11-05 DPS  STORAGE CUTOFF MOVED CHG3287                   *
      * 2007-07-02 TWH  STORAGE ROUNDING - HALF UP PRB3424             *
      * 2008-03-27 DPS  RETRIEVE-FEE NOTICE TEXT PER LEGAL INC6592     *
      * 2009-01-25 LMB  ITEM CUTOFF MOVED REQ9246                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDCIM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "CIMSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "CIMMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CIMM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY CIMMST.

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
           DISPLAY 'LDCIM LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE CIM-MASTER-REC
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
           MOVE WS-F(1) TO CIMM-KEY
           MOVE WS-F(2) TO CIMM-CUST-ID
           MOVE WS-F(3) TO CIMM-NAME
           MOVE WS-F(4) TO CIMM-DESC
           MOVE WS-F(5) TO CIMM-REF-NO
           MOVE WS-F(6) TO CIMM-ADDR
           MOVE WS-F(7) TO CIMM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE CIMM-ITEM-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE CIMM-ITEM-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE CIMM-RETRIEVE-FEE-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE CIMM-RETRIEVE-FEE-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE CIMM-IMAGE-FEE-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE CIMM-IMAGE-FEE-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE CIMM-STORAGE-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE CIMM-STORAGE-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE CIMM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE CIMM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE CIMM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE CIMM-EFFECTIVE-DATE =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE CIMM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE CIMM-LAST-STMT-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE CIMM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE CIMM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE CIMM-NEXT-REVIEW-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           MOVE WS-F(25) TO CIMM-CAPTURE-SITE-CD
           MOVE WS-F(26) TO CIMM-ITEM-TYPE-CD
           MOVE WS-F(27) TO CIMM-TYPE-CD
           MOVE WS-F(28) TO CIMM-BRANCH-CD
           MOVE WS-F(29) TO CIMM-STATUS-CD
           MOVE WS-F(30) TO CIMM-CHANNEL-CD
           MOVE WS-F(31) TO CIMM-HOLD-FLAG
           MOVE WS-F(32) TO CIMM-REVIEW-FLAG
           MOVE WS-F(33) TO CIMM-SUSPEND-FLAG
           MOVE WS-F(34) TO CIMM-OVERRIDE-FLAG
           MOVE WS-F(35) TO CIMM-PREFERRED-FLAG
           MOVE WS-F(36) TO CIMM-EXEMPT-FLAG
           IF WS-F(37) NOT = SPACES
               COMPUTE CIMM-NOTICE-CNT =
                   FUNCTION NUMVAL(WS-F(37))
           END-IF
           IF WS-F(38) NOT = SPACES
               COMPUTE CIMM-RETRY-CNT =
                   FUNCTION NUMVAL(WS-F(38))
           END-IF
           IF WS-F(39) NOT = SPACES
               COMPUTE CIMM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(39))
           END-IF
           IF WS-F(40) NOT = SPACES
               COMPUTE CIMM-EXCEPT-CNT =
                   FUNCTION NUMVAL(WS-F(40))
           END-IF
           IF WS-F(41) NOT = SPACES
               COMPUTE CIMM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE CIMM-MARGIN =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE CIMM-PENALTY-PCT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           WRITE CIM-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDCIM WRITE FAILED ' WS-MST-STATUS ' '
                       CIMM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
