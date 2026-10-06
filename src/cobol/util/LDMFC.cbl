      *================================================================*
      * PROGRAM  : LDMFC                                               *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE MICROFICHE STATEMENTS MASTER (MFCMST) FROM*
      *            A PIPE-DELIMITED EXTRACT. LINES BEGINNING WITH '#'  *
      *            ARE COMMENTS.                                       *
      * INPUT    : MFCSEED  PIPE-DELIMITED, 43 FIELDS                  *
      * OUTPUT   : MFCMST   (KSDS)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-10-05 JLM  ORIGINAL                                       *
      * 1997-02-12 PQR  AUDIT FINDING - RESEARCH CONTROL TOTALS INC3083*
      * 1998-12-06 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-04-11 GAF  POSTAGE CUTOFF MOVED CHG4306                   *
      * 2000-07-26 DPS  RESEARCH LIMIT REVISED REQ7237                 *
      * 2000-10-21 LMB  STORAGE STATUS CODES ADDED PRB9698             *
      * 2003-07-18 SKN  RESEARCH LIMIT REVISED INC8523                 *
      * 2008-10-06 EWS  PERFORMANCE - RESEARCH READ AHEAD REQ7024      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDMFC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "MFCSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT MASTER-FILE ASSIGN TO "MFCMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS MFCM-KEY
               FILE STATUS IS WS-MST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(1000).
       FD  MASTER-FILE.
       COPY MFCMST.

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
           DISPLAY 'LDMFC LOADED=' WS-LOADED ' SKIPPED=' WS-SKIPPED
           STOP RUN.

       1000-LOAD-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               ADD 1 TO WS-SKIPPED
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-FIELDS
           INITIALIZE MFC-MASTER-REC
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
           MOVE WS-F(1) TO MFCM-KEY
           MOVE WS-F(2) TO MFCM-CUST-ID
           MOVE WS-F(3) TO MFCM-NAME
           MOVE WS-F(4) TO MFCM-DESC
           MOVE WS-F(5) TO MFCM-REF-NO
           MOVE WS-F(6) TO MFCM-ADDR
           MOVE WS-F(7) TO MFCM-CONTACT
           IF WS-F(8) NOT = SPACES
               COMPUTE MFCM-RESEARCH-AMT =
                   FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               COMPUTE MFCM-RESEARCH-YTD =
                   FUNCTION NUMVAL(WS-F(9))
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE MFCM-COPY-FEE-AMT =
                   FUNCTION NUMVAL(WS-F(10))
           END-IF
           IF WS-F(11) NOT = SPACES
               COMPUTE MFCM-COPY-FEE-YTD =
                   FUNCTION NUMVAL(WS-F(11))
           END-IF
           IF WS-F(12) NOT = SPACES
               COMPUTE MFCM-POSTAGE-AMT =
                   FUNCTION NUMVAL(WS-F(12))
           END-IF
           IF WS-F(13) NOT = SPACES
               COMPUTE MFCM-POSTAGE-YTD =
                   FUNCTION NUMVAL(WS-F(13))
           END-IF
           IF WS-F(14) NOT = SPACES
               COMPUTE MFCM-STORAGE-AMT =
                   FUNCTION NUMVAL(WS-F(14))
           END-IF
           IF WS-F(15) NOT = SPACES
               COMPUTE MFCM-STORAGE-YTD =
                   FUNCTION NUMVAL(WS-F(15))
           END-IF
           IF WS-F(16) NOT = SPACES
               COMPUTE MFCM-OPEN-DATE =
                   FUNCTION NUMVAL(WS-F(16))
           END-IF
           IF WS-F(17) NOT = SPACES
               COMPUTE MFCM-NOTICE-DATE =
                   FUNCTION NUMVAL(WS-F(17))
           END-IF
           IF WS-F(18) NOT = SPACES
               COMPUTE MFCM-NEXT-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(18))
           END-IF
           IF WS-F(19) NOT = SPACES
               COMPUTE MFCM-CLOSE-DATE =
                   FUNCTION NUMVAL(WS-F(19))
           END-IF
           IF WS-F(20) NOT = SPACES
               COMPUTE MFCM-EXPIRY-DATE =
                   FUNCTION NUMVAL(WS-F(20))
           END-IF
           IF WS-F(21) NOT = SPACES
               COMPUTE MFCM-MATURITY-DATE =
                   FUNCTION NUMVAL(WS-F(21))
           END-IF
           IF WS-F(22) NOT = SPACES
               COMPUTE MFCM-LAST-STMT-DATE =
                   FUNCTION NUMVAL(WS-F(22))
           END-IF
           IF WS-F(23) NOT = SPACES
               COMPUTE MFCM-LAST-PAY-DATE =
                   FUNCTION NUMVAL(WS-F(23))
           END-IF
           IF WS-F(24) NOT = SPACES
               COMPUTE MFCM-LAST-MAINT-DATE =
                   FUNCTION NUMVAL(WS-F(24))
           END-IF
           MOVE WS-F(25) TO MFCM-FICHE-TYPE-CD
           MOVE WS-F(26) TO MFCM-RETENTION-CD
           MOVE WS-F(27) TO MFCM-CHANNEL-CD
           MOVE WS-F(28) TO MFCM-BRANCH-CD
           MOVE WS-F(29) TO MFCM-TYPE-CD
           MOVE WS-F(30) TO MFCM-STATUS-CD
           MOVE WS-F(31) TO MFCM-AUTO-RENEW-FLAG
           MOVE WS-F(32) TO MFCM-EXEMPT-FLAG
           MOVE WS-F(33) TO MFCM-REVIEW-FLAG
           MOVE WS-F(34) TO MFCM-NOTICE-SENT-FLAG
           MOVE WS-F(35) TO MFCM-HOLD-FLAG
           MOVE WS-F(36) TO MFCM-STOP-FLAG
           IF WS-F(37) NOT = SPACES
               COMPUTE MFCM-CYCLE-CNT =
                   FUNCTION NUMVAL(WS-F(37))
           END-IF
           IF WS-F(38) NOT = SPACES
               COMPUTE MFCM-PAYMENT-CNT =
                   FUNCTION NUMVAL(WS-F(38))
           END-IF
           IF WS-F(39) NOT = SPACES
               COMPUTE MFCM-NOTICE-CNT =
                   FUNCTION NUMVAL(WS-F(39))
           END-IF
           IF WS-F(40) NOT = SPACES
               COMPUTE MFCM-ITEM-CNT =
                   FUNCTION NUMVAL(WS-F(40))
           END-IF
           IF WS-F(41) NOT = SPACES
               COMPUTE MFCM-MARGIN =
                   FUNCTION NUMVAL(WS-F(41))
           END-IF
           IF WS-F(42) NOT = SPACES
               COMPUTE MFCM-DISCOUNT =
                   FUNCTION NUMVAL(WS-F(42))
           END-IF
           IF WS-F(43) NOT = SPACES
               COMPUTE MFCM-FEE-PCT =
                   FUNCTION NUMVAL(WS-F(43))
           END-IF
           WRITE MFC-MASTER-REC
           IF WS-MST-STATUS = '00'
               ADD 1 TO WS-LOADED
           ELSE
               DISPLAY 'LDMFC WRITE FAILED ' WS-MST-STATUS ' '
                       MFCM-KEY
               ADD 1 TO WS-SKIPPED
           END-IF.
