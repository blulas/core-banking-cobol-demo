      *================================================================*
      * PROGRAM  : HOLDREL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : EOD STEP 09 - RELEASES OR EXPIRES ACCOUNT HOLDS     *
      *            WHOSE RELEASE DATE IS ON OR BEFORE THE NEXT         *
      *            BUSINESS DATE, SO THE FUNDS ARE AVAILABLE WHEN THE  *
      *            BANK OPENS. THE HELD AMOUNT IS ADDED BACK TO        *
      *            ACCT-AVAIL-BAL AND REMOVED FROM ACCT-HOLD-AMT.      *
      *            - CK / CX (REG CC) HOLDS     -> STATUS R (RELEASED) *
      *            - AD / LN / FR / AU HOLDS    -> STATUS E (EXPIRED)  *
      *            - LG (LEGAL) HOLDS NEVER AUTO-RELEASE; THEY ARE     *
      *              RELEASED ONLY THROUGH HOLDMNT BY OPS / MGR        *
      *            - RELEASE DATE 99999999 = INDEFINITE, NEVER         *
      *            LEDGER BALANCE IS NOT CHANGED, SO NO TRANPOST ITEM. *
      * INPUT    : BANKCTL VIA GETCTL                                  *
      * I-O      : HOLDMST, ACCTMST                                    *
      * OUTPUT   : HOLDRPT                                             *
      * RETURN   : 0 OK, 4 HOLD FOR AN ACCOUNT NOT ON FILE             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-04-19 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2008-02-11 BHS  LEGAL HOLDS EXCLUDED (INC12904 - LEVY RELEASED *
      *                 IN ERROR)                                      *
      * 2011-07-21 DPS  RELEASE ON NEXT BUSINESS DATE, NOT CALENDAR    *
      * 2016-03-14 DPS  EXPIRED STATUS FOR NON-CHECK HOLDS             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOLDREL.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HOLD-FILE ASSIGN TO "HOLDMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HOLD-KEY
               FILE STATUS IS WS-HOLD-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "HOLDRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  HOLD-FILE.
       COPY HOLDREC.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-HOLD-STATUS                  PIC X(02).
       01  WS-ACCT-STATUS                  PIC X(02).
       01  WS-RPT-STATUS                   PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-INDEFINITE                   PIC 9(08) VALUE 99999999.
       01  WS-COUNTERS.
           05  WS-READ                     PIC 9(07) VALUE 0.
           05  WS-ACTIVE                   PIC 9(07) VALUE 0.
           05  WS-RELEASED                 PIC 9(07) VALUE 0.
           05  WS-EXPIRED                  PIC 9(07) VALUE 0.
           05  WS-LEGAL-KEPT               PIC 9(07) VALUE 0.
           05  WS-STILL-HELD               PIC 9(07) VALUE 0.
           05  WS-NOT-FOUND                PIC 9(07) VALUE 0.
           05  WS-RELEASED-AMT             PIC S9(15)V99 COMP-3
                                                     VALUE 0.
       01  WS-RPT-DETAIL.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-ACCT                     PIC X(12).
           05  FILLER                      PIC X(01) VALUE '/'.
           05  RD-SEQ                      PIC 9(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-TYPE                     PIC X(02).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-AMT                      PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-PLACED                   PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-RELEASE                  PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-ACTION                   PIC X(10).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-REASON                   PIC X(30).
       01  WS-RPT-DATE-ED                  PIC 9999/99/99.
       01  WS-RPT-NEXT-ED                  PIC 9999/99/99.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM UNTIL WS-EOF
               READ HOLD-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2000-EVALUATE-HOLD
               END-READ
           END-PERFORM
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN I-O HOLD-FILE ACCOUNT-FILE
           IF WS-HOLD-STATUS NOT = '00' OR WS-ACCT-STATUS NOT = '00'
               MOVE 'HOLDREL' TO ABND-PROGRAM
               MOVE '1000-INITIALISE' TO ABND-PARAGRAPH
               MOVE 'HOLDMST' TO ABND-FILE-NAME
               MOVE WS-HOLD-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - HOLDMST / ACCTMST' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO WS-RPT-DATE-ED
           MOVE CTL-NEXT-BUS-DATE TO WS-RPT-NEXT-ED
           MOVE SPACES TO REPORT-LINE
           STRING 'CBS  HOLDREL  HOLD RELEASE REPORT   BUSINESS DATE '
                  WS-RPT-DATE-ED '   RELEASING THROUGH '
                  WS-RPT-NEXT-ED DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE.

       2000-EVALUATE-HOLD.
           ADD 1 TO WS-READ
           IF NOT HOLD-ACTIVE
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-ACTIVE
           EVALUATE TRUE
               WHEN HOLD-LEGAL
                   ADD 1 TO WS-LEGAL-KEPT
                   MOVE 'LEGAL-KEPT' TO RD-ACTION
                   PERFORM 8000-REPORT-LINE
               WHEN HOLD-RELEASE-DATE = WS-INDEFINITE
                   ADD 1 TO WS-STILL-HELD
               WHEN HOLD-RELEASE-DATE > CTL-NEXT-BUS-DATE
                   ADD 1 TO WS-STILL-HELD
               WHEN OTHER
                   PERFORM 3000-RELEASE-HOLD
           END-EVALUATE.

       3000-RELEASE-HOLD.
           MOVE HOLD-ACCT-NUMBER TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   ADD 1 TO WS-NOT-FOUND
                   DISPLAY 'HOLDREL ACCOUNT NOT FOUND ' HOLD-ACCT-NUMBER
                   EXIT PARAGRAPH
           END-READ
           ADD HOLD-AMOUNT      TO ACCT-AVAIL-BAL
           SUBTRACT HOLD-AMOUNT FROM ACCT-HOLD-AMT
           IF ACCT-HOLD-AMT < 0
               MOVE ZERO TO ACCT-HOLD-AMT
           END-IF
           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'HOLDREL' TO ABND-PROGRAM
               MOVE '3000-RELEASE-HOLD' TO ABND-PARAGRAPH
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           IF HOLD-CHECK-DEPOSIT OR HOLD-CHECK-EXCEPTION
               SET HOLD-RELEASED TO TRUE
               ADD 1 TO WS-RELEASED
               MOVE 'RELEASED' TO RD-ACTION
           ELSE
               SET HOLD-EXPIRED TO TRUE
               ADD 1 TO WS-EXPIRED
               MOVE 'EXPIRED' TO RD-ACTION
           END-IF
           ADD HOLD-AMOUNT TO WS-RELEASED-AMT
           REWRITE HOLD-RECORD
           IF WS-HOLD-STATUS NOT = '00'
               MOVE 'HOLDREL' TO ABND-PROGRAM
               MOVE '3000-RELEASE-HOLD' TO ABND-PARAGRAPH
               MOVE 'HOLDMST' TO ABND-FILE-NAME
               MOVE WS-HOLD-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM 8000-REPORT-LINE.

       8000-REPORT-LINE.
           MOVE HOLD-ACCT-NUMBER  TO RD-ACCT
           MOVE HOLD-SEQ          TO RD-SEQ
           MOVE HOLD-TYPE         TO RD-TYPE
           MOVE HOLD-AMOUNT       TO RD-AMT
           MOVE HOLD-PLACED-DATE  TO RD-PLACED
           MOVE HOLD-RELEASE-DATE TO RD-RELEASE
           MOVE HOLD-REASON       TO RD-REASON
           WRITE REPORT-LINE FROM WS-RPT-DETAIL.

       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           STRING '  HOLDS READ ' WS-READ '  ACTIVE ' WS-ACTIVE
                  '  RELEASED ' WS-RELEASED '  EXPIRED ' WS-EXPIRED
                  '  LEGAL KEPT ' WS-LEGAL-KEPT
                  '  STILL HELD ' WS-STILL-HELD
                  DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           CLOSE HOLD-FILE ACCOUNT-FILE REPORT-FILE
           DISPLAY 'HOLDREL READ=' WS-READ ' ACTIVE=' WS-ACTIVE
                   ' RELEASED=' WS-RELEASED ' EXPIRED=' WS-EXPIRED
                   ' LEGAL-KEPT=' WS-LEGAL-KEPT
                   ' STILL-HELD=' WS-STILL-HELD
                   ' NOT-FOUND=' WS-NOT-FOUND
           IF WS-NOT-FOUND > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
