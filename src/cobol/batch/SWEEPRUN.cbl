      *================================================================*
      * PROGRAM  : SWEEPRUN                                            *
      * TYPE     : BATCH - END OF DAY (EOD STEP 10)                    *
      * PURPOSE  : END-OF-DAY LIQUIDITY MOVEMENTS FOR TRANSACTION      *
      *            BANKING CLIENTS.                                    *
      *            PHASE 1 - VIRTUAL ACCOUNT CONCENTRATION: EVERY      *
      *              ACTIVE VIRTUAL ACCOUNT (ACCT-VIRTUAL-FLAG = Y) IS *
      *              BROUGHT TO ZERO AGAINST ITS HEADER ACCOUNT        *
      *              (ACCT-PARENT-ACCT) BY BOOK TRANSFER WXFR/DXFR.    *
      *            PHASE 2 - SWEEPS FROM SWEEPMST IN PRIORITY ORDER    *
      *              (ZBA BEFORE TARGET SO THE PARENT IS FUNDED        *
      *              BEFORE ITS EXCESS IS INVESTED). AMOUNTS FROM      *
      *              SWEEPRUL. POSTED AS WSWP / DSWP PAIRS.            *
      *            EVERY MOVEMENT UPDATES ACCTMST AND WRITES TRANPOST  *
      *            (TRAN-ID-SEQ 30000001 - 34999999).                  *
      * INPUT    : BANKCTL (VIA GETCTL), SWEEPMST, ACCTMST             *
      * OUTPUT   : ACCTMST (UPDATED), SWEEPMST (LAST SWEEP), TRANPOST  *
      *            (EXTEND), ALERTS (EXTEND), SWEEPRPT                 *
      * RETURN   : 0 OK, 4 WARNINGS (PARTIAL/UNFUNDED ZBA, SKIPPED     *
      *            SWEEPS), 16 ABEND                                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2001-10-01 BHS  ORIGINAL - TXB CASH CONCENTRATION              *
      * 2004-05-17 BHS  RANGE SWEEPS                                   *
      * 2008-09-29 DPS  INC11402 ZBA PARTIAL FUNDING ALERT             *
      * 2011-04-12 DPS  CHG1544 PRIORITY ORDER, SWEEP CODES DSWP/WSWP  *
      * 2015-10-02 DPS  TRANPOST OUTPUT FOR GL EXTRACT                 *
      * 2019-03-25 AKW  VIRTUAL ACCOUNT CONCENTRATION (VAM PHASE 1)    *
      * 2021-02-08 AKW  INC30412 SKIP FROZEN / RESTRICTED SOURCE ACCTS *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SWEEPRUN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SWEEP-FILE ASSIGN TO "SWEEPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SWP-ID
               FILE STATUS IS WS-SWP-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "SWEEPRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SWEEP-FILE.
       COPY SWEEPREC.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  TRANPOST-FILE.
       COPY TRANREC.
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-SWP-STATUS               PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-POST-STATUS              PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-FLAGS.
           05  WS-EOF-FLAG                 PIC X(01).
               88  WS-EOF                            VALUE 'Y'.
           05  WS-POST-OK-FLAG             PIC X(01).
               88  WS-POST-OK                        VALUE 'Y'.
       01  WS-SEQ-RANGE.
           05  WS-TRAN-SEQ                 PIC 9(08) VALUE 30000000.
           05  WS-SEQ-LIMIT                PIC 9(08) VALUE 34999999.
       01  WS-COUNTERS.
           05  WS-SWEEPS-READ              PIC 9(05) VALUE 0.
           05  WS-SWEEPS-ELIGIBLE          PIC 9(05) VALUE 0.
           05  WS-SWEEPS-MOVED             PIC 9(05) VALUE 0.
           05  WS-SWEEPS-SKIPPED           PIC 9(05) VALUE 0.
           05  WS-VIRTUAL-MOVED            PIC 9(05) VALUE 0.
           05  WS-ALERT-COUNT              PIC 9(05) VALUE 0.
           05  WS-POST-COUNT               PIC 9(07) VALUE 0.
           05  WS-TOTAL-OUT                PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-IN                 PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-VIRTUAL            PIC S9(15)V99 COMP-3
                                           VALUE 0.
       01  WS-RUN-RC                       PIC 9(02) VALUE 0.
       01  WS-DAY-OF-WEEK                  PIC 9(01).

      *--- SWEEP DEFINITIONS ELIGIBLE TODAY, SORTED BY PRIORITY -------*
       01  WS-SWEEP-TABLE.
           05  WS-SWT-COUNT                PIC 9(03) VALUE 0.
           05  WS-SWT-ENTRY OCCURS 200 TIMES.
               10  WS-SWT-PRIORITY         PIC 9(02).
               10  WS-SWT-ID               PIC X(08).
       01  WS-SWT-HOLD                     PIC X(10).
       01  WS-IX                           PIC 9(03).
       01  WS-JX                           PIC 9(03).

      *--- VIRTUAL ACCOUNTS -------------------------------------------*
       01  WS-VIRTUAL-TABLE.
           05  WS-VAT-COUNT                PIC 9(03) VALUE 0.
           05  WS-VAT-ENTRY OCCURS 200 TIMES.
               10  WS-VAT-ACCT             PIC X(12).
               10  WS-VAT-PARENT           PIC X(12).
               10  WS-VAT-AVAIL            PIC S9(13)V99 COMP-3.

      *--- SNAPSHOTS OF SOURCE / TARGET ------------------------------*
       01  WS-SRC.
           05  WS-SRC-ACCT                 PIC X(12).
           05  WS-SRC-LEDGER               PIC S9(13)V99 COMP-3.
           05  WS-SRC-AVAIL                PIC S9(13)V99 COMP-3.
           05  WS-SRC-HOLD                 PIC S9(13)V99 COMP-3.
           05  WS-SRC-STATUS               PIC X(01).
           05  WS-SRC-CUST                 PIC X(10).
       01  WS-TGT.
           05  WS-TGT-ACCT                 PIC X(12).
           05  WS-TGT-AVAIL                PIC S9(13)V99 COMP-3.
           05  WS-TGT-PRODUCT              PIC X(04).
           05  WS-TGT-STATUS               PIC X(01).
       01  WS-SRC-FOUND                    PIC X(01).
       01  WS-TGT-FOUND                    PIC X(01).
       01  WS-SKIP-REASON                  PIC X(40).

      *--- TRANSFER REQUEST (8000-POST-TRANSFER) ---------------------*
       01  WS-XFER.
           05  WS-XFER-FROM                PIC X(12).
           05  WS-XFER-TO                  PIC X(12).
           05  WS-XFER-AMOUNT              PIC S9(13)V99 COMP-3.
           05  WS-XFER-DR-CODE             PIC X(04).
           05  WS-XFER-CR-CODE             PIC X(04).
           05  WS-XFER-REF                 PIC X(20).
           05  WS-XFER-DESC                PIC X(40).
       01  WS-POST-ACCT                    PIC X(12).
       01  WS-POST-CODE                    PIC X(04).
       01  WS-POST-DRCR                    PIC X(01).
       01  WS-POST-COUNTER                 PIC X(12).
       01  WS-AFTER-SRC-BAL                PIC S9(13)V99 COMP-3.

      *--- REPORT ----------------------------------------------------*
       01  WS-RPT-HEAD1.
           05  FILLER   PIC X(40) VALUE
               'CBS  SWEEPRUN  - END OF DAY SWEEP REPORT'.
           05  FILLER   PIC X(20) VALUE '   BUSINESS DATE  '.
           05  WS-RH-DATE                  PIC 9(08).
           05  FILLER   PIC X(64) VALUE SPACES.
       01  WS-RPT-HEAD2.
           05  FILLER   PIC X(36) VALUE
               'SWEEP-ID T SOURCE ACCT  TARGET ACCT '.
           05  FILLER   PIC X(36) VALUE
               '     AVAIL BEFORE      AMOUNT MOVED '.
           05  FILLER   PIC X(60) VALUE 'D RSN  NOTE'.
       01  WS-RPT-DETAIL.
           05  WS-RD-ID                    PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-TYPE                  PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-SOURCE                PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-TARGET                PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-BEFORE                PIC -(12)9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-AMOUNT                PIC -(12)9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-DIR                   PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-REASON                PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-NOTE                  PIC X(48).
       01  WS-RPT-TOTAL.
           05  WS-RT-LABEL                 PIC X(40).
           05  WS-RT-AMOUNT                PIC -(14)9.99.
           05  FILLER                      PIC X(74) VALUE SPACES.

       COPY SWRPARM.
       COPY DATEPARM.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-VIRTUAL-CONCENTRATION
           PERFORM 3000-LOAD-SWEEPS
           PERFORM 3500-SORT-SWEEPS
           PERFORM 4000-PROCESS-SWEEPS
               VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-SWT-COUNT
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           INITIALIZE DATE-PARMS
           SET DTU-DAY-OF-WEEK TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-NUMBER TO WS-DAY-OF-WEEK

           OPEN I-O SWEEP-FILE
           IF WS-SWP-STATUS NOT = '00'
               MOVE 'SWEEPMST' TO ABND-FILE-NAME
               MOVE WS-SWP-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN I-O ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN EXTEND TRANPOST-FILE
           IF WS-POST-STATUS NOT = '00' AND WS-POST-STATUS NOT = '05'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN EXTEND ALERT-FILE
           IF WS-ALRT-STATUS NOT = '00' AND WS-ALRT-STATUS NOT = '05'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO WS-RH-DATE
           WRITE REPORT-LINE FROM WS-RPT-HEAD1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM WS-RPT-HEAD2.

      *----------------------------------------------------------------*
      * PHASE 1 - VIRTUAL ACCOUNTS. COLLECT FIRST (SEQUENTIAL BROWSE), *
      * THEN MOVE (RANDOM UPDATE) SO THE BROWSE IS NOT DISTURBED.      *
      *----------------------------------------------------------------*
       2000-VIRTUAL-CONCENTRATION.
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY IS NOT LESS THAN ACCT-NUMBER
           MOVE 'N' TO WS-EOF-FLAG
           IF WS-ACCT-STATUS NOT = '00'
               SET WS-EOF TO TRUE
           END-IF
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF ACCT-IS-VIRTUAL AND ACCT-ACTIVE
                          AND ACCT-PARENT-ACCT NOT = SPACES
                          AND ACCT-AVAIL-BAL NOT = 0
                          AND WS-VAT-COUNT < 200
                           ADD 1 TO WS-VAT-COUNT
                           MOVE ACCT-NUMBER
                             TO WS-VAT-ACCT(WS-VAT-COUNT)
                           MOVE ACCT-PARENT-ACCT
                             TO WS-VAT-PARENT(WS-VAT-COUNT)
                           MOVE ACCT-AVAIL-BAL
                             TO WS-VAT-AVAIL(WS-VAT-COUNT)
                       END-IF
               END-READ
           END-PERFORM

           PERFORM VARYING WS-JX FROM 1 BY 1
                   UNTIL WS-JX > WS-VAT-COUNT
               PERFORM 2100-MOVE-VIRTUAL
           END-PERFORM.

      *    VAM DESIGN SPEC 2019-03: VIRTUAL ACTIVITY IS REPORTED ON THE
      *    HEADER STATEMENT AS A BOOK TRANSFER FROM THE VIRTUAL ACCOUNT
       2100-MOVE-VIRTUAL.
           INITIALIZE WS-RPT-DETAIL
           MOVE 'VIRTUAL'  TO WS-RD-ID
           MOVE 'V'        TO WS-RD-TYPE
           MOVE WS-VAT-ACCT(WS-JX)   TO WS-RD-SOURCE
           MOVE WS-VAT-PARENT(WS-JX) TO WS-RD-TARGET
           MOVE WS-VAT-AVAIL(WS-JX)  TO WS-RD-BEFORE
           MOVE 'WXFR' TO WS-XFER-DR-CODE
           MOVE 'DXFR' TO WS-XFER-CR-CODE
           MOVE 'VAM'  TO WS-XFER-REF
           IF WS-VAT-AVAIL(WS-JX) > 0
               MOVE WS-VAT-ACCT(WS-JX)   TO WS-XFER-FROM
               MOVE WS-VAT-PARENT(WS-JX) TO WS-XFER-TO
               MOVE WS-VAT-AVAIL(WS-JX)  TO WS-XFER-AMOUNT
               MOVE 'VIRTUAL ACCOUNT CONCENTRATION TO HEADER'
                 TO WS-XFER-DESC
               MOVE 'O' TO WS-RD-DIR
           ELSE
               MOVE WS-VAT-PARENT(WS-JX) TO WS-XFER-FROM
               MOVE WS-VAT-ACCT(WS-JX)   TO WS-XFER-TO
               COMPUTE WS-XFER-AMOUNT = 0 - WS-VAT-AVAIL(WS-JX)
               MOVE 'VIRTUAL ACCOUNT FUNDED FROM HEADER'
                 TO WS-XFER-DESC
               MOVE 'I' TO WS-RD-DIR
           END-IF
           PERFORM 8000-POST-TRANSFER
           IF WS-POST-OK
               ADD 1 TO WS-VIRTUAL-MOVED
               ADD WS-XFER-AMOUNT TO WS-TOTAL-VIRTUAL
               MOVE WS-XFER-AMOUNT TO WS-RD-AMOUNT
               MOVE 'VCON' TO WS-RD-REASON
               MOVE 'VIRTUAL ACCOUNT BROUGHT TO ZERO' TO WS-RD-NOTE
           ELSE
               MOVE ZERO   TO WS-RD-AMOUNT
               MOVE 'N'    TO WS-RD-DIR
               MOVE 'SKIP' TO WS-RD-REASON
               MOVE 'HEADER ACCOUNT NOT FOUND' TO WS-RD-NOTE
               MOVE 4 TO WS-RUN-RC
           END-IF
           WRITE REPORT-LINE FROM WS-RPT-DETAIL.

      *----------------------------------------------------------------*
      * PHASE 2 - LOAD SWEEP DEFINITIONS DUE TODAY                     *
      *----------------------------------------------------------------*
       3000-LOAD-SWEEPS.
           MOVE LOW-VALUES TO SWP-ID
           START SWEEP-FILE KEY IS NOT LESS THAN SWP-ID
           MOVE 'N' TO WS-EOF-FLAG
           IF WS-SWP-STATUS NOT = '00'
               SET WS-EOF TO TRUE
           END-IF
           PERFORM UNTIL WS-EOF
               READ SWEEP-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-SWEEPS-READ
                       PERFORM 3100-CHECK-ELIGIBLE
               END-READ
           END-PERFORM.

       3100-CHECK-ELIGIBLE.
           IF NOT SWP-ACTIVE
               EXIT PARAGRAPH
           END-IF
           IF SWP-EFF-DATE > CTL-BUSINESS-DATE
               EXIT PARAGRAPH
           END-IF
           IF SWP-EXP-DATE NOT = 0 AND SWP-EXP-DATE < CTL-BUSINESS-DATE
               EXIT PARAGRAPH
           END-IF
      *    WEEKLY SWEEPS RUN FRIDAY, MONTHLY ON THE MONTH-END CYCLE
           EVALUATE TRUE
               WHEN SWP-FREQ-DAILY
                   CONTINUE
               WHEN SWP-FREQ-WEEKLY AND WS-DAY-OF-WEEK = 5
                   CONTINUE
               WHEN SWP-FREQ-MONTHLY AND CTL-IS-MONTH-END
                   CONTINUE
               WHEN OTHER
                   EXIT PARAGRAPH
           END-EVALUATE
      *    ALREADY SWEPT TODAY (RERUN PROTECTION)
           IF SWP-LAST-SWEEP-DATE = CTL-BUSINESS-DATE
               DISPLAY 'SWEEPRUN ' SWP-ID ' ALREADY SWEPT TODAY'
               EXIT PARAGRAPH
           END-IF
           IF WS-SWT-COUNT < 200
               ADD 1 TO WS-SWT-COUNT
               ADD 1 TO WS-SWEEPS-ELIGIBLE
               MOVE SWP-PRIORITY TO WS-SWT-PRIORITY(WS-SWT-COUNT)
               MOVE SWP-ID       TO WS-SWT-ID(WS-SWT-COUNT)
           END-IF.

      *    SIMPLE EXCHANGE SORT - PRIORITY, THEN SWEEP ID
       3500-SORT-SWEEPS.
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX >= WS-SWT-COUNT
               PERFORM VARYING WS-JX FROM WS-IX BY 1
                       UNTIL WS-JX > WS-SWT-COUNT
                   IF WS-SWT-ENTRY(WS-JX) < WS-SWT-ENTRY(WS-IX)
                       MOVE WS-SWT-ENTRY(WS-IX) TO WS-SWT-HOLD
                       MOVE WS-SWT-ENTRY(WS-JX) TO WS-SWT-ENTRY(WS-IX)
                       MOVE WS-SWT-HOLD         TO WS-SWT-ENTRY(WS-JX)
                   END-IF
               END-PERFORM
           END-PERFORM.

      *----------------------------------------------------------------*
      * ONE SWEEP                                                      *
      *----------------------------------------------------------------*
       4000-PROCESS-SWEEPS.
           MOVE WS-SWT-ID(WS-IX) TO SWP-ID
           READ SWEEP-FILE
               INVALID KEY
                   DISPLAY 'SWEEPRUN SWEEP VANISHED ' SWP-ID
                   EXIT PARAGRAPH
           END-READ
           INITIALIZE WS-RPT-DETAIL
           MOVE SWP-ID          TO WS-RD-ID
           MOVE SWP-TYPE        TO WS-RD-TYPE
           MOVE SWP-SOURCE-ACCT TO WS-RD-SOURCE
           MOVE SWP-TARGET-ACCT TO WS-RD-TARGET
           MOVE SPACES          TO WS-SKIP-REASON

           PERFORM 4100-READ-SOURCE-TARGET
           IF WS-SKIP-REASON NOT = SPACES
               ADD 1 TO WS-SWEEPS-SKIPPED
               MOVE 4 TO WS-RUN-RC
               MOVE 'N'    TO WS-RD-DIR
               MOVE 'SKIP' TO WS-RD-REASON
               MOVE WS-SKIP-REASON TO WS-RD-NOTE
               WRITE REPORT-LINE FROM WS-RPT-DETAIL
               EXIT PARAGRAPH
           END-IF
           MOVE WS-SRC-AVAIL TO WS-RD-BEFORE

           INITIALIZE SWEEP-RULE-PARMS
           MOVE SWP-TYPE         TO SWR-SWEEP-TYPE
           MOVE WS-SRC-LEDGER    TO SWR-SRC-LEDGER-BAL
           MOVE WS-SRC-AVAIL     TO SWR-SRC-AVAIL-BAL
           MOVE WS-SRC-HOLD      TO SWR-SRC-HOLD-AMT
           MOVE WS-TGT-PRODUCT   TO SWR-TGT-PRODUCT-CODE
           MOVE WS-TGT-AVAIL     TO SWR-TGT-AVAIL-BAL
           MOVE SWP-TARGET-BAL   TO SWR-TARGET-BAL
           MOVE SWP-MIN-BAL      TO SWR-MIN-BAL
           MOVE SWP-MAX-BAL      TO SWR-MAX-BAL
           MOVE SWP-INCREMENT    TO SWR-INCREMENT
           MOVE SWP-MIN-TRANSFER TO SWR-MIN-TRANSFER
           CALL 'SWEEPRUL' USING SWEEP-RULE-PARMS

           MOVE SWR-DIRECTION   TO WS-RD-DIR
           MOVE SWR-REASON-CODE TO WS-RD-REASON
           MOVE SWR-REASON-TEXT TO WS-RD-NOTE
           MOVE SWR-SWEEP-AMOUNT TO WS-RD-AMOUNT
           IF SWR-RETURN-CODE > 0
               MOVE 4 TO WS-RUN-RC
           END-IF

           IF SWR-SWEEP-AMOUNT > 0 AND NOT SWR-DIR-NONE
               PERFORM 4200-MOVE-FUNDS
           END-IF

           IF SWR-RAISE-ALERT
               PERFORM 8500-WRITE-ALERT
           END-IF
           WRITE REPORT-LINE FROM WS-RPT-DETAIL
           PERFORM 4300-UPDATE-SWEEP.

       4100-READ-SOURCE-TARGET.
           MOVE SWP-SOURCE-ACCT TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   MOVE 'SOURCE ACCOUNT NOT ON FILE' TO WS-SKIP-REASON
                   EXIT PARAGRAPH
           END-READ
           MOVE ACCT-NUMBER          TO WS-SRC-ACCT
           MOVE ACCT-LEDGER-BAL      TO WS-SRC-LEDGER
           MOVE ACCT-AVAIL-BAL       TO WS-SRC-AVAIL
           MOVE ACCT-HOLD-AMT        TO WS-SRC-HOLD
           MOVE ACCT-STATUS          TO WS-SRC-STATUS
           MOVE ACCT-PRIMARY-CUST-ID TO WS-SRC-CUST
      *    INC30412 - NO SWEEPS ON FROZEN, RESTRICTED OR CLOSED ACCTS
           IF NOT ACCT-ACTIVE
               MOVE 'SOURCE ACCOUNT NOT ACTIVE' TO WS-SKIP-REASON
               EXIT PARAGRAPH
           END-IF
           MOVE SWP-TARGET-ACCT TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   MOVE 'TARGET ACCOUNT NOT ON FILE' TO WS-SKIP-REASON
                   EXIT PARAGRAPH
           END-READ
           MOVE ACCT-NUMBER       TO WS-TGT-ACCT
           MOVE ACCT-AVAIL-BAL    TO WS-TGT-AVAIL
           MOVE ACCT-PRODUCT-CODE TO WS-TGT-PRODUCT
           MOVE ACCT-STATUS       TO WS-TGT-STATUS
           IF NOT ACCT-ACTIVE
               MOVE 'TARGET ACCOUNT NOT ACTIVE' TO WS-SKIP-REASON
           END-IF.

       4200-MOVE-FUNDS.
           MOVE 'WSWP' TO WS-XFER-DR-CODE
           MOVE 'DSWP' TO WS-XFER-CR-CODE
           MOVE SWP-ID TO WS-XFER-REF
           MOVE SWR-SWEEP-AMOUNT TO WS-XFER-AMOUNT
           IF SWR-DIR-OUT
               MOVE SWP-SOURCE-ACCT TO WS-XFER-FROM
               MOVE SWP-TARGET-ACCT TO WS-XFER-TO
               EVALUATE TRUE
                   WHEN SWP-ZBA
                       MOVE 'ZBA CONCENTRATION TO PARENT'
                         TO WS-XFER-DESC
                   WHEN OTHER
                       MOVE 'SWEEP TO INVESTMENT ACCOUNT'
                         TO WS-XFER-DESC
               END-EVALUATE
           ELSE
               MOVE SWP-TARGET-ACCT TO WS-XFER-FROM
               MOVE SWP-SOURCE-ACCT TO WS-XFER-TO
               EVALUATE TRUE
                   WHEN SWP-ZBA
                       MOVE 'ZBA FUNDING FROM PARENT' TO WS-XFER-DESC
                   WHEN OTHER
                       MOVE 'SWEEP RETURN FROM INVESTMENT ACCOUNT'
                         TO WS-XFER-DESC
               END-EVALUATE
           END-IF
           PERFORM 8000-POST-TRANSFER
           IF WS-POST-OK
               ADD 1 TO WS-SWEEPS-MOVED
               IF SWR-DIR-OUT
                   ADD SWR-SWEEP-AMOUNT TO WS-TOTAL-OUT
               ELSE
                   ADD SWR-SWEEP-AMOUNT TO WS-TOTAL-IN
               END-IF
           END-IF.

       4300-UPDATE-SWEEP.
           MOVE WS-SWT-ID(WS-IX) TO SWP-ID
           READ SWEEP-FILE
               INVALID KEY EXIT PARAGRAPH
           END-READ
           MOVE CTL-BUSINESS-DATE TO SWP-LAST-SWEEP-DATE
           MOVE SWR-SWEEP-AMOUNT  TO SWP-LAST-SWEEP-AMT
           MOVE SWR-DIRECTION     TO SWP-LAST-SWEEP-DIR
           IF SWR-SWEEP-AMOUNT > 0
               ADD 1 TO SWP-SWEEP-COUNT-YTD
           END-IF
           REWRITE SWEEP-RECORD
           IF WS-SWP-STATUS NOT = '00'
               MOVE 'SWEEPMST' TO ABND-FILE-NAME
               MOVE WS-SWP-STATUS TO ABND-FILE-STATUS
               MOVE '4300-UPDATE-SWEEP' TO ABND-PARAGRAPH
               MOVE 'REWRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF.

      *----------------------------------------------------------------*
      * POST A BALANCED TRANSFER: DEBIT FROM-ACCT, CREDIT TO-ACCT      *
      *----------------------------------------------------------------*
       8000-POST-TRANSFER.
           MOVE 'N' TO WS-POST-OK-FLAG
      *    BOTH ACCOUNTS MUST EXIST BEFORE EITHER LEG IS POSTED
           MOVE WS-XFER-TO TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY EXIT PARAGRAPH
           END-READ
           MOVE WS-XFER-FROM    TO WS-POST-ACCT
           MOVE WS-XFER-DR-CODE TO WS-POST-CODE
           MOVE 'D'             TO WS-POST-DRCR
           MOVE WS-XFER-TO      TO WS-POST-COUNTER
           PERFORM 8100-POST-LEG
           IF NOT WS-POST-OK
               EXIT PARAGRAPH
           END-IF
           MOVE WS-XFER-TO      TO WS-POST-ACCT
           MOVE WS-XFER-CR-CODE TO WS-POST-CODE
           MOVE 'C'             TO WS-POST-DRCR
           MOVE WS-XFER-FROM    TO WS-POST-COUNTER
           PERFORM 8100-POST-LEG.

       8100-POST-LEG.
           MOVE 'N' TO WS-POST-OK-FLAG
           MOVE WS-POST-ACCT TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY 'SWEEPRUN ACCOUNT NOT FOUND ' WS-POST-ACCT
                   EXIT PARAGRAPH
           END-READ
           IF WS-POST-DRCR = 'D'
               SUBTRACT WS-XFER-AMOUNT FROM ACCT-LEDGER-BAL
                                            ACCT-AVAIL-BAL
               ADD 1 TO ACCT-DR-COUNT-CYC
           ELSE
               ADD WS-XFER-AMOUNT TO ACCT-LEDGER-BAL ACCT-AVAIL-BAL
               ADD 1 TO ACCT-CR-COUNT-CYC
           END-IF
           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE '8100-POST-LEG' TO ABND-PARAGRAPH
               MOVE 'ACCOUNT REWRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF

           ADD 1 TO WS-TRAN-SEQ
           IF WS-TRAN-SEQ > WS-SEQ-LIMIT
               MOVE 'TRAN-ID-SEQ RANGE 30000000-34999999 EXHAUSTED'
                 TO ABND-MESSAGE
               MOVE '8100-POST-LEG' TO ABND-PARAGRAPH
               PERFORM 9910-ABEND
           END-IF
           INITIALIZE TRANSACTION-RECORD
           MOVE CTL-BUSINESS-DATE TO TRAN-ID-DATE
                                     TRAN-EFFECTIVE-DATE
                                     TRAN-POST-DATE
           MOVE WS-TRAN-SEQ       TO TRAN-ID-SEQ
           MOVE WS-POST-ACCT      TO TRAN-ACCT-NUMBER
           MOVE 220000            TO TRAN-TIME
           MOVE WS-POST-CODE      TO TRAN-CODE
           MOVE WS-POST-DRCR      TO TRAN-DR-CR
           MOVE WS-XFER-AMOUNT    TO TRAN-AMOUNT
           MOVE 'BAT'             TO TRAN-CHANNEL
           MOVE WS-XFER-DESC      TO TRAN-DESCRIPTION
           MOVE WS-XFER-REF       TO TRAN-REFERENCE
           MOVE WS-POST-COUNTER   TO TRAN-COUNTER-ACCT
           MOVE 'S'               TO TRAN-STATUS
           MOVE 'SWEEPRUN'        TO TRAN-USER-ID TRAN-SOURCE-PGM
           MOVE ACCT-COST-CENTER  TO TRAN-COST-CENTER
           MOVE 'N'               TO TRAN-CASH-FLAG TRAN-REGD-FLAG
                                     TRAN-OD-FLAG
           WRITE TRANSACTION-RECORD
           IF WS-POST-STATUS NOT = '00'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               MOVE '8100-POST-LEG' TO ABND-PARAGRAPH
               MOVE 'TRANPOST WRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF
           ADD 1 TO WS-POST-COUNT
           MOVE 'Y' TO WS-POST-OK-FLAG.

       8500-WRITE-ALERT.
           INITIALIZE ALERT-RECORD
           MOVE CTL-BUSINESS-DATE TO ALRT-DATE
           MOVE 220000            TO ALRT-TIME
           MOVE 'LIMT'            TO ALRT-TYPE
           MOVE 'H'               TO ALRT-SEVERITY
           MOVE WS-SRC-CUST       TO ALRT-CUST-ID
           MOVE SWP-SOURCE-ACCT   TO ALRT-ACCT-NUMBER
           MOVE SWR-REQUIRED-AMT  TO ALRT-AMOUNT
           STRING 'SWP-' SWR-REASON-CODE DELIMITED BY SIZE
             INTO ALRT-RULE-ID
           STRING SWP-ID ' ' SWR-REASON-TEXT ' PARENT '
                  SWP-TARGET-ACCT DELIMITED BY SIZE
             INTO ALRT-DESCRIPTION
           MOVE 'O'               TO ALRT-STATUS
           MOVE 'SWEEPRUN'        TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD
           ADD 1 TO WS-ALERT-COUNT.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'TOTAL VIRTUAL CONCENTRATION' TO WS-RT-LABEL
           MOVE WS-TOTAL-VIRTUAL TO WS-RT-AMOUNT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'TOTAL SWEPT OUT (TO PARENT / INVESTMENT)'
             TO WS-RT-LABEL
           MOVE WS-TOTAL-OUT TO WS-RT-AMOUNT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'TOTAL SWEPT IN (FUNDING / REDEMPTION)'
             TO WS-RT-LABEL
           MOVE WS-TOTAL-IN TO WS-RT-AMOUNT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           CLOSE SWEEP-FILE ACCOUNT-FILE TRANPOST-FILE ALERT-FILE
                 REPORT-FILE
           DISPLAY 'SWEEPRUN SWEEPS READ=' WS-SWEEPS-READ
                   ' ELIGIBLE=' WS-SWEEPS-ELIGIBLE
                   ' MOVED=' WS-SWEEPS-MOVED
                   ' SKIPPED=' WS-SWEEPS-SKIPPED
           DISPLAY 'SWEEPRUN VIRTUAL MOVED=' WS-VIRTUAL-MOVED
                   ' TRANPOST WRITTEN=' WS-POST-COUNT
                   ' ALERTS=' WS-ALERT-COUNT
           MOVE WS-RUN-RC TO RETURN-CODE.

       9900-ABEND-OPEN.
           MOVE '1000-INITIALISE' TO ABND-PARAGRAPH
           MOVE 'OPEN FAILED' TO ABND-MESSAGE
           PERFORM 9910-ABEND.

       9910-ABEND.
           MOVE 'SWEEPRUN' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
