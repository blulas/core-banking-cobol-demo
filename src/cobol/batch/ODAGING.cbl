      *================================================================*
      * PROGRAM  : ODAGING                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : EOD STEP 13 - OVERDRAFT AGING. FOR EVERY DEPOSIT    *
      *            ACCOUNT WITH A NEGATIVE LEDGER BALANCE:             *
      *            - ADD 1 TO ACCT-DAYS-OVERDRAWN (ONCE PER BUSINESS   *
      *              DAY RUN)                                          *
      *            - DAY 7   SUSTAINED OVERDRAFT FEE $35.00, ONE TIME, *
      *                      WAIVED FOR PRIVATE WEALTH                 *
      *            - DAY 30  RESTRICT ACCOUNT (STATUS R, REASON 'OD')  *
      *                      AND RAISE AN ALERT                        *
      *            - DAY 60+ CHARGE OFF: DCOF CREDIT BRINGS LEDGER TO  *
      *                      ZERO, STATUS X, ALERT ODCO                *
      *            ACCOUNTS BACK TO ZERO OR BETTER HAVE THE COUNTER    *
      *            RESET; AN 'OD' RESTRICTION IS LIFTED.               *
      *            CORPORATE OPERATING ACCOUNTS (COPR) ARE EXCLUDED -  *
      *            THEY ARE COVERED BY THE POOLING ARRANGEMENT         *
      *            (POOLCALC) AND THE CREDIT FACILITY, NOT RETAIL OD.  *
      *            ZBA SUB-ACCOUNTS ARE EXCLUDED - SWEEPRUN FUNDS THEM.*
      * INPUT    : CUSTMST, BANKCTL                                    *
      * I-O      : ACCTMST                                             *
      * OUTPUT   : TRANPOST, ALERTS (EXTEND), ODAGRPT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-06-03 RJT  ORIGINAL - CHARGE OFF AT 90 DAYS               *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-04-19 BHS  CHARGE OFF AT 60 DAYS (FFIEC UNIFORM RETAIL    *
      *                 CREDIT CLASSIFICATION POLICY)                  *
      * 2012-02-27 DPS  SUSTAINED OD FEE DAY 7 $35 (CHG1102)           *
      * 2013-09-16 DPS  RESTRICT AT DAY 30                             *
      * 2015-10-02 DPS  CORE CONVERSION - PW FEE WAIVER, SEQ 70000000+ *
      * 2019-03-25 AKW  COPR EXCLUDED - POOLED (INC24006)              *
      * 2019-11-04 AKW  ZBA SUB-ACCOUNTS EXCLUDED (INC25120)           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ODAGING.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ODAGRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  TRANPOST-FILE.
       01  TRANPOST-REC                    PIC X(281).
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-ACCT-STATUS                  PIC X(02).
       01  WS-CUST-STATUS                  PIC X(02).
       01  WS-POST-STATUS                  PIC X(02).
       01  WS-ALRT-STATUS                  PIC X(02).
       01  WS-RPT-STATUS                   PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.

      *----------------------------------------------------------------*
      * OVERDRAFT AGING POLICY                                         *
      *----------------------------------------------------------------*
       01  WS-AGING-POLICY.
           05  WS-SUSTAINED-FEE-DAY        PIC 9(03) VALUE 007.
           05  WS-SUSTAINED-FEE            PIC S9(05)V99 VALUE 35.00.
           05  WS-RESTRICT-DAY             PIC 9(03) VALUE 030.
      *    2004-04 BHS WAS 090
           05  WS-CHARGE-OFF-DAY           PIC 9(03) VALUE 060.

       01  WS-WORK.
           05  WS-SEGMENT                  PIC X(02).
           05  WS-CO-AMT                   PIC S9(13)V99 COMP-3.
           05  WS-GEN-SEQ                  PIC 9(08) VALUE 70000000.
           05  WS-CHANGED                  PIC X(01).
           05  WS-ACTION                   PIC X(24).
       01  WS-COUNTERS.
           05  WS-READ                     PIC 9(07) VALUE 0.
           05  WS-OVERDRAWN                PIC 9(07) VALUE 0.
           05  WS-EXCLUDED                 PIC 9(07) VALUE 0.
           05  WS-RESET                    PIC 9(07) VALUE 0.
           05  WS-FEES                     PIC 9(07) VALUE 0.
           05  WS-RESTRICTED               PIC 9(07) VALUE 0.
           05  WS-CHARGED-OFF              PIC 9(07) VALUE 0.
           05  WS-CO-TOTAL                 PIC S9(15)V99 COMP-3
                                                     VALUE 0.
       01  WS-RPT-DETAIL.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-ACCT                     PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-PROD                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-STATUS                   PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-LEDGER                   PIC -,---,---,--9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-DAYS                     PIC ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-ACTION                   PIC X(24).
       01  WS-RPT-DATE-ED                  PIC 9999/99/99.

       COPY TRANREC.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY IS NOT LESS THAN ACCT-NUMBER
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2000-AGE-ACCOUNT
               END-READ
           END-PERFORM
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN I-O ACCOUNT-FILE
           OPEN INPUT CUSTOMER-FILE
           IF WS-ACCT-STATUS NOT = '00' OR WS-CUST-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - ACCTMST / CUSTMST' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN EXTEND TRANPOST-FILE ALERT-FILE
           IF (WS-POST-STATUS NOT = '00' AND NOT = '05')
              OR (WS-ALRT-STATUS NOT = '00' AND NOT = '05')
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - TRANPOST / ALERTS' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO WS-RPT-DATE-ED
           MOVE SPACES TO REPORT-LINE
           STRING 'CBS  ODAGING  OVERDRAFT AGING REPORT  '
                  '  BUSINESS DATE '
                  WS-RPT-DATE-ED DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE '  ACCOUNT       PROD  S          LEDGER   DAYS  ACTION'
             TO REPORT-LINE
           WRITE REPORT-LINE.

       2000-AGE-ACCOUNT.
           ADD 1 TO WS-READ
           MOVE 'N' TO WS-CHANGED
           MOVE SPACES TO WS-ACTION
      *    NOTHING TO AGE ON CLOSED, ESCHEATED OR CHARGED-OFF ACCOUNTS
           IF ACCT-CLOSED OR ACCT-ESCHEATED OR ACCT-CHARGED-OFF
               EXIT PARAGRAPH
           END-IF
           IF ACCT-PRODUCT-CODE = 'COPR'
               IF ACCT-LEDGER-BAL < 0
                   ADD 1 TO WS-EXCLUDED
                   MOVE 'EXCLUDED - POOLED COPR' TO WS-ACTION
                   PERFORM 8000-REPORT-LINE
               END-IF
               EXIT PARAGRAPH
           END-IF
      *    ZBA SUB-ACCOUNTS ARE FUNDED BY THE PARENT IN SWEEPRUN
           IF ACCT-PARENT-ACCT NOT = SPACES AND ACCT-HAS-SWEEP
               IF ACCT-LEDGER-BAL < 0
                   ADD 1 TO WS-EXCLUDED
                   MOVE 'EXCLUDED - ZBA SUB-ACCT' TO WS-ACTION
                   PERFORM 8000-REPORT-LINE
               END-IF
               EXIT PARAGRAPH
           END-IF

           IF ACCT-LEDGER-BAL < 0
               ADD 1 TO WS-OVERDRAWN
               IF ACCT-DAYS-OVERDRAWN < 999
                   ADD 1 TO ACCT-DAYS-OVERDRAWN
               END-IF
               MOVE 'Y' TO WS-CHANGED
               MOVE 'AGED' TO WS-ACTION
               EVALUATE TRUE
                   WHEN ACCT-DAYS-OVERDRAWN = WS-SUSTAINED-FEE-DAY
                       PERFORM 3000-SUSTAINED-FEE
                   WHEN ACCT-DAYS-OVERDRAWN = WS-RESTRICT-DAY
                       PERFORM 4000-RESTRICT
                   WHEN ACCT-DAYS-OVERDRAWN NOT < WS-CHARGE-OFF-DAY
                       PERFORM 5000-CHARGE-OFF
               END-EVALUATE
           ELSE
               IF ACCT-DAYS-OVERDRAWN > 0
                   MOVE 0 TO ACCT-DAYS-OVERDRAWN
                   MOVE 'Y' TO WS-CHANGED
                   ADD 1 TO WS-RESET
                   MOVE 'RESET - BROUGHT CURRENT' TO WS-ACTION
                   IF ACCT-RESTRICTED AND ACCT-RESTRICT-REASON = 'OD'
                       SET ACCT-ACTIVE TO TRUE
                       MOVE SPACES TO ACCT-RESTRICT-REASON
                       MOVE 'RESET - OD RESTRICT LIFTED' TO WS-ACTION
                   END-IF
               END-IF
           END-IF

           IF WS-CHANGED = 'Y'
               MOVE CTL-BUSINESS-DATE TO ACCT-LAST-MAINT-DATE
               MOVE 'ODAGING'         TO ACCT-LAST-MAINT-USER
               REWRITE ACCOUNT-RECORD
               IF WS-ACCT-STATUS NOT = '00'
                   MOVE 'ACCTMST' TO ABND-FILE-NAME
                   MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
                   MOVE 'REWRITE FAILED' TO ABND-MESSAGE
                   PERFORM 9900-ABEND
               END-IF
               PERFORM 8000-REPORT-LINE
           END-IF.

      *----------------------------------------------------------------*
      * DAY 7 - ONE-TIME SUSTAINED OVERDRAFT FEE (NOT FOR PW)          *
      *----------------------------------------------------------------*
       3000-SUSTAINED-FEE.
           MOVE 'RT' TO WS-SEGMENT
           MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
               NOT INVALID KEY MOVE CUST-SEGMENT TO WS-SEGMENT
           END-READ
           IF WS-SEGMENT = 'PW'
               MOVE 'DAY 7 - FEE WAIVED (PW)' TO WS-ACTION
               EXIT PARAGRAPH
           END-IF
           SUBTRACT WS-SUSTAINED-FEE FROM ACCT-LEDGER-BAL
                                          ACCT-AVAIL-BAL
           ADD WS-SUSTAINED-FEE TO ACCT-FEES-MTD ACCT-FEES-YTD
           MOVE 'WFOD' TO TRAN-CODE
           MOVE 'D'    TO TRAN-DR-CR
           MOVE WS-SUSTAINED-FEE TO TRAN-AMOUNT
           MOVE 'SUSTAINED OVERDRAFT FEE - DAY 7' TO TRAN-DESCRIPTION
           PERFORM 7000-WRITE-TRANPOST
           ADD 1 TO WS-FEES
           MOVE 'DAY 7 - SUSTAINED OD FEE' TO WS-ACTION.

      *----------------------------------------------------------------*
      * DAY 30 - RESTRICT (UNLESS ALREADY RESTRICTED FOR ANOTHER       *
      * REASON, E.G. OFAC) AND ALERT                                   *
      *----------------------------------------------------------------*
       4000-RESTRICT.
           IF ACCT-ACTIVE OR ACCT-DORMANT
               SET ACCT-RESTRICTED TO TRUE
               MOVE 'OD' TO ACCT-RESTRICT-REASON
               ADD 1 TO WS-RESTRICTED
               MOVE 'DAY 30 - RESTRICTED OD' TO WS-ACTION
           ELSE
               MOVE 'DAY 30 - ALREADY RESTR' TO WS-ACTION
           END-IF
           MOVE 'LIMT' TO ALRT-TYPE
           MOVE 'H'    TO ALRT-SEVERITY
           MOVE 'ODAG0030' TO ALRT-RULE-ID
           MOVE 'ACCOUNT OVERDRAWN 30 DAYS - RESTRICTED, COLLECTIONS'
             TO ALRT-DESCRIPTION
           PERFORM 7500-WRITE-ALERT.

      *----------------------------------------------------------------*
      * DAY 60 - CHARGE OFF THE NEGATIVE LEDGER BALANCE                *
      *----------------------------------------------------------------*
       5000-CHARGE-OFF.
           COMPUTE WS-CO-AMT = 0 - ACCT-LEDGER-BAL
           ADD WS-CO-AMT TO ACCT-LEDGER-BAL ACCT-AVAIL-BAL
           ADD WS-CO-AMT TO WS-CO-TOTAL
           SET ACCT-CHARGED-OFF TO TRUE
           MOVE 'DCOF' TO TRAN-CODE
           MOVE 'C'    TO TRAN-DR-CR
           MOVE WS-CO-AMT TO TRAN-AMOUNT
           MOVE 'OVERDRAFT CHARGE-OFF' TO TRAN-DESCRIPTION
           PERFORM 7000-WRITE-TRANPOST
           MOVE 0 TO ACCT-DAYS-OVERDRAWN
           ADD 1 TO WS-CHARGED-OFF
           MOVE 'DAY 60 - CHARGED OFF' TO WS-ACTION
           MOVE 'ODCO' TO ALRT-TYPE
           MOVE 'C'    TO ALRT-SEVERITY
           MOVE 'ODAG0060' TO ALRT-RULE-ID
           MOVE 'OVERDRAFT CHARGED OFF AT 60 DAYS - REFER TO RECOVERY'
             TO ALRT-DESCRIPTION
           PERFORM 7500-WRITE-ALERT.

       7000-WRITE-TRANPOST.
           ADD 1 TO WS-GEN-SEQ
           MOVE CTL-BUSINESS-DATE  TO TRAN-ID-DATE
           MOVE WS-GEN-SEQ         TO TRAN-ID-SEQ
           MOVE ACCT-NUMBER        TO TRAN-ACCT-NUMBER
           MOVE CTL-BUSINESS-DATE  TO TRAN-EFFECTIVE-DATE
                                      TRAN-POST-DATE
           MOVE ZERO               TO TRAN-TIME TRAN-HOLD-DAYS
                                      TRAN-FEE-AMOUNT
           MOVE 'BAT'              TO TRAN-CHANNEL
           MOVE SPACES             TO TRAN-REFERENCE TRAN-COUNTER-ACCT
                                      TRAN-COUNTERPARTY-NAME
                                      TRAN-COUNTERPARTY-ROUTING
                                      TRAN-COUNTERPARTY-COUNTRY
                                      TRAN-REJECT-CODE TRAN-ORIG-TRAN-ID
                                      TRAN-GL-ACCOUNT
           SET TRAN-POSTED TO TRUE
           MOVE 'ODAGING'          TO TRAN-USER-ID TRAN-SOURCE-PGM
           MOVE ACCT-COST-CENTER   TO TRAN-COST-CENTER
           MOVE 'N'                TO TRAN-CASH-FLAG TRAN-REGD-FLAG
           IF TRAN-IS-DEBIT
               MOVE 'Y' TO TRAN-OD-FLAG
           ELSE
               MOVE 'N' TO TRAN-OD-FLAG
           END-IF
           WRITE TRANPOST-REC FROM TRANSACTION-RECORD
           IF WS-POST-STATUS NOT = '00'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               MOVE 'WRITE FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF.

       7500-WRITE-ALERT.
           MOVE CTL-BUSINESS-DATE    TO ALRT-DATE
           MOVE ZERO                 TO ALRT-TIME
           MOVE ACCT-PRIMARY-CUST-ID TO ALRT-CUST-ID
           MOVE ACCT-NUMBER          TO ALRT-ACCT-NUMBER
           MOVE ACCT-LEDGER-BAL      TO ALRT-AMOUNT
           IF ALRT-TYPE = 'ODCO'
               MOVE WS-CO-AMT        TO ALRT-AMOUNT
           END-IF
           SET ALRT-OPEN TO TRUE
           MOVE 'ODAGING'            TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD
           IF WS-ALRT-STATUS NOT = '00'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               MOVE 'WRITE FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF.

       8000-REPORT-LINE.
           MOVE ACCT-NUMBER         TO RD-ACCT
           MOVE ACCT-PRODUCT-CODE   TO RD-PROD
           MOVE ACCT-STATUS         TO RD-STATUS
           MOVE ACCT-LEDGER-BAL     TO RD-LEDGER
           MOVE ACCT-DAYS-OVERDRAWN TO RD-DAYS
           MOVE WS-ACTION           TO RD-ACTION
           WRITE REPORT-LINE FROM WS-RPT-DETAIL.

       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           STRING '  READ ' WS-READ '  OVERDRAWN ' WS-OVERDRAWN
                  '  EXCLUDED ' WS-EXCLUDED '  RESET ' WS-RESET
                  '  FEES ' WS-FEES '  RESTRICTED ' WS-RESTRICTED
                  '  CHARGED OFF ' WS-CHARGED-OFF
                  DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           CLOSE ACCOUNT-FILE CUSTOMER-FILE TRANPOST-FILE ALERT-FILE
                 REPORT-FILE
           DISPLAY 'ODAGING READ=' WS-READ ' OVERDRAWN=' WS-OVERDRAWN
                   ' EXCLUDED=' WS-EXCLUDED ' RESET=' WS-RESET
                   ' FEES=' WS-FEES ' RESTRICTED=' WS-RESTRICTED
                   ' CHARGED-OFF=' WS-CHARGED-OFF
           MOVE 0 TO RETURN-CODE.

       9900-ABEND.
           MOVE 'ODAGING' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
