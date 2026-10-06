      *================================================================*
      * PROGRAM  : CDMATUR                                             *
      * TYPE     : BATCH - EOD STEP 14 (DAILY)                         *
      * PURPOSE  : CERTIFICATE OF DEPOSIT MATURITY PROCESSING.         *
      *            FOR EVERY CD MATURING ON OR BEFORE THE BUSINESS     *
      *            DATE:                                               *
      *            1. PAY THE ACCRUED INTEREST TO THE CD (DINT), WITH  *
      *               24% BACKUP WITHHOLDING WHEN B-NOTICED (WBWH)     *
      *            2. APPLY THE RENEWAL INSTRUCTION                    *
      *               A - RENEW FOR THE SAME TERM AT THE CURRENT CDT1  *
      *                   RATE (RATETIER); NEW MATURITY = OLD MATURITY *
      *                   + TERM MONTHS (DATEUTIL ADDM); GRACE PERIOD  *
      *                   ENDS 10 CALENDAR DAYS AFTER OLD MATURITY     *
      *               M - MOVE PROCEEDS TO THE LINKED SAVINGS / MONEY  *
      *                   MARKET ACCOUNT (WMAT / DMAT) AND CLOSE. AN   *
      *                   INVALID SAVINGS ACCOUNT FALLS BACK TO AUTO   *
      *                   RENEWAL (DEPOSITOR PROTECTION, POL-DEP-014)  *
      *               C - PAY PROCEEDS TO THE PAYOUT ACCOUNT AND CLOSE.*
      *                   AN INVALID PAYOUT ACCOUNT LEAVES THE FUNDS   *
      *                   IN THE MATURED CD FOR BRANCH FOLLOW-UP       *
      *            3. A CD WHOSE OWNER IS DECEASED IS NEVER RENEWED -  *
      *               FUNDS ARE HELD FOR THE ESTATE                    *
      *            CDS IN THEIR POST-RENEWAL GRACE PERIOD NEED NO      *
      *            ACTION AND ARE LISTED ONLY. REG DD 1030.5(B) PRE-   *
      *            MATURITY NOTICES: AUTO-RENEW CDS 30 DAYS BEFORE     *
      *            MATURITY, NON-RENEWING CDS OVER 1 YEAR 10 DAYS.     *
      * INPUT    : BANKCTL (GETCTL), PRODMST, CUSTMST                  *
      * I-O      : ACCTMST                                             *
      * OUTPUT   : TRANPOST (EXTEND), CDMATRPT NOTIFICATION REPORT     *
      * CALLS    : GETCTL, RATETIER, DATEUTIL, ABENDRTN                *
      * TRAN IDS : 45000001 - 49999999                                 *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1994-10-03 RJT  TRUTH IN SAVINGS - MATURITY NOTICES            *
      * 2004-11-15 BHS  GRACE PERIOD 10 CALENDAR DAYS - CHG0301        *
      * 2008-11-24 DPS  RENEWAL RATE FROM RATETIER (WAS RATE TABLE     *
      *                 READ INLINE)                                   *
      * 2012-08-14 DPS  MATURITY DATE VIA DATEUTIL ADDM (EOM CLAMP)    *
      * 2015-04-27 DPS  INVALID SAVINGS ACCT FALLS BACK TO RENEWAL     *
      * 2019-02-11 AKW  NO RENEWAL WHEN OWNER DECEASED - INC18820      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDMATUR.

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
           SELECT PRODUCT-FILE ASSIGN TO "PRODMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PROD-CODE
               FILE STATUS IS WS-PROD-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CDMATRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  PRODUCT-FILE.
       COPY PRODREC.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  TRANPOST-FILE.
       COPY TRANREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-PROD-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-POST-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).

       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.

       01  WS-CONSTANTS.
           05  WS-GRACE-DAYS               PIC S9(05) VALUE 10.
           05  WS-AUTO-NOTICE-DAYS         PIC S9(05) VALUE 30.
           05  WS-NONAUTO-NOTICE-DAYS      PIC S9(05) VALUE 10.
           05  WS-BACKUP-WH-RATE           PIC V9(04) VALUE .2400.

       01  WS-MATURING-TABLE.
           05  WS-MAT-COUNT                PIC 9(04) COMP VALUE 0.
           05  WS-MAT-ACCT                 PIC X(12) OCCURS 500
                                           INDEXED BY WS-MT-IDX.

       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-RENEW-COUNT              PIC 9(07) VALUE 0.
           05  WS-PAYOUT-COUNT             PIC 9(07) VALUE 0.
           05  WS-HELD-COUNT               PIC 9(07) VALUE 0.
           05  WS-GRACE-COUNT              PIC 9(07) VALUE 0.
           05  WS-NOTICE-COUNT             PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
           05  WS-TRAN-SEQ                 PIC 9(08) VALUE 45000000.

       01  WS-WORK.
           05  WS-DAYS-TO-MAT              PIC S9(07).
           05  WS-OLD-MATURITY             PIC 9(08).
           05  WS-OLD-RATE                 PIC 9(03)V9(06).
           05  WS-PAY-AMT                  PIC S9(13)V99 COMP-3.
           05  WS-WH-AMT                   PIC S9(13)V99 COMP-3.
           05  WS-PROCEEDS                 PIC S9(13)V99 COMP-3.
           05  WS-CD-ACCT                  PIC X(12).
           05  WS-CD-PROD                  PIC X(04).
           05  WS-PAYOUT-ACCT              PIC X(12).
           05  WS-COST-CENTER              PIC X(05).
           05  WS-ACTION                   PIC X(01).
               88  WS-DO-RENEW                       VALUE 'A'.
               88  WS-DO-PAYOUT                      VALUE 'P'.
               88  WS-DO-HOLD                        VALUE 'H'.
           05  WS-PAYOUT-OK                PIC X(01).
           05  WS-POST-ACCT                PIC X(12).
           05  WS-POST-AMT                 PIC S9(13)V99 COMP-3.
           05  WS-POST-CODE                PIC X(04).
           05  WS-POST-DRCR                PIC X(01).
           05  WS-POST-DESC                PIC X(40).
           05  WS-POST-COUNTER             PIC X(17).
           05  WS-OWNER-NAME               PIC X(40).
           05  WS-DECEASED                 PIC X(01).

       01  WS-HEADING-1.
           05  FILLER PIC X(08) VALUE 'CDMATUR '.
           05  FILLER PIC X(50)
               VALUE 'CD MATURITY / RENEWAL NOTIFICATION REPORT'.
           05  FILLER PIC X(15) VALUE 'BUSINESS DATE '.
           05  H1-BUS-DATE                 PIC 9(08).
           05  FILLER PIC X(51) VALUE SPACES.
       01  WS-MSG-LINE.
           05  ML-ACCT                     PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  ML-PROD                     PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  ML-OWNER                    PIC X(30).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  ML-TEXT                     PIC X(83).
       01  WS-TEXT-WORK.
           05  WT-AMT                      PIC ---,---,---,--9.99.
           05  WT-RATE                     PIC ZZ9.999999.
           05  WT-APY                      PIC ZZ9.999999.
           05  WT-DAYS                     PIC ZZZ9.
       01  WS-TOTAL-LINE.
           05  TL-LABEL                    PIC X(30).
           05  TL-COUNT                    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(95) VALUE SPACES.

       COPY BANKCTL.
       COPY RTTPARM.
       COPY DATEPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-SCAN-CDS
           PERFORM 3000-PROCESS-MATURITIES
               VARYING WS-MT-IDX FROM 1 BY 1
               UNTIL WS-MT-IDX > WS-MAT-COUNT
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALIZE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN I-O ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN INPUT PRODUCT-FILE CUSTOMER-FILE
           IF WS-PROD-STATUS NOT = '00' OR WS-CUST-STATUS NOT = '00'
               MOVE 'PRODMST' TO ABND-FILE-NAME
               MOVE WS-PROD-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN EXTEND TRANPOST-FILE
           IF WS-POST-STATUS NOT = '00' AND '05'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO H1-BUS-DATE
           WRITE REPORT-LINE FROM WS-HEADING-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE.

      *----------------------------------------------------------------*
      * PASS 1 - COLLECT MATURING CDS; LIST GRACE PERIOD AND NOTICES.  *
      * (MATURITIES ARE PROCESSED IN PASS 2 BY KEY BECAUSE THE PAYOUT  *
      * ACCOUNT MUST BE READ RANDOMLY.)                                *
      *----------------------------------------------------------------*
       2000-SCAN-CDS.
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY >= ACCT-NUMBER
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-CHECK-CD
               END-READ
           END-PERFORM.

       2100-CHECK-CD.
           ADD 1 TO WS-READ-COUNT
           IF ACCT-CATEGORY NOT = 'TD' OR NOT ACCT-ACTIVE
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-NUMBER       TO WS-CD-ACCT
           MOVE ACCT-PRODUCT-CODE TO WS-CD-PROD
           IF ACCT-MATURITY-DATE = ZERO
               EXIT PARAGRAPH
           END-IF
           IF ACCT-MATURITY-DATE <= CTL-BUSINESS-DATE
               IF ACCT-LEDGER-BAL > ZERO OR ACCT-ACCRUED-INT > ZERO
                   IF WS-MAT-COUNT < 500
                       ADD 1 TO WS-MAT-COUNT
                       MOVE ACCT-NUMBER TO WS-MAT-ACCT(WS-MAT-COUNT)
                   END-IF
               END-IF
               EXIT PARAGRAPH
           END-IF

           PERFORM 8300-GET-OWNER
           IF ACCT-GRACE-END-DATE >= CTL-BUSINESS-DATE
               STRING 'IN GRACE PERIOD UNTIL ' ACCT-GRACE-END-DATE
                      ' - NO ACTION (RENEWED, PENALTY-FREE WD)'
                      DELIMITED BY SIZE INTO ML-TEXT
               PERFORM 8400-WRITE-MSG
               ADD 1 TO WS-GRACE-COUNT
               EXIT PARAGRAPH
           END-IF

           INITIALIZE DATE-PARMS
           SET DTU-DAYS-BETWEEN TO TRUE
           MOVE CTL-BUSINESS-DATE  TO DTU-DATE-1
           MOVE ACCT-MATURITY-DATE TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-NUMBER TO WS-DAYS-TO-MAT
           MOVE WS-DAYS-TO-MAT TO WT-DAYS
           EVALUATE TRUE
               WHEN ACCT-RENEW-AUTO AND ACCT-TERM-MONTHS > 1
                AND WS-DAYS-TO-MAT <= WS-AUTO-NOTICE-DAYS
                   STRING 'PRE-MATURITY NOTICE DUE (AUTO-RENEW) - '
                          'MATURES ' ACCT-MATURITY-DATE ' IN '
                          WT-DAYS ' DAYS'
                          DELIMITED BY SIZE INTO ML-TEXT
                   PERFORM 8400-WRITE-MSG
                   ADD 1 TO WS-NOTICE-COUNT
               WHEN NOT ACCT-RENEW-AUTO AND ACCT-TERM-MONTHS > 12
                AND WS-DAYS-TO-MAT <= WS-NONAUTO-NOTICE-DAYS
                   STRING 'PRE-MATURITY NOTICE DUE (NO RENEWAL) - '
                          'MATURES ' ACCT-MATURITY-DATE ' IN '
                          WT-DAYS ' DAYS'
                          DELIMITED BY SIZE INTO ML-TEXT
                   PERFORM 8400-WRITE-MSG
                   ADD 1 TO WS-NOTICE-COUNT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS 2 - ONE MATURING CD                                       *
      *----------------------------------------------------------------*
       3000-PROCESS-MATURITIES.
           MOVE WS-MAT-ACCT(WS-MT-IDX) TO ACCT-NUMBER WS-CD-ACCT
           READ ACCOUNT-FILE
               INVALID KEY
                   ADD 1 TO WS-ERROR-COUNT
                   EXIT PARAGRAPH
           END-READ
           MOVE ACCT-PRODUCT-CODE TO WS-CD-PROD
           MOVE ACCT-PRODUCT-CODE TO PROD-CODE
           READ PRODUCT-FILE
               INVALID KEY
                   ADD 1 TO WS-ERROR-COUNT
                   EXIT PARAGRAPH
           END-READ
           PERFORM 8300-GET-OWNER
           MOVE ACCT-MATURITY-DATE TO WS-OLD-MATURITY
           MOVE ACCT-INT-RATE      TO WS-OLD-RATE
           MOVE ACCT-PAYOUT-ACCT   TO WS-PAYOUT-ACCT
           MOVE ACCT-COST-CENTER   TO WS-COST-CENTER

           PERFORM 3100-PAY-INTEREST
           PERFORM 3200-DECIDE-ACTION
           EVALUATE TRUE
               WHEN WS-DO-RENEW
                   PERFORM 3300-RENEW
               WHEN WS-DO-PAYOUT
                   PERFORM 3400-PAYOUT
               WHEN OTHER
                   PERFORM 8500-REWRITE-ACCT
                   ADD 1 TO WS-HELD-COUNT
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST TO THE CD ITSELF. ROUNDING REMAINDER IS DROPPED AT    *
      * MATURITY (A NEW TERM STARTS FROM ZERO ACCRUAL).                *
      *----------------------------------------------------------------*
       3100-PAY-INTEREST.
           COMPUTE WS-PAY-AMT ROUNDED = ACCT-ACCRUED-INT
           MOVE ZERO TO ACCT-ACCRUED-INT
           IF WS-PAY-AMT > ZERO
               MOVE ACCT-NUMBER TO WS-POST-ACCT
               MOVE WS-PAY-AMT  TO WS-POST-AMT
               MOVE 'DINT' TO WS-POST-CODE
               MOVE 'C'    TO WS-POST-DRCR
               MOVE 'CD INTEREST PAID AT MATURITY' TO WS-POST-DESC
               MOVE SPACES TO WS-POST-COUNTER
               PERFORM 8000-WRITE-TRANPOST
               ADD WS-PAY-AMT TO ACCT-LEDGER-BAL ACCT-AVAIL-BAL
                                 ACCT-INT-PAID-YTD
               MOVE CTL-BUSINESS-DATE TO ACCT-LAST-CAP-DATE
               MOVE WS-PAY-AMT TO WT-AMT
               STRING 'MATURED ' WS-OLD-MATURITY
                      ' - INTEREST PAID ' WT-AMT
                      DELIMITED BY SIZE INTO ML-TEXT
               PERFORM 8400-WRITE-MSG
               IF ACCT-BACKUP-WITHHOLDING
                   COMPUTE WS-WH-AMT ROUNDED =
                           WS-PAY-AMT * WS-BACKUP-WH-RATE
                   MOVE WS-WH-AMT TO WS-POST-AMT
                   MOVE 'WBWH' TO WS-POST-CODE
                   MOVE 'D'    TO WS-POST-DRCR
                   MOVE 'IRS BACKUP WITHHOLDING 24%' TO WS-POST-DESC
                   PERFORM 8000-WRITE-TRANPOST
                   SUBTRACT WS-WH-AMT FROM ACCT-LEDGER-BAL
                                           ACCT-AVAIL-BAL
                   ADD WS-WH-AMT TO ACCT-WITHHELD-YTD
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * RENEWAL DECISION                                               *
      *----------------------------------------------------------------*
       3200-DECIDE-ACTION.
           EVALUATE TRUE
               WHEN WS-DECEASED = 'Y'
                   SET WS-DO-HOLD TO TRUE
                   MOVE 'OWNER DECEASED - NOT RENEWED, HELD FOR ESTATE'
                     TO ML-TEXT
                   PERFORM 8400-WRITE-MSG
               WHEN ACCT-RENEW-AUTO
                   SET WS-DO-RENEW TO TRUE
               WHEN ACCT-RENEW-TO-SAVINGS
                   PERFORM 3250-CHECK-PAYOUT-ACCT
                   IF WS-PAYOUT-OK = 'S'
                       SET WS-DO-PAYOUT TO TRUE
                   ELSE
                       SET WS-DO-RENEW TO TRUE
                       MOVE 'SAVINGS ACCT INVALID - AUTO RENEWED'
                         TO ML-TEXT
                       PERFORM 8400-WRITE-MSG
                   END-IF
               WHEN ACCT-RENEW-PAYOUT
                   PERFORM 3250-CHECK-PAYOUT-ACCT
                   IF WS-PAYOUT-OK = 'S' OR 'D'
                       SET WS-DO-PAYOUT TO TRUE
                   ELSE
                       SET WS-DO-HOLD TO TRUE
                       MOVE 'PAYOUT ACCT INVALID - FUNDS HELD IN CD'
                         TO ML-TEXT
                       PERFORM 8400-WRITE-MSG
                   END-IF
               WHEN OTHER
                   SET WS-DO-RENEW TO TRUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WS-PAYOUT-OK: S = OPEN SAVINGS/MM, D = OPEN DEMAND, N = BAD.   *
      * THE CD RECORD IS RE-READ AFTERWARDS (SAME FILE BUFFER).        *
      *----------------------------------------------------------------*
       3250-CHECK-PAYOUT-ACCT.
           MOVE 'N' TO WS-PAYOUT-OK
           PERFORM 8500-REWRITE-ACCT
           IF WS-PAYOUT-ACCT NOT = SPACES
               MOVE WS-PAYOUT-ACCT TO ACCT-NUMBER
               READ ACCOUNT-FILE
                   INVALID KEY CONTINUE
                   NOT INVALID KEY
                       IF ACCT-ACTIVE
                           EVALUATE ACCT-CATEGORY
                               WHEN 'SV'
                               WHEN 'MM'
                                   MOVE 'S' TO WS-PAYOUT-OK
                               WHEN 'DD'
                                   MOVE 'D' TO WS-PAYOUT-OK
                           END-EVALUATE
                       END-IF
               END-READ
           END-IF
           MOVE WS-CD-ACCT TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY ADD 1 TO WS-ERROR-COUNT
           END-READ.

      *----------------------------------------------------------------*
       3300-RENEW.
           INITIALIZE RATETIER-PARMS
           MOVE ACCT-PRODUCT-CODE  TO RTT-PRODUCT-CODE
           MOVE ACCT-LEDGER-BAL    TO RTT-BALANCE
           MOVE ACCT-TERM-MONTHS   TO RTT-TERM-MONTHS
           IF RTT-TERM-MONTHS = ZERO
               MOVE PROD-TERM-MONTHS TO RTT-TERM-MONTHS
               MOVE PROD-TERM-MONTHS TO ACCT-TERM-MONTHS
           END-IF
           MOVE CTL-BUSINESS-DATE  TO RTT-AS-OF-DATE
           MOVE 'N'                TO RTT-EMPLOYEE-FLAG
           CALL 'RATETIER' USING RATETIER-PARMS
           IF RTT-RETURN-CODE > 04
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'RATETIER FAILED - RENEWED AT PRIOR RATE'
                 TO ML-TEXT
               PERFORM 8400-WRITE-MSG
           ELSE
               MOVE RTT-RESULT-RATE TO ACCT-INT-RATE
           END-IF

           INITIALIZE DATE-PARMS
           SET DTU-ADD-MONTHS TO TRUE
           MOVE WS-OLD-MATURITY  TO DTU-DATE-1
           MOVE ACCT-TERM-MONTHS TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO ACCT-MATURITY-DATE

           INITIALIZE DATE-PARMS
           SET DTU-ADD-DAYS TO TRUE
           MOVE WS-OLD-MATURITY TO DTU-DATE-1
           MOVE WS-GRACE-DAYS   TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO ACCT-GRACE-END-DATE

           MOVE ACCT-LEDGER-BAL TO ACCT-ORIG-DEPOSIT-AMT
      *    NEW TERM ACCRUES FROM THE OLD MATURITY DATE (INTACCR)
           INITIALIZE DATE-PARMS
           SET DTU-ADD-DAYS TO TRUE
           MOVE WS-OLD-MATURITY TO DTU-DATE-1
           MOVE -1 TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO ACCT-LAST-ACCRUAL-DATE
           MOVE CTL-BUSINESS-DATE TO ACCT-LAST-MAINT-DATE
           MOVE 'CDMATUR' TO ACCT-LAST-MAINT-USER
           PERFORM 8500-REWRITE-ACCT
           ADD 1 TO WS-RENEW-COUNT

           MOVE ACCT-LEDGER-BAL TO WT-AMT
           MOVE ACCT-INT-RATE   TO WT-RATE
           MOVE RTT-RESULT-APY  TO WT-APY
           STRING 'RENEWED ' ACCT-TERM-MONTHS 'M ' WT-AMT
                  ' @' WT-RATE ' APY' WT-APY
                  DELIMITED BY SIZE INTO ML-TEXT
           PERFORM 8400-WRITE-MSG
           STRING '  NEW MATURITY ' ACCT-MATURITY-DATE
                  '  GRACE PERIOD ENDS ' ACCT-GRACE-END-DATE
                  DELIMITED BY SIZE INTO ML-TEXT
           PERFORM 8400-WRITE-MSG.

      *----------------------------------------------------------------*
      * WMAT FROM THE CD, DMAT TO THE PAYOUT ACCOUNT, CLOSE THE CD.    *
      *----------------------------------------------------------------*
       3400-PAYOUT.
           MOVE ACCT-LEDGER-BAL TO WS-PROCEEDS
           IF WS-PROCEEDS > ZERO
               MOVE ACCT-NUMBER    TO WS-POST-ACCT
               MOVE WS-PROCEEDS    TO WS-POST-AMT
               MOVE 'WMAT' TO WS-POST-CODE
               MOVE 'D'    TO WS-POST-DRCR
               STRING 'CD MATURITY PAYOUT TO ' WS-PAYOUT-ACCT
                      DELIMITED BY SIZE INTO WS-POST-DESC
               MOVE WS-PAYOUT-ACCT TO WS-POST-COUNTER
               PERFORM 8000-WRITE-TRANPOST
           END-IF
           MOVE ZERO TO ACCT-LEDGER-BAL ACCT-AVAIL-BAL
           SET ACCT-CLOSED TO TRUE
           MOVE CTL-BUSINESS-DATE TO ACCT-CLOSE-DATE
                                     ACCT-LAST-MAINT-DATE
           MOVE 'CDMATUR' TO ACCT-LAST-MAINT-USER
           PERFORM 8500-REWRITE-ACCT

           IF WS-PROCEEDS > ZERO
               MOVE WS-PAYOUT-ACCT TO ACCT-NUMBER
               READ ACCOUNT-FILE
                   INVALID KEY
                       ADD 1 TO WS-ERROR-COUNT
                       EXIT PARAGRAPH
               END-READ
               MOVE ACCT-NUMBER    TO WS-POST-ACCT
               MOVE 'DMAT' TO WS-POST-CODE
               MOVE 'C'    TO WS-POST-DRCR
               STRING 'CD MATURITY PROCEEDS FROM ' WS-CD-ACCT
                      DELIMITED BY SIZE INTO WS-POST-DESC
               MOVE WS-CD-ACCT TO WS-POST-COUNTER
               PERFORM 8000-WRITE-TRANPOST
               ADD WS-PROCEEDS TO ACCT-LEDGER-BAL ACCT-AVAIL-BAL
               ADD 1 TO ACCT-CR-COUNT-CYC
               PERFORM 8500-REWRITE-ACCT
               MOVE WS-CD-ACCT TO ACCT-NUMBER
           END-IF
           ADD 1 TO WS-PAYOUT-COUNT
           MOVE WS-PROCEEDS TO WT-AMT
           STRING 'PAID OUT ' WT-AMT ' TO ' WS-PAYOUT-ACCT
                  ' - CD CLOSED'
                  DELIMITED BY SIZE INTO ML-TEXT
           PERFORM 8400-WRITE-MSG.

      *----------------------------------------------------------------*
       8000-WRITE-TRANPOST.
           INITIALIZE TRANSACTION-RECORD
           ADD 1 TO WS-TRAN-SEQ
           MOVE CTL-BUSINESS-DATE TO TRAN-ID-DATE
                                     TRAN-EFFECTIVE-DATE
                                     TRAN-POST-DATE
           MOVE WS-TRAN-SEQ       TO TRAN-ID-SEQ
           MOVE WS-POST-ACCT      TO TRAN-ACCT-NUMBER
           MOVE 230500            TO TRAN-TIME
           MOVE WS-POST-CODE      TO TRAN-CODE
           MOVE WS-POST-DRCR      TO TRAN-DR-CR
           MOVE WS-POST-AMT       TO TRAN-AMOUNT
           MOVE 'BAT'             TO TRAN-CHANNEL
           MOVE WS-POST-DESC      TO TRAN-DESCRIPTION
           MOVE WS-POST-COUNTER   TO TRAN-COUNTER-ACCT
           SET TRAN-POSTED        TO TRUE
           MOVE 'CDMATUR'         TO TRAN-USER-ID
                                     TRAN-SOURCE-PGM
           MOVE WS-COST-CENTER    TO TRAN-COST-CENTER
           MOVE 'N'               TO TRAN-CASH-FLAG TRAN-REGD-FLAG
                                     TRAN-OD-FLAG
           WRITE TRANSACTION-RECORD
           IF WS-POST-STATUS NOT = '00'
               MOVE 'CDMATUR' TO ABND-PROGRAM
               MOVE '8000-WRITE-TRANPOST' TO ABND-PARAGRAPH
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               MOVE 'WRITE FAILED' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF.

      *----------------------------------------------------------------*
       8300-GET-OWNER.
           MOVE 'N' TO WS-DECEASED
           MOVE SPACES TO WS-OWNER-NAME
           MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE '** CUSTOMER NOT ON FILE **' TO WS-OWNER-NAME
               NOT INVALID KEY
                   STRING FUNCTION TRIM(CUST-FIRST-NAME) ' '
                          FUNCTION TRIM(CUST-LAST-NAME)
                          DELIMITED BY SIZE INTO WS-OWNER-NAME
                   IF CUST-DECEASED
                       MOVE 'Y' TO WS-DECEASED
                   END-IF
           END-READ.

       8400-WRITE-MSG.
           MOVE WS-CD-ACCT TO ML-ACCT
           MOVE WS-CD-PROD TO ML-PROD
           MOVE WS-OWNER-NAME TO ML-OWNER
           WRITE REPORT-LINE FROM WS-MSG-LINE
           MOVE SPACES TO ML-TEXT.

       8500-REWRITE-ACCT.
           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'CDMATUR' TO ABND-PROGRAM
               MOVE '8500-REWRITE-ACCT' TO ABND-PARAGRAPH
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'CDS RENEWED' TO TL-LABEL
           MOVE WS-RENEW-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'CDS PAID OUT AND CLOSED' TO TL-LABEL
           MOVE WS-PAYOUT-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'MATURED - FUNDS HELD' TO TL-LABEL
           MOVE WS-HELD-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'IN GRACE PERIOD' TO TL-LABEL
           MOVE WS-GRACE-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'PRE-MATURITY NOTICES DUE' TO TL-LABEL
           MOVE WS-NOTICE-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           CLOSE ACCOUNT-FILE PRODUCT-FILE CUSTOMER-FILE
                 TRANPOST-FILE REPORT-FILE
           DISPLAY 'CDMATUR MATURED=' WS-MAT-COUNT
                   ' RENEWED=' WS-RENEW-COUNT
                   ' PAIDOUT=' WS-PAYOUT-COUNT
                   ' HELD=' WS-HELD-COUNT
                   ' NOTICES=' WS-NOTICE-COUNT
                   ' ERRORS=' WS-ERROR-COUNT
           IF WS-ERROR-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND-OPEN.
           MOVE 'CDMATUR' TO ABND-PROGRAM
           MOVE '1000-INITIALIZE' TO ABND-PARAGRAPH
           MOVE 'FILE OPEN FAILED' TO ABND-MESSAGE
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
