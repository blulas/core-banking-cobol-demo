      *================================================================*
      * PROGRAM  : KYCREVW                                             *
      * TYPE     : BATCH - MONTH-END (EOD STEP 16, AFTER DORMSCAN)     *
      * PURPOSE  : PERIODIC KYC REVIEW SWEEP OF THE CUSTOMER MASTER.   *
      *            1. RE-SCORE CUSTOMER RISK (RISKSCOR) - UPGRADES     *
      *               APPLY AUTOMATICALLY, DOWNGRADES ARE REPORTED FOR *
      *               ANALYST REVIEW ONLY                              *
      *            2. RE-TIER (KYCTIER) - ESCALATION TO EDD SETS KYC   *
      *               STATUS R AND A 30-DAY EDD COMPLETION DEADLINE    *
      *            3. REVIEW DATE PASSED -> KYC STATUS E (EXPIRED)     *
      *            4. EXPIRED MORE THAN 90 DAYS -> ACTIVE ACCOUNTS     *
      *               RESTRICTED (ACCT-STATUS R, REASON 'KY')          *
      *            5. EDD OUTSTANDING / CIP PENDING > 30 DAYS ALERTS   *
      *            RUNS ON MONTH-END ONLY UNLESS PARM 'FORCE'.         *
      * INPUT    : BANKCTL (VIA GETCTL), TRANHIST + TRANPOST (CASH     *
      *            LOOKBACK, BOTH OPTIONAL)                            *
      * I-O      : CUSTMST, ACCTMST (VIA AIX ON PRIMARY CUSTOMER)      *
      * OUTPUT   : ALERTS (EXTEND, TYPE KYCX), KYCRPT                  *
      * CALLS    : RISKSCOR, KYCTIER, DATEUTIL, GETCTL, ABENDRTN       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2002-04-01 BHS  ORIGINAL - PATRIOT ACT PERIODIC REVIEW         *
      * 2006-08-30 BHS  PEP / EDD ALERTS                               *
      * 2010-03-08 DPS  ACCOUNT RESTRICTION AFTER 60 DAY GRACE         *
      * 2012-10-15 DPS  GRACE PERIOD 60 -> 90 DAYS (CHG0644)           *
      * 2015-06-22 DPS  NO AUTOMATIC RISK DOWNGRADES (OCC MRA 2015-07) *
      * 2018-05-11 AKW  FINCEN CDD RULE - RE-TIER VIA KYCTIER          *
      * 2019-10-07 AKW  CASH LOOKBACK 90 DAYS FOR RISKSCOR (CHG1844)   *
      * 2022-02-14 JLM  CIP PENDING > 30 DAYS ALERT (INC17730)         *
      * 2024-05-20 TKO  INCLUDE TODAY'S TRANPOST IN CASH LOOKBACK      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KYCREVW.
       AUTHOR. B H SANDOVAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT OPTIONAL TRANHIST-FILE ASSIGN TO "TRANHIST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-HIST-STATUS.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "KYCRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  TRANHIST-FILE.
       COPY TRANREC.
       FD  TRANPOST-FILE.
       01  TRANPOST-REC                    PIC X(281).
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-HIST-STATUS              PIC X(02).
           05  WS-POST-STATUS              PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).

       01  WS-PARM                         PIC X(10).
       01  WS-FLAGS.
           05  WS-EOF-FLAG                 PIC X(01) VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-TRAN-EOF-FLAG            PIC X(01).
               88  WS-TRAN-EOF                       VALUE 'Y'.
           05  WS-AIX-DONE-FLAG            PIC X(01).
               88  WS-AIX-DONE                       VALUE 'Y'.
           05  WS-CUST-CHANGED-FLAG        PIC X(01).
               88  WS-CUST-CHANGED                   VALUE 'Y'.
           05  WS-NEWLY-EXPIRED-FLAG       PIC X(01).
               88  WS-NEWLY-EXPIRED                  VALUE 'Y'.
           05  WS-EDD-ESCALATED-FLAG       PIC X(01).
               88  WS-EDD-ESCALATED                  VALUE 'Y'.

      *--- RULE VALUES ------------------------------------------------*
       01  WS-RULE-VALUES.
           05  WS-GRACE-DAYS               PIC S9(05) VALUE +90.
      *    2012-10 DPS CHG0644 - WAS 60
      *    05  WS-GRACE-DAYS               PIC S9(05) VALUE +60.
           05  WS-EDD-DEADLINE-DAYS        PIC S9(05) VALUE +30.
           05  WS-CIP-PENDING-DAYS         PIC S9(05) VALUE +30.
           05  WS-DUE-SOON-DAYS            PIC S9(05) VALUE +60.
           05  WS-CASH-LOOKBACK-DAYS       PIC S9(05) VALUE -90.
           05  WS-CASH-HEAVY-AMT           PIC S9(13)V99 COMP-3
                                           VALUE 10000.00.
           05  WS-CASH-MODERATE-AMT        PIC S9(13)V99 COMP-3
                                           VALUE 3000.00.

      *--- CASH ACTIVITY BY ACCOUNT (LOOKBACK WINDOW) ----------------*
       01  WS-CASH-TABLE.
           05  WS-CASH-COUNT               PIC 9(05) VALUE 0.
           05  WS-CASH-ENTRY OCCURS 1 TO 5000 TIMES
                             DEPENDING ON WS-CASH-COUNT
                             INDEXED BY WS-CSH-IDX.
               10  WS-CASH-ACCT            PIC X(12).
               10  WS-CASH-AMT             PIC S9(13)V99 COMP-3.
       01  WS-CASH-MAX                     PIC 9(05) VALUE 5000.

       01  WS-WORK.
           05  WS-CASH-CUTOFF              PIC 9(08).
           05  WS-CUST-CASH                PIC S9(13)V99 COMP-3.
           05  WS-OLD-KYC-STATUS           PIC X(01).
           05  WS-OLD-TIER                 PIC X(01).
           05  WS-OLD-RISK                 PIC X(01).
           05  WS-OLD-SCORE                PIC 9(03).
           05  WS-OLD-RANK                 PIC 9(01).
           05  WS-NEW-RANK                 PIC 9(01).
           05  WS-RANK-IN                  PIC X(01).
           05  WS-RANK-OUT                 PIC 9(01).
           05  WS-DAYS                     PIC S9(07).
           05  WS-EDD-DEADLINE             PIC 9(08).
           05  WS-RESTRICT-COUNT           PIC 9(03).
           05  WS-OPEN-ACCT-COUNT          PIC 9(03).
           05  WS-ALERT-SEV                PIC X(01).
           05  WS-ALERT-RULE               PIC X(08).
           05  WS-ALERT-TEXT               PIC X(80).
           05  WS-ALERT-ACCT               PIC X(12).
           05  WS-RPT-ACTION               PIC X(60).
           05  WS-EDIT-DAYS                PIC ZZZ9.
           05  WS-EDIT-CNT3                PIC ZZ9.

       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-SKIP-COUNT               PIC 9(07) VALUE 0.
           05  WS-UPDATE-COUNT             PIC 9(07) VALUE 0.
           05  WS-EXPIRED-COUNT            PIC 9(07) VALUE 0.
           05  WS-GRACE-COUNT              PIC 9(07) VALUE 0.
           05  WS-RESTRICTED-CUST-COUNT    PIC 9(07) VALUE 0.
           05  WS-RESTRICTED-ACCT-COUNT    PIC 9(07) VALUE 0.
           05  WS-EDD-ESC-COUNT            PIC 9(07) VALUE 0.
           05  WS-EDD-OUT-COUNT            PIC 9(07) VALUE 0.
           05  WS-RISK-UP-COUNT            PIC 9(07) VALUE 0.
           05  WS-RISK-DOWN-COUNT          PIC 9(07) VALUE 0.
           05  WS-ALERT-COUNT              PIC 9(07) VALUE 0.
           05  WS-HIST-READ-COUNT          PIC 9(07) VALUE 0.
       01  WS-EDIT-CNT                     PIC Z,ZZZ,ZZ9.

       01  WS-HEADING-1.
           05  FILLER PIC X(40)
               VALUE 'CBS KYCREVW  PERIODIC KYC REVIEW REPORT '.
           05  FILLER PIC X(15) VALUE 'BUSINESS DATE: '.
           05  WS-H1-DATE                  PIC 9(08).
       01  WS-HEADING-2.
           05  FILLER PIC X(66) VALUE
           'CUSTOMER   NAME                 KYC  TIER RISK SCORE'.
           05  FILLER PIC X(40) VALUE 'ACTION'.
       01  WS-DETAIL-LINE.
           05  WS-D-CUST                   PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-NAME                   PIC X(20).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-KYC-OLD                PIC X(01).
           05  FILLER                      PIC X(01) VALUE '>'.
           05  WS-D-KYC-NEW                PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-TIER-OLD               PIC X(01).
           05  FILLER                      PIC X(01) VALUE '>'.
           05  WS-D-TIER-NEW               PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-RISK-OLD               PIC X(01).
           05  FILLER                      PIC X(01) VALUE '>'.
           05  WS-D-RISK-NEW               PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-SCORE                  PIC ZZ9.
           05  FILLER                      PIC X(01) VALUE '/'.
           05  WS-D-CALC-SCORE             PIC ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-D-ACTION                 PIC X(60).

       COPY BANKCTL.
       COPY DATEPARM.
       COPY RSKPARM.
       COPY KYCTPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-PARM FROM COMMAND-LINE
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           IF NOT CTL-IS-MONTH-END AND WS-PARM(1:5) NOT = 'FORCE'
               DISPLAY 'KYCREVW  NOT MONTH-END (' CTL-BUSINESS-DATE
                       ') - NO ACTION'
               MOVE 0 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 1000-INITIALIZE
           PERFORM 1500-LOAD-CASH-ACTIVITY
           PERFORM 8000-READ-NEXT-CUSTOMER
           PERFORM 2000-PROCESS-CUSTOMER UNTIL WS-EOF
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALIZE.
           OPEN I-O CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN I-O ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN EXTEND ALERT-FILE
           IF WS-ALRT-STATUS NOT = '00' AND NOT = '05'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN EXTEND FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO WS-H1-DATE
           WRITE REPORT-LINE FROM WS-HEADING-1
           WRITE REPORT-LINE FROM WS-HEADING-2
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE
           INITIALIZE DATE-PARMS
           SET DTU-ADD-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE     TO DTU-DATE-1
           MOVE WS-CASH-LOOKBACK-DAYS TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-CASH-CUTOFF.

      *----------------------------------------------------------------*
      * CASH IN + CASH OUT PER ACCOUNT, POSTED ON OR AFTER THE CUTOFF  *
      *----------------------------------------------------------------*
       1500-LOAD-CASH-ACTIVITY.
           OPEN INPUT TRANHIST-FILE
           IF WS-HIST-STATUS = '00'
               MOVE 'N' TO WS-TRAN-EOF-FLAG
               PERFORM UNTIL WS-TRAN-EOF
                   READ TRANHIST-FILE
                       AT END SET WS-TRAN-EOF TO TRUE
                       NOT AT END PERFORM 1510-ACCUMULATE-CASH
                   END-READ
               END-PERFORM
               CLOSE TRANHIST-FILE
           END-IF
           OPEN INPUT TRANPOST-FILE
           IF WS-POST-STATUS = '00'
               MOVE 'N' TO WS-TRAN-EOF-FLAG
               PERFORM UNTIL WS-TRAN-EOF
                   READ TRANPOST-FILE INTO TRANSACTION-RECORD
                       AT END SET WS-TRAN-EOF TO TRUE
                       NOT AT END PERFORM 1510-ACCUMULATE-CASH
                   END-READ
               END-PERFORM
               CLOSE TRANPOST-FILE
           END-IF.

       1510-ACCUMULATE-CASH.
           ADD 1 TO WS-HIST-READ-COUNT
           IF NOT TRAN-IS-CASH OR NOT TRAN-POSTED
              OR TRAN-POST-DATE < WS-CASH-CUTOFF
               EXIT PARAGRAPH
           END-IF
           SET WS-CSH-IDX TO 1
           SEARCH WS-CASH-ENTRY
               AT END
                   IF WS-CASH-COUNT < WS-CASH-MAX
                       ADD 1 TO WS-CASH-COUNT
                       MOVE TRAN-ACCT-NUMBER
                         TO WS-CASH-ACCT(WS-CASH-COUNT)
                       MOVE TRAN-AMOUNT TO WS-CASH-AMT(WS-CASH-COUNT)
                   END-IF
               WHEN WS-CASH-ACCT(WS-CSH-IDX) = TRAN-ACCT-NUMBER
                   ADD TRAN-AMOUNT TO WS-CASH-AMT(WS-CSH-IDX)
           END-SEARCH.

      *----------------------------------------------------------------*
       2000-PROCESS-CUSTOMER.
           ADD 1 TO WS-READ-COUNT
           MOVE 'N' TO WS-CUST-CHANGED-FLAG WS-NEWLY-EXPIRED-FLAG
                       WS-EDD-ESCALATED-FLAG
           MOVE CUST-KYC-STATUS  TO WS-OLD-KYC-STATUS
           MOVE CUST-KYC-TIER    TO WS-OLD-TIER
           MOVE CUST-RISK-RATING TO WS-OLD-RISK
           MOVE CUST-RISK-SCORE  TO WS-OLD-SCORE
           MOVE ZERO TO RSK-RESULT-SCORE
      *    DECEASED, INACTIVE AND PROSPECT CUSTOMERS ARE NOT REVIEWED
           IF CUST-DECEASED OR CUST-INACTIVE OR CUST-PROSPECT
               ADD 1 TO WS-SKIP-COUNT
           ELSE
               PERFORM 2100-GATHER-PRODUCT-MIX
               PERFORM 2200-RESCORE-RISK
               PERFORM 2300-RETIER
               PERFORM 2400-KYC-STATUS-RULES
               IF CUST-KYC-EXPIRED
                   PERFORM 2500-EXPIRED-GRACE-RULE
               END-IF
               IF WS-CUST-CHANGED
                   MOVE CTL-BUSINESS-DATE TO CUST-LAST-MAINT-DATE
                   MOVE 'KYCREVW'         TO CUST-LAST-MAINT-USER
                   REWRITE CUSTOMER-RECORD
                   IF WS-CUST-STATUS NOT = '00'
                       MOVE 'CUSTMST' TO ABND-FILE-NAME
                       MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
                       MOVE 'REWRITE FAILED' TO ABND-MESSAGE
                       PERFORM 9900-ABEND
                   END-IF
                   ADD 1 TO WS-UPDATE-COUNT
               END-IF
           END-IF
           PERFORM 8000-READ-NEXT-CUSTOMER.

      *----------------------------------------------------------------*
      * PRODUCT MIX AND CASH TOTAL FROM ACCOUNTS WHERE THIS CUSTOMER   *
      * IS PRIMARY OWNER (AIX ACCT-PRIMARY-CUST-ID). JOINT-OWNED       *
      * ACCOUNTS ARE COUNTED FOR THE PRIMARY OWNER ONLY.               *
      *----------------------------------------------------------------*
       2100-GATHER-PRODUCT-MIX.
           INITIALIZE RISKSCOR-PARMS
           MOVE ZERO TO WS-CUST-CASH WS-OPEN-ACCT-COUNT
           MOVE 'N' TO WS-AIX-DONE-FLAG
           MOVE CUST-ID TO ACCT-PRIMARY-CUST-ID
           START ACCOUNT-FILE KEY IS EQUAL TO ACCT-PRIMARY-CUST-ID
               INVALID KEY SET WS-AIX-DONE TO TRUE
           END-START
           PERFORM UNTIL WS-AIX-DONE
               READ ACCOUNT-FILE NEXT RECORD
                   AT END SET WS-AIX-DONE TO TRUE
               END-READ
               IF NOT WS-AIX-DONE
                   IF ACCT-PRIMARY-CUST-ID NOT = CUST-ID
                       SET WS-AIX-DONE TO TRUE
                   ELSE
                       PERFORM 2110-COUNT-ACCOUNT
                   END-IF
               END-IF
           END-PERFORM
           EVALUATE TRUE
               WHEN WS-CUST-CASH >= WS-CASH-HEAVY-AMT
                   MOVE 'H' TO RSK-CASH-ACTIVITY
               WHEN WS-CUST-CASH >= WS-CASH-MODERATE-AMT
                   MOVE 'M' TO RSK-CASH-ACTIVITY
               WHEN OTHER
                   MOVE 'N' TO RSK-CASH-ACTIVITY
           END-EVALUATE.

       2110-COUNT-ACCOUNT.
           IF ACCT-CLOSED OR ACCT-ESCHEATED OR ACCT-CHARGED-OFF
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-OPEN-ACCT-COUNT
           EVALUATE TRUE
               WHEN ACCT-CATEGORY = 'DD' AND ACCT-OWN-CORPORATE
                   ADD 1 TO RSK-CNT-CORP-DDA
               WHEN ACCT-CATEGORY = 'DD'
                   ADD 1 TO RSK-CNT-DDA
               WHEN ACCT-CATEGORY = 'SV'
                   ADD 1 TO RSK-CNT-SAV
               WHEN ACCT-CATEGORY = 'MM'
                   ADD 1 TO RSK-CNT-MMA
               WHEN ACCT-CATEGORY = 'TD'
                   ADD 1 TO RSK-CNT-CD
           END-EVALUATE
           SET WS-CSH-IDX TO 1
           IF WS-CASH-COUNT > 0
               SEARCH WS-CASH-ENTRY
                   AT END CONTINUE
                   WHEN WS-CASH-ACCT(WS-CSH-IDX) = ACCT-NUMBER
                       ADD WS-CASH-AMT(WS-CSH-IDX) TO WS-CUST-CASH
               END-SEARCH
           END-IF.

      *----------------------------------------------------------------*
      * RISK RE-SCORE. UPGRADES ARE APPLIED. A LOWER COMPUTED RATING   *
      * IS NEVER APPLIED BY BATCH - REPORTED AS A DOWNGRADE CANDIDATE. *
      *----------------------------------------------------------------*
       2200-RESCORE-RISK.
           MOVE CUST-TYPE            TO RSK-CUST-TYPE
           MOVE CUST-SEGMENT         TO RSK-SEGMENT
           MOVE CUST-PEP-FLAG        TO RSK-PEP-FLAG
           MOVE CUST-MSB-FLAG        TO RSK-MSB-FLAG
           MOVE CUST-INDUSTRY-CODE   TO RSK-INDUSTRY-CODE
           MOVE CUST-COUNTRY         TO RSK-COUNTRY
           MOVE CUST-ZIP             TO RSK-ZIP
           MOVE CUST-TAX-ID-TYPE     TO RSK-TAX-ID-TYPE
           MOVE CUST-OFAC-STATUS     TO RSK-OFAC-STATUS
           MOVE CUST-ANNUAL-INCOME   TO RSK-ANNUAL-INCOME
           MOVE CUST-SINCE-DATE      TO RSK-CUST-SINCE-DATE
           MOVE CTL-BUSINESS-DATE    TO RSK-AS-OF-DATE
           CALL 'RISKSCOR' USING RISKSCOR-PARMS
           IF RSK-RETURN-CODE = 12
               MOVE 'RISKSCOR REJECTED INPUT - NOT RESCORED'
                 TO WS-RPT-ACTION
               PERFORM 8300-WRITE-DETAIL
               EXIT PARAGRAPH
           END-IF
           MOVE CUST-RISK-RATING  TO WS-RANK-IN
           PERFORM 8400-RATING-RANK
           MOVE WS-RANK-OUT       TO WS-OLD-RANK
           MOVE RSK-RESULT-RATING TO WS-RANK-IN
           PERFORM 8400-RATING-RANK
           MOVE WS-RANK-OUT       TO WS-NEW-RANK
           EVALUATE TRUE
               WHEN WS-NEW-RANK > WS-OLD-RANK
                   MOVE RSK-RESULT-RATING TO CUST-RISK-RATING
                   MOVE RSK-RESULT-SCORE  TO CUST-RISK-SCORE
                   SET WS-CUST-CHANGED TO TRUE
                   ADD 1 TO WS-RISK-UP-COUNT
                   MOVE SPACES TO WS-RPT-ACTION
                   STRING 'RISK UPGRADED ' WS-OLD-RISK ' TO '
                          RSK-RESULT-RATING
                          DELIMITED BY SIZE INTO WS-RPT-ACTION
                   PERFORM 8300-WRITE-DETAIL
                   IF RSK-RATED-PROHIBITED
                       MOVE 'C'        TO WS-ALERT-SEV
                       MOVE 'KYC-007'  TO WS-ALERT-RULE
                       MOVE SPACES TO WS-ALERT-TEXT
                       STRING 'RISK RATING PROHIBITED - '
                              'RELATIONSHIP EXIT REVIEW REQUIRED'
                              DELIMITED BY SIZE INTO WS-ALERT-TEXT
                   ELSE
                       MOVE 'M'        TO WS-ALERT-SEV
                       MOVE 'KYC-008'  TO WS-ALERT-RULE
                       MOVE SPACES TO WS-ALERT-TEXT
                       STRING 'CUSTOMER RISK RATING UPGRADED '
                              WS-OLD-RISK ' TO ' RSK-RESULT-RATING
                              ' BY PERIODIC RE-SCORE'
                              DELIMITED BY SIZE INTO WS-ALERT-TEXT
                   END-IF
                   MOVE SPACES TO WS-ALERT-ACCT
                   PERFORM 8200-WRITE-ALERT
               WHEN WS-NEW-RANK < WS-OLD-RANK
                   ADD 1 TO WS-RISK-DOWN-COUNT
                   MOVE SPACES TO WS-RPT-ACTION
                   STRING 'DOWNGRADE CANDIDATE ' WS-OLD-RISK ' TO '
                          RSK-RESULT-RATING ' - ANALYST REVIEW'
                          DELIMITED BY SIZE INTO WS-RPT-ACTION
                   PERFORM 8300-WRITE-DETAIL
               WHEN OTHER
                   IF RSK-RESULT-SCORE NOT = CUST-RISK-SCORE
                       MOVE RSK-RESULT-SCORE TO CUST-RISK-SCORE
                       SET WS-CUST-CHANGED TO TRUE
                   END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RE-TIER. ESCALATION APPLIES; IF EDD BECOMES REQUIRED THE KYC   *
      * STATUS GOES TO R AND THE REVIEW DATE IS PULLED IN TO NO LATER  *
      * THAN BUSINESS DATE + 30 DAYS. DE-ESCALATION IS REPORT ONLY.    *
      *----------------------------------------------------------------*
       2300-RETIER.
           INITIALIZE KYCTIER-PARMS
           MOVE CUST-TYPE          TO KYT-CUST-TYPE
           MOVE CUST-PEP-FLAG      TO KYT-PEP-FLAG
           MOVE CUST-MSB-FLAG      TO KYT-MSB-FLAG
           MOVE CUST-RISK-RATING   TO KYT-RISK-RATING
           MOVE CUST-COUNTRY       TO KYT-COUNTRY
           MOVE CUST-SEGMENT       TO KYT-SEGMENT
           MOVE CUST-ANNUAL-INCOME TO KYT-ANNUAL-INCOME
           MOVE CUST-INDUSTRY-CODE TO KYT-INDUSTRY-CODE
           MOVE CUST-OFAC-STATUS   TO KYT-OFAC-STATUS
           MOVE CTL-BUSINESS-DATE  TO KYT-AS-OF-DATE
           CALL 'KYCTIER' USING KYCTIER-PARMS
           IF KYT-RETURN-CODE = 12
               MOVE 'KYCTIER REJECTED INPUT - NOT RE-TIERED'
                 TO WS-RPT-ACTION
               PERFORM 8300-WRITE-DETAIL
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN KYT-RESULT-TIER > CUST-KYC-TIER
                   MOVE KYT-RESULT-TIER TO CUST-KYC-TIER
                   SET WS-CUST-CHANGED TO TRUE
                   MOVE SPACES TO WS-RPT-ACTION
                   STRING 'TIER ESCALATED ' WS-OLD-TIER ' TO '
                          KYT-RESULT-TIER ' REASON ' KYT-REASON-CODE
                          DELIMITED BY SIZE INTO WS-RPT-ACTION
                   PERFORM 8300-WRITE-DETAIL
                   IF KYT-EDD-IS-REQUIRED AND CUST-KYC-VERIFIED
                       PERFORM 2310-SET-EDD-REQUIRED
                   END-IF
               WHEN KYT-RESULT-TIER < CUST-KYC-TIER
                   MOVE SPACES TO WS-RPT-ACTION
                   STRING 'TIER DOWNGRADE CANDIDATE ' WS-OLD-TIER
                          ' TO ' KYT-RESULT-TIER
                          DELIMITED BY SIZE INTO WS-RPT-ACTION
                   PERFORM 8300-WRITE-DETAIL
           END-EVALUATE.

       2310-SET-EDD-REQUIRED.
           SET CUST-KYC-EDD-REQUIRED TO TRUE
           SET WS-EDD-ESCALATED TO TRUE
           INITIALIZE DATE-PARMS
           SET DTU-ADD-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE    TO DTU-DATE-1
           MOVE WS-EDD-DEADLINE-DAYS TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-EDD-DEADLINE
           IF CUST-KYC-REVIEW-DATE = ZERO
              OR CUST-KYC-REVIEW-DATE > WS-EDD-DEADLINE
               MOVE WS-EDD-DEADLINE TO CUST-KYC-REVIEW-DATE
           END-IF
           ADD 1 TO WS-EDD-ESC-COUNT
           MOVE 'H'       TO WS-ALERT-SEV
           MOVE 'KYC-004' TO WS-ALERT-RULE
           MOVE SPACES TO WS-ALERT-TEXT
           STRING 'EDD REQUIRED - TIER ESCALATED TO 3 ('
                  KYT-REASON-CODE
                  ') - COMPLETE BY ' CUST-KYC-REVIEW-DATE
                  DELIMITED BY SIZE INTO WS-ALERT-TEXT
           MOVE SPACES TO WS-ALERT-ACCT
           PERFORM 8200-WRITE-ALERT.

      *----------------------------------------------------------------*
      * KYC STATUS RULES                                               *
      *----------------------------------------------------------------*
       2400-KYC-STATUS-RULES.
           EVALUATE TRUE
               WHEN (CUST-KYC-VERIFIED OR CUST-KYC-EDD-REQUIRED)
                    AND CUST-KYC-REVIEW-DATE < CTL-BUSINESS-DATE
                   SET CUST-KYC-EXPIRED TO TRUE
                   SET WS-CUST-CHANGED  TO TRUE
                   SET WS-NEWLY-EXPIRED TO TRUE
                   ADD 1 TO WS-EXPIRED-COUNT
                   MOVE 'H'       TO WS-ALERT-SEV
                   MOVE 'KYC-001' TO WS-ALERT-RULE
                   MOVE SPACES TO WS-ALERT-TEXT
                   STRING 'KYC PERIODIC REVIEW EXPIRED - '
                          'REVIEW WAS DUE '
                          CUST-KYC-REVIEW-DATE
                          DELIMITED BY SIZE INTO WS-ALERT-TEXT
                   MOVE SPACES TO WS-ALERT-ACCT
                   PERFORM 8200-WRITE-ALERT
                   MOVE 'KYC EXPIRED - STATUS SET TO E' TO WS-RPT-ACTION
                   PERFORM 8300-WRITE-DETAIL
               WHEN CUST-KYC-EDD-REQUIRED AND WS-EDD-ESCALATED
                   CONTINUE
               WHEN CUST-KYC-EDD-REQUIRED
                   PERFORM 2410-DAYS-TO-REVIEW
                   ADD 1 TO WS-EDD-OUT-COUNT
                   IF WS-DAYS <= WS-EDD-DEADLINE-DAYS
                       MOVE 'H' TO WS-ALERT-SEV
                   ELSE
                       MOVE 'M' TO WS-ALERT-SEV
                   END-IF
                   MOVE 'KYC-005' TO WS-ALERT-RULE
                   MOVE SPACES TO WS-ALERT-TEXT
                   STRING 'EDD REQUIRED - REVIEW NOT COMPLETED - DUE '
                          CUST-KYC-REVIEW-DATE
                          DELIMITED BY SIZE INTO WS-ALERT-TEXT
                   MOVE SPACES TO WS-ALERT-ACCT
                   PERFORM 8200-WRITE-ALERT
                   MOVE 'EDD OUTSTANDING - ALERT RAISED'
                     TO WS-RPT-ACTION
                   PERFORM 8300-WRITE-DETAIL
               WHEN CUST-KYC-PENDING
                   INITIALIZE DATE-PARMS
                   SET DTU-DAYS-BETWEEN TO TRUE
                   MOVE CUST-SINCE-DATE   TO DTU-DATE-1
                   MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
                   CALL 'DATEUTIL' USING DATE-PARMS
                   IF DTU-RESULT-NUMBER > WS-CIP-PENDING-DAYS
                       MOVE 'M'       TO WS-ALERT-SEV
                       MOVE 'KYC-006' TO WS-ALERT-RULE
                       MOVE 'CIP VERIFICATION OUTSTANDING > 30 DAYS'
                         TO WS-ALERT-TEXT
                       MOVE SPACES TO WS-ALERT-ACCT
                       PERFORM 8200-WRITE-ALERT
                       MOVE 'CIP PENDING > 30 DAYS' TO WS-RPT-ACTION
                       PERFORM 8300-WRITE-DETAIL
                   END-IF
               WHEN CUST-KYC-VERIFIED
                   PERFORM 2410-DAYS-TO-REVIEW
                   IF WS-DAYS <= WS-DUE-SOON-DAYS
                       MOVE SPACES TO WS-RPT-ACTION
                       STRING 'REVIEW DUE SOON ' CUST-KYC-REVIEW-DATE
                              DELIMITED BY SIZE INTO WS-RPT-ACTION
                       PERFORM 8300-WRITE-DETAIL
                   END-IF
           END-EVALUATE.

       2410-DAYS-TO-REVIEW.
           INITIALIZE DATE-PARMS
           SET DTU-DAYS-BETWEEN TO TRUE
           MOVE CTL-BUSINESS-DATE    TO DTU-DATE-1
           MOVE CUST-KYC-REVIEW-DATE TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DTU-RETURN-CODE = 00
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           ELSE
               MOVE ZERO TO WS-DAYS
           END-IF.

      *----------------------------------------------------------------*
      * EXPIRED KYC: WITHIN 90 DAYS OF THE REVIEW DATE -> ALERT ONLY.  *
      * MORE THAN 90 DAYS -> RESTRICT EVERY ACTIVE ACCOUNT THE         *
      * CUSTOMER OWNS AS PRIMARY (STATUS R, REASON 'KY').              *
      *----------------------------------------------------------------*
       2500-EXPIRED-GRACE-RULE.
           INITIALIZE DATE-PARMS
           SET DTU-DAYS-BETWEEN TO TRUE
           MOVE CUST-KYC-REVIEW-DATE TO DTU-DATE-1
           MOVE CTL-BUSINESS-DATE    TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-NUMBER TO WS-DAYS
           MOVE WS-DAYS TO WS-EDIT-DAYS
           IF WS-DAYS > WS-GRACE-DAYS
               PERFORM 2510-RESTRICT-ACCOUNTS
               ADD 1 TO WS-RESTRICTED-CUST-COUNT
               MOVE WS-RESTRICT-COUNT TO WS-EDIT-CNT3
               MOVE 'C'       TO WS-ALERT-SEV
               MOVE 'KYC-003' TO WS-ALERT-RULE
               MOVE SPACES TO WS-ALERT-TEXT
               STRING 'KYC EXPIRED ' WS-EDIT-DAYS
                      ' DAYS (> 90 GRACE) - ' WS-EDIT-CNT3
                      ' ACTIVE ACCOUNT(S) RESTRICTED'
                      DELIMITED BY SIZE INTO WS-ALERT-TEXT
               MOVE SPACES TO WS-ALERT-ACCT
               PERFORM 8200-WRITE-ALERT
               MOVE SPACES TO WS-RPT-ACTION
               STRING 'PAST GRACE - ' WS-EDIT-CNT3
                      ' ACCOUNT(S) RESTRICTED KY'
                      DELIMITED BY SIZE INTO WS-RPT-ACTION
               PERFORM 8300-WRITE-DETAIL
           ELSE
               ADD 1 TO WS-GRACE-COUNT
               MOVE 'M'       TO WS-ALERT-SEV
               MOVE 'KYC-002' TO WS-ALERT-RULE
               MOVE SPACES TO WS-ALERT-TEXT
               STRING 'KYC EXPIRED ' WS-EDIT-DAYS
                      ' DAYS - WITHIN 90 DAY GRACE, NO RESTRICTION'
                      DELIMITED BY SIZE INTO WS-ALERT-TEXT
               MOVE SPACES TO WS-ALERT-ACCT
               PERFORM 8200-WRITE-ALERT
               MOVE SPACES TO WS-RPT-ACTION
               STRING 'EXPIRED ' WS-EDIT-DAYS ' DAYS - IN GRACE'
                      DELIMITED BY SIZE INTO WS-RPT-ACTION
               PERFORM 8300-WRITE-DETAIL
           END-IF.

       2510-RESTRICT-ACCOUNTS.
           MOVE ZERO TO WS-RESTRICT-COUNT
           MOVE 'N' TO WS-AIX-DONE-FLAG
           MOVE CUST-ID TO ACCT-PRIMARY-CUST-ID
           START ACCOUNT-FILE KEY IS EQUAL TO ACCT-PRIMARY-CUST-ID
               INVALID KEY SET WS-AIX-DONE TO TRUE
           END-START
           PERFORM UNTIL WS-AIX-DONE
               READ ACCOUNT-FILE NEXT RECORD
                   AT END SET WS-AIX-DONE TO TRUE
               END-READ
               IF NOT WS-AIX-DONE
                   IF ACCT-PRIMARY-CUST-ID NOT = CUST-ID
                       SET WS-AIX-DONE TO TRUE
                   ELSE
                       IF ACCT-ACTIVE
                           SET ACCT-RESTRICTED TO TRUE
                           MOVE 'KY' TO ACCT-RESTRICT-REASON
                           MOVE CTL-BUSINESS-DATE
                             TO ACCT-LAST-MAINT-DATE
                           MOVE 'KYCREVW' TO ACCT-LAST-MAINT-USER
                           REWRITE ACCOUNT-RECORD
                           IF WS-ACCT-STATUS NOT = '00'
                               MOVE 'ACCTMST' TO ABND-FILE-NAME
                               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
                               MOVE 'REWRITE FAILED' TO ABND-MESSAGE
                               PERFORM 9900-ABEND
                           END-IF
                           ADD 1 TO WS-RESTRICT-COUNT
                                    WS-RESTRICTED-ACCT-COUNT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       8000-READ-NEXT-CUSTOMER.
           READ CUSTOMER-FILE NEXT RECORD
               AT END SET WS-EOF TO TRUE
           END-READ
           IF NOT WS-EOF AND WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               MOVE 'READ NEXT FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF.

      *----------------------------------------------------------------*
       8200-WRITE-ALERT.
           INITIALIZE ALERT-RECORD
           MOVE CTL-BUSINESS-DATE TO ALRT-DATE
           MOVE 223000            TO ALRT-TIME
           MOVE 'KYCX'            TO ALRT-TYPE
           MOVE WS-ALERT-SEV      TO ALRT-SEVERITY
           MOVE CUST-ID           TO ALRT-CUST-ID
           MOVE WS-ALERT-ACCT     TO ALRT-ACCT-NUMBER
           MOVE CUST-TOTAL-REL-BALANCE TO ALRT-AMOUNT
           MOVE WS-ALERT-RULE     TO ALRT-RULE-ID
           MOVE WS-ALERT-TEXT     TO ALRT-DESCRIPTION
           SET ALRT-OPEN          TO TRUE
           MOVE 'KYCREVW'         TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD
           IF WS-ALRT-STATUS NOT = '00'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               MOVE 'WRITE FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-ALERT-COUNT.

      *----------------------------------------------------------------*
       8300-WRITE-DETAIL.
           MOVE CUST-ID           TO WS-D-CUST
           IF CUST-FIRST-NAME = SPACES
               MOVE CUST-LAST-NAME TO WS-D-NAME
           ELSE
               MOVE SPACES TO WS-D-NAME
               STRING FUNCTION TRIM(CUST-LAST-NAME) ', '
                      CUST-FIRST-NAME
                      DELIMITED BY SIZE INTO WS-D-NAME
           END-IF
           MOVE WS-OLD-KYC-STATUS TO WS-D-KYC-OLD
           MOVE CUST-KYC-STATUS   TO WS-D-KYC-NEW
           MOVE WS-OLD-TIER       TO WS-D-TIER-OLD
           MOVE CUST-KYC-TIER     TO WS-D-TIER-NEW
           MOVE WS-OLD-RISK       TO WS-D-RISK-OLD
           MOVE CUST-RISK-RATING  TO WS-D-RISK-NEW
           MOVE WS-OLD-SCORE      TO WS-D-SCORE
           MOVE RSK-RESULT-SCORE  TO WS-D-CALC-SCORE
           MOVE WS-RPT-ACTION     TO WS-D-ACTION
           WRITE REPORT-LINE FROM WS-DETAIL-LINE.

      *----------------------------------------------------------------*
       8400-RATING-RANK.
           EVALUATE WS-RANK-IN
               WHEN 'L'   MOVE 1 TO WS-RANK-OUT
               WHEN 'M'   MOVE 2 TO WS-RANK-OUT
               WHEN 'H'   MOVE 3 TO WS-RANK-OUT
               WHEN 'P'   MOVE 4 TO WS-RANK-OUT
               WHEN OTHER MOVE 0 TO WS-RANK-OUT
           END-EVALUATE.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-READ-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'CUSTOMERS READ ........... ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-SKIP-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'NOT REVIEWED (D/I/P) ..... ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-UPDATE-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'CUSTOMERS UPDATED ........ ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-EXPIRED-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'NEWLY EXPIRED ............ ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-GRACE-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'EXPIRED WITHIN GRACE ..... ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-RESTRICTED-ACCT-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'ACCOUNTS RESTRICTED (KY) . ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-EDD-ESC-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'EDD ESCALATIONS .......... ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-EDD-OUT-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'EDD OUTSTANDING .......... ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-RISK-UP-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'RISK UPGRADES ............ ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-RISK-DOWN-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'DOWNGRADE CANDIDATES ..... ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-ALERT-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'KYCX ALERTS WRITTEN ...... ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           CLOSE CUSTOMER-FILE ACCOUNT-FILE ALERT-FILE REPORT-FILE
           DISPLAY 'KYCREVW  READ=' WS-READ-COUNT
                   ' UPDATED=' WS-UPDATE-COUNT
                   ' EXPIRED=' WS-EXPIRED-COUNT
                   ' RESTRICTED-ACCTS=' WS-RESTRICTED-ACCT-COUNT
                   ' ALERTS=' WS-ALERT-COUNT
                   ' TRAN-SCANNED=' WS-HIST-READ-COUNT
           MOVE 0 TO RETURN-CODE.

      *----------------------------------------------------------------*
       9900-ABEND.
           MOVE 'KYCREVW' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
