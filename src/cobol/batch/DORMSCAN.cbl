      *================================================================*
      * PROGRAM  : DORMSCAN                                            *
      * TYPE     : BATCH - MONTH-END (EOD STEP 16)                     *
      * PURPOSE  : DORMANCY CLASSIFICATION, DORMANT ACCOUNT FEE AND    *
      *            UNCLAIMED PROPERTY (ESCHEAT) REMITTANCE.            *
      *            1. ACTIVE ACCOUNT WITH NO CUSTOMER-INITIATED        *
      *               ACTIVITY FOR THE INACTIVITY PERIOD -> DORMANT    *
      *            2. DORMANT ACCOUNT PAST THE STATE DORMANCY PERIOD   *
      *               (MEASURED FROM LAST ACTIVITY) -> BALANCE         *
      *               REMITTED VIA WESC, STATUS ESCHEATED              *
      *            3. DORMANT ACCOUNT FEE (WFOT) WHERE STATE PERMITS   *
      *            RUNS ON MONTH-END ONLY UNLESS PARM 'FORCE'.         *
      * INPUT    : BANKCTL (VIA GETCTL), CUSTMST, HOLDMST (OPTIONAL)   *
      * I-O      : ACCTMST                                             *
      * OUTPUT   : TRANPOST (EXTEND), ALERTS (EXTEND), DORMRPT         *
      * TRAN IDS : TRAN-ID-SEQ 80000001 - 89999999                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - 12 MONTH INACTIVITY FLAG            *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-02-17 BHS  ESCHEAT REMITTANCE (WESC), STATE TABLE         *
      * 2009-11-17 DPS  DORMANT FEE SUPPRESSED IN FEE-PROHIBITED STATES*
      *                 (REG-2009-UP)                                  *
      * 2011-07-21 DPS  EXCLUDE ACCOUNTS UNDER LEGAL HOLD / LEVY       *
      * 2014-01-06 DPS  USE ACCT-ESCHEAT-STATE, FALL BACK TO CUST-STATE*
      * 2016-03-30 DPS  AUTO-RENEWING CDS ARE NEVER DORMANT (CHG1290)  *
      * 2019-06-03 AKW  SAVINGS / MMDA INACTIVITY 24 -> 12 MONTHS      *
      *                 (CHG1802 - UNCLAIMED PROPERTY AUDIT FINDING)   *
      * 2021-09-14 JLM  CREDIT ACCRUED INTEREST BEFORE REMITTANCE      *
      * 2023-01-09 TKO  DUE-DILIGENCE LETTER WINDOW 120 DAYS (INC18802)*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DORMSCAN.
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
           SELECT OPTIONAL HOLD-FILE ASSIGN TO "HOLDMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HOLD-KEY
               FILE STATUS IS WS-HOLD-STATUS.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "DORMRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  HOLD-FILE.
       COPY HOLDREC.
       FD  TRANPOST-FILE.
       COPY TRANREC.
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-HOLD-STATUS              PIC X(02).
           05  WS-POST-STATUS              PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).

       01  WS-PARM                         PIC X(10).
       01  WS-FLAGS.
           05  WS-EOF-FLAG                 PIC X(01) VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-HOLDS-AVAIL-FLAG         PIC X(01) VALUE 'N'.
               88  WS-HOLDS-AVAILABLE                VALUE 'Y'.
           05  WS-EXCLUDE-FLAG             PIC X(01).
               88  WS-EXCLUDED                       VALUE 'Y'.
           05  WS-UPDATE-FLAG              PIC X(01).
               88  WS-ACCT-UPDATED                   VALUE 'Y'.
           05  WS-NEWLY-DORMANT-FLAG       PIC X(01).
               88  WS-NEWLY-DORMANT                  VALUE 'Y'.
           05  WS-NO-FEE-FLAG              PIC X(01).
               88  WS-FEE-PROHIBITED                 VALUE 'Y'.
           05  WS-HOLD-DONE-FLAG           PIC X(01).

       01  WS-EXCLUDE-REASON               PIC X(30).

      *--- INACTIVITY PERIOD (MONTHS) BY ACCOUNT CATEGORY -----------*
      *    2019-06 AKW CHG1802 - SV AND MM REDUCED FROM 24 TO 12
       01  WS-INACTIVITY-VALUES.
           05  FILLER PIC X(05) VALUE 'DD012'.
           05  FILLER PIC X(05) VALUE 'SV012'.
           05  FILLER PIC X(05) VALUE 'MM012'.
           05  FILLER PIC X(05) VALUE 'TD012'.
       01  WS-INACTIVITY-TABLE REDEFINES WS-INACTIVITY-VALUES.
           05  WS-INACT-ENTRY OCCURS 4 TIMES INDEXED BY WS-INA-IDX.
               10  WS-INACT-CATEGORY       PIC X(02).
               10  WS-INACT-TBL-MONTHS     PIC 9(03).
       01  WS-INACT-DEFAULT-MONTHS         PIC 9(03) VALUE 012.

      *--- STATE DORMANCY (ESCHEAT) PERIOD IN YEARS FROM LAST OWNER  *
      *    ACTIVITY. STATE NOT IN TABLE -> DEFAULT 5. NO STATE -> DE  *
      *    (STATE OF INCORPORATION, TEXAS V. NEW JERSEY PRIORITY).    *
       01  WS-ESCHEAT-STATE-VALUES.
           05  FILLER PIC X(04) VALUE 'NY03'.
           05  FILLER PIC X(04) VALUE 'NJ03'.
           05  FILLER PIC X(04) VALUE 'OH03'.
           05  FILLER PIC X(04) VALUE 'CA03'.
           05  FILLER PIC X(04) VALUE 'TX03'.
           05  FILLER PIC X(04) VALUE 'MA03'.
           05  FILLER PIC X(04) VALUE 'IL03'.
           05  FILLER PIC X(04) VALUE 'WA03'.
           05  FILLER PIC X(04) VALUE 'PA03'.
           05  FILLER PIC X(04) VALUE 'CT03'.
           05  FILLER PIC X(04) VALUE 'FL05'.
           05  FILLER PIC X(04) VALUE 'DE05'.
           05  FILLER PIC X(04) VALUE 'CO05'.
           05  FILLER PIC X(04) VALUE 'NC05'.
           05  FILLER PIC X(04) VALUE 'GA05'.
       01  WS-ESCHEAT-STATE-TABLE REDEFINES WS-ESCHEAT-STATE-VALUES.
           05  WS-ESC-ENTRY OCCURS 15 TIMES INDEXED BY WS-ESC-IDX.
               10  WS-ESC-TBL-STATE        PIC X(02).
               10  WS-ESC-TBL-YEARS        PIC 9(02).
       01  WS-ESC-DEFAULT-YEARS            PIC 9(02) VALUE 05.
       01  WS-ESC-DEFAULT-STATE            PIC X(02) VALUE 'DE'.

      *--- STATES THAT PROHIBIT DORMANCY / INACTIVITY FEES ----------*
       01  WS-NO-FEE-STATE-VALUES          PIC X(20)
                                   VALUE 'CACTILMAMEMNNHNYRIVT'.
       01  WS-NO-FEE-STATE-TABLE REDEFINES WS-NO-FEE-STATE-VALUES.
           05  WS-NO-FEE-STATE PIC X(02) OCCURS 10 TIMES
                               INDEXED BY WS-NF-IDX.

       01  WS-RULE-VALUES.
           05  WS-DORMANT-FEE              PIC S9(13)V99 COMP-3
                                           VALUE 5.00.
           05  WS-NOTICE-DAYS              PIC S9(05) VALUE +30.
           05  WS-DUE-DILIGENCE-DAYS       PIC S9(05) VALUE +120.

       01  WS-WORK.
           05  WS-INACT-MONTHS             PIC 9(03).
           05  WS-ESC-YEARS                PIC 9(02).
           05  WS-ESC-STATE                PIC X(02).
           05  WS-LAST-ACTIVITY            PIC 9(08).
           05  WS-DORMANT-ON-DATE          PIC 9(08).
           05  WS-ESCHEAT-ON-DATE          PIC 9(08).
           05  WS-DAYS-TO-GO               PIC S9(07).
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-INT-CREDIT               PIC S9(13)V99 COMP-3.
           05  WS-TRAN-SEQ                 PIC 9(08) VALUE 80000000.
           05  WS-ALERT-SEV                PIC X(01).
           05  WS-ALERT-TEXT               PIC X(80).
           05  WS-RPT-ACTION               PIC X(20).
           05  WS-RPT-TEXT                 PIC X(60).
           05  WS-TRAN-CODE                PIC X(04).
           05  WS-TRAN-DESC                PIC X(40).

       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-SKIP-COUNT               PIC 9(07) VALUE 0.
           05  WS-EXCL-COUNT               PIC 9(07) VALUE 0.
           05  WS-NOTICE-COUNT             PIC 9(07) VALUE 0.
           05  WS-DORMANT-COUNT            PIC 9(07) VALUE 0.
           05  WS-DUEDIL-COUNT             PIC 9(07) VALUE 0.
           05  WS-ESCHEAT-COUNT            PIC 9(07) VALUE 0.
           05  WS-FEE-COUNT                PIC 9(07) VALUE 0.
           05  WS-FEE-SUPPR-COUNT          PIC 9(07) VALUE 0.
           05  WS-ESCHEAT-TOTAL            PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-FEE-TOTAL                PIC S9(13)V99 COMP-3
                                           VALUE 0.

       01  WS-EDIT-AMT                     PIC Z,ZZZ,ZZZ,ZZ9.99-.
       01  WS-EDIT-CNT                     PIC Z,ZZZ,ZZ9.

       01  WS-HEADING-1.
           05  FILLER PIC X(40)
               VALUE 'CBS DORMSCAN  DORMANCY / ESCHEAT REPORT '.
           05  FILLER PIC X(15) VALUE 'BUSINESS DATE: '.
           05  WS-H1-DATE                  PIC 9(08).
       01  WS-HEADING-2.
           05  FILLER PIC X(66) VALUE
           'ACCOUNT      PROD ST LAST-ACT ESC-ST ACTION              '.
           05  FILLER PIC X(40) VALUE 'DETAIL'.
       01  WS-DETAIL-LINE.
           05  WS-D-ACCT                   PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-PROD                   PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-STATUS                 PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-LAST-ACT               PIC 9(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-ESC-STATE              PIC X(02).
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  WS-D-ACTION                 PIC X(20).
           05  WS-D-TEXT                   PIC X(60).

       COPY BANKCTL.
       COPY DATEPARM.
       COPY TRANCODE.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-PARM FROM COMMAND-LINE
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           IF NOT CTL-IS-MONTH-END AND WS-PARM(1:5) NOT = 'FORCE'
               DISPLAY 'DORMSCAN NOT MONTH-END (' CTL-BUSINESS-DATE
                       ') - NO ACTION'
               MOVE 0 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-ACCOUNT UNTIL WS-EOF
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALIZE.
           OPEN I-O ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN INPUT CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN INPUT HOLD-FILE
           IF WS-HOLD-STATUS = '00'
               SET WS-HOLDS-AVAILABLE TO TRUE
           ELSE
               DISPLAY 'DORMSCAN HOLDMST NOT AVAILABLE (' WS-HOLD-STATUS
                       ') - HOLD EXCLUSION SKIPPED'
           END-IF
           OPEN EXTEND TRANPOST-FILE
           IF WS-POST-STATUS NOT = '00' AND NOT = '05'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN EXTEND FAILED' TO ABND-MESSAGE
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
           PERFORM 8000-READ-NEXT-ACCOUNT.

      *----------------------------------------------------------------*
       2000-PROCESS-ACCOUNT.
           ADD 1 TO WS-READ-COUNT
           MOVE 'N' TO WS-UPDATE-FLAG WS-NEWLY-DORMANT-FLAG
      *    ACTIVE, DORMANT, FROZEN AND RESTRICTED DEPOSIT ACCOUNTS ARE
      *    CANDIDATES (FROZEN / RESTRICTED ARE REPORTED AS EXCLUDED).
      *    VIRTUAL AND ZBA SUB-ACCOUNTS FOLLOW THEIR HEADER ACCOUNT.
      *    RESTART GUARD - ACCOUNT ALREADY UPDATED BY THIS RUN DATE
           IF (NOT ACCT-ACTIVE AND NOT ACCT-DORMANT
               AND NOT ACCT-FROZEN AND NOT ACCT-RESTRICTED)
              OR (ACCT-LAST-MAINT-USER = 'DORMSCAN'
                  AND ACCT-LAST-MAINT-DATE = CTL-BUSINESS-DATE)
              OR ACCT-IS-VIRTUAL
              OR ACCT-PARENT-ACCT NOT = SPACES
              OR (ACCT-CATEGORY NOT = 'DD' AND NOT = 'SV'
                  AND NOT = 'MM' AND NOT = 'TD')
               ADD 1 TO WS-SKIP-COUNT
           ELSE
               PERFORM 2100-CHECK-EXCLUSIONS
               IF WS-EXCLUDED
                   ADD 1 TO WS-EXCL-COUNT
                   MOVE 'EXCLUDED'        TO WS-RPT-ACTION
                   MOVE WS-EXCLUDE-REASON TO WS-RPT-TEXT
                   PERFORM 8300-WRITE-DETAIL
               ELSE
                   PERFORM 2200-SET-PERIODS
                   IF ACCT-ACTIVE
                       PERFORM 2300-DORMANCY-TEST
                   END-IF
                   IF ACCT-DORMANT
                       PERFORM 2400-ESCHEAT-TEST
                   END-IF
                   IF ACCT-DORMANT AND NOT WS-NEWLY-DORMANT
                      AND ACCT-DORMANT-DATE(1:6) <
                          CTL-BUSINESS-DATE(1:6)
                       PERFORM 2500-DORMANT-FEE
                   END-IF
               END-IF
           END-IF
           IF WS-ACCT-UPDATED
               MOVE CTL-BUSINESS-DATE TO ACCT-LAST-MAINT-DATE
               MOVE 'DORMSCAN'        TO ACCT-LAST-MAINT-USER
               REWRITE ACCOUNT-RECORD
               IF WS-ACCT-STATUS NOT = '00'
                   MOVE 'ACCTMST' TO ABND-FILE-NAME
                   MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
                   MOVE 'REWRITE FAILED' TO ABND-MESSAGE
                   PERFORM 9900-ABEND
               END-IF
           END-IF
           PERFORM 8000-READ-NEXT-ACCOUNT.

      *----------------------------------------------------------------*
      * EXCLUSIONS: FROZEN (DECEASED ESTATE / COURT ORDER), RESTRICTED,*
      * PRIMARY OWNER DECEASED, AUTO-RENEWING CD, ACTIVE LEGAL HOLD,   *
      * LEVY / LIEN OR FRAUD HOLD.                                     *
      *----------------------------------------------------------------*
       2100-CHECK-EXCLUSIONS.
           MOVE 'N' TO WS-EXCLUDE-FLAG
           MOVE SPACES TO WS-EXCLUDE-REASON
           EVALUATE TRUE
               WHEN ACCT-FROZEN
                   MOVE 'Y' TO WS-EXCLUDE-FLAG
                   MOVE 'ACCOUNT FROZEN' TO WS-EXCLUDE-REASON
               WHEN ACCT-RESTRICTED
                   MOVE 'Y' TO WS-EXCLUDE-FLAG
                   MOVE 'ACCOUNT RESTRICTED' TO WS-EXCLUDE-REASON
               WHEN ACCT-CATEGORY = 'TD' AND ACCT-RENEW-AUTO
                   MOVE 'Y' TO WS-EXCLUDE-FLAG
                   MOVE 'AUTO-RENEWING CD' TO WS-EXCLUDE-REASON
           END-EVALUATE
           IF NOT WS-EXCLUDED
               MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
               READ CUSTOMER-FILE
                   INVALID KEY
                       INITIALIZE CUSTOMER-RECORD
                   NOT INVALID KEY
                       IF CUST-DECEASED
                           MOVE 'Y' TO WS-EXCLUDE-FLAG
                           MOVE 'PRIMARY OWNER DECEASED'
                             TO WS-EXCLUDE-REASON
                       END-IF
               END-READ
           END-IF
           IF NOT WS-EXCLUDED AND WS-HOLDS-AVAILABLE
               PERFORM 2110-CHECK-HOLDS
           END-IF.

       2110-CHECK-HOLDS.
           MOVE ACCT-NUMBER TO HOLD-ACCT-NUMBER
           MOVE ZERO        TO HOLD-SEQ
           MOVE 'N'         TO WS-HOLD-DONE-FLAG
           START HOLD-FILE KEY IS NOT LESS THAN HOLD-KEY
               INVALID KEY MOVE 'Y' TO WS-HOLD-DONE-FLAG
           END-START
           PERFORM UNTIL WS-HOLD-DONE-FLAG = 'Y'
               READ HOLD-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-HOLD-DONE-FLAG
               END-READ
               IF WS-HOLD-DONE-FLAG = 'N'
                   IF HOLD-ACCT-NUMBER NOT = ACCT-NUMBER
                       MOVE 'Y' TO WS-HOLD-DONE-FLAG
                   ELSE
                       IF HOLD-ACTIVE
                          AND (HOLD-LEGAL OR HOLD-LIEN OR HOLD-FRAUD)
                           MOVE 'Y' TO WS-EXCLUDE-FLAG
                           MOVE 'Y' TO WS-HOLD-DONE-FLAG
                           STRING 'ACTIVE HOLD TYPE ' HOLD-TYPE
                               DELIMITED BY SIZE
                               INTO WS-EXCLUDE-REASON
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2200-SET-PERIODS.
           MOVE WS-INACT-DEFAULT-MONTHS TO WS-INACT-MONTHS
           SET WS-INA-IDX TO 1
           SEARCH WS-INACT-ENTRY
               AT END CONTINUE
               WHEN WS-INACT-CATEGORY(WS-INA-IDX) = ACCT-CATEGORY
                   MOVE WS-INACT-TBL-MONTHS(WS-INA-IDX)
                     TO WS-INACT-MONTHS
           END-SEARCH
      *    ESCHEAT STATE: ACCOUNT, THEN OWNER ADDRESS, THEN DE
           MOVE ACCT-ESCHEAT-STATE TO WS-ESC-STATE
           IF WS-ESC-STATE = SPACES
               MOVE CUST-STATE TO WS-ESC-STATE
           END-IF
           IF WS-ESC-STATE = SPACES
               MOVE WS-ESC-DEFAULT-STATE TO WS-ESC-STATE
           END-IF
           MOVE WS-ESC-DEFAULT-YEARS TO WS-ESC-YEARS
           SET WS-ESC-IDX TO 1
           SEARCH WS-ESC-ENTRY
               AT END CONTINUE
               WHEN WS-ESC-TBL-STATE(WS-ESC-IDX) = WS-ESC-STATE
                   MOVE WS-ESC-TBL-YEARS(WS-ESC-IDX) TO WS-ESC-YEARS
           END-SEARCH
      *    LAST OWNER ACTIVITY. FOR A NON-RENEWING CD THE CLOCK DOES
      *    NOT START UNTIL MATURITY.
           MOVE ACCT-LAST-ACTIVITY-DATE TO WS-LAST-ACTIVITY
           IF WS-LAST-ACTIVITY = ZERO
               MOVE ACCT-OPEN-DATE TO WS-LAST-ACTIVITY
           END-IF
           IF ACCT-CATEGORY = 'TD'
              AND ACCT-MATURITY-DATE > WS-LAST-ACTIVITY
               MOVE ACCT-MATURITY-DATE TO WS-LAST-ACTIVITY
           END-IF.

      *----------------------------------------------------------------*
      * ACTIVE -> DORMANT WHEN LAST ACTIVITY + INACTIVITY MONTHS IS ON *
      * OR BEFORE THE BUSINESS DATE. WITHIN 30 DAYS OF THAT DATE AN    *
      * INACTIVITY NOTICE IS REPORTED FOR THE CUSTOMER LETTER RUN.     *
      *----------------------------------------------------------------*
       2300-DORMANCY-TEST.
           INITIALIZE DATE-PARMS
           SET DTU-ADD-MONTHS TO TRUE
           MOVE WS-LAST-ACTIVITY TO DTU-DATE-1
           MOVE WS-INACT-MONTHS  TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-DORMANT-ON-DATE
           IF WS-DORMANT-ON-DATE <= CTL-BUSINESS-DATE
               SET ACCT-DORMANT TO TRUE
               MOVE CTL-BUSINESS-DATE TO ACCT-DORMANT-DATE
               IF ACCT-ESCHEAT-STATE = SPACES
                   MOVE WS-ESC-STATE TO ACCT-ESCHEAT-STATE
               END-IF
               MOVE 'Y' TO WS-UPDATE-FLAG WS-NEWLY-DORMANT-FLAG
               ADD 1 TO WS-DORMANT-COUNT
               MOVE 'L' TO WS-ALERT-SEV
               MOVE SPACES TO WS-ALERT-TEXT
               STRING 'ACCOUNT CLASSIFIED DORMANT - NO OWNER ACTIVITY '
                      'SINCE ' WS-LAST-ACTIVITY
                      DELIMITED BY SIZE INTO WS-ALERT-TEXT
               MOVE ACCT-LEDGER-BAL TO WS-AMOUNT
               PERFORM 8200-WRITE-ALERT
               MOVE 'DORMANT'   TO WS-RPT-ACTION
               MOVE SPACES TO WS-RPT-TEXT
               STRING 'INACTIVE ' WS-INACT-MONTHS ' MONTHS'
                      DELIMITED BY SIZE INTO WS-RPT-TEXT
               PERFORM 8300-WRITE-DETAIL
           ELSE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CTL-BUSINESS-DATE  TO DTU-DATE-1
               MOVE WS-DORMANT-ON-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-RESULT-NUMBER <= WS-NOTICE-DAYS
                   ADD 1 TO WS-NOTICE-COUNT
                   MOVE 'INACTIVITY NOTICE' TO WS-RPT-ACTION
                   MOVE SPACES TO WS-RPT-TEXT
                   STRING 'DORMANT ON ' WS-DORMANT-ON-DATE
                          ' - SEND LETTER'
                          DELIMITED BY SIZE INTO WS-RPT-TEXT
                   PERFORM 8300-WRITE-DETAIL
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * DORMANT -> ESCHEAT WHEN LAST ACTIVITY + STATE YEARS IS ON OR   *
      * BEFORE THE BUSINESS DATE. WITHIN 120 DAYS A DUE-DILIGENCE      *
      * LETTER IS REQUIRED BY MOST STATES.                             *
      *----------------------------------------------------------------*
       2400-ESCHEAT-TEST.
           INITIALIZE DATE-PARMS
           SET DTU-ADD-MONTHS TO TRUE
           MOVE WS-LAST-ACTIVITY TO DTU-DATE-1
           COMPUTE DTU-NUMBER = WS-ESC-YEARS * 12
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-ESCHEAT-ON-DATE
           IF WS-ESCHEAT-ON-DATE <= CTL-BUSINESS-DATE
               PERFORM 2410-REMIT-TO-STATE
           ELSE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CTL-BUSINESS-DATE  TO DTU-DATE-1
               MOVE WS-ESCHEAT-ON-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-RESULT-NUMBER <= WS-DUE-DILIGENCE-DAYS
                   ADD 1 TO WS-DUEDIL-COUNT
                   MOVE 'DUE DILIGENCE' TO WS-RPT-ACTION
                   MOVE SPACES TO WS-RPT-TEXT
                   STRING 'ESCHEATS ' WS-ESCHEAT-ON-DATE ' TO '
                          WS-ESC-STATE ' - SEND OWNER NOTICE'
                          DELIMITED BY SIZE INTO WS-RPT-TEXT
                   PERFORM 8300-WRITE-DETAIL
                   MOVE 'L' TO WS-ALERT-SEV
                   MOVE SPACES TO WS-ALERT-TEXT
                   STRING 'ESCHEAT DUE DILIGENCE NOTICE REQUIRED - '
                          'REMIT DATE ' WS-ESCHEAT-ON-DATE
                          DELIMITED BY SIZE INTO WS-ALERT-TEXT
                   MOVE ACCT-LEDGER-BAL TO WS-AMOUNT
                   PERFORM 8200-WRITE-ALERT
               END-IF
           END-IF.

       2410-REMIT-TO-STATE.
      *    ACCRUED INTEREST IS CREDITED BEFORE REMITTANCE (CHG 2021-09)
           IF ACCT-ACCRUED-INT >= 0.01
               COMPUTE WS-INT-CREDIT ROUNDED = ACCT-ACCRUED-INT
               ADD WS-INT-CREDIT TO ACCT-LEDGER-BAL ACCT-AVAIL-BAL
               MOVE ZERO TO ACCT-ACCRUED-INT
               MOVE 'DINT' TO WS-TRAN-CODE
               MOVE 'INTEREST CREDIT PRIOR TO ESCHEAT'
                 TO WS-TRAN-DESC
               MOVE WS-INT-CREDIT TO WS-AMOUNT
               PERFORM 8100-WRITE-TRANPOST
           END-IF
           IF ACCT-LEDGER-BAL > ZERO
               MOVE ACCT-LEDGER-BAL TO WS-AMOUNT
               MOVE 'WESC' TO WS-TRAN-CODE
               MOVE SPACES TO WS-TRAN-DESC
               STRING 'UNCLAIMED PROPERTY REMITTED TO ' WS-ESC-STATE
                      DELIMITED BY SIZE INTO WS-TRAN-DESC
               PERFORM 8100-WRITE-TRANPOST
               SUBTRACT WS-AMOUNT FROM ACCT-LEDGER-BAL ACCT-AVAIL-BAL
               SET ACCT-ESCHEATED TO TRUE
               MOVE WS-ESC-STATE TO ACCT-ESCHEAT-STATE
               MOVE 'Y' TO WS-UPDATE-FLAG
               ADD 1 TO WS-ESCHEAT-COUNT
               ADD WS-AMOUNT TO WS-ESCHEAT-TOTAL
               MOVE 'M' TO WS-ALERT-SEV
               MOVE SPACES TO WS-ALERT-TEXT
               STRING 'BALANCE ESCHEATED TO STATE OF ' WS-ESC-STATE
                      ' - LAST OWNER ACTIVITY ' WS-LAST-ACTIVITY
                      DELIMITED BY SIZE INTO WS-ALERT-TEXT
               PERFORM 8200-WRITE-ALERT
               MOVE WS-AMOUNT TO WS-EDIT-AMT
               MOVE 'ESCHEATED' TO WS-RPT-ACTION
               MOVE SPACES TO WS-RPT-TEXT
               STRING 'REMITTED ' WS-EDIT-AMT ' TO ' WS-ESC-STATE
                      ' (' WS-ESC-YEARS ' YRS)'
                      DELIMITED BY SIZE INTO WS-RPT-TEXT
               PERFORM 8300-WRITE-DETAIL
           ELSE
      *        NOTHING TO REMIT - REPORTED FOR MANUAL CLOSURE
               MOVE 'ESCHEAT - ZERO BAL' TO WS-RPT-ACTION
               MOVE 'NO BALANCE TO REMIT - REFER FOR CLOSURE'
                 TO WS-RPT-TEXT
               PERFORM 8300-WRITE-DETAIL
           END-IF.

      *----------------------------------------------------------------*
      * MONTHLY DORMANT ACCOUNT FEE. NOT CHARGED IN THE MONTH THE      *
      * ACCOUNT BECOMES DORMANT, NOT CHARGED IN FEE-PROHIBITED STATES, *
      * NEVER TAKES THE ACCOUNT BELOW ZERO.                            *
      *----------------------------------------------------------------*
       2500-DORMANT-FEE.
           MOVE 'N' TO WS-NO-FEE-FLAG
           SET WS-NF-IDX TO 1
           SEARCH WS-NO-FEE-STATE
               AT END CONTINUE
               WHEN WS-NO-FEE-STATE(WS-NF-IDX) = WS-ESC-STATE
                   SET WS-FEE-PROHIBITED TO TRUE
           END-SEARCH
           EVALUATE TRUE
               WHEN WS-FEE-PROHIBITED
                   ADD 1 TO WS-FEE-SUPPR-COUNT
                   MOVE 'FEE SUPPRESSED' TO WS-RPT-ACTION
                   MOVE SPACES TO WS-RPT-TEXT
                   STRING 'STATE ' WS-ESC-STATE
                          ' PROHIBITS DORMANCY FEES'
                          DELIMITED BY SIZE INTO WS-RPT-TEXT
                   PERFORM 8300-WRITE-DETAIL
               WHEN ACCT-AVAIL-BAL < WS-DORMANT-FEE
                   ADD 1 TO WS-FEE-SUPPR-COUNT
                   MOVE 'FEE SUPPRESSED' TO WS-RPT-ACTION
                   MOVE 'INSUFFICIENT AVAILABLE BALANCE'
                     TO WS-RPT-TEXT
                   PERFORM 8300-WRITE-DETAIL
               WHEN OTHER
                   MOVE WS-DORMANT-FEE TO WS-AMOUNT
                   MOVE 'WFOT' TO WS-TRAN-CODE
                   MOVE 'DORMANT ACCOUNT FEE' TO WS-TRAN-DESC
                   PERFORM 8100-WRITE-TRANPOST
                   SUBTRACT WS-AMOUNT FROM ACCT-LEDGER-BAL
                                           ACCT-AVAIL-BAL
                   ADD WS-AMOUNT TO ACCT-FEES-MTD ACCT-FEES-YTD
                   MOVE 'Y' TO WS-UPDATE-FLAG
                   ADD 1 TO WS-FEE-COUNT
                   ADD WS-AMOUNT TO WS-FEE-TOTAL
                   MOVE 'DORMANT FEE' TO WS-RPT-ACTION
                   MOVE WS-AMOUNT TO WS-EDIT-AMT
                   MOVE SPACES TO WS-RPT-TEXT
                   STRING 'CHARGED ' WS-EDIT-AMT
                          DELIMITED BY SIZE INTO WS-RPT-TEXT
                   PERFORM 8300-WRITE-DETAIL
           END-EVALUATE.

      *----------------------------------------------------------------*
       8000-READ-NEXT-ACCOUNT.
           READ ACCOUNT-FILE NEXT RECORD
               AT END SET WS-EOF TO TRUE
           END-READ
           IF NOT WS-EOF AND WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'READ NEXT FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF.

      *----------------------------------------------------------------*
       8100-WRITE-TRANPOST.
           INITIALIZE TRANSACTION-RECORD
           ADD 1 TO WS-TRAN-SEQ
           MOVE CTL-BUSINESS-DATE  TO TRAN-ID-DATE
                                      TRAN-EFFECTIVE-DATE
                                      TRAN-POST-DATE
           MOVE WS-TRAN-SEQ        TO TRAN-ID-SEQ
           MOVE ACCT-NUMBER        TO TRAN-ACCT-NUMBER
           MOVE 220000             TO TRAN-TIME
           MOVE WS-TRAN-CODE       TO TRAN-CODE
           MOVE 'D'                TO TRAN-DR-CR
           SET TC-IDX TO 1
           SEARCH TC-ENTRY
               AT END CONTINUE
               WHEN TC-CODE(TC-IDX) = WS-TRAN-CODE
                   MOVE TC-DR-CR(TC-IDX)     TO TRAN-DR-CR
                   MOVE TC-CASH-FLAG(TC-IDX) TO TRAN-CASH-FLAG
                   MOVE TC-REGD-FLAG(TC-IDX) TO TRAN-REGD-FLAG
           END-SEARCH
           MOVE WS-AMOUNT          TO TRAN-AMOUNT
           MOVE 'BAT'              TO TRAN-CHANNEL
           MOVE WS-TRAN-DESC       TO TRAN-DESCRIPTION
           MOVE 'DORMSCAN'         TO TRAN-USER-ID
           MOVE ACCT-COST-CENTER   TO TRAN-COST-CENTER
           SET TRAN-POSTED         TO TRUE
           MOVE 'DORMSCAN'         TO TRAN-SOURCE-PGM
           IF WS-TRAN-CODE = 'WESC'
               MOVE ACCT-ESCHEAT-STATE TO TRAN-REFERENCE(1:2)
               MOVE 'STATE UNCLAIMED PROPERTY DIVISION'
                 TO TRAN-COUNTERPARTY-NAME
               MOVE 'US' TO TRAN-COUNTERPARTY-COUNTRY
           END-IF
           WRITE TRANSACTION-RECORD
           IF WS-POST-STATUS NOT = '00'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               MOVE 'WRITE FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF.

      *----------------------------------------------------------------*
       8200-WRITE-ALERT.
           INITIALIZE ALERT-RECORD
           MOVE CTL-BUSINESS-DATE    TO ALRT-DATE
           MOVE 220000               TO ALRT-TIME
           MOVE 'DORM'               TO ALRT-TYPE
           MOVE WS-ALERT-SEV         TO ALRT-SEVERITY
           MOVE ACCT-PRIMARY-CUST-ID TO ALRT-CUST-ID
           MOVE ACCT-NUMBER          TO ALRT-ACCT-NUMBER
           MOVE WS-AMOUNT            TO ALRT-AMOUNT
           MOVE 'DORM-001'           TO ALRT-RULE-ID
           IF ACCT-ESCHEATED
               MOVE 'DORM-002'       TO ALRT-RULE-ID
           END-IF
           MOVE WS-ALERT-TEXT        TO ALRT-DESCRIPTION
           SET ALRT-OPEN             TO TRUE
           MOVE 'DORMSCAN'           TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD
           IF WS-ALRT-STATUS NOT = '00'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               MOVE 'WRITE FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF.

      *----------------------------------------------------------------*
       8300-WRITE-DETAIL.
           MOVE ACCT-NUMBER        TO WS-D-ACCT
           MOVE ACCT-PRODUCT-CODE  TO WS-D-PROD
           MOVE ACCT-STATUS        TO WS-D-STATUS
           MOVE ACCT-LAST-ACTIVITY-DATE TO WS-D-LAST-ACT
           MOVE WS-ESC-STATE       TO WS-D-ESC-STATE
           IF WS-EXCLUDED
               MOVE ACCT-ESCHEAT-STATE TO WS-D-ESC-STATE
           END-IF
           MOVE WS-RPT-ACTION      TO WS-D-ACTION
           MOVE WS-RPT-TEXT        TO WS-D-TEXT
           WRITE REPORT-LINE FROM WS-DETAIL-LINE.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-READ-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'ACCOUNTS READ ............ ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-EXCL-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'EXCLUDED ................. ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-NOTICE-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'INACTIVITY NOTICES ....... ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-DORMANT-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'NEWLY DORMANT ............ ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-DUEDIL-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'DUE DILIGENCE LETTERS .... ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-ESCHEAT-COUNT TO WS-EDIT-CNT
           MOVE WS-ESCHEAT-TOTAL TO WS-EDIT-AMT
           MOVE SPACES TO REPORT-LINE
           STRING 'ESCHEATED ................ ' WS-EDIT-CNT
                  '  AMOUNT ' WS-EDIT-AMT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-FEE-COUNT TO WS-EDIT-CNT
           MOVE WS-FEE-TOTAL TO WS-EDIT-AMT
           MOVE SPACES TO REPORT-LINE
           STRING 'DORMANT FEES CHARGED ..... ' WS-EDIT-CNT
                  '  AMOUNT ' WS-EDIT-AMT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-FEE-SUPPR-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'DORMANT FEES SUPPRESSED .. ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           CLOSE ACCOUNT-FILE CUSTOMER-FILE TRANPOST-FILE
                 ALERT-FILE REPORT-FILE
           IF WS-HOLDS-AVAILABLE
               CLOSE HOLD-FILE
           END-IF
           DISPLAY 'DORMSCAN READ=' WS-READ-COUNT
                   ' DORMANT=' WS-DORMANT-COUNT
                   ' ESCHEATED=' WS-ESCHEAT-COUNT
                   ' FEES=' WS-FEE-COUNT
                   ' EXCLUDED=' WS-EXCL-COUNT
           MOVE 0 TO RETURN-CODE.

      *----------------------------------------------------------------*
       9900-ABEND.
           MOVE 'DORMSCAN' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
