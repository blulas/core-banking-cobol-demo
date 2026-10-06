      *================================================================*
      * PROGRAM  : STMTGEN                                             *
      * TYPE     : BATCH - EOD STEP 16 (MONTH-END)                     *
      * PURPOSE  : DEPOSIT ACCOUNT PERIODIC STATEMENTS (REG DD         *
      *            1030.6). PRINT-IMAGE FILE, 132 COLUMNS, ASA         *
      *            CARRIAGE CONTROL IN COLUMN 1.                       *
      *            - A STATEMENT IS DUE WHEN THE ACCOUNT'S CYCLE DAY   *
      *              (ACCT-STMT-CYCLE-DAY, 31 = LAST DAY OF MONTH,     *
      *              CLAMPED TO MONTH END) FALLS AFTER THE PREVIOUS    *
      *              BUSINESS DATE AND ON OR BEFORE TODAY              *
      *            - QUARTERLY STATEMENTS (ACCT-STMT-FREQ Q, AND ALL   *
      *              PASSBOOK SAVINGS SVST) ONLY IN MAR/JUN/SEP/DEC    *
      *            - CDS RECEIVE AN ANNUAL STATEMENT IN DECEMBER ONLY  *
      *            - SUPPRESSED FOR ZERO BALANCE WITH NO ACTIVITY      *
      *            - CONTENT: OPENING BALANCE, EVERY TRANSACTION FROM  *
      *              TRANHIST + TODAY'S TRANPOST POSTED IN THE PERIOD, *
      *              INTEREST PAID, FEES, ANNUAL PERCENTAGE YIELD      *
      *              EARNED (REG DD APPENDIX A PART II), AVERAGE DAILY *
      *              BALANCE, OVERDRAFT AND RETURNED ITEM FEE TOTALS   *
      *              FOR THE PERIOD AND YEAR TO DATE (1030.11),        *
      *              CLOSING BALANCE                                   *
      *            - AFTER PRINTING, THE CYCLE ACCUMULATORS ARE RESET  *
      *              AND ACCT-LAST-STMT-DATE IS SET. THE REG D COUNTER *
      *              IS RESET BY EODCTL, NOT HERE.                     *
      * PARM     : FORCE = RUN ON A NON MONTH-END DATE                 *
      * INPUT    : BANKCTL, PRODMST, CUSTMST, TRANHIST, TRANPOST       *
      * I-O      : ACCTMST                                             *
      * OUTPUT   : STMTOUT (STATEMENTS), STMTRPT (CONTROL REPORT)      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1993-06-21 RJT  TRUTH IN SAVINGS - APY EARNED                  *
      * 1998-11-02 MKL  Y2K                                            *
      * 2006-07-10 BHS  REG DD 2006 - OD / RETURNED ITEM FEE TOTALS    *
      * 2009-03-02 DPS  SUPPRESS ZERO BALANCE / NO ACTIVITY STATEMENTS *
      * 2017-02-08 AKW  ONLINE SAVINGS MARKETING MESSAGE               *
      * 2023-03-13 JQP  MARKETING MESSAGE RATE UPDATE - MKT-2023-07    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STMTGEN.

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
           SELECT OPTIONAL TRANHIST-FILE ASSIGN TO "TRANHIST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-HIST-STATUS.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT STATEMENT-FILE ASSIGN TO "STMTOUT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STMT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "STMTRPT"
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
       FD  TRANHIST-FILE.
       COPY TRANREC REPLACING ==TRANSACTION-RECORD== BY ==HIST-RECORD==
                     LEADING ==TRAN-== BY ==HST-==.
       FD  TRANPOST-FILE.
       COPY TRANREC.
       FD  STATEMENT-FILE.
       01  STMT-LINE                       PIC X(132).
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-PROD-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-HIST-STATUS              PIC X(02).
           05  WS-POST-STATUS              PIC X(02).
           05  WS-STMT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).

       01  WS-PARM                         PIC X(10).
           88  WS-PARM-FORCE                         VALUE 'FORCE'.
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-DUE-FLAG                     PIC X(01).
           88  WS-STMT-DUE                           VALUE 'Y'.

      *--- MARKETING MESSAGE (MKT-2023-07) ----------------------------*
       01  WS-MKT-MESSAGE-1.
           05  FILLER PIC X(40) VALUE
               'GROW YOUR SAVINGS - OPEN AN ONLINE HIGH '.
           05  FILLER PIC X(41) VALUE
               'YIELD SAVINGS ACCOUNT AND EARN 4.25% APY*'.
       01  WS-MKT-MESSAGE-2.
           05  FILLER PIC X(40) VALUE
               '*ANNUAL PERCENTAGE YIELD ACCURATE AS OF '.
           05  FILLER PIC X(39) VALUE
               'STATEMENT DATE. RATE MAY CHANGE.'.
      *    2017-02 AKW ORIGINAL MESSAGE
      *    'OPEN AN ONLINE SAVINGS ACCOUNT AND EARN 1.05% APY*'

      *--- TRANSACTIONS POSTED THIS YEAR (TRANHIST + TRANPOST) --------*
       01  WS-TXN-TABLE.
           05  WS-TXN-COUNT                PIC 9(05) COMP VALUE 0.
           05  WS-TXN-OVERFLOW             PIC X(01) VALUE 'N'.
           05  WS-TXN-ENTRY OCCURS 8000 TIMES INDEXED BY WS-TX-IDX.
               10  WS-TXN-ACCT             PIC X(12).
               10  WS-TXN-POST-DATE        PIC 9(08).
               10  WS-TXN-EFF-DATE         PIC 9(08).
               10  WS-TXN-CODE             PIC X(04).
               10  WS-TXN-DRCR             PIC X(01).
               10  WS-TXN-AMOUNT           PIC S9(13)V99 COMP-3.
               10  WS-TXN-DESC             PIC X(40).
               10  WS-TXN-SEQ              PIC 9(08).

       01  WS-DATES.
           05  WS-YEAR-START               PIC 9(08).
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-R REDEFINES WS-BUS-DATE.
               10  WS-BUS-CCYY             PIC 9(04).
               10  WS-BUS-MM               PIC 9(02).
               10  WS-BUS-DD               PIC 9(02).
           05  WS-EOM-DATE                 PIC 9(08).
           05  WS-EOM-R REDEFINES WS-EOM-DATE.
               10  WS-EOM-CCYY             PIC 9(04).
               10  WS-EOM-MM               PIC 9(02).
               10  WS-EOM-DD               PIC 9(02).
           05  WS-TARGET-DATE              PIC 9(08).
           05  WS-TARGET-R REDEFINES WS-TARGET-DATE.
               10  WS-TGT-CCYY             PIC 9(04).
               10  WS-TGT-MM               PIC 9(02).
               10  WS-TGT-DD               PIC 9(02).
           05  WS-PERIOD-START             PIC 9(08).
           05  WS-PERIOD-DAYS              PIC S9(05) COMP.

       01  WS-STMT-TOTALS.
           05  WS-OPENING-BAL              PIC S9(13)V99 COMP-3.
           05  WS-CLOSING-BAL              PIC S9(13)V99 COMP-3.
           05  WS-RUNNING-BAL              PIC S9(13)V99 COMP-3.
           05  WS-TOT-CREDITS              PIC S9(13)V99 COMP-3.
           05  WS-TOT-DEBITS               PIC S9(13)V99 COMP-3.
           05  WS-CR-COUNT                 PIC 9(05).
           05  WS-DR-COUNT                 PIC 9(05).
           05  WS-INT-PAID                 PIC S9(13)V99 COMP-3.
           05  WS-WITHHELD                 PIC S9(13)V99 COMP-3.
           05  WS-FEES                     PIC S9(13)V99 COMP-3.
           05  WS-OD-FEES-PER              PIC S9(09)V99 COMP-3.
           05  WS-OD-FEES-YTD              PIC S9(09)V99 COMP-3.
           05  WS-NSF-FEES-PER             PIC S9(09)V99 COMP-3.
           05  WS-NSF-FEES-YTD             PIC S9(09)V99 COMP-3.
           05  WS-AVG-BAL                  PIC S9(13)V99 COMP-3.
           05  WS-APYE                     PIC S9(03)V99 COMP-3.
           05  WS-APYE-WORK                PIC S9(05)V9(12) COMP-3.

       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-STMT-COUNT               PIC 9(07) VALUE 0.
           05  WS-SUPPRESS-COUNT           PIC 9(07) VALUE 0.
           05  WS-NOTDUE-COUNT             PIC 9(07) VALUE 0.
           05  WS-LINE-COUNT               PIC 9(07) VALUE 0.

       01  WS-OWNER-NAME                   PIC X(45).
       01  WS-JOINT-NAME                   PIC X(45).
       01  WS-ADDR-SWAP                    PIC X(45).
       01  WS-DAYS-EDIT                    PIC ZZZ9.
       01  WS-SAVE-CUST.
           05  WS-SV-ADDR1                 PIC X(35).
           05  WS-SV-ADDR2                 PIC X(35).
           05  WS-SV-CITY                  PIC X(25).
           05  WS-SV-STATE                 PIC X(02).
           05  WS-SV-ZIP                   PIC X(10).
           05  WS-SV-SEGMENT               PIC X(02).

      *--- STATEMENT PRINT LINES --------------------------------------*
       01  PL-BLANK                        PIC X(132) VALUE SPACES.
       01  PL-RULE.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  FILLER                      PIC X(131) VALUE ALL '-'.
       01  PL-HEAD-1.
           05  PL-CC                       PIC X(01) VALUE '1'.
           05  FILLER                      PIC X(40)
               VALUE 'CBS BANK, N.A.'.
           05  FILLER                      PIC X(40)
               VALUE 'DEPOSIT ACCOUNT STATEMENT'.
           05  FILLER                      PIC X(16)
               VALUE 'STATEMENT DATE '.
           05  PH1-DATE                    PIC 9999/99/99.
           05  FILLER                      PIC X(25) VALUE SPACES.
       01  PL-HEAD-2.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  FILLER                      PIC X(40)
               VALUE 'MEMBER FDIC - EQUAL HOUSING LENDER'.
           05  FILLER                      PIC X(40) VALUE SPACES.
           05  FILLER                      PIC X(16)
               VALUE 'PERIOD         '.
           05  PH2-FROM                    PIC 9999/99/99.
           05  FILLER                      PIC X(04) VALUE ' TO '.
           05  PH2-TO                      PIC 9999/99/99.
           05  FILLER                      PIC X(11) VALUE SPACES.
       01  PL-ADDR.
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  PA-TEXT                     PIC X(60).
           05  FILLER                      PIC X(14) VALUE SPACES.
           05  PA-LABEL                    PIC X(16).
           05  PA-VALUE                    PIC X(36).
       01  PL-SUMMARY.
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  PS-LABEL                    PIC X(40).
           05  PS-AMOUNT                   PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  PS-LABEL-2                  PIC X(30).
           05  PS-AMOUNT-2                 PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(16) VALUE SPACES.
       01  PL-TXN-HEAD.
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  FILLER                      PIC X(30) VALUE
               'POSTED      EFFECTIVE   DESCRI'.
           05  FILLER                      PIC X(30) VALUE
               'PTION                         '.
           05  FILLER                      PIC X(30) VALUE
               '           DEBITS            C'.
           05  FILLER                      PIC X(36) VALUE
               'REDITS            BALANCE'.
       01  PL-TXN.
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  PT-POST                     PIC 9999/99/99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  PT-EFF                      PIC 9999/99/99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  PT-CODE                     PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  PT-DESC                     PIC X(40).
           05  PT-DEBIT                    PIC Z,ZZZ,ZZZ,ZZ9.99
                                           BLANK WHEN ZERO.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  PT-CREDIT                   PIC Z,ZZZ,ZZZ,ZZ9.99
                                           BLANK WHEN ZERO.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  PT-BALANCE                  PIC -,---,---,--9.99.
           05  FILLER                      PIC X(04) VALUE SPACES.
       01  PL-TEXT.
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  PX-TEXT                     PIC X(126).
       01  PL-APY.
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  FILLER                      PIC X(40)
               VALUE 'ANNUAL PERCENTAGE YIELD EARNED'.
           05  PY-APYE                     PIC ZZ9.99.
           05  FILLER                      PIC X(01) VALUE '%'.
           05  FILLER                      PIC X(19) VALUE SPACES.
           05  FILLER                      PIC X(17)
               VALUE 'CURRENT RATE    '.
           05  PY-RATE                     PIC ZZ9.999999.
           05  FILLER                      PIC X(01) VALUE '%'.
           05  FILLER                      PIC X(32) VALUE SPACES.

      *--- CONTROL REPORT ---------------------------------------------*
       01  RL-DETAIL.
           05  RL-ACCT                     PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  RL-PROD                     PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  RL-ACTION                   PIC X(30).
           05  RL-FROM                     PIC 9(08).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  RL-TO                       PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  RL-TXNS                     PIC ZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  RL-CLOSE                    PIC -,---,---,--9.99.
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  RL-APYE                     PIC ZZ9.99.
           05  FILLER                      PIC X(35) VALUE SPACE.
       01  RL-TOTAL.
           05  RLT-LABEL                   PIC X(30).
           05  RLT-COUNT                   PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(95) VALUE SPACES.

       COPY BANKCTL.
       COPY DATEPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-PARM FROM COMMAND-LINE
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           IF NOT CTL-IS-MONTH-END AND NOT WS-PARM-FORCE
               DISPLAY 'STMTGEN NOT MONTH-END (' CTL-BUSINESS-DATE
                       ') - NO STATEMENTS TODAY'
               MOVE 0 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 1000-INITIALIZE
           PERFORM 1100-LOAD-TRANSACTIONS
           PERFORM 2000-PROCESS-ACCOUNTS
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALIZE.
           MOVE CTL-BUSINESS-DATE TO WS-BUS-DATE
           COMPUTE WS-YEAR-START = WS-BUS-CCYY * 10000 + 0101
           INITIALIZE DATE-PARMS
           SET DTU-END-OF-MONTH TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-EOM-DATE

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
           OPEN OUTPUT STATEMENT-FILE REPORT-FILE
           IF WS-STMT-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
               MOVE 'STMTOUT' TO ABND-FILE-NAME
               MOVE WS-STMT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           MOVE SPACES TO REPORT-LINE
           STRING 'STMTGEN  STATEMENT CONTROL REPORT   BUSINESS DATE '
                  CTL-BUSINESS-DATE DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE.

      *----------------------------------------------------------------*
      * ALL TRANSACTIONS POSTED SINCE 1 JANUARY (YTD FEE TOTALS NEED   *
      * THE WHOLE YEAR). PRODUCTION SORTS TRANHIST BY ACCOUNT IN THE   *
      * PRECEDING JCL STEP; HERE THE TABLE IS SCANNED PER ACCOUNT.     *
      *----------------------------------------------------------------*
       1100-LOAD-TRANSACTIONS.
           OPEN INPUT TRANHIST-FILE
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ TRANHIST-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF HST-POSTED
                          AND HST-POST-DATE >= WS-YEAR-START
                           IF WS-TXN-COUNT < 8000
                               ADD 1 TO WS-TXN-COUNT
                               SET WS-TX-IDX TO WS-TXN-COUNT
                               MOVE HST-ACCT-NUMBER
                                 TO WS-TXN-ACCT(WS-TX-IDX)
                               MOVE HST-POST-DATE
                                 TO WS-TXN-POST-DATE(WS-TX-IDX)
                               MOVE HST-EFFECTIVE-DATE
                                 TO WS-TXN-EFF-DATE(WS-TX-IDX)
                               MOVE HST-CODE TO WS-TXN-CODE(WS-TX-IDX)
                               MOVE HST-DR-CR TO WS-TXN-DRCR(WS-TX-IDX)
                               MOVE HST-AMOUNT
                                 TO WS-TXN-AMOUNT(WS-TX-IDX)
                               MOVE HST-DESCRIPTION
                                 TO WS-TXN-DESC(WS-TX-IDX)
                               MOVE HST-ID-SEQ TO WS-TXN-SEQ(WS-TX-IDX)
                           ELSE
                               MOVE 'Y' TO WS-TXN-OVERFLOW
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TRANHIST-FILE

           OPEN INPUT TRANPOST-FILE
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ TRANPOST-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF TRAN-POSTED
                           IF WS-TXN-COUNT < 8000
                               ADD 1 TO WS-TXN-COUNT
                               SET WS-TX-IDX TO WS-TXN-COUNT
                               MOVE TRAN-ACCT-NUMBER
                                 TO WS-TXN-ACCT(WS-TX-IDX)
                               MOVE TRAN-POST-DATE
                                 TO WS-TXN-POST-DATE(WS-TX-IDX)
                               IF TRAN-POST-DATE = ZERO
                                   MOVE CTL-BUSINESS-DATE
                                     TO WS-TXN-POST-DATE(WS-TX-IDX)
                               END-IF
                               MOVE TRAN-EFFECTIVE-DATE
                                 TO WS-TXN-EFF-DATE(WS-TX-IDX)
                               MOVE TRAN-CODE TO WS-TXN-CODE(WS-TX-IDX)
                               MOVE TRAN-DR-CR
                                 TO WS-TXN-DRCR(WS-TX-IDX)
                               MOVE TRAN-AMOUNT
                                 TO WS-TXN-AMOUNT(WS-TX-IDX)
                               MOVE TRAN-DESCRIPTION
                                 TO WS-TXN-DESC(WS-TX-IDX)
                               MOVE TRAN-ID-SEQ
                                 TO WS-TXN-SEQ(WS-TX-IDX)
                           ELSE
                               MOVE 'Y' TO WS-TXN-OVERFLOW
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TRANPOST-FILE
           IF WS-TXN-OVERFLOW = 'Y'
               DISPLAY 'STMTGEN TRANSACTION TABLE FULL (8000) - '
                       'STATEMENTS MAY BE INCOMPLETE'
           END-IF.

      *----------------------------------------------------------------*
       2000-PROCESS-ACCOUNTS.
           MOVE 'N' TO WS-EOF-FLAG
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY >= ACCT-NUMBER
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-ONE-ACCOUNT
               END-READ
           END-PERFORM.

       2100-ONE-ACCOUNT.
           ADD 1 TO WS-READ-COUNT
           IF ACCT-CATEGORY NOT = 'SV' AND 'DD' AND 'TD' AND 'MM'
               EXIT PARAGRAPH
           END-IF
           IF ACCT-CLOSED OR ACCT-ESCHEATED OR ACCT-CHARGED-OFF
              OR ACCT-PENDING-OPEN
               EXIT PARAGRAPH
           END-IF
           PERFORM 2200-CHECK-DUE
           IF NOT WS-STMT-DUE
               ADD 1 TO WS-NOTDUE-COUNT
               EXIT PARAGRAPH
           END-IF

           PERFORM 2300-PERIOD-TOTALS
           MOVE ACCT-NUMBER       TO RL-ACCT
           MOVE ACCT-PRODUCT-CODE TO RL-PROD
           MOVE WS-PERIOD-START   TO RL-FROM
           MOVE CTL-BUSINESS-DATE TO RL-TO
           COMPUTE RL-TXNS = WS-CR-COUNT + WS-DR-COUNT
           MOVE ACCT-LEDGER-BAL   TO RL-CLOSE
           MOVE ZERO              TO RL-APYE

           IF ACCT-LEDGER-BAL = ZERO
              AND WS-CR-COUNT = 0 AND WS-DR-COUNT = 0
               MOVE 'SUPPRESSED - ZERO, NO ACTIVITY' TO RL-ACTION
               WRITE REPORT-LINE FROM RL-DETAIL
               ADD 1 TO WS-SUPPRESS-COUNT
               PERFORM 2900-RESET-CYCLE
               EXIT PARAGRAPH
           END-IF

           MOVE ACCT-PRODUCT-CODE TO PROD-CODE
           READ PRODUCT-FILE
               INVALID KEY INITIALIZE PRODUCT-RECORD
           END-READ
           PERFORM 2400-COMPUTE-APYE
           PERFORM 3000-PRINT-STATEMENT
           MOVE WS-APYE TO RL-APYE
           MOVE 'STATEMENT PRODUCED' TO RL-ACTION
           WRITE REPORT-LINE FROM RL-DETAIL
           ADD 1 TO WS-STMT-COUNT
           PERFORM 2900-RESET-CYCLE.

      *----------------------------------------------------------------*
      * CYCLE DAY THIS MONTH (CLAMPED TO MONTH END) MUST FALL IN       *
      * (PREVIOUS BUSINESS DATE, BUSINESS DATE].                       *
      *----------------------------------------------------------------*
       2200-CHECK-DUE.
           MOVE 'N' TO WS-DUE-FLAG
           MOVE WS-EOM-DATE TO WS-TARGET-DATE
           IF ACCT-STMT-CYCLE-DAY > 0
              AND ACCT-STMT-CYCLE-DAY < WS-EOM-DD
               MOVE ACCT-STMT-CYCLE-DAY TO WS-TGT-DD
           END-IF
           IF WS-TARGET-DATE > CTL-PREV-BUS-DATE
              AND WS-TARGET-DATE <= CTL-BUSINESS-DATE
               SET WS-STMT-DUE TO TRUE
           END-IF
      *    FORCE RUNS PRODUCE STATEMENTS REGARDLESS OF CYCLE DAY
           IF WS-PARM-FORCE
               SET WS-STMT-DUE TO TRUE
           END-IF
           IF WS-STMT-DUE
              AND (ACCT-STMT-QUARTERLY OR ACCT-PRODUCT-CODE = 'SVST')
              AND WS-BUS-MM NOT = 03 AND 06 AND 09 AND 12
               MOVE 'N' TO WS-DUE-FLAG
           END-IF
           IF WS-STMT-DUE AND ACCT-CATEGORY = 'TD'
              AND WS-BUS-MM NOT = 12
               MOVE 'N' TO WS-DUE-FLAG
           END-IF.

      *----------------------------------------------------------------*
       2300-PERIOD-TOTALS.
           IF ACCT-LAST-STMT-DATE > ZERO
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE ACCT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE 1 TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-PERIOD-START
           ELSE
      *        NO PRIOR STATEMENT - START OF THIS MONTH (QUARTER FOR
      *        QUARTERLY CYCLES), OR THE OPEN DATE IF LATER
               MOVE WS-BUS-DATE TO WS-TARGET-DATE
               MOVE 01 TO WS-TGT-DD
               IF ACCT-STMT-QUARTERLY OR ACCT-PRODUCT-CODE = 'SVST'
                   INITIALIZE DATE-PARMS
                   SET DTU-ADD-MONTHS TO TRUE
                   MOVE WS-TARGET-DATE TO DTU-DATE-1
                   MOVE -2 TO DTU-NUMBER
                   CALL 'DATEUTIL' USING DATE-PARMS
                   MOVE DTU-RESULT-DATE TO WS-TARGET-DATE
               END-IF
               MOVE WS-TARGET-DATE TO WS-PERIOD-START
               IF ACCT-OPEN-DATE > WS-PERIOD-START
                   MOVE ACCT-OPEN-DATE TO WS-PERIOD-START
               END-IF
           END-IF
           INITIALIZE DATE-PARMS
           SET DTU-DAYS-BETWEEN TO TRUE
           MOVE WS-PERIOD-START   TO DTU-DATE-1
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           COMPUTE WS-PERIOD-DAYS = DTU-RESULT-NUMBER + 1

           MOVE ZERO TO WS-TOT-CREDITS WS-TOT-DEBITS WS-CR-COUNT
                        WS-DR-COUNT WS-INT-PAID WS-WITHHELD WS-FEES
                        WS-OD-FEES-PER WS-OD-FEES-YTD
                        WS-NSF-FEES-PER WS-NSF-FEES-YTD
           PERFORM VARYING WS-TX-IDX FROM 1 BY 1
                   UNTIL WS-TX-IDX > WS-TXN-COUNT
               IF WS-TXN-ACCT(WS-TX-IDX) = ACCT-NUMBER
                   PERFORM 2310-ACCUMULATE
               END-IF
           END-PERFORM
           MOVE ACCT-LEDGER-BAL TO WS-CLOSING-BAL
           COMPUTE WS-OPENING-BAL = WS-CLOSING-BAL
                                  - WS-TOT-CREDITS + WS-TOT-DEBITS
           IF ACCT-DAYS-IN-CYCLE > 0
               COMPUTE WS-AVG-BAL ROUNDED =
                       ACCT-BAL-ACCUM-CYC / ACCT-DAYS-IN-CYCLE
           ELSE
               MOVE ACCT-LEDGER-BAL TO WS-AVG-BAL
           END-IF.

       2310-ACCUMULATE.
           IF WS-TXN-CODE(WS-TX-IDX) = 'WFOD'
               ADD WS-TXN-AMOUNT(WS-TX-IDX) TO WS-OD-FEES-YTD
           END-IF
           IF WS-TXN-CODE(WS-TX-IDX) = 'WFNS'
               ADD WS-TXN-AMOUNT(WS-TX-IDX) TO WS-NSF-FEES-YTD
           END-IF
           IF WS-TXN-POST-DATE(WS-TX-IDX) < WS-PERIOD-START
              OR WS-TXN-POST-DATE(WS-TX-IDX) > CTL-BUSINESS-DATE
               EXIT PARAGRAPH
           END-IF
           IF WS-TXN-DRCR(WS-TX-IDX) = 'C'
               ADD WS-TXN-AMOUNT(WS-TX-IDX) TO WS-TOT-CREDITS
               ADD 1 TO WS-CR-COUNT
           ELSE
               ADD WS-TXN-AMOUNT(WS-TX-IDX) TO WS-TOT-DEBITS
               ADD 1 TO WS-DR-COUNT
           END-IF
           EVALUATE WS-TXN-CODE(WS-TX-IDX)
               WHEN 'DINT'
                   ADD WS-TXN-AMOUNT(WS-TX-IDX) TO WS-INT-PAID
               WHEN 'WBWH'
                   ADD WS-TXN-AMOUNT(WS-TX-IDX) TO WS-WITHHELD
               WHEN 'WFOD'
                   ADD WS-TXN-AMOUNT(WS-TX-IDX) TO WS-OD-FEES-PER
                                                   WS-FEES
               WHEN 'WFNS'
                   ADD WS-TXN-AMOUNT(WS-TX-IDX) TO WS-NSF-FEES-PER
                                                   WS-FEES
               WHEN 'DRFD'
                   SUBTRACT WS-TXN-AMOUNT(WS-TX-IDX) FROM WS-FEES
               WHEN OTHER
                   IF WS-TXN-CODE(WS-TX-IDX)(1:2) = 'WF'
                       ADD WS-TXN-AMOUNT(WS-TX-IDX) TO WS-FEES
                   END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REG DD APPENDIX A PART II:                                     *
      *   APY EARNED = 100 * ((1 + INTEREST / BALANCE) ** (365/DAYS)-1)*
      *   BALANCE = AVERAGE DAILY BALANCE FOR THE PERIOD               *
      *----------------------------------------------------------------*
       2400-COMPUTE-APYE.
           MOVE ZERO TO WS-APYE
           IF WS-INT-PAID > ZERO AND WS-AVG-BAL > ZERO
              AND WS-PERIOD-DAYS > 0
               COMPUTE WS-APYE-WORK ROUNDED =
                   100 * ((1 + WS-INT-PAID / WS-AVG-BAL)
                          ** (365 / WS-PERIOD-DAYS) - 1)
               COMPUTE WS-APYE ROUNDED = WS-APYE-WORK
           END-IF.

      *----------------------------------------------------------------*
       3000-PRINT-STATEMENT.
           PERFORM 3100-GET-NAMES
           MOVE CTL-BUSINESS-DATE TO PH1-DATE
           WRITE STMT-LINE FROM PL-HEAD-1
           MOVE WS-PERIOD-START   TO PH2-FROM
           MOVE CTL-BUSINESS-DATE TO PH2-TO
           WRITE STMT-LINE FROM PL-HEAD-2
           WRITE STMT-LINE FROM PL-BLANK

           MOVE WS-OWNER-NAME TO PA-TEXT
           MOVE 'ACCOUNT NUMBER' TO PA-LABEL
           MOVE ACCT-NUMBER TO PA-VALUE
           WRITE STMT-LINE FROM PL-ADDR
           MOVE SPACES TO PA-TEXT PA-LABEL PA-VALUE
           IF WS-JOINT-NAME NOT = SPACES
               MOVE WS-JOINT-NAME TO PA-TEXT
           ELSE
               MOVE WS-SV-ADDR1 TO PA-TEXT
           END-IF
           MOVE 'PRODUCT' TO PA-LABEL
           MOVE PROD-NAME TO PA-VALUE
           WRITE STMT-LINE FROM PL-ADDR
           MOVE SPACES TO PA-TEXT PA-LABEL PA-VALUE
           IF WS-JOINT-NAME NOT = SPACES
               MOVE WS-SV-ADDR1 TO PA-TEXT
               WRITE STMT-LINE FROM PL-ADDR
               MOVE SPACES TO PA-TEXT
           END-IF
           STRING FUNCTION TRIM(WS-SV-CITY) ' ' WS-SV-STATE ' '
                  WS-SV-ZIP DELIMITED BY SIZE INTO PA-TEXT
           MOVE 'DAYS IN PERIOD' TO PA-LABEL
           MOVE WS-PERIOD-DAYS TO WS-DAYS-EDIT
           MOVE WS-DAYS-EDIT TO PA-VALUE
           WRITE STMT-LINE FROM PL-ADDR
           WRITE STMT-LINE FROM PL-BLANK

           MOVE 'ACCOUNT SUMMARY' TO PX-TEXT
           WRITE STMT-LINE FROM PL-TEXT
           WRITE STMT-LINE FROM PL-RULE
           MOVE 'OPENING BALANCE' TO PS-LABEL
           MOVE WS-OPENING-BAL TO PS-AMOUNT
           MOVE 'AVERAGE DAILY BALANCE' TO PS-LABEL-2
           MOVE WS-AVG-BAL TO PS-AMOUNT-2
           WRITE STMT-LINE FROM PL-SUMMARY
           MOVE 'DEPOSITS AND OTHER CREDITS' TO PS-LABEL
           MOVE WS-TOT-CREDITS TO PS-AMOUNT
           MOVE 'MINIMUM DAILY BALANCE' TO PS-LABEL-2
           MOVE ACCT-MIN-BAL-CYC TO PS-AMOUNT-2
           WRITE STMT-LINE FROM PL-SUMMARY
           MOVE 'WITHDRAWALS AND OTHER DEBITS' TO PS-LABEL
           MOVE WS-TOT-DEBITS TO PS-AMOUNT
           MOVE 'INTEREST PAID THIS PERIOD' TO PS-LABEL-2
           MOVE WS-INT-PAID TO PS-AMOUNT-2
           WRITE STMT-LINE FROM PL-SUMMARY
           MOVE 'FEES CHARGED THIS PERIOD' TO PS-LABEL
           MOVE WS-FEES TO PS-AMOUNT
           MOVE 'INTEREST PAID YEAR TO DATE' TO PS-LABEL-2
           MOVE ACCT-INT-PAID-YTD TO PS-AMOUNT-2
           WRITE STMT-LINE FROM PL-SUMMARY
           IF WS-WITHHELD > ZERO OR ACCT-WITHHELD-YTD > ZERO
               MOVE 'FEDERAL TAX WITHHELD THIS PERIOD' TO PS-LABEL
               MOVE WS-WITHHELD TO PS-AMOUNT
               MOVE 'FEDERAL TAX WITHHELD YTD' TO PS-LABEL-2
               MOVE ACCT-WITHHELD-YTD TO PS-AMOUNT-2
               WRITE STMT-LINE FROM PL-SUMMARY
           END-IF
           MOVE 'CLOSING BALANCE' TO PS-LABEL
           MOVE WS-CLOSING-BAL TO PS-AMOUNT
           MOVE SPACES TO PS-LABEL-2
           MOVE ZERO TO PS-AMOUNT-2
           WRITE STMT-LINE FROM PL-SUMMARY
           IF PROD-RATE-TABLE-ID NOT = 'NONE' AND NOT = SPACES
               MOVE WS-APYE TO PY-APYE
               MOVE ACCT-INT-RATE TO PY-RATE
               WRITE STMT-LINE FROM PL-APY
           END-IF
           WRITE STMT-LINE FROM PL-BLANK

           PERFORM 3200-PRINT-TRANSACTIONS
           PERFORM 3300-PRINT-FEE-TOTALS
           PERFORM 3400-PRINT-MESSAGES.

      *----------------------------------------------------------------*
       3100-GET-NAMES.
           MOVE SPACES TO WS-OWNER-NAME WS-JOINT-NAME WS-SAVE-CUST
           MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE '** NAME NOT ON FILE **' TO WS-OWNER-NAME
               NOT INVALID KEY
                   PERFORM 3110-FORMAT-NAME
                   MOVE CUST-ADDR-LINE1 TO WS-SV-ADDR1
                   MOVE CUST-ADDR-LINE2 TO WS-SV-ADDR2
                   MOVE CUST-CITY       TO WS-SV-CITY
                   MOVE CUST-STATE      TO WS-SV-STATE
                   MOVE CUST-ZIP        TO WS-SV-ZIP
                   MOVE CUST-SEGMENT    TO WS-SV-SEGMENT
           END-READ
           IF ACCT-JOINT-CUST-ID NOT = SPACES
               MOVE WS-OWNER-NAME TO WS-JOINT-NAME
               MOVE ACCT-JOINT-CUST-ID TO CUST-ID
               READ CUSTOMER-FILE
                   INVALID KEY
                       MOVE SPACES TO WS-JOINT-NAME
                   NOT INVALID KEY
                       PERFORM 3110-FORMAT-NAME
                       MOVE WS-OWNER-NAME TO WS-ADDR-SWAP
                       MOVE WS-JOINT-NAME TO WS-OWNER-NAME
                       MOVE WS-ADDR-SWAP  TO WS-JOINT-NAME
               END-READ
           END-IF.

       3110-FORMAT-NAME.
           MOVE SPACES TO WS-OWNER-NAME
           IF CUST-INDIVIDUAL
               STRING FUNCTION TRIM(CUST-FIRST-NAME) ' '
                      FUNCTION TRIM(CUST-LAST-NAME)
                      DELIMITED BY SIZE INTO WS-OWNER-NAME
           ELSE
               MOVE CUST-LAST-NAME TO WS-OWNER-NAME
           END-IF.

      *----------------------------------------------------------------*
       3200-PRINT-TRANSACTIONS.
           MOVE 'TRANSACTION DETAIL' TO PX-TEXT
           WRITE STMT-LINE FROM PL-TEXT
           WRITE STMT-LINE FROM PL-RULE
           WRITE STMT-LINE FROM PL-TXN-HEAD
           MOVE WS-OPENING-BAL TO WS-RUNNING-BAL
           MOVE ZERO TO PT-DEBIT PT-CREDIT
           MOVE WS-PERIOD-START TO PT-POST PT-EFF
           MOVE SPACES TO PT-CODE
           MOVE 'OPENING BALANCE' TO PT-DESC
           MOVE WS-RUNNING-BAL TO PT-BALANCE
           WRITE STMT-LINE FROM PL-TXN
           PERFORM VARYING WS-TX-IDX FROM 1 BY 1
                   UNTIL WS-TX-IDX > WS-TXN-COUNT
               IF WS-TXN-ACCT(WS-TX-IDX) = ACCT-NUMBER
                  AND WS-TXN-POST-DATE(WS-TX-IDX) >= WS-PERIOD-START
                  AND WS-TXN-POST-DATE(WS-TX-IDX) <= CTL-BUSINESS-DATE
                   MOVE WS-TXN-POST-DATE(WS-TX-IDX) TO PT-POST
                   MOVE WS-TXN-EFF-DATE(WS-TX-IDX)  TO PT-EFF
                   MOVE WS-TXN-CODE(WS-TX-IDX)      TO PT-CODE
                   MOVE WS-TXN-DESC(WS-TX-IDX)      TO PT-DESC
                   MOVE ZERO TO PT-DEBIT PT-CREDIT
                   IF WS-TXN-DRCR(WS-TX-IDX) = 'C'
                       MOVE WS-TXN-AMOUNT(WS-TX-IDX) TO PT-CREDIT
                       ADD WS-TXN-AMOUNT(WS-TX-IDX) TO WS-RUNNING-BAL
                   ELSE
                       MOVE WS-TXN-AMOUNT(WS-TX-IDX) TO PT-DEBIT
                       SUBTRACT WS-TXN-AMOUNT(WS-TX-IDX)
                           FROM WS-RUNNING-BAL
                   END-IF
                   MOVE WS-RUNNING-BAL TO PT-BALANCE
                   WRITE STMT-LINE FROM PL-TXN
               END-IF
           END-PERFORM
           IF WS-CR-COUNT = 0 AND WS-DR-COUNT = 0
               MOVE 'NO ACTIVITY THIS PERIOD' TO PX-TEXT
               WRITE STMT-LINE FROM PL-TEXT
           END-IF
           WRITE STMT-LINE FROM PL-BLANK.

      *----------------------------------------------------------------*
      * REG DD 1030.11(A) - AGGREGATE OD AND RETURNED ITEM FEES        *
      *----------------------------------------------------------------*
       3300-PRINT-FEE-TOTALS.
           IF ACCT-CATEGORY NOT = 'DD'
               EXIT PARAGRAPH
           END-IF
           MOVE 'TOTAL OVERDRAFT AND RETURNED ITEM FEES' TO PX-TEXT
           WRITE STMT-LINE FROM PL-TEXT
           WRITE STMT-LINE FROM PL-RULE
           MOVE 'TOTAL OVERDRAFT FEES - THIS PERIOD' TO PS-LABEL
           MOVE WS-OD-FEES-PER TO PS-AMOUNT
           MOVE 'YEAR TO DATE' TO PS-LABEL-2
           MOVE WS-OD-FEES-YTD TO PS-AMOUNT-2
           WRITE STMT-LINE FROM PL-SUMMARY
           MOVE 'TOTAL RETURNED ITEM FEES - THIS PERIOD' TO PS-LABEL
           MOVE WS-NSF-FEES-PER TO PS-AMOUNT
           MOVE 'YEAR TO DATE' TO PS-LABEL-2
           MOVE WS-NSF-FEES-YTD TO PS-AMOUNT-2
           WRITE STMT-LINE FROM PL-SUMMARY
           WRITE STMT-LINE FROM PL-BLANK.

      *----------------------------------------------------------------*
       3400-PRINT-MESSAGES.
           IF ACCT-FEE-WAIVER-CODE NOT = SPACES
               AND ACCT-FEE-WAIVER-CODE NOT = 'NF'
               MOVE SPACES TO PX-TEXT
               STRING 'YOUR MONTHLY MAINTENANCE FEE WAS WAIVED THIS '
                      'PERIOD (CODE ' ACCT-FEE-WAIVER-CODE ').'
                      DELIMITED BY SIZE INTO PX-TEXT
               WRITE STMT-LINE FROM PL-TEXT
           END-IF
           IF ACCT-PRODUCT-CODE NOT = 'HYSV'
              AND WS-SV-SEGMENT = 'RT' OR 'MA'
               MOVE WS-MKT-MESSAGE-1 TO PX-TEXT
               WRITE STMT-LINE FROM PL-TEXT
               MOVE WS-MKT-MESSAGE-2 TO PX-TEXT
               WRITE STMT-LINE FROM PL-TEXT
           END-IF
           MOVE SPACES TO PX-TEXT
           STRING 'IN CASE OF ERRORS OR QUESTIONS ABOUT YOUR '
                  'ELECTRONIC TRANSFERS CALL 1-800-555-0100 OR '
                  'WRITE TO US (REG E).'
                  DELIMITED BY SIZE INTO PX-TEXT
           WRITE STMT-LINE FROM PL-TEXT
           WRITE STMT-LINE FROM PL-BLANK.

      *----------------------------------------------------------------*
       2900-RESET-CYCLE.
           MOVE ZERO TO ACCT-BAL-ACCUM-CYC ACCT-DAYS-IN-CYCLE
                        ACCT-DR-COUNT-CYC ACCT-CR-COUNT-CYC
           MOVE ACCT-LEDGER-BAL TO ACCT-MIN-BAL-CYC
           MOVE CTL-BUSINESS-DATE TO ACCT-LAST-STMT-DATE
           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'STMTGEN' TO ABND-PROGRAM
               MOVE '2900-RESET-CYCLE' TO ABND-PARAGRAPH
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'STATEMENTS PRODUCED' TO RLT-LABEL
           MOVE WS-STMT-COUNT TO RLT-COUNT
           WRITE REPORT-LINE FROM RL-TOTAL
           MOVE 'SUPPRESSED (ZERO / NO ACTIVITY)' TO RLT-LABEL
           MOVE WS-SUPPRESS-COUNT TO RLT-COUNT
           WRITE REPORT-LINE FROM RL-TOTAL
           MOVE 'NOT DUE THIS RUN' TO RLT-LABEL
           MOVE WS-NOTDUE-COUNT TO RLT-COUNT
           WRITE REPORT-LINE FROM RL-TOTAL
           CLOSE ACCOUNT-FILE PRODUCT-FILE CUSTOMER-FILE
                 STATEMENT-FILE REPORT-FILE
           DISPLAY 'STMTGEN READ=' WS-READ-COUNT
                   ' STATEMENTS=' WS-STMT-COUNT
                   ' SUPPRESSED=' WS-SUPPRESS-COUNT
                   ' NOTDUE=' WS-NOTDUE-COUNT
           IF WS-TXN-OVERFLOW = 'Y'
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND-OPEN.
           MOVE 'STMTGEN' TO ABND-PROGRAM
           MOVE '1000-INITIALIZE' TO ABND-PARAGRAPH
           MOVE 'FILE OPEN FAILED' TO ABND-MESSAGE
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
