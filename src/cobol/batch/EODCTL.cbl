      *================================================================*
      * PROGRAM  : EODCTL                                              *
      * TYPE     : BATCH - EOD CYCLE CONTROLLER (STEPS 01 AND 21)      *
      * PURPOSE  : OPENS AND CLOSES THE END-OF-DAY CYCLE.              *
      *   PARM START                                                   *
      *     - CYCLE STATUS MUST BE O (ONLINE). SETS E (EOD RUNNING).   *
      *       IF ALREADY E THE RUN IS TREATED AS A RESTART (RC 04).    *
      *     - MONTH-END  = BUSINESS DATE IS THE LAST BUSINESS DAY OF   *
      *                    THE MONTH (DATEUTIL LBDM)                   *
      *     - QUARTER-END = MONTH-END IN MAR / JUN / SEP / DEC         *
      *     - YEAR-END   = MONTH-END IN DECEMBER                       *
      *     - NEXT BUSINESS DATE = BUSINESS DATE + 1 BUSINESS DAY      *
      *   PARM END                                                     *
      *     - CYCLE STATUS MUST BE E                                   *
      *     - APPENDS TODAY'S TRANPOST TO TRANHIST                     *
      *     - RESETS THE DAILY ACCTMST FIELDS (MEMO DR/CR, DAILY       *
      *       WITHDRAWAL USED, CASH IN/OUT, OD FEES TODAY) AND SETS    *
      *       PREVIOUS-DAY LEDGER = LEDGER                             *
      *     - AT MONTH-END RESETS THE REG D CONVENIENT WITHDRAWAL      *
      *       COUNT; MORE THAN 6 IN THE CYCLE ADDS 1 TO THE 12-MONTH   *
      *       VIOLATION COUNT                                          *
      *     - ROLLS PREV = BUSINESS, BUSINESS = NEXT, NEXT = NEW       *
      *       BUSINESS + 1 BUSINESS DAY; RECOMPUTES ME/QE/YE FOR THE   *
      *       NEW DATE; STATUS O; INCREMENTS CTL-LAST-TRAN-SEQ         *
      *     - EOD SUMMARY: TRANPOST BY SOURCE PROGRAM, DR/CR TOTALS,   *
      *       ALERTS BY TYPE                                           *
      * INPUT    : BANKCTL, TRANPOST, ALERTS (END)                     *
      * OUTPUT   : BANKCTL, TRANHIST (EXTEND), ACCTMST (I-O), EODRPT   *
      * RETURN   : 00 OK, 04 RESTART, 12 WRONG CYCLE STATUS / BAD PARM *
      *            16 FILE ERROR                                       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-02-14 RJT  ORIGINAL                                       *
      * 1994-12-01 RJT  BUSINESS DAY CALENDAR VIA DATEUTIL             *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-09-10 BHS  RESET CASH IN/OUT TODAY (CTR AGGREGATION)      *
      * 2009-11-17 DPS  RESET OD FEES TODAY (REG E FEE CAP)            *
      * 2012-08-14 DPS  MONTH-END = LAST BUSINESS DAY (WAS CALENDAR)   *
      * 2015-04-20 DPS  INC25118 START ON A RUNNING CYCLE NOW RC 4     *
      *                 RESTART INSTEAD OF RC 12                       *
      * 2020-05-01 AKW  REG D - FED REMOVED 6 LIMIT, BANK POLICY KEPT. *
      *                 MONTH-END RESET OF REG D COUNT MOVED HERE      *
      * 2023-02-06 AKW  CHG2140 EOD SUMMARY - ALERTS BY TYPE           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EODCTL.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANKCTL-FILE ASSIGN TO "BANKCTL"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CTL-STATUS.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-STATUS.
           SELECT OPTIONAL TRANHIST-FILE ASSIGN TO "TRANHIST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-HIST-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT OPTIONAL REPORT-FILE ASSIGN TO "EODRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  BANKCTL-FILE.
       01  BANKCTL-FILE-REC                PIC X(121).
       FD  TRANPOST-FILE.
       01  TRANPOST-REC                    PIC X(281).
       FD  TRANHIST-FILE.
       01  TRANHIST-REC                    PIC X(281).
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME                 PIC X(08) VALUE 'EODCTL'.
       01  WS-PARM                         PIC X(10).
           88  WS-PARM-START                         VALUE 'START'.
           88  WS-PARM-END                           VALUE 'END'.
       01  WS-FILE-STATUSES.
           05  WS-CTL-STATUS               PIC X(02).
           05  WS-TRAN-STATUS              PIC X(02).
           05  WS-HIST-STATUS              PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01).
           88  WS-EOF                                VALUE 'Y'.
       01  WS-RETURN-CODE                  PIC 9(02) VALUE 0.

      *    BANK POLICY - CONVENIENT WITHDRAWALS PER STATEMENT CYCLE
       01  WS-REGD-POLICY-LIMIT            PIC 9(03) VALUE 6.

       01  WS-DATE-WORK.
           05  WS-WORK-DATE                PIC 9(08).
           05  WS-WORK-DATE-R REDEFINES WS-WORK-DATE.
               10  WS-WORK-CCYY            PIC 9(04).
               10  WS-WORK-MM              PIC 9(02).
               10  WS-WORK-DD              PIC 9(02).
           05  WS-ME-FLAG                  PIC X(01).
           05  WS-QE-FLAG                  PIC X(01).
           05  WS-YE-FLAG                  PIC X(01).
           05  WS-OLD-BUS-DATE             PIC 9(08).

       01  WS-COUNTERS.
           05  WS-POST-COUNT               PIC 9(07) VALUE 0.
           05  WS-DR-COUNT                 PIC 9(07) VALUE 0.
           05  WS-CR-COUNT                 PIC 9(07) VALUE 0.
           05  WS-ACCT-COUNT               PIC 9(07) VALUE 0.
           05  WS-REGD-RESET-COUNT         PIC 9(07) VALUE 0.
           05  WS-REGD-VIOL-COUNT          PIC 9(07) VALUE 0.
           05  WS-ALERT-TODAY              PIC 9(07) VALUE 0.
       01  WS-TOTALS.
           05  WS-DR-TOTAL                 PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-CR-TOTAL                 PIC S9(15)V99 COMP-3
                                           VALUE 0.

       01  WS-SOURCE-TABLE.
           05  WS-SRC-COUNT                PIC 9(02) COMP VALUE 0.
           05  WS-SRC-ENTRY OCCURS 40 TIMES.
               10  WS-SRC-PGM              PIC X(08).
               10  WS-SRC-ITEMS            PIC 9(07).
               10  WS-SRC-DR               PIC S9(15)V99 COMP-3.
               10  WS-SRC-CR               PIC S9(15)V99 COMP-3.
       01  WS-ALERT-TYPE-TABLE.
           05  WS-ATY-COUNT                PIC 9(02) COMP VALUE 0.
           05  WS-ATY-ENTRY OCCURS 40 TIMES.
               10  WS-ATY-TYPE             PIC X(04).
               10  WS-ATY-ITEMS            PIC 9(07).
       01  WS-IX                           PIC 9(02) COMP.
       01  WS-FOUND                        PIC X(01).

       01  WS-RPT-TITLE.
           05  FILLER PIC X(09) VALUE 'EODCTL   '.
           05  WS-T-TEXT                   PIC X(50).
           05  FILLER PIC X(15) VALUE 'BUSINESS DATE '.
           05  WS-T-DATE                   PIC 9(08).
           05  FILLER                      PIC X(50) VALUE SPACES.
       01  WS-RPT-KV.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-KV-LABEL                 PIC X(38).
           05  WS-KV-VALUE                 PIC X(40).
           05  FILLER                      PIC X(52) VALUE SPACES.
       01  WS-RPT-SRC.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  WS-S-PGM                    PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-S-ITEMS                  PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-S-DR                     PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-S-CR                     PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(71) VALUE SPACES.
       01  WS-NUM-ED                       PIC ZZZ,ZZZ,ZZ9.
       01  WS-AMT-ED                       PIC ZZZ,ZZZ,ZZZ,ZZ9.99.

       COPY BANKCTL.
       COPY TRANREC.
       COPY DATEPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-PARM FROM COMMAND-LINE
           MOVE FUNCTION UPPER-CASE(WS-PARM) TO WS-PARM
           PERFORM 1000-READ-CONTROL
           PERFORM 1100-OPEN-REPORT
           EVALUATE TRUE
               WHEN WS-PARM-START
                   PERFORM 2000-START-OF-CYCLE
               WHEN WS-PARM-END
                   PERFORM 3000-END-OF-CYCLE
               WHEN OTHER
                   DISPLAY 'EODCTL   INVALID PARM [' WS-PARM
                           '] - EXPECTED START OR END'
                   MOVE 12 TO WS-RETURN-CODE
           END-EVALUATE
           CLOSE REPORT-FILE
           MOVE WS-RETURN-CODE TO RETURN-CODE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-READ-CONTROL.
           OPEN INPUT BANKCTL-FILE
           IF WS-CTL-STATUS NOT = '00'
               MOVE 'BANKCTL' TO ABND-FILE-NAME
               MOVE WS-CTL-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           READ BANKCTL-FILE INTO BANK-CONTROL-RECORD
           IF WS-CTL-STATUS NOT = '00'
               MOVE 'BANKCTL' TO ABND-FILE-NAME
               MOVE WS-CTL-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           CLOSE BANKCTL-FILE.

       1100-OPEN-REPORT.
           OPEN EXTEND REPORT-FILE
           IF WS-RPT-STATUS NOT = '00' AND WS-RPT-STATUS NOT = '05'
               MOVE 'EODRPT' TO ABND-FILE-NAME
               MOVE WS-RPT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF.

       1200-WRITE-CONTROL.
           OPEN OUTPUT BANKCTL-FILE
           IF WS-CTL-STATUS NOT = '00'
               MOVE 'BANKCTL' TO ABND-FILE-NAME
               MOVE WS-CTL-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           WRITE BANKCTL-FILE-REC FROM BANK-CONTROL-RECORD
           IF WS-CTL-STATUS NOT = '00'
               MOVE 'BANKCTL' TO ABND-FILE-NAME
               MOVE WS-CTL-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           CLOSE BANKCTL-FILE.

      *================================================================*
      * START OF CYCLE                                                 *
      *================================================================*
       2000-START-OF-CYCLE.
           MOVE 'END-OF-DAY CYCLE START' TO WS-T-TEXT
           MOVE CTL-BUSINESS-DATE TO WS-T-DATE
           WRITE REPORT-LINE FROM WS-RPT-TITLE
           EVALUATE TRUE
               WHEN CTL-ONLINE-OPEN
                   CONTINUE
               WHEN CTL-EOD-RUNNING
                   DISPLAY 'EODCTL   CYCLE ALREADY IN EOD (STATUS E) - '
                           'RESTART ASSUMED'
                   MOVE 'STATUS E - RESTART ASSUMED' TO WS-KV-VALUE
                   MOVE 'CYCLE STATUS' TO WS-KV-LABEL
                   WRITE REPORT-LINE FROM WS-RPT-KV
                   MOVE 04 TO WS-RETURN-CODE
               WHEN OTHER
                   DISPLAY 'EODCTL   CYCLE STATUS ' CTL-CYCLE-STATUS
                           ' - START NOT ALLOWED'
                   MOVE 'START REJECTED - STATUS NOT O' TO WS-KV-VALUE
                   MOVE 'CYCLE STATUS' TO WS-KV-LABEL
                   WRITE REPORT-LINE FROM WS-RPT-KV
                   MOVE 12 TO WS-RETURN-CODE
                   EXIT PARAGRAPH
           END-EVALUATE

           MOVE CTL-BUSINESS-DATE TO WS-WORK-DATE
           PERFORM 8000-PERIOD-END-FLAGS
           MOVE WS-ME-FLAG TO CTL-MONTH-END-FLAG
           MOVE WS-QE-FLAG TO CTL-QUARTER-END-FLAG
           MOVE WS-YE-FLAG TO CTL-YEAR-END-FLAG
           INITIALIZE DATE-PARMS
           SET DTU-ADD-BUS-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           MOVE 1 TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO CTL-NEXT-BUS-DATE
           SET CTL-EOD-RUNNING TO TRUE
           PERFORM 1200-WRITE-CONTROL

           DISPLAY 'EODCTL   START  BUSINESS=' CTL-BUSINESS-DATE
                   ' NEXT=' CTL-NEXT-BUS-DATE
                   ' ME=' CTL-MONTH-END-FLAG
                   ' QE=' CTL-QUARTER-END-FLAG
                   ' YE=' CTL-YEAR-END-FLAG
                   ' STATUS=' CTL-CYCLE-STATUS
           MOVE 'NEXT BUSINESS DATE' TO WS-KV-LABEL
           MOVE CTL-NEXT-BUS-DATE TO WS-KV-VALUE
           WRITE REPORT-LINE FROM WS-RPT-KV
           MOVE 'MONTH-END / QUARTER-END / YEAR-END' TO WS-KV-LABEL
           MOVE SPACES TO WS-KV-VALUE
           STRING CTL-MONTH-END-FLAG ' / ' CTL-QUARTER-END-FLAG
                  ' / ' CTL-YEAR-END-FLAG DELIMITED BY SIZE
             INTO WS-KV-VALUE
           END-STRING
           WRITE REPORT-LINE FROM WS-RPT-KV
           MOVE 'CYCLE STATUS' TO WS-KV-LABEL
           MOVE 'E - EOD RUNNING' TO WS-KV-VALUE
           WRITE REPORT-LINE FROM WS-RPT-KV.

      *================================================================*
      * END OF CYCLE                                                   *
      *================================================================*
       3000-END-OF-CYCLE.
           MOVE 'END-OF-DAY CYCLE END - SUMMARY' TO WS-T-TEXT
           MOVE CTL-BUSINESS-DATE TO WS-T-DATE
           WRITE REPORT-LINE FROM WS-RPT-TITLE
           IF NOT CTL-EOD-RUNNING
               DISPLAY 'EODCTL   CYCLE STATUS ' CTL-CYCLE-STATUS
                       ' - END REQUIRES STATUS E (RUN START FIRST)'
               MOVE 'CYCLE STATUS' TO WS-KV-LABEL
               MOVE 'END REJECTED - STATUS NOT E' TO WS-KV-VALUE
               WRITE REPORT-LINE FROM WS-RPT-KV
               MOVE 12 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           PERFORM 3100-APPEND-HISTORY
           PERFORM 3200-RESET-ACCOUNTS
           PERFORM 3300-COUNT-ALERTS
           PERFORM 3400-ROLL-DATES
           PERFORM 3500-SUMMARY-REPORT.

      *----------------------------------------------------------------*
       3100-APPEND-HISTORY.
           OPEN INPUT TRANPOST-FILE
           IF WS-TRAN-STATUS NOT = '00' AND WS-TRAN-STATUS NOT = '05'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-TRAN-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN EXTEND TRANHIST-FILE
           IF WS-HIST-STATUS NOT = '00' AND WS-HIST-STATUS NOT = '05'
               MOVE 'TRANHIST' TO ABND-FILE-NAME
               MOVE WS-HIST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ TRANPOST-FILE INTO TRANSACTION-RECORD
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       WRITE TRANHIST-REC FROM TRANSACTION-RECORD
                       IF WS-HIST-STATUS NOT = '00'
                           MOVE 'TRANHIST' TO ABND-FILE-NAME
                           MOVE WS-HIST-STATUS TO ABND-FILE-STATUS
                           PERFORM 9900-ABEND
                       END-IF
                       PERFORM 3110-ACCUMULATE
               END-READ
           END-PERFORM
           CLOSE TRANPOST-FILE TRANHIST-FILE.

       3110-ACCUMULATE.
           ADD 1 TO WS-POST-COUNT
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-SRC-COUNT OR WS-FOUND = 'Y'
               IF WS-SRC-PGM(WS-IX) = TRAN-SOURCE-PGM
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'Y'
               SUBTRACT 1 FROM WS-IX
           ELSE
               IF WS-SRC-COUNT < 40
                   ADD 1 TO WS-SRC-COUNT
               END-IF
               MOVE WS-SRC-COUNT TO WS-IX
               MOVE TRAN-SOURCE-PGM TO WS-SRC-PGM(WS-IX)
               MOVE 0 TO WS-SRC-ITEMS(WS-IX) WS-SRC-DR(WS-IX)
                         WS-SRC-CR(WS-IX)
           END-IF
           ADD 1 TO WS-SRC-ITEMS(WS-IX)
           IF TRAN-IS-DEBIT
               ADD 1 TO WS-DR-COUNT
               ADD TRAN-AMOUNT TO WS-DR-TOTAL WS-SRC-DR(WS-IX)
           ELSE
               ADD 1 TO WS-CR-COUNT
               ADD TRAN-AMOUNT TO WS-CR-TOTAL WS-SRC-CR(WS-IX)
           END-IF.

      *----------------------------------------------------------------*
      * RESET DAILY FIELDS ON EVERY ACCOUNT. AT MONTH-END ALSO CLOSE   *
      * THE REG D COUNTING PERIOD.                                     *
      *----------------------------------------------------------------*
       3200-RESET-ACCOUNTS.
           OPEN I-O ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT RECORD
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       PERFORM 3210-RESET-ONE-ACCOUNT
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE.

       3210-RESET-ONE-ACCOUNT.
           ADD 1 TO WS-ACCT-COUNT
           MOVE ZERO TO ACCT-MEMO-DR-TODAY ACCT-MEMO-CR-TODAY
                        ACCT-DAILY-WD-USED
                        ACCT-CASH-IN-TODAY ACCT-CASH-OUT-TODAY
                        ACCT-OD-FEES-TODAY
           MOVE ACCT-LEDGER-BAL TO ACCT-PREV-DAY-LEDGER-BAL
           IF CTL-IS-MONTH-END
               IF ACCT-REGD-WD-COUNT > WS-REGD-POLICY-LIMIT
                   IF ACCT-REGD-VIOLATIONS-12M < 99
                       ADD 1 TO ACCT-REGD-VIOLATIONS-12M
                   END-IF
                   ADD 1 TO WS-REGD-VIOL-COUNT
               END-IF
               IF ACCT-REGD-WD-COUNT > 0
                   ADD 1 TO WS-REGD-RESET-COUNT
               END-IF
               MOVE 0 TO ACCT-REGD-WD-COUNT
           END-IF
           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF.

      *----------------------------------------------------------------*
       3300-COUNT-ALERTS.
           OPEN INPUT ALERT-FILE
           IF WS-ALRT-STATUS NOT = '00' AND WS-ALRT-STATUS NOT = '05'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ ALERT-FILE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       IF ALRT-DATE = CTL-BUSINESS-DATE
                           PERFORM 3310-COUNT-TYPE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ALERT-FILE.

       3310-COUNT-TYPE.
           ADD 1 TO WS-ALERT-TODAY
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-ATY-COUNT OR WS-FOUND = 'Y'
               IF WS-ATY-TYPE(WS-IX) = ALRT-TYPE
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'Y'
               SUBTRACT 1 FROM WS-IX
           ELSE
               IF WS-ATY-COUNT < 40
                   ADD 1 TO WS-ATY-COUNT
               END-IF
               MOVE WS-ATY-COUNT TO WS-IX
               MOVE ALRT-TYPE TO WS-ATY-TYPE(WS-IX)
               MOVE 0 TO WS-ATY-ITEMS(WS-IX)
           END-IF
           ADD 1 TO WS-ATY-ITEMS(WS-IX).

      *----------------------------------------------------------------*
       3400-ROLL-DATES.
           MOVE CTL-BUSINESS-DATE TO WS-OLD-BUS-DATE
           MOVE CTL-BUSINESS-DATE TO CTL-PREV-BUS-DATE
           MOVE CTL-NEXT-BUS-DATE TO CTL-BUSINESS-DATE
           INITIALIZE DATE-PARMS
           SET DTU-ADD-BUS-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           MOVE 1 TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO CTL-NEXT-BUS-DATE
           MOVE CTL-BUSINESS-DATE TO WS-WORK-DATE
           PERFORM 8000-PERIOD-END-FLAGS
           MOVE WS-ME-FLAG TO CTL-MONTH-END-FLAG
           MOVE WS-QE-FLAG TO CTL-QUARTER-END-FLAG
           MOVE WS-YE-FLAG TO CTL-YEAR-END-FLAG
           ADD 1 TO CTL-LAST-TRAN-SEQ
           SET CTL-ONLINE-OPEN TO TRUE
           PERFORM 1200-WRITE-CONTROL
           DISPLAY 'EODCTL   END    PREV=' CTL-PREV-BUS-DATE
                   ' BUSINESS=' CTL-BUSINESS-DATE
                   ' NEXT=' CTL-NEXT-BUS-DATE
                   ' ME=' CTL-MONTH-END-FLAG
                   ' STATUS=' CTL-CYCLE-STATUS
                   ' TRANHIST+=' WS-POST-COUNT
                   ' ACCTS=' WS-ACCT-COUNT.

      *----------------------------------------------------------------*
       3500-SUMMARY-REPORT.
           MOVE 'TRANPOST APPENDED TO TRANHIST' TO WS-KV-LABEL
           MOVE WS-POST-COUNT TO WS-NUM-ED
           MOVE WS-NUM-ED TO WS-KV-VALUE
           WRITE REPORT-LINE FROM WS-RPT-KV
           MOVE 'DEBITS  - COUNT / AMOUNT' TO WS-KV-LABEL
           MOVE WS-DR-COUNT TO WS-NUM-ED
           MOVE WS-DR-TOTAL TO WS-AMT-ED
           MOVE SPACES TO WS-KV-VALUE
           STRING WS-NUM-ED ' / ' WS-AMT-ED DELIMITED BY SIZE
             INTO WS-KV-VALUE
           END-STRING
           WRITE REPORT-LINE FROM WS-RPT-KV
           MOVE 'CREDITS - COUNT / AMOUNT' TO WS-KV-LABEL
           MOVE WS-CR-COUNT TO WS-NUM-ED
           MOVE WS-CR-TOTAL TO WS-AMT-ED
           MOVE SPACES TO WS-KV-VALUE
           STRING WS-NUM-ED ' / ' WS-AMT-ED DELIMITED BY SIZE
             INTO WS-KV-VALUE
           END-STRING
           WRITE REPORT-LINE FROM WS-RPT-KV
           MOVE '  SOURCE      ITEMS              DEBITS'
             TO REPORT-LINE
           MOVE '             CREDITS' TO REPORT-LINE(41:20)
           WRITE REPORT-LINE
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-SRC-COUNT
               MOVE WS-SRC-PGM(WS-IX)   TO WS-S-PGM
               MOVE WS-SRC-ITEMS(WS-IX) TO WS-S-ITEMS
               MOVE WS-SRC-DR(WS-IX)    TO WS-S-DR
               MOVE WS-SRC-CR(WS-IX)    TO WS-S-CR
               WRITE REPORT-LINE FROM WS-RPT-SRC
           END-PERFORM
           MOVE 'ACCOUNTS RESET' TO WS-KV-LABEL
           MOVE WS-ACCT-COUNT TO WS-NUM-ED
           MOVE WS-NUM-ED TO WS-KV-VALUE
           WRITE REPORT-LINE FROM WS-RPT-KV
           MOVE 'REG D COUNTS RESET (MONTH-END)' TO WS-KV-LABEL
           MOVE WS-REGD-RESET-COUNT TO WS-NUM-ED
           MOVE WS-NUM-ED TO WS-KV-VALUE
           WRITE REPORT-LINE FROM WS-RPT-KV
           MOVE 'REG D VIOLATIONS RECORDED (> 6)' TO WS-KV-LABEL
           MOVE WS-REGD-VIOL-COUNT TO WS-NUM-ED
           MOVE WS-NUM-ED TO WS-KV-VALUE
           WRITE REPORT-LINE FROM WS-RPT-KV
           MOVE 'ALERTS RAISED TODAY' TO WS-KV-LABEL
           MOVE WS-ALERT-TODAY TO WS-NUM-ED
           MOVE WS-NUM-ED TO WS-KV-VALUE
           WRITE REPORT-LINE FROM WS-RPT-KV
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-ATY-COUNT
               MOVE SPACES TO WS-KV-LABEL
               STRING '  ALERT TYPE ' WS-ATY-TYPE(WS-IX)
                      DELIMITED BY SIZE INTO WS-KV-LABEL
               END-STRING
               MOVE WS-ATY-ITEMS(WS-IX) TO WS-NUM-ED
               MOVE WS-NUM-ED TO WS-KV-VALUE
               WRITE REPORT-LINE FROM WS-RPT-KV
           END-PERFORM
           MOVE 'NEW BUSINESS DATE / NEXT' TO WS-KV-LABEL
           MOVE SPACES TO WS-KV-VALUE
           STRING CTL-BUSINESS-DATE ' / ' CTL-NEXT-BUS-DATE
                  DELIMITED BY SIZE INTO WS-KV-VALUE
           END-STRING
           WRITE REPORT-LINE FROM WS-RPT-KV
           MOVE 'CYCLE STATUS' TO WS-KV-LABEL
           MOVE 'O - ONLINE OPEN' TO WS-KV-VALUE
           WRITE REPORT-LINE FROM WS-RPT-KV.

      *----------------------------------------------------------------*
      * PERIOD-END FLAGS FOR WS-WORK-DATE                              *
      *----------------------------------------------------------------*
       8000-PERIOD-END-FLAGS.
           MOVE 'N' TO WS-ME-FLAG WS-QE-FLAG WS-YE-FLAG
           INITIALIZE DATE-PARMS
           SET DTU-LAST-BUS-DAY-MONTH TO TRUE
           MOVE WS-WORK-DATE TO DTU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DTU-RESULT-DATE = WS-WORK-DATE
               MOVE 'Y' TO WS-ME-FLAG
               IF WS-WORK-MM = 03 OR 06 OR 09 OR 12
                   MOVE 'Y' TO WS-QE-FLAG
               END-IF
               IF WS-WORK-MM = 12
                   MOVE 'Y' TO WS-YE-FLAG
               END-IF
           END-IF.

       9900-ABEND.
           MOVE WS-PROGRAM-NAME TO ABND-PROGRAM
           MOVE 'FILE I/O' TO ABND-PARAGRAPH
           MOVE 'UNEXPECTED FILE STATUS' TO ABND-MESSAGE
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
