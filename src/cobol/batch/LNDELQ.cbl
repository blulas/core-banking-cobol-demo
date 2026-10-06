      *================================================================*
      * PROGRAM  : LNDELQ                                              *
      * TYPE     : BATCH - EOD STEP 15 (AFTER LNBILL)                  *
      * PURPOSE  : LOAN DELINQUENCY AGING.                             *
      *            - ROLLS THE NEXT DUE DATE FORWARD ONCE IT HAS       *
      *              PASSED (THE UNPAID INSTALLMENT STAYS ON THE DUE   *
      *              TABLE AS PAST DUE)                                *
      *            - DAYS PAST DUE = BUSINESS DATE - OLDEST UNPAID     *
      *              INSTALLMENT DUE DATE; BUCKET AND STATUS FROM      *
      *              DELQBKT                                           *
      *            - LATE FEES (DELQBKT LFEE) RECORDED IN LOAN FEES    *
      *              OUTSTANDING - NO DEPOSIT TRANSACTION              *
      *            - 30/60/90 COUNTERS (24 MONTH WINDOW)               *
      *            - NON-ACCRUAL (COMMERCIAL 90+): ACCRUED INTEREST    *
      *              REVERSED TO MEMO                                  *
      *            - CHARGE-OFF (CONSUMER 120+ CLOSED-END / 180+ OPEN- *
      *              END): PRINCIPAL CHARGED OFF, INTEREST REVERSED,   *
      *              FEES WAIVED                                       *
      *            - LNDQ ALERTS AT 60 / 90 DPD, NON-ACCRUAL, CHARGE-  *
      *              OFF                                               *
      * INPUT    : BANKCTL (VIA GETCTL)                                *
      * I-O      : LOANMST                                             *
      * OUTPUT   : ALERTS (EXTEND), LNDQRPT                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - COMMERCIAL NON-ACCRUAL LIST         *
      * 1999-06-30 MKL  FFIEC CHARGE-OFF POLICY - CONSUMER LOANS       *
      * 2001-10-26 BHS  ALERTS FILE                                    *
      * 2006-02-13 BHS  DUE TABLE AGING - CHG0412                      *
      * 2009-08-03 DPS  30/60/90 COUNTERS, 24 MONTH WINDOW             *
      * 2012-04-30 DPS  NON-ACCRUAL INTEREST REVERSAL TO MEMO CHG1187  *
      * 2016-04-25 AKW  LPER LATE FEES VIA DELQBKT                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNDELQ.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO "LOANMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOAN-NUMBER
               ALTERNATE RECORD KEY IS LOAN-BORROWER-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-LOAN-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALERT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "LNDQRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
       COPY LOANREC.
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-LOAN-STATUS                  PIC X(02).
       01  WS-ALERT-STATUS                 PIC X(02).
       01  WS-RPT-STATUS                   PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTER-WINDOW-DAYS          PIC 9(04) VALUE 730.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-AGED-COUNT               PIC 9(07) VALUE 0.
           05  WS-DELQ-COUNT               PIC 9(07) VALUE 0.
           05  WS-FEE-COUNT                PIC 9(07) VALUE 0.
           05  WS-NONACCR-COUNT            PIC 9(07) VALUE 0.
           05  WS-CHGOFF-COUNT             PIC 9(07) VALUE 0.
           05  WS-ALERT-COUNT              PIC 9(07) VALUE 0.
       01  WS-TOTALS.
           05  WS-TOT-FEES                 PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-TOT-CHGOFF               PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-TOT-PAST-DUE             PIC S9(13)V99 COMP-3 VALUE 0.
       01  WS-WORK.
           05  WS-I                        PIC 9(02).
           05  WS-MONTHS                   PIC 9(02).
           05  WS-PRIOR-DPD                PIC 9(04).
           05  WS-PRIOR-STATUS             PIC X(01).
           05  WS-DAYS                     PIC S9(07).
           05  WS-FEES-TODAY               PIC S9(07)V99 COMP-3.
           05  WS-ACCR-CENTS               PIC S9(09)V99 COMP-3.
           05  WS-PAST-DUE-TOTAL           PIC S9(13)V99 COMP-3.
           05  WS-ACTION                   PIC X(30).
           05  WS-ALERT-DESC               PIC X(80).
           05  WS-DPD-ED                   PIC ZZZ9.
           05  WS-AMT-ED                   PIC Z,ZZZ,ZZZ,ZZ9.99.

       01  WS-HDR-1.
           05  FILLER PIC X(40) VALUE
               'LNDELQ   CBS LOAN DELINQUENCY AGING     '.
           05  FILLER PIC X(16) VALUE '  BUSINESS DATE '.
           05  WS-HDR-DATE                 PIC 9(08).
       01  WS-HDR-2.
           05  FILLER PIC X(45) VALUE
               'LOAN NUMBER  PROD ST>ST  DPD BUCKET      PAST'.
           05  FILLER PIC X(45) VALUE
               ' DUE AMT    FEES TODAY   FEES OUTST  30 60 90 '.
           05  FILLER PIC X(30) VALUE 'ACTION'.
       01  WS-DTL.
           05  WS-D-LOAN                   PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-PROD                   PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-OLD-ST                 PIC X(01).
           05  FILLER                      PIC X(01) VALUE '>'.
           05  WS-D-NEW-ST                 PIC X(01).
           05  WS-D-DPD                    PIC ZZZZ9.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-BUCKET                 PIC X(10).
           05  WS-D-PAST                   PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-D-FEES                   PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-D-OUTST                  PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-T30                    PIC Z9.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-T60                    PIC Z9.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-T90                    PIC Z9.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-ACTION                 PIC X(30).
       01  WS-TRL-1.
           05  FILLER PIC X(19) VALUE 'LNDELQ TOTALS READ='.
           05  WS-T-READ                   PIC ZZZ,ZZ9.
           05  FILLER PIC X(06) VALUE ' AGED='.
           05  WS-T-AGED                   PIC ZZZ,ZZ9.
           05  FILLER PIC X(12) VALUE ' DELINQUENT='.
           05  WS-T-DELQ                   PIC ZZZ,ZZ9.
           05  FILLER PIC X(11) VALUE ' LATE FEES='.
           05  WS-T-FEES                   PIC ZZZ,ZZ9.
           05  FILLER PIC X(02) VALUE ' $'.
           05  WS-T-FEE-AMT                PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-TRL-2.
           05  FILLER PIC X(19) VALUE '       NON-ACCRUAL='.
           05  WS-T-NACR                   PIC ZZZ,ZZ9.
           05  FILLER PIC X(13) VALUE ' CHARGE-OFFS='.
           05  WS-T-CHGO                   PIC ZZZ,ZZ9.
           05  FILLER PIC X(02) VALUE ' $'.
           05  WS-T-CHGO-AMT               PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER PIC X(08) VALUE ' ALERTS='.
           05  WS-T-ALERTS                 PIC ZZZ,ZZ9.

       COPY BANKCTL.
       COPY DATEPARM.
       COPY DLQPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM UNTIL WS-EOF
               READ LOAN-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2000-PROCESS-LOAN
               END-READ
           END-PERFORM
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN I-O LOAN-FILE
           IF WS-LOAN-STATUS NOT = '00'
               MOVE 'LOANMST' TO ABND-FILE-NAME
               MOVE WS-LOAN-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - LOAN MASTER' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN EXTEND ALERT-FILE
           IF WS-ALERT-STATUS NOT = '00' AND WS-ALERT-STATUS NOT = '05'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALERT-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - ALERTS' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
               MOVE 'LNDQRPT' TO ABND-FILE-NAME
               MOVE WS-RPT-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - REPORT' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           MOVE CTL-BUSINESS-DATE TO WS-HDR-DATE
           WRITE REPORT-LINE FROM WS-HDR-1
           WRITE REPORT-LINE FROM WS-HDR-2.

      *================================================================*
       2000-PROCESS-LOAN.
           ADD 1 TO WS-READ-COUNT
           IF LOAN-PAID-OFF OR LOAN-CHARGED-OFF
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-AGED-COUNT
           MOVE SPACES TO WS-ACTION
           MOVE ZERO TO WS-FEES-TODAY
           MOVE LOAN-DAYS-PAST-DUE TO WS-PRIOR-DPD
           MOVE LOAN-STATUS        TO WS-PRIOR-STATUS

           PERFORM 2100-ROLL-NEXT-DUE
           PERFORM 2200-COMPUTE-PAST-DUE
           PERFORM 2400-LATE-FEES
           PERFORM 2300-AGE
           PERFORM 2500-COUNTERS
           PERFORM 2600-STATUS-CHANGE
           IF DLQ-ALERT-FLAG = 'Y'
               PERFORM 2700-WRITE-ALERT
           END-IF

           MOVE LOAN-LATE-FEES-OUTST TO LOAN-PAST-DUE-FEES
           MOVE CTL-BUSINESS-DATE TO LOAN-LAST-MAINT-DATE
           MOVE 'LNDELQ' TO LOAN-LAST-MAINT-USER
           REWRITE LOAN-RECORD
           IF WS-LOAN-STATUS NOT = '00'
               MOVE 'LOANMST' TO ABND-FILE-NAME
               MOVE WS-LOAN-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED - LOAN MASTER' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           IF LOAN-DAYS-PAST-DUE > ZERO OR WS-ACTION NOT = SPACES
              OR WS-PRIOR-STATUS NOT = LOAN-STATUS
               PERFORM 8000-WRITE-DETAIL
           END-IF.

      *----------------------------------------------------------------*
      * DUE DATE HAS PASSED - ADVANCE TO THE NEXT CONTRACTUAL DATE     *
      *----------------------------------------------------------------*
       2100-ROLL-NEXT-DUE.
           IF LOAN-PAY-QUARTERLY
               MOVE 3 TO WS-MONTHS
           ELSE
               MOVE 1 TO WS-MONTHS
           END-IF
           PERFORM UNTIL LOAN-NEXT-DUE-DATE = ZERO
                      OR LOAN-NEXT-DUE-DATE NOT < CTL-BUSINESS-DATE
                      OR LOAN-NEXT-DUE-DATE NOT < LOAN-MATURITY-DATE
               INITIALIZE DATE-PARMS
               MOVE 'ADDM' TO DTU-FUNCTION
               MOVE LOAN-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE WS-MONTHS TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO LOAN-NEXT-DUE-DATE
           END-PERFORM.

       2200-COMPUTE-PAST-DUE.
           MOVE ZERO TO LOAN-PAST-DUE-PRIN LOAN-PAST-DUE-INT
                        LOAN-OLDEST-DUE-DATE
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LOAN-DUE-COUNT
               IF LOAN-DUE-DATE(WS-I) < CTL-BUSINESS-DATE
                   ADD LOAN-DUE-PRIN(WS-I) TO LOAN-PAST-DUE-PRIN
                   ADD LOAN-DUE-INT(WS-I)  TO LOAN-PAST-DUE-INT
                   IF LOAN-OLDEST-DUE-DATE = ZERO
                       MOVE LOAN-DUE-DATE(WS-I) TO LOAN-OLDEST-DUE-DATE
                   END-IF
               END-IF
           END-PERFORM
           IF LOAN-OLDEST-DUE-DATE = ZERO
               MOVE ZERO TO LOAN-DAYS-PAST-DUE
           ELSE
               INITIALIZE DATE-PARMS
               MOVE 'DIFF' TO DTU-FUNCTION
               MOVE LOAN-OLDEST-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE    TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO LOAN-DAYS-PAST-DUE
           END-IF
           COMPUTE WS-PAST-DUE-TOTAL =
               LOAN-PAST-DUE-PRIN + LOAN-PAST-DUE-INT.

       2300-AGE.
           INITIALIZE DELQ-PARMS
           MOVE 'AGE '              TO DLQ-FUNCTION
           MOVE LOAN-PRODUCT-CODE   TO DLQ-PRODUCT-CODE
           MOVE LOAN-CATEGORY       TO DLQ-CATEGORY
           MOVE LOAN-STATUS         TO DLQ-CURRENT-STATUS
           MOVE LOAN-DAYS-PAST-DUE  TO DLQ-DAYS-PAST-DUE
           MOVE WS-PRIOR-DPD        TO DLQ-PRIOR-DPD
           MOVE LOAN-BORROWER-STATE TO DLQ-STATE
           CALL 'DELQBKT' USING DELQ-PARMS
           IF NOT DLQ-OK
               MOVE 'DELQBKT' TO ABND-FILE-NAME
               MOVE 'DELINQUENCY RULE MODULE REJECTED LOAN'
                 TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           MOVE DLQ-BUCKET      TO LOAN-DELQ-BUCKET
           MOVE DLQ-BUREAU-FLAG TO LOAN-BUREAU-RPT-FLAG
           IF LOAN-DAYS-PAST-DUE > ZERO
               ADD 1 TO WS-DELQ-COUNT
               ADD WS-PAST-DUE-TOTAL TO WS-TOT-PAST-DUE
           END-IF.

      *----------------------------------------------------------------*
      * ONE LATE FEE PER INSTALLMENT (DELQBKT DECIDES GRACE / AMOUNT)  *
      *----------------------------------------------------------------*
       2400-LATE-FEES.
           INITIALIZE DELQ-PARMS
           MOVE LOAN-PRODUCT-CODE   TO DLQ-PRODUCT-CODE
           MOVE LOAN-CATEGORY       TO DLQ-CATEGORY
           MOVE LOAN-STATUS         TO DLQ-CURRENT-STATUS
           MOVE LOAN-BORROWER-STATE TO DLQ-STATE
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LOAN-DUE-COUNT
               IF LOAN-DUE-DATE(WS-I) < CTL-BUSINESS-DATE
                  AND NOT LOAN-DUE-FEE-ASSESSED(WS-I)
                   INITIALIZE DATE-PARMS
                   MOVE 'DIFF' TO DTU-FUNCTION
                   MOVE LOAN-DUE-DATE(WS-I) TO DTU-DATE-1
                   MOVE CTL-BUSINESS-DATE   TO DTU-DATE-2
                   CALL 'DATEUTIL' USING DATE-PARMS
                   MOVE 'LFEE' TO DLQ-FUNCTION
                   MOVE DTU-RESULT-NUMBER TO DLQ-INSTALL-DAYS-LATE
                   MOVE LOAN-DUE-FEE-FLAG(WS-I)
                     TO DLQ-FEE-ASSESSED-FLAG
                   IF LOAN-PAYMENT-AMT > ZERO
                       MOVE LOAN-PAYMENT-AMT TO DLQ-INSTALLMENT-AMT
                   ELSE
                       COMPUTE DLQ-INSTALLMENT-AMT =
                           LOAN-DUE-INT(WS-I) + LOAN-DUE-PRIN(WS-I)
                   END-IF
                   CALL 'DELQBKT' USING DELQ-PARMS
                   IF DLQ-OK AND DLQ-ASSESS-FEE-FLAG = 'Y'
                       ADD DLQ-LATE-FEE-AMT TO LOAN-LATE-FEES-OUTST
                                               WS-FEES-TODAY
                                               WS-TOT-FEES
                       ADD 1 TO LOAN-LATE-FEE-COUNT WS-FEE-COUNT
                       MOVE 'Y' TO LOAN-DUE-FEE-FLAG(WS-I)
                       MOVE 'LATE FEE ASSESSED' TO WS-ACTION
                   END-IF
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * TIMES 30/60/90 DPD IN THE LAST 24 MONTHS                       *
      *----------------------------------------------------------------*
       2500-COUNTERS.
           INITIALIZE DATE-PARMS
           MOVE 'DIFF' TO DTU-FUNCTION
           MOVE LOAN-DLQ-CTR-START-DATE TO DTU-DATE-1
           MOVE CTL-BUSINESS-DATE       TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DTU-OK AND DTU-RESULT-NUMBER > WS-COUNTER-WINDOW-DAYS
               MOVE ZERO TO LOAN-TIMES-30-DPD LOAN-TIMES-60-DPD
                            LOAN-TIMES-90-DPD
               MOVE CTL-BUSINESS-DATE TO LOAN-DLQ-CTR-START-DATE
           END-IF
           IF DLQ-CROSSED-30 = 'Y' AND LOAN-TIMES-30-DPD < 99
               ADD 1 TO LOAN-TIMES-30-DPD
           END-IF
           IF DLQ-CROSSED-60 = 'Y' AND LOAN-TIMES-60-DPD < 99
               ADD 1 TO LOAN-TIMES-60-DPD
           END-IF
           IF DLQ-CROSSED-90 = 'Y' AND LOAN-TIMES-90-DPD < 99
               ADD 1 TO LOAN-TIMES-90-DPD
           END-IF.

      *----------------------------------------------------------------*
       2600-STATUS-CHANGE.
           IF DLQ-NEW-STATUS = LOAN-STATUS
               EXIT PARAGRAPH
           END-IF
           EVALUATE DLQ-NEW-STATUS
               WHEN 'N'
                   PERFORM 2610-NON-ACCRUAL
               WHEN 'C'
                   PERFORM 2620-CHARGE-OFF
               WHEN 'D'
                   MOVE 'NOW DELINQUENT' TO WS-ACTION
               WHEN 'A'
                   MOVE 'CURED - RETURNED TO ACTIVE' TO WS-ACTION
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           MOVE DLQ-NEW-STATUS    TO LOAN-STATUS
           MOVE CTL-BUSINESS-DATE TO LOAN-STATUS-DATE.

      *    COMMERCIAL 90+ - REVERSE ACCRUED INTEREST TO MEMO (CHG1187)
       2610-NON-ACCRUAL.
           COMPUTE WS-ACCR-CENTS ROUNDED = LOAN-ACCRUED-INT
           ADD WS-ACCR-CENTS TO LOAN-NONACCR-INT-MEMO
           MOVE ZERO TO LOAN-ACCRUED-INT
           MOVE CTL-BUSINESS-DATE TO LOAN-NONACCRUAL-DATE
           ADD 1 TO WS-NONACCR-COUNT
           MOVE 'PLACED ON NON-ACCRUAL' TO WS-ACTION.

      *    CONSUMER 120+ CLOSED-END / 180+ OPEN-END (FFIEC)
       2620-CHARGE-OFF.
           COMPUTE WS-ACCR-CENTS ROUNDED = LOAN-ACCRUED-INT
           ADD WS-ACCR-CENTS TO LOAN-NONACCR-INT-MEMO
           MOVE ZERO TO LOAN-ACCRUED-INT
           MOVE LOAN-PRINCIPAL-BAL TO LOAN-CHARGEOFF-AMT
           MOVE CTL-BUSINESS-DATE  TO LOAN-CHARGEOFF-DATE
           ADD LOAN-PRINCIPAL-BAL TO WS-TOT-CHGOFF
           MOVE ZERO TO LOAN-PRINCIPAL-BAL
      *    LATE FEES WAIVED AT CHARGE-OFF
           MOVE ZERO TO LOAN-LATE-FEES-OUTST
           MOVE 'N' TO LOAN-AUTOPAY-FLAG
           ADD 1 TO WS-CHGOFF-COUNT
           MOVE 'CHARGED OFF' TO WS-ACTION.

      *----------------------------------------------------------------*
       2700-WRITE-ALERT.
           INITIALIZE ALERT-RECORD
           MOVE CTL-BUSINESS-DATE     TO ALRT-DATE
           MOVE ZERO                  TO ALRT-TIME
           MOVE 'LNDQ'                TO ALRT-TYPE
           MOVE DLQ-ALERT-SEVERITY    TO ALRT-SEVERITY
           MOVE LOAN-BORROWER-CUST-ID TO ALRT-CUST-ID
           MOVE LOAN-NUMBER           TO ALRT-ACCT-NUMBER
           MOVE LOAN-DAYS-PAST-DUE    TO WS-DPD-ED
           MOVE SPACES TO WS-ALERT-DESC
           EVALUATE TRUE
               WHEN DLQ-CHARGEOFF-FLAG = 'Y'
                   MOVE 'LNDQCHGO'         TO ALRT-RULE-ID
                   MOVE LOAN-CHARGEOFF-AMT TO ALRT-AMOUNT
                   STRING 'LOAN CHARGED OFF AT ' DELIMITED BY SIZE
                          WS-DPD-ED ' DPD - REFER TO RECOVERY'
                          DELIMITED BY SIZE
                     INTO WS-ALERT-DESC
                   END-STRING
               WHEN DLQ-NONACCRUAL-FLAG = 'Y'
                   MOVE 'LNDQNACR'         TO ALRT-RULE-ID
                   MOVE WS-PAST-DUE-TOTAL  TO ALRT-AMOUNT
                   STRING 'COMMERCIAL LOAN PLACED ON NON-ACCRUAL AT '
                          DELIMITED BY SIZE
                          WS-DPD-ED ' DPD' DELIMITED BY SIZE
                     INTO WS-ALERT-DESC
                   END-STRING
               WHEN DLQ-CROSSED-90 = 'Y'
                   MOVE 'LNDQ090'          TO ALRT-RULE-ID
                   MOVE WS-PAST-DUE-TOTAL  TO ALRT-AMOUNT
                   MOVE 'LOAN 90 DAYS PAST DUE' TO WS-ALERT-DESC
               WHEN OTHER
                   MOVE 'LNDQ060'          TO ALRT-RULE-ID
                   MOVE WS-PAST-DUE-TOTAL  TO ALRT-AMOUNT
                   MOVE 'LOAN 60 DAYS PAST DUE - COLLECTIONS QUEUE'
                     TO WS-ALERT-DESC
           END-EVALUATE
           MOVE WS-ALERT-DESC TO ALRT-DESCRIPTION
           SET ALRT-OPEN TO TRUE
           MOVE 'LNDELQ' TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD
           IF WS-ALERT-STATUS NOT = '00'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALERT-STATUS TO ABND-FILE-STATUS
               MOVE 'WRITE FAILED - ALERTS' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-ALERT-COUNT.

      *================================================================*
       8000-WRITE-DETAIL.
           MOVE LOAN-NUMBER          TO WS-D-LOAN
           MOVE LOAN-PRODUCT-CODE    TO WS-D-PROD
           MOVE WS-PRIOR-STATUS      TO WS-D-OLD-ST
           MOVE LOAN-STATUS          TO WS-D-NEW-ST
           MOVE LOAN-DAYS-PAST-DUE   TO WS-D-DPD
           MOVE DLQ-BUCKET-DESC      TO WS-D-BUCKET
           MOVE WS-PAST-DUE-TOTAL    TO WS-D-PAST
           MOVE WS-FEES-TODAY        TO WS-D-FEES
           MOVE LOAN-LATE-FEES-OUTST TO WS-D-OUTST
           MOVE LOAN-TIMES-30-DPD    TO WS-D-T30
           MOVE LOAN-TIMES-60-DPD    TO WS-D-T60
           MOVE LOAN-TIMES-90-DPD    TO WS-D-T90
           MOVE WS-ACTION            TO WS-D-ACTION
           WRITE REPORT-LINE FROM WS-DTL.

       9000-TERMINATE.
           MOVE WS-READ-COUNT    TO WS-T-READ
           MOVE WS-AGED-COUNT    TO WS-T-AGED
           MOVE WS-DELQ-COUNT    TO WS-T-DELQ
           MOVE WS-FEE-COUNT     TO WS-T-FEES
           MOVE WS-TOT-FEES      TO WS-T-FEE-AMT
           MOVE WS-NONACCR-COUNT TO WS-T-NACR
           MOVE WS-CHGOFF-COUNT  TO WS-T-CHGO
           MOVE WS-TOT-CHGOFF    TO WS-T-CHGO-AMT
           MOVE WS-ALERT-COUNT   TO WS-T-ALERTS
           WRITE REPORT-LINE FROM WS-TRL-1
           WRITE REPORT-LINE FROM WS-TRL-2
           CLOSE LOAN-FILE ALERT-FILE REPORT-FILE
           DISPLAY 'LNDELQ  READ=' WS-READ-COUNT
                   ' DELINQUENT=' WS-DELQ-COUNT
                   ' LATE-FEES=' WS-FEE-COUNT
                   ' NON-ACCRUAL=' WS-NONACCR-COUNT
                   ' CHARGE-OFFS=' WS-CHGOFF-COUNT
                   ' ALERTS=' WS-ALERT-COUNT
           MOVE 0 TO RETURN-CODE.

       9900-ABEND.
           MOVE 'LNDELQ' TO ABND-PROGRAM
           MOVE '2000-PROCESS-LOAN' TO ABND-PARAGRAPH
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
