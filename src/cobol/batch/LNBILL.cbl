      *================================================================*
      * PROGRAM  : LNBILL                                              *
      * TYPE     : BATCH - EOD STEP 15 (BEFORE LNDELQ)                 *
      * PURPOSE  : LOAN BILLING AND AUTOPAY.                           *
      *            1. BILLING - A LOAN WHOSE NEXT DUE DATE FALLS WITHIN*
      *               THE BILLING LEAD WINDOW (20 DAYS MONTHLY, 30     *
      *               DAYS QUARTERLY) AND HAS NOT BEEN BILLED FOR THAT *
      *               DATE GETS A BILL: THE INSTALLMENT IS SPLIT INTO  *
      *               INTEREST (ACCRUED + PROJECTED TO THE DUE DATE,   *
      *               LESS INTEREST ALREADY BILLED) AND PRINCIPAL      *
      *               (PAYMENT LESS INTEREST; NONE DURING INTEREST-    *
      *               ONLY PERIODS OR ON REVOLVERS; ALL REMAINING      *
      *               PRINCIPAL AT MATURITY), ADDED TO THE DUE TABLE   *
      *               AND WRITTEN TO LNBILLS WITH THE PAST-DUE AMOUNT  *
      *               AND LATE FEES.                                   *
      *            2. AUTOPAY - FOR AUTOPAY LOANS WHOSE NEXT DUE DATE  *
      *               IS ON OR BEFORE THE NEXT BUSINESS DATE, A WLNP   *
      *               DEBIT FOR THE TOTAL AMOUNT DUE IS WRITTEN TO     *
      *               LNAUTOPY (TRANREC, STATUS P, EFFECTIVE NEXT      *
      *               BUSINESS DATE, COUNTER ACCOUNT = LOAN NUMBER).   *
      *               THE EOD DRIVER APPENDS LNAUTOPY TO THE NEXT      *
      *               CYCLE'S TRANIN; THE DEBIT IS EDITED AND POSTED   *
      *               TO THE DEPOSIT ACCOUNT BY TRANEDIT / POSTTRAN    *
      *               AND APPLIED TO THE LOAN BY LNPAYAP (STEP 07).    *
      * INPUT    : BANKCTL (VIA GETCTL)                                *
      * I-O      : LOANMST                                             *
      * OUTPUT   : LNBILLS (LNBILREC), LNAUTOPY (TRANREC), LNBLRPT     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - COUPON BOOK REPLACEMENT             *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-01-20 BHS  CRE INTEREST-ONLY PERIOD / PAYMENT RESET       *
      * 2006-02-13 BHS  DUE TABLE - CHG0412                            *
      * 2010-02-22 DPS  LATE FEE WARNING ON BILL - CHG0977             *
      * 2016-04-25 AKW  LPER BILLING, ACTUAL/365 PROJECTION            *
      * 2017-03-20 AKW  AUTOPAY DEBITS TO LNAUTOPY - CHG2104           *
      * 2019-11-04 JPL  QUARTERLY LEAD 30 DAYS - INC19022              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNBILL.

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
           SELECT BILL-FILE ASSIGN TO "LNBILLS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-BILL-STATUS.
           SELECT AUTOPAY-FILE ASSIGN TO "LNAUTOPY"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-AUTO-STATUS.
           SELECT REPORT-FILE ASSIGN TO "LNBLRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
       COPY LOANREC.
       FD  BILL-FILE.
       COPY LNBILREC.
       FD  AUTOPAY-FILE.
       COPY TRANREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-LOAN-STATUS                  PIC X(02).
       01  WS-BILL-STATUS                  PIC X(02).
       01  WS-AUTO-STATUS                  PIC X(02).
       01  WS-RPT-STATUS                   PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
      *----------------------------------------------------------------*
      * BILLING POLICY                                                 *
      *----------------------------------------------------------------*
       01  WS-BILL-POLICY.
           05  WS-LEAD-DAYS-MONTHLY        PIC 9(03) VALUE 20.
           05  WS-LEAD-DAYS-QUARTERLY      PIC 9(03) VALUE 30.
      *    LATE FEE WARNING PRINTED ON THE BILL
           05  WS-WARN-GRACE-DAYS          PIC 9(02) VALUE 10.
           05  WS-WARN-CONS-MIN            PIC 9(03)V99 VALUE 15.00.
           05  WS-WARN-CONS-MAX            PIC 9(03)V99 VALUE 40.00.
           05  WS-WARN-PCT                 PIC 9(02)V99 VALUE 5.00.
           05  WS-WARN-COMM-MIN            PIC 9(05)V99 VALUE 100.00.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-BILL-COUNT               PIC 9(07) VALUE 0.
           05  WS-AUTO-COUNT               PIC 9(07) VALUE 0.
           05  WS-PMT-RESET-COUNT          PIC 9(07) VALUE 0.
           05  WS-AUTO-SEQ                 PIC 9(08) VALUE 61000000.
       01  WS-TOTALS.
           05  WS-TOT-BILLED               PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-TOT-AUTOPAY              PIC S9(13)V99 COMP-3
                                           VALUE 0.
       01  WS-WORK.
           05  WS-I                        PIC 9(02).
           05  WS-N                        PIC 9(02).
           05  WS-DAYS-TO-DUE              PIC S9(07).
           05  WS-LEAD-DAYS                PIC 9(03).
           05  WS-PROJ-DAYS                PIC S9(07).
           05  WS-BASIS-DAYS               PIC 9(03).
           05  WS-MONTHS-PER-PERIOD        PIC 9(02).
           05  WS-INSTALLMENT-NO           PIC 9(03).
           05  WS-IO-PERIODS               PIC 9(03).
           05  WS-BILLED-INT               PIC S9(11)V99 COMP-3.
           05  WS-BILLED-PRIN              PIC S9(13)V99 COMP-3.
           05  WS-PROJ-INT                 PIC S9(11)V9(06) COMP-3.
           05  WS-CURR-INT                 PIC S9(11)V99 COMP-3.
           05  WS-CURR-PRIN                PIC S9(13)V99 COMP-3.
           05  WS-UNBILLED-PRIN            PIC S9(13)V99 COMP-3.
           05  WS-PAST-DUE                 PIC S9(13)V99 COMP-3.
           05  WS-TOTAL-DUE                PIC S9(13)V99 COMP-3.
           05  WS-INSTALL-AMT              PIC S9(13)V99 COMP-3.
           05  WS-WARN-FEE                 PIC S9(07)V99 COMP-3.
           05  WS-PENDING-FEES             PIC S9(07)V99 COMP-3.
           05  WS-DAYS-LATE                PIC S9(07).
           05  WS-WARN-DATE                PIC 9(08).
           05  WS-DUE-DATE-WORK            PIC 9(08).
           05  WS-NOTE                     PIC X(30).

       01  WS-HDR-1.
           05  FILLER PIC X(40) VALUE
               'LNBILL   CBS LOAN BILLING AND AUTOPAY   '.
           05  FILLER PIC X(16) VALUE '  BUSINESS DATE '.
           05  WS-HDR-DATE                 PIC 9(08).
       01  WS-HDR-2.
           05  FILLER PIC X(45) VALUE
               'LOAN NUMBER  PROD DUE DATE  INST     INTEREST'.
           05  FILLER PIC X(45) VALUE
               '      PRINCIPAL      PAST DUE    FEES+PENDING'.
           05  FILLER PIC X(42) VALUE
               '      TOTAL DUE NOTE'.
       01  WS-DTL.
           05  WS-D-LOAN                   PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-PROD                   PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-DUE                    PIC 9(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-INST                   PIC ZZ9.
           05  WS-D-INT                    PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-D-PRIN                   PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-D-PAST                   PIC ZZ,ZZZ,ZZ9.99.
           05  WS-D-FEES                   PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-D-TOTAL                  PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-NOTE                   PIC X(30).
       01  WS-TRL.
           05  FILLER PIC X(13) VALUE 'LNBILL TOTALS'.
           05  FILLER PIC X(06) VALUE ' READ='.
           05  WS-T-READ                   PIC ZZZ,ZZ9.
           05  FILLER PIC X(07) VALUE ' BILLS='.
           05  WS-T-BILLS                  PIC ZZZ,ZZ9.
           05  FILLER PIC X(09) VALUE ' BILLED $'.
           05  WS-T-BILLED                 PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER PIC X(10) VALUE ' AUTOPAYS='.
           05  WS-T-AUTO                   PIC ZZZ,ZZ9.
           05  FILLER PIC X(02) VALUE ' $'.
           05  WS-T-AUTO-AMT               PIC ZZ,ZZZ,ZZZ,ZZ9.99.

       COPY BANKCTL.
       COPY DATEPARM.
       COPY AMRTPARM.
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
           OPEN OUTPUT BILL-FILE AUTOPAY-FILE REPORT-FILE
           IF WS-BILL-STATUS NOT = '00' OR WS-AUTO-STATUS NOT = '00'
              OR WS-RPT-STATUS NOT = '00'
               MOVE 'LNBILLS' TO ABND-FILE-NAME
               MOVE WS-BILL-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - BILLS / AUTOPAY / REPORT'
                 TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           MOVE CTL-BUSINESS-DATE TO WS-HDR-DATE
           WRITE REPORT-LINE FROM WS-HDR-1
           WRITE REPORT-LINE FROM WS-HDR-2.

      *================================================================*
       2000-PROCESS-LOAN.
           ADD 1 TO WS-READ-COUNT
      *    NO BILLS ON PAID-OFF, CHARGED-OFF OR FORBEARANCE LOANS
           IF LOAN-PAID-OFF OR LOAN-CHARGED-OFF OR LOAN-FORBEARANCE
               EXIT PARAGRAPH
           END-IF
           IF LOAN-NEXT-DUE-DATE = ZERO
               EXIT PARAGRAPH
           END-IF
           IF LOAN-PAY-QUARTERLY
               MOVE WS-LEAD-DAYS-QUARTERLY TO WS-LEAD-DAYS
               MOVE 3 TO WS-MONTHS-PER-PERIOD
           ELSE
               MOVE WS-LEAD-DAYS-MONTHLY TO WS-LEAD-DAYS
               MOVE 1 TO WS-MONTHS-PER-PERIOD
           END-IF
           MOVE CTL-BUSINESS-DATE  TO DTU-DATE-1
           MOVE LOAN-NEXT-DUE-DATE TO DTU-DATE-2
           PERFORM 8800-DAYS-BETWEEN
           MOVE DTU-RESULT-NUMBER TO WS-DAYS-TO-DUE

           IF WS-DAYS-TO-DUE NOT > WS-LEAD-DAYS
              AND LOAN-LAST-BILLED-DUE-DATE < LOAN-NEXT-DUE-DATE
               PERFORM 3000-BILL-INSTALLMENT
           END-IF
           IF LOAN-ON-AUTOPAY
               PERFORM 4000-AUTOPAY
           END-IF
           MOVE CTL-BUSINESS-DATE TO LOAN-LAST-MAINT-DATE
           MOVE 'LNBILL' TO LOAN-LAST-MAINT-USER
           REWRITE LOAN-RECORD
           IF WS-LOAN-STATUS NOT = '00'
               MOVE 'LOANMST' TO ABND-FILE-NAME
               MOVE WS-LOAN-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED - LOAN MASTER' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF.

      *================================================================*
      * BILL THE INSTALLMENT DUE ON LOAN-NEXT-DUE-DATE                 *
      *================================================================*
       3000-BILL-INSTALLMENT.
           MOVE SPACES TO WS-NOTE
           COMPUTE WS-INSTALLMENT-NO = LOAN-INSTALLMENTS-BILLED + 1
           COMPUTE WS-IO-PERIODS =
               LOAN-IO-MONTHS / WS-MONTHS-PER-PERIOD
           PERFORM 3100-SUM-BILLED
           PERFORM 3200-CALC-INTEREST
           PERFORM 3300-CALC-PRINCIPAL
           PERFORM 3400-ADD-DUE-ENTRY
           PERFORM 3500-LATE-FEE-WARNING
           PERFORM 3600-WRITE-BILL
           MOVE WS-INSTALLMENT-NO  TO LOAN-INSTALLMENTS-BILLED
           MOVE LOAN-NEXT-DUE-DATE TO LOAN-LAST-BILLED-DUE-DATE
           MOVE CTL-BUSINESS-DATE  TO LOAN-LAST-BILL-DATE.

       3100-SUM-BILLED.
           MOVE ZERO TO WS-BILLED-INT WS-BILLED-PRIN WS-PAST-DUE
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LOAN-DUE-COUNT
               ADD LOAN-DUE-INT(WS-I)  TO WS-BILLED-INT
               ADD LOAN-DUE-PRIN(WS-I) TO WS-BILLED-PRIN
               IF LOAN-DUE-DATE(WS-I) < CTL-BUSINESS-DATE
                   ADD LOAN-DUE-INT(WS-I) LOAN-DUE-PRIN(WS-I)
                       TO WS-PAST-DUE
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * INTEREST = ACCRUED + PROJECTED (LAST ACCRUAL -> DUE DATE)      *
      *            - INTEREST ALREADY ON THE DUE TABLE                 *
      *----------------------------------------------------------------*
       3200-CALC-INTEREST.
           MOVE LOAN-LAST-ACCRUAL-DATE TO DTU-DATE-1
           MOVE LOAN-NEXT-DUE-DATE     TO DTU-DATE-2
           EVALUATE TRUE
               WHEN LOAN-30-360
                   INITIALIZE DTU-RESULT-NUMBER
                   MOVE 'D360' TO DTU-FUNCTION
                   CALL 'DATEUTIL' USING DATE-PARMS
                   MOVE 360 TO WS-BASIS-DAYS
               WHEN LOAN-ACTUAL-365
                   PERFORM 8800-DAYS-BETWEEN
                   MOVE 'DIYR' TO DTU-FUNCTION
                   MOVE DTU-RESULT-NUMBER TO WS-PROJ-DAYS
                   MOVE LOAN-NEXT-DUE-DATE TO DTU-DATE-1
                   CALL 'DATEUTIL' USING DATE-PARMS
                   MOVE DTU-RESULT-NUMBER TO WS-BASIS-DAYS
                   MOVE WS-PROJ-DAYS TO DTU-RESULT-NUMBER
               WHEN OTHER
                   PERFORM 8800-DAYS-BETWEEN
                   MOVE 360 TO WS-BASIS-DAYS
           END-EVALUATE
           MOVE DTU-RESULT-NUMBER TO WS-PROJ-DAYS
           IF WS-PROJ-DAYS < ZERO
               MOVE ZERO TO WS-PROJ-DAYS
           END-IF
           IF LOAN-NON-ACCRUAL
      *        NON-ACCRUAL - BILL CONTRACTUAL INTEREST FOR THE PERIOD
               COMPUTE WS-PROJ-INT =
                   LOAN-PRINCIPAL-BAL * LOAN-INT-RATE / 100
                   * WS-PROJ-DAYS / WS-BASIS-DAYS
           ELSE
               COMPUTE WS-PROJ-INT =
                   LOAN-ACCRUED-INT
                   + LOAN-PRINCIPAL-BAL * LOAN-INT-RATE / 100
                     * WS-PROJ-DAYS / WS-BASIS-DAYS
                   - WS-BILLED-INT
           END-IF
           COMPUTE WS-CURR-INT ROUNDED = WS-PROJ-INT
           IF WS-CURR-INT < ZERO
               MOVE ZERO TO WS-CURR-INT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL PORTION                                              *
      *----------------------------------------------------------------*
       3300-CALC-PRINCIPAL.
           COMPUTE WS-UNBILLED-PRIN =
               LOAN-PRINCIPAL-BAL - WS-BILLED-PRIN
           EVALUATE TRUE
               WHEN LOAN-NEXT-DUE-DATE NOT < LOAN-MATURITY-DATE
                   MOVE WS-UNBILLED-PRIN TO WS-CURR-PRIN
                   IF LOAN-AMORT-MONTHS > LOAN-TERM-MONTHS
                       MOVE 'BALLOON AT MATURITY' TO WS-NOTE
                   ELSE
                       MOVE 'FINAL PAYMENT' TO WS-NOTE
                   END-IF
               WHEN LOAN-IS-REVOLVER
                   MOVE ZERO TO WS-CURR-PRIN
                   MOVE 'INTEREST DUE - REVOLVER' TO WS-NOTE
               WHEN WS-INSTALLMENT-NO NOT > WS-IO-PERIODS
                   MOVE ZERO TO WS-CURR-PRIN
                   MOVE 'INTEREST ONLY PERIOD' TO WS-NOTE
               WHEN OTHER
                   IF WS-IO-PERIODS > ZERO
                      AND WS-INSTALLMENT-NO = WS-IO-PERIODS + 1
                       PERFORM 3310-RESET-PAYMENT-AFTER-IO
                   END-IF
                   COMPUTE WS-CURR-PRIN =
                       LOAN-PAYMENT-AMT - WS-CURR-INT
           END-EVALUATE
           IF WS-CURR-PRIN < ZERO
               MOVE ZERO TO WS-CURR-PRIN
           END-IF
           IF WS-CURR-PRIN > WS-UNBILLED-PRIN
               MOVE WS-UNBILLED-PRIN TO WS-CURR-PRIN
           END-IF.

      *    END OF INTEREST-ONLY PERIOD - LEVEL P&I PAYMENT (CRE)
       3310-RESET-PAYMENT-AFTER-IO.
           INITIALIZE AMORT-PARMS
           MOVE 'PMT '               TO AMT-FUNCTION
           MOVE LOAN-PRINCIPAL-BAL   TO AMT-PRINCIPAL
           MOVE LOAN-INT-RATE        TO AMT-ANNUAL-RATE
           MOVE LOAN-ACCRUAL-BASIS   TO AMT-ACCRUAL-BASIS
           MOVE LOAN-PAYMENT-FREQ    TO AMT-PAYMENT-FREQ
           COMPUTE AMT-TERM-MONTHS =
               LOAN-TERM-MONTHS - LOAN-IO-MONTHS
           MOVE LOAN-AMORT-MONTHS    TO AMT-AMORT-MONTHS
           MOVE ZERO                 TO AMT-IO-MONTHS
           CALL 'AMORTCLC' USING AMORT-PARMS
           IF AMT-OK
               MOVE AMT-PAYMENT-AMT TO LOAN-PAYMENT-AMT
               ADD 1 TO WS-PMT-RESET-COUNT
               MOVE 'P&I PAYMENT RESET AFTER IO' TO WS-NOTE
           END-IF.

       3400-ADD-DUE-ENTRY.
           IF LOAN-DUE-COUNT < 8
               ADD 1 TO LOAN-DUE-COUNT
               MOVE LOAN-DUE-COUNT TO WS-N
               MOVE LOAN-NEXT-DUE-DATE TO LOAN-DUE-DATE(WS-N)
               MOVE WS-CURR-INT        TO LOAN-DUE-INT(WS-N)
               MOVE WS-CURR-PRIN       TO LOAN-DUE-PRIN(WS-N)
               MOVE 'N'                TO LOAN-DUE-FEE-FLAG(WS-N)
           ELSE
      *        TABLE FULL - ROLL INTO THE NEWEST ENTRY (INC08861)
               ADD WS-CURR-INT  TO LOAN-DUE-INT(8)
               ADD WS-CURR-PRIN TO LOAN-DUE-PRIN(8)
               MOVE LOAN-NEXT-DUE-DATE TO LOAN-DUE-DATE(8)
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE WARNING. FEES PENDING ON PAST-DUE INSTALLMENTS MORE   *
      * THAN 10 DAYS LATE ARE ADDED TO THE AMOUNT DUE, AND THE BILL    *
      * SHOWS THE FEE FOR THIS INSTALLMENT IF NOT PAID WITHIN 10 DAYS  *
      *----------------------------------------------------------------*
       3500-LATE-FEE-WARNING.
           MOVE ZERO TO WS-PENDING-FEES
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LOAN-DUE-COUNT
               IF LOAN-DUE-DATE(WS-I) < CTL-BUSINESS-DATE
                  AND LOAN-DUE-FEE-FLAG(WS-I) NOT = 'Y'
                   MOVE LOAN-DUE-DATE(WS-I) TO DTU-DATE-1
                   MOVE CTL-BUSINESS-DATE   TO DTU-DATE-2
                   PERFORM 8800-DAYS-BETWEEN
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS-LATE
                   IF WS-DAYS-LATE > WS-WARN-GRACE-DAYS
                       COMPUTE WS-INSTALL-AMT =
                           LOAN-DUE-INT(WS-I) + LOAN-DUE-PRIN(WS-I)
                       PERFORM 3510-WARNING-FEE
                       ADD WS-WARN-FEE TO WS-PENDING-FEES
                   END-IF
               END-IF
           END-PERFORM
           COMPUTE WS-INSTALL-AMT = WS-CURR-INT + WS-CURR-PRIN
           PERFORM 3510-WARNING-FEE
           MOVE LOAN-NEXT-DUE-DATE TO DTU-DATE-1
           MOVE WS-WARN-GRACE-DAYS TO DTU-NUMBER
           MOVE 'ADDD' TO DTU-FUNCTION
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-WARN-DATE.

       3510-WARNING-FEE.
           IF LOAN-IS-PERSONAL
               COMPUTE WS-WARN-FEE ROUNDED =
                   WS-INSTALL-AMT * WS-WARN-PCT / 100
               IF WS-WARN-FEE < WS-WARN-CONS-MIN
                   MOVE WS-WARN-CONS-MIN TO WS-WARN-FEE
               END-IF
               IF WS-WARN-FEE > WS-WARN-CONS-MAX
                   MOVE WS-WARN-CONS-MAX TO WS-WARN-FEE
               END-IF
           ELSE
               COMPUTE WS-WARN-FEE ROUNDED =
                   WS-INSTALL-AMT * WS-WARN-PCT / 100
               IF WS-WARN-FEE < WS-WARN-COMM-MIN
                   MOVE WS-WARN-COMM-MIN TO WS-WARN-FEE
               END-IF
           END-IF.

       3600-WRITE-BILL.
           COMPUTE WS-TOTAL-DUE =
               WS-CURR-INT + WS-CURR-PRIN + WS-PAST-DUE
               + LOAN-LATE-FEES-OUTST + WS-PENDING-FEES
           INITIALIZE LOAN-BILL-RECORD
           MOVE LOAN-NUMBER           TO LBL-LOAN-NUMBER
           MOVE LOAN-PRODUCT-CODE     TO LBL-PRODUCT-CODE
           MOVE LOAN-BORROWER-CUST-ID TO LBL-BORROWER-CUST-ID
           MOVE CTL-BUSINESS-DATE     TO LBL-BILL-DATE
           MOVE LOAN-NEXT-DUE-DATE    TO LBL-DUE-DATE
           MOVE WS-INSTALLMENT-NO     TO LBL-INSTALLMENT-NO
           MOVE WS-CURR-INT           TO LBL-CURR-INTEREST
           MOVE WS-CURR-PRIN          TO LBL-CURR-PRINCIPAL
           MOVE WS-PAST-DUE           TO LBL-PAST-DUE-AMT
           COMPUTE LBL-LATE-FEES-DUE =
               LOAN-LATE-FEES-OUTST + WS-PENDING-FEES
           MOVE WS-TOTAL-DUE          TO LBL-TOTAL-DUE
           MOVE LOAN-PRINCIPAL-BAL    TO LBL-PRINCIPAL-BAL
           MOVE LOAN-INT-RATE         TO LBL-INT-RATE
           MOVE LOAN-DAYS-PAST-DUE    TO LBL-DAYS-PAST-DUE
           MOVE WS-WARN-FEE           TO LBL-LATE-FEE-WARNING
           MOVE LOAN-AUTOPAY-FLAG     TO LBL-AUTOPAY-FLAG
           MOVE LOAN-AUTOPAY-ACCT     TO LBL-AUTOPAY-ACCT
           EVALUATE TRUE
               WHEN WS-PAST-DUE > ZERO
                   MOVE 'PDUE' TO LBL-MESSAGE-CODE
               WHEN LOAN-ON-AUTOPAY
                   MOVE 'AUTO' TO LBL-MESSAGE-CODE
               WHEN OTHER
                   MOVE 'LF10' TO LBL-MESSAGE-CODE
           END-EVALUATE
           WRITE LOAN-BILL-RECORD
           IF WS-BILL-STATUS NOT = '00'
               MOVE 'LNBILLS' TO ABND-FILE-NAME
               MOVE WS-BILL-STATUS TO ABND-FILE-STATUS
               MOVE 'WRITE FAILED - BILL FILE' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-BILL-COUNT
           ADD WS-TOTAL-DUE TO WS-TOT-BILLED
           MOVE LOAN-NUMBER        TO WS-D-LOAN
           MOVE LOAN-PRODUCT-CODE  TO WS-D-PROD
           MOVE LOAN-NEXT-DUE-DATE TO WS-D-DUE
           MOVE WS-INSTALLMENT-NO  TO WS-D-INST
           MOVE WS-CURR-INT        TO WS-D-INT
           MOVE WS-CURR-PRIN       TO WS-D-PRIN
           MOVE WS-PAST-DUE        TO WS-D-PAST
           MOVE LBL-LATE-FEES-DUE  TO WS-D-FEES
           MOVE WS-TOTAL-DUE       TO WS-D-TOTAL
           IF WS-NOTE = SPACES
               STRING 'LATE FEE AFTER ' DELIMITED BY SIZE
                      WS-WARN-DATE DELIMITED BY SIZE
                 INTO WS-NOTE
               END-STRING
           END-IF
           MOVE WS-NOTE TO WS-D-NOTE
           WRITE REPORT-LINE FROM WS-DTL.

      *================================================================*
      * AUTOPAY - DEBIT THE DEPOSIT ACCOUNT FOR THE TOTAL AMOUNT DUE   *
      *================================================================*
       4000-AUTOPAY.
           IF LOAN-AUTOPAY-ACCT = SPACES OR LOAN-DUE-COUNT = ZERO
               EXIT PARAGRAPH
           END-IF
           IF LOAN-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
              OR LOAN-AUTOPAY-GEN-DUE-DATE NOT < LOAN-NEXT-DUE-DATE
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-TOTAL-DUE
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LOAN-DUE-COUNT
               ADD LOAN-DUE-INT(WS-I) LOAN-DUE-PRIN(WS-I)
                   TO WS-TOTAL-DUE
           END-PERFORM
           ADD LOAN-LATE-FEES-OUTST TO WS-TOTAL-DUE
           IF WS-TOTAL-DUE NOT > ZERO
               EXIT PARAGRAPH
           END-IF

           INITIALIZE TRANSACTION-RECORD
           ADD 1 TO WS-AUTO-SEQ
           MOVE CTL-BUSINESS-DATE   TO TRAN-ID-DATE
           MOVE WS-AUTO-SEQ         TO TRAN-ID-SEQ
           MOVE LOAN-AUTOPAY-ACCT   TO TRAN-ACCT-NUMBER
           MOVE CTL-NEXT-BUS-DATE   TO TRAN-EFFECTIVE-DATE
           MOVE ZERO                TO TRAN-POST-DATE TRAN-TIME
           MOVE 'WLNP'              TO TRAN-CODE
           MOVE 'D'                 TO TRAN-DR-CR
           MOVE WS-TOTAL-DUE        TO TRAN-AMOUNT
           MOVE 'BAT'               TO TRAN-CHANNEL
           STRING 'LOAN AUTOPAY ' DELIMITED BY SIZE
                  LOAN-NUMBER     DELIMITED BY SIZE
             INTO TRAN-DESCRIPTION
           END-STRING
           STRING 'LNAP' DELIMITED BY SIZE
                  CTL-BUSINESS-DATE DELIMITED BY SIZE
                  WS-AUTO-SEQ DELIMITED BY SIZE
             INTO TRAN-REFERENCE
           END-STRING
           MOVE LOAN-NUMBER         TO TRAN-COUNTER-ACCT
           MOVE 'CBS LOAN SERVICING' TO TRAN-COUNTERPARTY-NAME
           MOVE 'US'                TO TRAN-COUNTERPARTY-COUNTRY
           SET TRAN-PENDING         TO TRUE
           MOVE 'LNBILL'            TO TRAN-USER-ID
                                       TRAN-SOURCE-PGM
           MOVE LOAN-COST-CENTER    TO TRAN-COST-CENTER
           MOVE 'N'                 TO TRAN-CASH-FLAG TRAN-REGD-FLAG
                                       TRAN-OD-FLAG
           WRITE TRANSACTION-RECORD
           IF WS-AUTO-STATUS NOT = '00'
               MOVE 'LNAUTOPY' TO ABND-FILE-NAME
               MOVE WS-AUTO-STATUS TO ABND-FILE-STATUS
               MOVE 'WRITE FAILED - AUTOPAY FILE' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           MOVE LOAN-NEXT-DUE-DATE TO LOAN-AUTOPAY-GEN-DUE-DATE
           ADD 1 TO WS-AUTO-COUNT
           ADD WS-TOTAL-DUE TO WS-TOT-AUTOPAY
           MOVE SPACES TO WS-DTL
           MOVE LOAN-NUMBER        TO WS-D-LOAN
           MOVE LOAN-PRODUCT-CODE  TO WS-D-PROD
           MOVE LOAN-NEXT-DUE-DATE TO WS-D-DUE
           MOVE ZERO TO WS-D-INST WS-D-INT WS-D-PRIN WS-D-PAST
                        WS-D-FEES
           MOVE WS-TOTAL-DUE       TO WS-D-TOTAL
           STRING 'AUTOPAY DEBIT ' DELIMITED BY SIZE
                  LOAN-AUTOPAY-ACCT DELIMITED BY SIZE
             INTO WS-D-NOTE
           END-STRING
           WRITE REPORT-LINE FROM WS-DTL.

      *----------------------------------------------------------------*
       8800-DAYS-BETWEEN.
           MOVE 'DIFF' TO DTU-FUNCTION
           CALL 'DATEUTIL' USING DATE-PARMS.

       9000-TERMINATE.
           MOVE WS-READ-COUNT  TO WS-T-READ
           MOVE WS-BILL-COUNT  TO WS-T-BILLS
           MOVE WS-TOT-BILLED  TO WS-T-BILLED
           MOVE WS-AUTO-COUNT  TO WS-T-AUTO
           MOVE WS-TOT-AUTOPAY TO WS-T-AUTO-AMT
           WRITE REPORT-LINE FROM WS-TRL
           CLOSE LOAN-FILE BILL-FILE AUTOPAY-FILE REPORT-FILE
           DISPLAY 'LNBILL  READ=' WS-READ-COUNT
                   ' BILLS=' WS-BILL-COUNT
                   ' AUTOPAY=' WS-AUTO-COUNT
                   ' PMT-RESETS=' WS-PMT-RESET-COUNT
           MOVE 0 TO RETURN-CODE.

       9900-ABEND.
           MOVE 'LNBILL' TO ABND-PROGRAM
           MOVE '2000-PROCESS-LOAN' TO ABND-PARAGRAPH
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
