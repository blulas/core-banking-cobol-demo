      *================================================================*
      * PROGRAM  : AMORTCLC                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : LOAN AMORTIZATION.                                  *
      *            PMT  - LEVEL PAYMENT  P * R / (1 - (1 + R) ** -N)   *
      *                   ROUNDED UP TO THE NEXT CENT, PLUS THE FINAL  *
      *                   (ADJUSTED) PAYMENT, BALLOON AND TOTAL        *
      *                   INTEREST FOR THE WHOLE SCHEDULE.             *
      *            LINE - SCHEDULE LINE FOR PERIOD K (INTEREST,        *
      *                   PRINCIPAL, ENDING BALANCE).                  *
      *            MAXP - LARGEST PRINCIPAL A GIVEN PAYMENT SUPPORTS   *
      *                   (USED FOR CONSUMER COUNTEROFFERS).           *
      *            PERIODIC RATE BY ACCRUAL BASIS:                     *
      *              ACTUAL/365 AND 30/360 : ANNUAL / 12 PER MONTH     *
      *              ACTUAL/360            : ANNUAL * 365/360 / 12     *
      *            INTEREST-ONLY MONTHS (CRE) PRECEDE AMORTIZATION;    *
      *            WHEN AMORTIZATION RUNS PAST MATURITY THE REMAINING  *
      *            BALANCE IS DUE AS A BALLOON AT MATURITY.            *
      * LINKAGE  : AMRTPARM                                            *
      * CALLED BY: LNBILL, LDLOAN, LNORIG, CRDECIDE                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - COMMERCIAL LOAN SYSTEM              *
      * 1993-02-11 RJT  ROUND PAYMENT UP TO NEXT CENT PER LOAN OPS     *
      *                 (FINAL PAYMENT ABSORBS THE DIFFERENCE)         *
      * 1998-11-02 MKL  Y2K REVIEW - NO DATE FIELDS                    *
      * 2003-01-20 BHS  CRE - INTEREST ONLY PERIOD AND BALLOON         *
      * 2003-03-14 BHS  ACTUAL/360 PERIODIC RATE USES 365/360 - CHG0388*
      * 2007-06-05 BHS  REPLACED ** OPERATOR WITH LOOP - S0C7/PRECISION*
      *                 ON LONG CRE TERMS (INC10422)                   *
      * 2016-04-25 AKW  MAXP FUNCTION FOR LPER COUNTEROFFER            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AMORTCLC.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RATE-WORK.
           05  WS-MONTHS-PER-PERIOD        PIC 9(02) COMP.
           05  WS-ONE-PLUS-R               PIC 9(03)V9(15) COMP-3.
           05  WS-FACTOR                   PIC 9(07)V9(15) COMP-3.
           05  WS-PMT-RAW                  PIC S9(13)V9(08) COMP-3.
           05  WS-PMT-CENTS                PIC S9(13)V99 COMP-3.
           05  WS-MAXP-RAW                 PIC S9(13)V9(08) COMP-3.
       01  WS-COUNTS.
           05  WS-IO-PERIODS               PIC 9(03) COMP.
           05  WS-I                        PIC 9(03) COMP.
           05  WS-LAST-PERIOD              PIC 9(03) COMP.
       01  WS-SCHEDULE-WORK.
           05  WS-BAL                      PIC S9(13)V99 COMP-3.
           05  WS-INT                      PIC S9(13)V99 COMP-3.
           05  WS-PRIN                     PIC S9(13)V99 COMP-3.
           05  WS-PAY                      PIC S9(13)V99 COMP-3.
           05  WS-TYPE                     PIC X(01).
           05  WS-TOT-INT                  PIC S9(13)V99 COMP-3.
       01  WS-BALLOON-FLAG                 PIC X(01).
           88  WS-HAS-BALLOON                        VALUE 'Y'.

       LINKAGE SECTION.
       COPY AMRTPARM.

       PROCEDURE DIVISION USING AMORT-PARMS.
       0000-MAIN.
           MOVE 00   TO AMT-RETURN-CODE
           MOVE ZERO TO AMT-PERIODIC-RATE AMT-NUM-PAYMENTS
                        AMT-NUM-AMORT-PAYMENTS AMT-IO-PAYMENT-AMT
                        AMT-FINAL-PAYMENT-AMT AMT-BALLOON-AMT
                        AMT-TOTAL-INTEREST AMT-MAX-PRINCIPAL
                        AMT-LINE-BEGIN-BAL AMT-LINE-PAYMENT
                        AMT-LINE-INTEREST AMT-LINE-PRINCIPAL
                        AMT-LINE-END-BAL
           MOVE SPACE TO AMT-LINE-TYPE

           PERFORM 1000-VALIDATE
           IF AMT-INVALID-INPUT
               GOBACK
           END-IF
           PERFORM 1100-SET-PERIODIC-RATE
           PERFORM 1200-SET-PERIOD-COUNTS

           EVALUATE TRUE
               WHEN AMT-CALC-PAYMENT
                   PERFORM 2000-CALC-PAYMENT
                   PERFORM 2500-RUN-SCHEDULE
               WHEN AMT-CALC-LINE
                   PERFORM 2000-CALC-PAYMENT
                   PERFORM 3000-SCHEDULE-LINE
               WHEN AMT-CALC-MAX-PRINCIPAL
                   PERFORM 4000-MAX-PRINCIPAL
               WHEN OTHER
                   MOVE 12 TO AMT-RETURN-CODE
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------*
       1000-VALIDATE.
           EVALUATE TRUE
               WHEN AMT-TERM-MONTHS = ZERO
               WHEN AMT-ANNUAL-RATE > 100
               WHEN NOT AMT-MONTHLY AND NOT AMT-QUARTERLY
               WHEN AMT-IO-MONTHS NOT < AMT-TERM-MONTHS
                   MOVE 12 TO AMT-RETURN-CODE
               WHEN AMT-CALC-MAX-PRINCIPAL
                    AND AMT-PAYMENT-AMT NOT > ZERO
                   MOVE 12 TO AMT-RETURN-CODE
               WHEN NOT AMT-CALC-MAX-PRINCIPAL
                    AND AMT-PRINCIPAL NOT > ZERO
                   MOVE 12 TO AMT-RETURN-CODE
               WHEN AMT-CALC-LINE AND AMT-PERIOD-NUMBER = ZERO
                   MOVE 12 TO AMT-RETURN-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PERIODIC RATE. ACTUAL/360 LOANS EARN 365/360 OF THE NOMINAL    *
      * RATE OVER A YEAR, SO THE PERIODIC RATE IS GROSSED UP (CHG0388) *
      *----------------------------------------------------------------*
       1100-SET-PERIODIC-RATE.
           IF AMT-QUARTERLY
               MOVE 3 TO WS-MONTHS-PER-PERIOD
           ELSE
               MOVE 1 TO WS-MONTHS-PER-PERIOD
           END-IF
           EVALUATE TRUE
               WHEN AMT-ACTUAL-360
                   COMPUTE AMT-PERIODIC-RATE ROUNDED =
                       AMT-ANNUAL-RATE / 100 * 365 / 360
                       * WS-MONTHS-PER-PERIOD / 12
               WHEN AMT-ACTUAL-365
               WHEN AMT-30-360
                   COMPUTE AMT-PERIODIC-RATE ROUNDED =
                       AMT-ANNUAL-RATE / 100
                       * WS-MONTHS-PER-PERIOD / 12
               WHEN OTHER
      *            UNKNOWN BASIS - TREAT AS 30/360 (LEGACY DEFAULT)
                   COMPUTE AMT-PERIODIC-RATE ROUNDED =
                       AMT-ANNUAL-RATE / 100
                       * WS-MONTHS-PER-PERIOD / 12
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-SET-PERIOD-COUNTS.
           COMPUTE AMT-NUM-PAYMENTS =
               AMT-TERM-MONTHS / WS-MONTHS-PER-PERIOD
           COMPUTE WS-IO-PERIODS =
               AMT-IO-MONTHS / WS-MONTHS-PER-PERIOD
           IF AMT-AMORT-MONTHS = ZERO
               COMPUTE AMT-NUM-AMORT-PAYMENTS =
                   AMT-NUM-PAYMENTS - WS-IO-PERIODS
           ELSE
               COMPUTE AMT-NUM-AMORT-PAYMENTS =
                   AMT-AMORT-MONTHS / WS-MONTHS-PER-PERIOD
           END-IF
           IF WS-IO-PERIODS + AMT-NUM-AMORT-PAYMENTS > AMT-NUM-PAYMENTS
               SET WS-HAS-BALLOON TO TRUE
           ELSE
               MOVE 'N' TO WS-BALLOON-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LEVEL PAYMENT, ROUNDED UP TO THE NEXT CENT (1993 LOAN OPS)     *
      *----------------------------------------------------------------*
       2000-CALC-PAYMENT.
           COMPUTE AMT-IO-PAYMENT-AMT ROUNDED =
               AMT-PRINCIPAL * AMT-PERIODIC-RATE
           IF AMT-PERIODIC-RATE = ZERO
               COMPUTE WS-PMT-RAW =
                   AMT-PRINCIPAL / AMT-NUM-AMORT-PAYMENTS
           ELSE
               PERFORM 8000-COMPUTE-FACTOR
               COMPUTE WS-PMT-RAW =
                   AMT-PRINCIPAL * AMT-PERIODIC-RATE * WS-FACTOR
                   / (WS-FACTOR - 1)
           END-IF
           MOVE WS-PMT-RAW TO WS-PMT-CENTS
           IF WS-PMT-RAW > WS-PMT-CENTS
               ADD 0.01 TO WS-PMT-CENTS
           END-IF
      *    1993-02 RJT WAS: COMPUTE AMT-PAYMENT-AMT ROUNDED = WS-PMT-RAW
           MOVE WS-PMT-CENTS TO AMT-PAYMENT-AMT.

      *----------------------------------------------------------------*
      * WALK THE WHOLE SCHEDULE FOR FINAL PAYMENT / BALLOON / TOTALS   *
      *----------------------------------------------------------------*
       2500-RUN-SCHEDULE.
           MOVE AMT-PRINCIPAL TO WS-BAL
           MOVE ZERO TO WS-TOT-INT
           MOVE AMT-NUM-PAYMENTS TO WS-LAST-PERIOD
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-LAST-PERIOD OR WS-BAL = ZERO
               PERFORM 3100-ONE-PERIOD
           END-PERFORM
           MOVE WS-TOT-INT TO AMT-TOTAL-INTEREST
           IF WS-TYPE = 'B'
               MOVE WS-PAY TO AMT-BALLOON-AMT
           END-IF
           MOVE WS-PAY TO AMT-FINAL-PAYMENT-AMT.

      *----------------------------------------------------------------*
      * SCHEDULE LINE FOR PERIOD K                                     *
      *----------------------------------------------------------------*
       3000-SCHEDULE-LINE.
           IF AMT-PERIOD-NUMBER > AMT-NUM-PAYMENTS
               MOVE 12 TO AMT-RETURN-CODE
           ELSE
               MOVE AMT-PRINCIPAL TO WS-BAL
               MOVE ZERO TO WS-TOT-INT
               MOVE AMT-PERIOD-NUMBER TO WS-LAST-PERIOD
               PERFORM VARYING WS-I FROM 1 BY 1
                       UNTIL WS-I > WS-LAST-PERIOD
                   MOVE WS-BAL TO AMT-LINE-BEGIN-BAL
                   PERFORM 3100-ONE-PERIOD
               END-PERFORM
               MOVE WS-TYPE  TO AMT-LINE-TYPE
               MOVE WS-PAY   TO AMT-LINE-PAYMENT
               MOVE WS-INT   TO AMT-LINE-INTEREST
               MOVE WS-PRIN  TO AMT-LINE-PRINCIPAL
               MOVE WS-BAL   TO AMT-LINE-END-BAL
               MOVE WS-TOT-INT TO AMT-TOTAL-INTEREST
           END-IF.

      *----------------------------------------------------------------*
      * ONE PERIOD OF THE SCHEDULE (PERIOD WS-I, BALANCE WS-BAL)       *
      *----------------------------------------------------------------*
       3100-ONE-PERIOD.
           COMPUTE WS-INT ROUNDED = WS-BAL * AMT-PERIODIC-RATE
           EVALUATE TRUE
               WHEN WS-BAL = ZERO
                   MOVE ZERO TO WS-INT WS-PRIN WS-PAY
                   MOVE 'F' TO WS-TYPE
               WHEN WS-I NOT > WS-IO-PERIODS
                   MOVE 'I'     TO WS-TYPE
                   MOVE WS-INT  TO WS-PAY
                   MOVE ZERO    TO WS-PRIN
               WHEN WS-I = AMT-NUM-PAYMENTS
                   IF WS-HAS-BALLOON
                       MOVE 'B' TO WS-TYPE
                   ELSE
                       MOVE 'F' TO WS-TYPE
                   END-IF
                   MOVE WS-BAL TO WS-PRIN
                   COMPUTE WS-PAY = WS-BAL + WS-INT
               WHEN AMT-PAYMENT-AMT - WS-INT NOT < WS-BAL
      *            ROUND-UP EXHAUSTED THE BALANCE EARLY
                   MOVE 'F' TO WS-TYPE
                   MOVE WS-BAL TO WS-PRIN
                   COMPUTE WS-PAY = WS-BAL + WS-INT
               WHEN OTHER
                   MOVE 'A' TO WS-TYPE
                   MOVE AMT-PAYMENT-AMT TO WS-PAY
                   COMPUTE WS-PRIN = AMT-PAYMENT-AMT - WS-INT
           END-EVALUATE
           SUBTRACT WS-PRIN FROM WS-BAL
           ADD WS-INT TO WS-TOT-INT.

      *----------------------------------------------------------------*
      * MAXIMUM PRINCIPAL FOR A PAYMENT: PMT * (1 - (1+R)**-N) / R     *
      * TRUNCATED TO THE CENT (NEVER OFFER MORE THAN THE PAYMENT BUYS) *
      *----------------------------------------------------------------*
       4000-MAX-PRINCIPAL.
           IF AMT-PERIODIC-RATE = ZERO
               COMPUTE WS-MAXP-RAW =
                   AMT-PAYMENT-AMT * AMT-NUM-AMORT-PAYMENTS
           ELSE
               PERFORM 8000-COMPUTE-FACTOR
               COMPUTE WS-MAXP-RAW =
                   AMT-PAYMENT-AMT * (WS-FACTOR - 1)
                   / (AMT-PERIODIC-RATE * WS-FACTOR)
           END-IF
           MOVE WS-MAXP-RAW TO AMT-MAX-PRINCIPAL.

      *----------------------------------------------------------------*
      * (1 + R) ** N BY REPEATED MULTIPLICATION (INC10422)             *
      *----------------------------------------------------------------*
       8000-COMPUTE-FACTOR.
           COMPUTE WS-ONE-PLUS-R = 1 + AMT-PERIODIC-RATE
           MOVE 1 TO WS-FACTOR
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > AMT-NUM-AMORT-PAYMENTS
               COMPUTE WS-FACTOR ROUNDED = WS-FACTOR * WS-ONE-PLUS-R
           END-PERFORM.
