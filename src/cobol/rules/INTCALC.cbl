      *================================================================*
      * PROGRAM  : INTCALC                                             *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : COMPUTES ONE CALENDAR DAY OF DEPOSIT INTEREST.      *
      *              ACCRUAL = PRINCIPAL * RATE / 100 / DAY BASIS      *
      *            - ACTUAL/365 USES 366 IN A LEAP YEAR (DATEUTIL DIYR)*
      *            - ACTUAL/360 ALWAYS 360                             *
      *            - 30/360: THE 31ST ACCRUES NOTHING; THE LAST DAY OF *
      *              FEBRUARY ACCRUES THE MISSING DAYS UP TO DAY 30    *
      *            - DAILY COMPOUNDING ADDS UNPAID ACCRUED INTEREST TO *
      *              THE PRINCIPAL                                     *
      *            - ZERO / NEGATIVE BALANCES ACCRUE NOTHING           *
      *            - MINIMUM BALANCE TO EARN: SEE 2000-MIN-BALANCE     *
      *            RESULT IS CARRIED AT 6 DECIMALS AND TRUNCATED, NOT  *
      *            ROUNDED (CONVERSION SPEC CNV-0042, 1988).           *
      * LINKAGE  : INTPARM                                             *
      * CALLS    : DATEUTIL                                            *
      * CALLED BY: INTACCR, CDQUOTE (CB21)                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL - ACTUAL/365 ONLY                     *
      * 1996-02-29 RJT  LEAP YEAR USES 366 - INC00318                  *
      * 2003-05-09 BHS  ACTUAL/360 AND 30/360 FOR INSTITUTIONAL MMKT   *
      * 2011-01-10 DPS  MMKT BELOW MINIMUM EARNS REDUCED RATE CHG0644  *
      * 2017-02-08 AKW  DAILY COMPOUNDING FOR HYSV                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTCALC.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY DATEPARM.

       01  WS-WORK-FIELDS.
           05  WS-DAY-FACTOR               PIC 9(02).
           05  WS-DATE-X                   PIC 9(08).
           05  WS-DATE-R REDEFINES WS-DATE-X.
               10  WS-CCYY                 PIC 9(04).
               10  WS-MM                   PIC 9(02).
               10  WS-DD                   PIC 9(02).

      *    MINIMUM-BALANCE-TO-EARN CONSTANTS (SEE 2000-MIN-BALANCE)
       01  WS-MIN-BAL-RULES.
           05  WS-MMKT-MIN-BAL             PIC 9(07)V99 VALUE 2500.00.
           05  WS-MMKT-REDUCED-RATE        PIC 9(03)V9(06)
                                           VALUE 0.100000.
           05  WS-SVST-MIN-BAL             PIC 9(07)V99 VALUE 300.00.

       LINKAGE SECTION.
       COPY INTPARM.

       PROCEDURE DIVISION USING INTCALC-PARMS.
       0000-MAIN.
           MOVE ZERO   TO INT-DAILY-ACCRUAL INT-DAY-BASIS
                          INT-PRINCIPAL-USED INT-RETURN-CODE
           MOVE INT-ANNUAL-RATE TO INT-APPLIED-RATE
           SET INT-RSN-ACCRUED TO TRUE
           MOVE 1 TO WS-DAY-FACTOR

           IF NOT INT-BASIS-ACT-365 AND NOT INT-BASIS-ACT-360
              AND NOT INT-BASIS-30-360
               MOVE 12 TO INT-RETURN-CODE
               GOBACK
           END-IF

           IF INT-BALANCE NOT > ZERO
               SET INT-RSN-NEGATIVE TO TRUE
               MOVE ZERO TO INT-APPLIED-RATE
               MOVE 04 TO INT-RETURN-CODE
               GOBACK
           END-IF

           PERFORM 2000-MIN-BALANCE
           IF INT-RETURN-CODE NOT = 00
               GOBACK
           END-IF

           PERFORM 3000-DAY-BASIS
           IF INT-RETURN-CODE NOT = 00
               GOBACK
           END-IF

           PERFORM 4000-COMPUTE-ACCRUAL
           GOBACK.

      *----------------------------------------------------------------*
      * MINIMUM BALANCE TO EARN (END-OF-DAY LEDGER BALANCE)            *
      *   MMKT : BELOW $2,500 THE WHOLE BALANCE EARNS 0.10% THAT DAY   *
      *   SVST : BELOW $300 NO INTEREST THAT DAY                       *
      *----------------------------------------------------------------*
       2000-MIN-BALANCE.
           EVALUATE INT-PRODUCT-CODE
               WHEN 'MMKT'
                   IF INT-BALANCE < WS-MMKT-MIN-BAL
                      AND INT-APPLIED-RATE > WS-MMKT-REDUCED-RATE
                       MOVE WS-MMKT-REDUCED-RATE TO INT-APPLIED-RATE
                       SET INT-RSN-REDUCED-RATE TO TRUE
                   END-IF
               WHEN 'SVST'
                   IF INT-BALANCE < WS-SVST-MIN-BAL
                       MOVE ZERO TO INT-APPLIED-RATE
                       SET INT-RSN-BELOW-MIN TO TRUE
                       MOVE 04 TO INT-RETURN-CODE
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
       3000-DAY-BASIS.
           MOVE INT-ACCRUAL-DATE TO WS-DATE-X
           EVALUATE TRUE
               WHEN INT-BASIS-ACT-365
                   INITIALIZE DATE-PARMS
                   SET DTU-DAYS-IN-YEAR TO TRUE
                   MOVE INT-ACCRUAL-DATE TO DTU-DATE-1
                   CALL 'DATEUTIL' USING DATE-PARMS
                   IF NOT DTU-OK
                       MOVE 12 TO INT-RETURN-CODE
                   ELSE
                       MOVE DTU-RESULT-NUMBER TO INT-DAY-BASIS
                   END-IF
               WHEN INT-BASIS-ACT-360
                   MOVE 360 TO INT-DAY-BASIS
               WHEN INT-BASIS-30-360
                   MOVE 360 TO INT-DAY-BASIS
                   PERFORM 3100-30-360-DAY-FACTOR
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 30/360: EVERY MONTH HAS 30 DAYS. DAY 31 EARNS NOTHING; FEB 28  *
      * (29 IN A LEAP YEAR) EARNS 3 (2) DAYS SO THE MONTH TOTALS 30.   *
      *----------------------------------------------------------------*
       3100-30-360-DAY-FACTOR.
           IF WS-DD = 31
               MOVE 0 TO WS-DAY-FACTOR
               SET INT-RSN-DAY-31 TO TRUE
               MOVE 04 TO INT-RETURN-CODE
           ELSE
               IF WS-MM = 2
                   INITIALIZE DATE-PARMS
                   SET DTU-END-OF-MONTH TO TRUE
                   MOVE INT-ACCRUAL-DATE TO DTU-DATE-1
                   CALL 'DATEUTIL' USING DATE-PARMS
                   IF DTU-RESULT-DATE = INT-ACCRUAL-DATE
                       COMPUTE WS-DAY-FACTOR = 30 - WS-DD + 1
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * NO ROUNDED - TRUNCATE AT 6 DECIMALS                            *
      *----------------------------------------------------------------*
       4000-COMPUTE-ACCRUAL.
           MOVE INT-BALANCE TO INT-PRINCIPAL-USED
           IF INT-COMPOUND-DAILY AND INT-ACCRUED-TO-DATE > ZERO
               ADD INT-ACCRUED-TO-DATE TO INT-PRINCIPAL-USED
           END-IF
           COMPUTE INT-DAILY-ACCRUAL =
                   (INT-PRINCIPAL-USED * INT-APPLIED-RATE
                    * WS-DAY-FACTOR) / (100 * INT-DAY-BASIS).
