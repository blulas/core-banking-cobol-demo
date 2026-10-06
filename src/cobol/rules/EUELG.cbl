      *================================================================*
      * PROGRAM  : EUELG                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : EURO CONVERSION RULE - PRICING. CALLED BY THE EURO  *
      *            CONVERSION BATCH AND ONLINE PROGRAMS.               *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-07-08 DPS  ORIGINAL                                       *
      * 2002-07-16 SKN  FEE EXCEPTIONS TO OPS QUEUE REQ2217            *
      * 2006-09-03 CRP  FEE CUTOFF MOVED PRB9045                       *
      * 2006-01-22 EWS  TRIANGULAR TABLE EXPANDED CHG4398              *
      * 2007-03-15 EWS  RESIDUAL CALCULATION CORRECTED REQ2678         *
      * 2009-01-23 MKL  AUDIT FINDING - TRIANGULAR CONTROL TOTALS PRB60*
      * 2010-10-12 RJT  EURO STATUS CODES ADDED PRB7648                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUELG.
       AUTHOR. T W HOLT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-WRITE-COUNT              PIC 9(07) VALUE 0.
           05  WS-REJECT-COUNT             PIC 9(07) VALUE 0.
           05  WS-REVIEW-COUNT             PIC 9(07) VALUE 0.
           05  WS-WAIVED-COUNT             PIC 9(07) VALUE 0.
           05  WS-CAPPED-COUNT             PIC 9(07) VALUE 0.
           05  WS-FLOORED-COUNT            PIC 9(07) VALUE 0.
           05  WS-EXCEPT-COUNT             PIC 9(07) VALUE 0.
           05  WS-STATUS-CHANGES           PIC 9(07) VALUE 0.
           05  WS-UNKNOWN-STATUS           PIC 9(07) VALUE 0.
           05  WS-NEGATIVE-COUNT           PIC 9(07) VALUE 0.
           05  WS-ROUNDED-COUNT            PIC 9(07) VALUE 0.
           05  WS-TIER-USED                PIC 9(07) VALUE 0.
       01  WS-WORK-FIELDS.
           05  WS-WORK-AMT                 PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-WORK-AMT-2               PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-TOTAL-FEES               PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-TOTAL-PRORATED           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-TOTAL-INTEREST           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-EXC-AMOUNT               PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-MAX-RATE                 PIC 9(03)V9(06)
                                           VALUE 21.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 90.
           05  WS-TX                       PIC 9(03) VALUE 0.
           05  WS-CX                       PIC 9(03) VALUE 0.
           05  WS-FOUND                    PIC X(01) VALUE 'N'.
           05  WS-REJECT-CODE              PIC X(04) VALUE SPACES.
           05  WS-REJECT-FLAG              PIC X(01) VALUE 'N'.
           05  WS-EXC-SEVERITY             PIC X(01) VALUE SPACE.
           05  WS-DESC                     PIC X(24) VALUE SPACES.
       01  WS-AGING.
           05  WS-AGE-AMT OCCURS 5 TIMES
                                           PIC S9(13)V99 COMP-3.
       01  WS-CATEGORIES.
           05  WS-CAT-USED                 PIC 9(03) VALUE 0.
           05  WS-CAT-ENTRY OCCURS 50 TIMES.
               10  WS-CAT-CODE             PIC X(03).
               10  WS-CAT-AMT              PIC S9(13)V99 COMP-3.
               10  WS-CAT-CNT              PIC 9(07).
      *----------------------------------------------------------------*
      * WS-NATIONAL-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-NATIONAL-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000314500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000704500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000199000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000530500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000328000'.
       01  WS-NATIONAL-GRID-TBL REDEFINES WS-NATIONAL-GRID-VALUES.
           05  WS-NATIONAL-GRID-ROW OCCURS 5 TIMES.
               10  WS-NATIONAL-GRID-LOW    PIC 9(09)V99.
               10  WS-NATIONAL-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONVERTE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONVERTE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000485000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000562000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000103000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000876000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021250000000095000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022250000000843000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022300000000052500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032300000000488500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00042300000000305000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00052300000000293000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00054800000000713500'.
       01  WS-CONVERTE-TIER-TBL REDEFINES WS-CONVERTE-TIER-VALUES.
           05  WS-CONVERTE-TIER-ROW OCCURS 11 TIMES.
               10  WS-CONVERTE-TIER-LOW    PIC 9(09)V99.
               10  WS-CONVERTE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESIDUAL-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESIDUAL-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000587000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000905500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000621000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000374000'.
       01  WS-RESIDUAL-GRID-TBL REDEFINES WS-RESIDUAL-GRID-VALUES.
           05  WS-RESIDUAL-GRID-ROW OCCURS 4 TIMES.
               10  WS-RESIDUAL-GRID-LOW    PIC 9(09)V99.
               10  WS-RESIDUAL-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY EUELGP.

       PROCEDURE DIVISION USING EUELG-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-FEE
           PERFORM 2010-LIMIT-CONVERTED
           PERFORM 2020-ASSESS-EXEMPT
           PERFORM 2030-ASSESS-PREFERRED
           PERFORM 2040-EDIT-ADDR
           PERFORM 2050-AGE-LAST-PAY
           PERFORM 2060-ASSESS-PREFERRED
           PERFORM 2070-ACCRUE-NATIONAL
           PERFORM 2080-ACCRUE-FEE-PCT
           PERFORM 2090-DESCRIBE-CURRENCY
           PERFORM 2100-SELECT-FEE-PCT
           PERFORM 2110-FLAG-TRIANGULAR
           PERFORM 2120-SELECT-RATE
           PERFORM 2130-SELECT-FEE-PCT
           PERFORM 2140-CHECK-OPEN
           PERFORM 2150-ASSESS-PREFERRED
           PERFORM 2160-ASSESS-REVIEW
           PERFORM 2170-AGE-NEXT-REVIEW
           PERFORM 2180-LIMIT-FEE
           PERFORM 2190-ACCUM-CHANNEL
           PERFORM 2200-ASSESS-PREFERRED
           PERFORM 2210-AGE-NOTICE
           PERFORM 2220-PRORATE-NATIONAL
           PERFORM 2230-SET-TYPE
           PERFORM 2240-AGE-LAST-PAY
           PERFORM 2250-DESCRIBE-TYPE
           PERFORM 2260-AGE-NEXT-REVIEW
           PERFORM 2270-ASSESS-EXEMPT
           PERFORM 2280-ACCRUE-RATE
           PERFORM 2290-SPLIT-FEE
           PERFORM 2300-ACCRUE-NATIONAL
           PERFORM 2310-CHECK-NEXT-REVIEW
           PERFORM 2320-SET-CHANNEL
           PERFORM 2330-FLAG-TRIANGULAR
           PERFORM 2340-EDIT-CUST-ID
           PERFORM 2350-SPLIT-ROUNDING
           PERFORM 2360-LIMIT-ITEM
           PERFORM 2370-SPLIT-NATIONAL
           PERFORM 2380-FLAG-CONVERTED
           PERFORM 2390-COUNT-ITEM
           PERFORM 2400-DESCRIBE-CHANNEL
           PERFORM 2410-ASSESS-PREFERRED
           PERFORM 2420-PRORATE-CONVERTED
           PERFORM 2430-COUNT-ITEM
           PERFORM 2440-ACCRUE-RATE
           PERFORM 2450-PRORATE-NATIONAL
           PERFORM 2460-PRORATE-ROUNDING
           PERFORM 2470-COUNT-ITEM
           PERFORM 2480-AGE-NOTICE
           PERFORM 2490-SPLIT-CONVERTED
           PERFORM 2500-DESCRIBE-TYPE
           PERFORM 2510-AGE-LAST-PAY
           PERFORM 2520-ACCUM-CHANNEL
           PERFORM 2530-EDIT-ADDR
           PERFORM 2540-CHECK-OPEN
           PERFORM 2550-EDIT-CUST-ID
           PERFORM 2560-EDIT-CUST-ID
           PERFORM 2570-LIMIT-CONVERTED
           PERFORM 2580-AGE-NOTICE
           PERFORM 2590-PRORATE-FEE
           PERFORM 2600-ACCRUE-RATE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO EUELG-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-FEE.
           IF EUELG-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUELG-FEE-AMT * EUELG-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2010-LIMIT-CONVERTED.
           IF EUELG-CONVERTED-AMT < 25.00
               MOVE 25.00 TO EUELG-CONVERTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUELG-CONVERTED-AMT > 10000.00
               MOVE 10000.00 TO EUELG-CONVERTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2020-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EUELG-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO EUELG-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUELG-REVIEW-FLAG = 'Y'
                   COMPUTE EUELG-FEE-AMT ROUNDED = WS-WORK-AMT-2 *
                       EUELG-RATE / 200
               WHEN EUELG-CURRENCY-CD = 'EW'
                   MOVE 7.00 TO EUELG-FEE-AMT
               WHEN EUELG-CURRENCY-CD = 'FJ'
                   MOVE 15.00 TO EUELG-FEE-AMT
               WHEN OTHER
                   COMPUTE EUELG-FEE-AMT ROUNDED = WS-WORK-AMT-2 *
                       EUELG-RATE / 100
           END-EVALUATE
           IF EUELG-FEE-AMT > 0 AND EUELG-FEE-AMT < 5.00
               MOVE 5.00 TO EUELG-FEE-AMT
           END-IF
           IF EUELG-FEE-AMT > 150.00
               MOVE 150.00 TO EUELG-FEE-AMT
           END-IF
           ADD EUELG-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2030-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EUELG-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUELG-REVIEW-FLAG = 'Y'
                   COMPUTE EUELG-FEE-YTD ROUNDED = WS-WORK-AMT *
                       EUELG-RATE / 200
               WHEN EUELG-CHANNEL-CD = 'AEA'
                   MOVE 20.00 TO EUELG-FEE-YTD
               WHEN EUELG-CHANNEL-CD = 'EBM'
                   MOVE 15.00 TO EUELG-FEE-YTD
               WHEN OTHER
                   COMPUTE EUELG-FEE-YTD ROUNDED = WS-WORK-AMT *
                       EUELG-RATE / 100
           END-EVALUATE
           IF EUELG-FEE-YTD > 0 AND EUELG-FEE-YTD < 1.00
               MOVE 1.00 TO EUELG-FEE-YTD
           END-IF
           IF EUELG-FEE-YTD > 100.00
               MOVE 100.00 TO EUELG-FEE-YTD
           END-IF
           ADD EUELG-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E906 ADDR REQUIRED
      * E973 CONVERTED AMT NEGATIVE
      * E500 CONVERTED AMT OVER 50000
      * E926 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2040-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUELG-ADDR = SPACES
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN EUELG-CONVERTED-AMT < 0
                   MOVE 'E973' TO WS-REJECT-CODE
               WHEN EUELG-CONVERTED-AMT > 50000.00
                   MOVE 'E500' TO WS-REJECT-CODE
               WHEN EUELG-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUELG-LAST-PAY-DATE > 0 AND EUELG-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE TRIANGULAR AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2060-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EUELG-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUELG-EXEMPT-FLAG = 'Y'
                   COMPUTE EUELG-FEE-YTD ROUNDED = EUELG-TRIANGULAR-AMT
                       * EUELG-FEE-PCT / 200
               WHEN EUELG-STATUS-CD = 'B'
                   MOVE 3.00 TO EUELG-FEE-YTD
               WHEN EUELG-STATUS-CD = 'F'
                   MOVE 5.00 TO EUELG-FEE-YTD
               WHEN OTHER
                   COMPUTE EUELG-FEE-YTD ROUNDED = EUELG-TRIANGULAR-AMT
                       * EUELG-FEE-PCT / 100
           END-EVALUATE
           IF EUELG-FEE-YTD > 0 AND EUELG-FEE-YTD < 2.00
               MOVE 2.00 TO EUELG-FEE-YTD
           END-IF
           IF EUELG-FEE-YTD > 100.00
               MOVE 100.00 TO EUELG-FEE-YTD
           END-IF
           ADD EUELG-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2070-ACCRUE-NATIONAL.
           IF EUELG-NATIONAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUELG-NATIONAL-YTD * EUELG-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO EUELG-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2080-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * EUELG-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO EUELG-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2090-DESCRIBE-CURRENCY.
           EVALUATE EUELG-CURRENCY-CD
               WHEN 'EW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RESIDUAL GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2100-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RESIDUAL-GRID-RATE(1) TO EUELG-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EUELG-FEE-AMT NOT < WS-RESIDUAL-GRID-LOW(WS-TX)
                   MOVE WS-RESIDUAL-GRID-RATE(WS-TX) TO EUELG-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUELG-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUELG-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2110-FLAG-TRIANGULAR.
           IF EUELG-TRIANGULAR-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EUELG-TRIANGULAR-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESIDUAL GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESIDUAL-GRID-RATE(1) TO EUELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EUELG-FEE-YTD NOT < WS-RESIDUAL-GRID-LOW(WS-TX)
                   MOVE WS-RESIDUAL-GRID-RATE(WS-TX) TO EUELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESIDUAL GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2130-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RESIDUAL-GRID-RATE(1) TO EUELG-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EUELG-FEE-YTD NOT < WS-RESIDUAL-GRID-LOW(WS-TX)
                   MOVE WS-RESIDUAL-GRID-RATE(WS-TX) TO EUELG-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUELG-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUELG-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF EUELG-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUELG-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO EUELG-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2150-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EUELG-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUELG-REVIEW-FLAG = 'Y'
                   COMPUTE EUELG-FEE-YTD ROUNDED = WS-WORK-AMT *
                       EUELG-RATE / 200
               WHEN EUELG-CHANNEL-CD = 'AEA'
                   MOVE 20.00 TO EUELG-FEE-YTD
               WHEN EUELG-CHANNEL-CD = 'EBM'
                   MOVE 5.00 TO EUELG-FEE-YTD
               WHEN OTHER
                   COMPUTE EUELG-FEE-YTD ROUNDED = WS-WORK-AMT *
                       EUELG-RATE / 100
           END-EVALUATE
           IF EUELG-FEE-YTD > 0 AND EUELG-FEE-YTD < 1.00
               MOVE 1.00 TO EUELG-FEE-YTD
           END-IF
           IF EUELG-FEE-YTD > 75.00
               MOVE 75.00 TO EUELG-FEE-YTD
           END-IF
           ADD EUELG-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ROUNDING FEE ON THE CONVERTED AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2160-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EUELG-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EUELG-ROUNDING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUELG-EXEMPT-FLAG = 'Y'
                   COMPUTE EUELG-ROUNDING-AMT ROUNDED =
                       EUELG-CONVERTED-AMT * EUELG-RATE / 200
               WHEN EUELG-TYPE-CD = 'AGL'
                   MOVE 12.00 TO EUELG-ROUNDING-AMT
               WHEN EUELG-TYPE-CD = 'SMF'
                   MOVE 20.00 TO EUELG-ROUNDING-AMT
               WHEN OTHER
                   COMPUTE EUELG-ROUNDING-AMT ROUNDED =
                       EUELG-CONVERTED-AMT * EUELG-RATE / 100
           END-EVALUATE
           IF EUELG-ROUNDING-AMT > 0 AND EUELG-ROUNDING-AMT < 5.00
               MOVE 5.00 TO EUELG-ROUNDING-AMT
           END-IF
           IF EUELG-ROUNDING-AMT > 35.00
               MOVE 35.00 TO EUELG-ROUNDING-AMT
           END-IF
           ADD EUELG-ROUNDING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2170-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EUELG-NEXT-REVIEW-DATE > 0 AND EUELG-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUELG-NATIONAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD EUELG-NATIONAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUELG-NATIONAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EUELG-NATIONAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUELG-NATIONAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2180-LIMIT-FEE.
           IF EUELG-FEE-AMT < 25.00
               MOVE 25.00 TO EUELG-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUELG-FEE-AMT > 50000.00
               MOVE 50000.00 TO EUELG-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NATIONAL YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2190-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUELG-CHANNEL-CD
                   ADD EUELG-NATIONAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUELG-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUELG-NATIONAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2200-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUELG-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       EUELG-RATE / 200
               WHEN EUELG-TYPE-CD = 'AGL'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN EUELG-TYPE-CD = 'SMF'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       EUELG-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2210-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUELG-NOTICE-DATE > 0 AND EUELG-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUELG-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NATIONAL YTD.
      *----------------------------------------------------------------*
       2220-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUELG-NATIONAL-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUELG-NATIONAL-YTD
           END-IF
           ADD EUELG-NATIONAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGL -> SMF WHEN NEXT REVIEW DATE IS PAST
      * SMF -> UEC WHEN NEXT REVIEW DATE IS PAST
      * UEC -> VJA WHEN NEXT REVIEW DATE IS PAST
      * VJA -> WPH WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2230-SET-TYPE.
           EVALUATE EUELG-TYPE-CD
               WHEN 'AGL'
                   IF EUELG-NEXT-REVIEW-DATE > 0 AND
                       EUELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SMF' TO EUELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMF'
                   IF EUELG-NEXT-REVIEW-DATE > 0 AND
                       EUELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UEC' TO EUELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UEC'
                   IF EUELG-NEXT-REVIEW-DATE > 0 AND
                       EUELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VJA' TO EUELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJA'
                   IF EUELG-NEXT-REVIEW-DATE > 0 AND
                       EUELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WPH' TO EUELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2240-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUELG-LAST-PAY-DATE > 0 AND EUELG-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUELG-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD EUELG-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD EUELG-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EUELG-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUELG-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2250-DESCRIBE-TYPE.
           EVALUATE EUELG-TYPE-CD
               WHEN 'AGL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EUELG-NEXT-REVIEW-DATE > 0 AND EUELG-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2270-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EUELG-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUELG-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       EUELG-RATE / 200
               WHEN EUELG-TYPE-CD = 'AGL'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN EUELG-TYPE-CD = 'SMF'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       EUELG-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * EUELG-RATE / 100 / 360
               ADD WS-WORK-AMT TO EUELG-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2290-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE EUELG-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2300-ACCRUE-NATIONAL.
           IF EUELG-NATIONAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUELG-NATIONAL-YTD * EUELG-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO EUELG-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EUELG-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EUELG-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AEA -> EBM WHEN NOTICE DATE IS PAST
      * EBM -> GAU WHEN NOTICE DATE IS PAST
      * GAU -> GSU WHEN NOTICE DATE IS PAST
      * GSU -> VWP WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2320-SET-CHANNEL.
           EVALUATE EUELG-CHANNEL-CD
               WHEN 'AEA'
                   IF EUELG-NOTICE-DATE > 0 AND EUELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EBM' TO EUELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EBM'
                   IF EUELG-NOTICE-DATE > 0 AND EUELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAU' TO EUELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAU'
                   IF EUELG-NOTICE-DATE > 0 AND EUELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GSU' TO EUELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GSU'
                   IF EUELG-NOTICE-DATE > 0 AND EUELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VWP' TO EUELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2330-FLAG-TRIANGULAR.
           IF EUELG-TRIANGULAR-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EUELG-TRIANGULAR-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E147 CUST ID REQUIRED
      * E387 WORK AMT NEGATIVE
      * E688 WORK AMT OVER 50000
      * E482 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUELG-CUST-ID = SPACES
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E688' TO WS-REJECT-CODE
               WHEN EUELG-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2350-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EUELG-ROUNDING-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EUELG-ROUNDING-AMT - (WS-WORK-AMT *
               3)
           COMPUTE EUELG-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2360-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2370-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EUELG-NATIONAL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = EUELG-NATIONAL-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2380-FLAG-CONVERTED.
           IF EUELG-CONVERTED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EUELG-CONVERTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2390-COUNT-ITEM.
           IF EUELG-ITEM-CNT < 9999999
               ADD 1 TO EUELG-ITEM-CNT
           END-IF
           IF EUELG-ITEM-CNT NOT < 6
               MOVE 'Y' TO EUELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2400-DESCRIBE-CHANNEL.
           EVALUATE EUELG-CHANNEL-CD
               WHEN 'AEA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EBM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GAU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GSU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VWP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XDF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2410-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EUELG-TRIANGULAR-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUELG-EXEMPT-FLAG = 'Y'
                   COMPUTE EUELG-TRIANGULAR-AMT ROUNDED = WS-WORK-AMT-2
                       * EUELG-FEE-PCT / 200
               WHEN EUELG-CURRENCY-CD = 'EW'
                   MOVE 5.00 TO EUELG-TRIANGULAR-AMT
               WHEN EUELG-CURRENCY-CD = 'FJ'
                   MOVE 5.00 TO EUELG-TRIANGULAR-AMT
               WHEN OTHER
                   COMPUTE EUELG-TRIANGULAR-AMT ROUNDED = WS-WORK-AMT-2
                       * EUELG-FEE-PCT / 100
           END-EVALUATE
           IF EUELG-TRIANGULAR-AMT > 0 AND EUELG-TRIANGULAR-AMT < 2.00
               MOVE 2.00 TO EUELG-TRIANGULAR-AMT
           END-IF
           IF EUELG-TRIANGULAR-AMT > 100.00
               MOVE 100.00 TO EUELG-TRIANGULAR-AMT
           END-IF
           ADD EUELG-TRIANGULAR-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONVERTED AMT.
      *----------------------------------------------------------------*
       2420-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUELG-CONVERTED-AMT ROUNDED =
                   EUELG-TRIANGULAR-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EUELG-CONVERTED-AMT
           END-IF
           ADD EUELG-CONVERTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-ITEM.
           IF EUELG-ITEM-CNT < 9999999
               ADD 1 TO EUELG-ITEM-CNT
           END-IF
           IF EUELG-ITEM-CNT NOT < 5
               MOVE 'Y' TO EUELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2440-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * EUELG-RATE / 100 / 365
               ADD WS-WORK-AMT TO EUELG-ROUNDING-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NATIONAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2450-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUELG-NATIONAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ROUNDING AMT.
      *----------------------------------------------------------------*
       2460-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUELG-ROUNDING-AMT ROUNDED =
                   EUELG-TRIANGULAR-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EUELG-ROUNDING-AMT
           END-IF
           ADD EUELG-ROUNDING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2470-COUNT-ITEM.
           IF EUELG-ITEM-CNT < 9999999
               ADD 1 TO EUELG-ITEM-CNT
           END-IF
           IF EUELG-ITEM-CNT NOT < 3
               MOVE 'Y' TO EUELG-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2480-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUELG-NOTICE-DATE > 0 AND EUELG-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUELG-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2490-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EUELG-CONVERTED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EUELG-CONVERTED-AMT - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2500-DESCRIBE-TYPE.
           EVALUATE EUELG-TYPE-CD
               WHEN 'AGL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2510-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUELG-LAST-PAY-DATE > 0 AND EUELG-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUELG-CONVERTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE NATIONAL YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2520-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUELG-CHANNEL-CD
                   ADD EUELG-NATIONAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUELG-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUELG-NATIONAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E936 ADDR REQUIRED
      * E161 CONVERTED AMT NEGATIVE
      * E245 CONVERTED AMT OVER 25000
      * E366 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2530-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUELG-ADDR = SPACES
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN EUELG-CONVERTED-AMT < 0
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN EUELG-CONVERTED-AMT > 25000.00
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN EUELG-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2540-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF EUELG-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUELG-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO EUELG-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E448 CUST ID REQUIRED
      * E229 WORK AMT NEGATIVE
      * E811 WORK AMT OVER 250000
      * E956 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2550-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUELG-CUST-ID = SPACES
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E811' TO WS-REJECT-CODE
               WHEN EUELG-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E960 CUST ID REQUIRED
      * E320 WORK AMT NEGATIVE
      * E672 WORK AMT OVER 10000
      * E269 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2560-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUELG-CUST-ID = SPACES
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E320' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN EUELG-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E269' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2570-LIMIT-CONVERTED.
           IF EUELG-CONVERTED-AMT < 25.00
               MOVE 25.00 TO EUELG-CONVERTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUELG-CONVERTED-AMT > 5000.00
               MOVE 5000.00 TO EUELG-CONVERTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2580-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUELG-NOTICE-DATE > 0 AND EUELG-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUELG-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUELG-ROUNDING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD EUELG-ROUNDING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUELG-ROUNDING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EUELG-ROUNDING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUELG-ROUNDING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2590-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUELG-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2600-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * EUELG-RATE / 100 / 365
               ADD WS-WORK-AMT TO EUELG-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO EUELG-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO EUELG-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO EUELG-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'EUELG EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
