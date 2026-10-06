      *================================================================*
      * PROGRAM  : CNFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : COIN AND CURRENCY RULE - PRICING. CALLED BY THE COIN*
      *            AND CURRENCY BATCH AND ONLINE PROGRAMS.             *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-12-11 MKL  ORIGINAL                                       *
      * 1986-06-19 TWH  CURRENCY CUTOFF MOVED PRB4480                  *
      * 1989-06-26 SKN  STRAP ROUNDING - HALF UP PRB2971               *
      * 1990-10-18 PQR  PERFORMANCE - VARIANCE READ AHEAD PRB8665      *
      * 1993-06-10 CRP  CURRENCY CUTOFF MOVED REQ9711                  *
      * 1998-02-12 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-07-04 TWH  STRAP TABLE EXPANDED INC7518                   *
      * 2004-03-23 EWS  AUDIT FINDING - SHIPMENT CONTROL TOTALS PRB5230*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNFEE.
       AUTHOR. B H SANDERS.

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
                                           VALUE 25.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 31.
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
      * WS-STRAP-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STRAP-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000226000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000263000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000181000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000584500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013050000000880000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015550000000200000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000645000'.
       01  WS-STRAP-GRID-TBL REDEFINES WS-STRAP-GRID-VALUES.
           05  WS-STRAP-GRID-ROW OCCURS 7 TIMES.
               10  WS-STRAP-GRID-LOW       PIC 9(09)V99.
               10  WS-STRAP-GRID-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000570000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000293000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000295000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000621500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000659000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000780000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000871500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000802500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018600000000511500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023600000000770500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024100000000669000'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 11 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CNFEEP.

       PROCEDURE DIVISION USING CNFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-VARIANCE
           PERFORM 2010-SELECT-FEE-PCT
           PERFORM 2020-LIMIT-COIN
           PERFORM 2030-ACCUM-SOURCE
           PERFORM 2040-ACCUM-DENOM
           PERFORM 2050-FLAG-COIN
           PERFORM 2060-PRORATE-CURRENCY
           PERFORM 2070-LIMIT-STRAP
           PERFORM 2080-SELECT-FEE-PCT
           PERFORM 2090-CHECK-NEXT-REVIEW
           PERFORM 2100-ASSESS-AUTO-RENEW
           PERFORM 2110-COUNT-NOTICE
           PERFORM 2120-FLAG-ITEM
           PERFORM 2130-EDIT-DESC
           PERFORM 2140-COUNT-NOTICE
           PERFORM 2150-SPLIT-FEE
           PERFORM 2160-PRORATE-STRAP
           PERFORM 2170-ACCRUE-FEE-PCT
           PERFORM 2180-ACCUM-REASON
           PERFORM 2190-ACCRUE-STRAP
           PERFORM 2200-SPLIT-STRAP
           PERFORM 2210-EDIT-REF-NO
           PERFORM 2220-EDIT-DESC
           PERFORM 2230-COUNT-CYCLE
           PERFORM 2240-AGE-NOTICE
           PERFORM 2250-EDIT-REF-NO
           PERFORM 2260-ACCUM-REASON
           PERFORM 2270-COUNT-CYCLE
           PERFORM 2280-DESCRIBE-CARRIER
           PERFORM 2290-CHECK-EFFECTIVE
           PERFORM 2300-DESCRIBE-CARRIER
           PERFORM 2310-CHECK-NEXT-REVIEW
           PERFORM 2320-DESCRIBE-SOURCE
           PERFORM 2330-ASSESS-HOLD
           PERFORM 2340-CHECK-EFFECTIVE
           PERFORM 2350-SPLIT-CURRENCY
           PERFORM 2360-AGE-NEXT-REVIEW
           PERFORM 2370-EDIT-DESC
           PERFORM 2380-SELECT-FEE-PCT
           PERFORM 2390-SELECT-RATE
           PERFORM 2400-SET-SOURCE
           PERFORM 2410-EDIT-DESC
           PERFORM 2420-PRORATE-STRAP
           PERFORM 2430-ASSESS-EXEMPT
           PERFORM 2440-FLAG-ITEM
           PERFORM 2450-SELECT-RATE
           PERFORM 2460-LIMIT-CURRENCY
           PERFORM 2470-SELECT-RATE
           PERFORM 2480-COUNT-CYCLE
           PERFORM 2490-ASSESS-AUTO-RENEW
           PERFORM 2500-SPLIT-VARIANCE
           PERFORM 2510-SPLIT-STRAP
           PERFORM 2520-PRORATE-STRAP
           PERFORM 2530-FLAG-CURRENCY
           PERFORM 2540-LIMIT-ITEM
           PERFORM 2550-DESCRIBE-DENOM
           PERFORM 2560-CHECK-NEXT-REVIEW
           PERFORM 2570-ACCRUE-RATE
           PERFORM 2580-SET-DENOM
           PERFORM 2590-SPLIT-COIN
           PERFORM 2600-ACCUM-CARRIER
           PERFORM 2610-LIMIT-ITEM
           PERFORM 2620-SELECT-RATE
           PERFORM 2630-SELECT-RATE
           PERFORM 2640-CHECK-NOTICE
           PERFORM 2650-CHECK-EFFECTIVE
           PERFORM 2660-ACCRUE-CURRENCY
           PERFORM 2670-EDIT-DESC
           PERFORM 2680-ACCUM-SOURCE
           PERFORM 2690-SELECT-RATE
           PERFORM 2700-EDIT-REF-NO
           PERFORM 2710-SET-SOURCE
           PERFORM 2720-AGE-NEXT-REVIEW
           PERFORM 2730-ACCRUE-FEE-PCT
           PERFORM 2740-CHECK-LAST-PAY
           PERFORM 2750-DESCRIBE-REASON
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CNFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE CNFEE-VARIANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STRAP GRID TIER FOR THE COIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2010-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STRAP-GRID-RATE(1) TO CNFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CNFEE-COIN-YTD NOT < WS-STRAP-GRID-LOW(WS-TX)
                   MOVE WS-STRAP-GRID-RATE(WS-TX) TO CNFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2020-LIMIT-COIN.
           IF CNFEE-COIN-YTD < 10.00
               MOVE 10.00 TO CNFEE-COIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNFEE-COIN-YTD > 50000.00
               MOVE 50000.00 TO CNFEE-COIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CURRENCY YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2030-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNFEE-SOURCE-CD
                   ADD CNFEE-CURRENCY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNFEE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNFEE-CURRENCY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2040-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNFEE-DENOM-CD
                   ADD CNFEE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNFEE-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNFEE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * COIN YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2050-FLAG-COIN.
           IF CNFEE-COIN-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CNFEE-COIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CURRENCY YTD.
      *----------------------------------------------------------------*
       2060-PRORATE-CURRENCY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNFEE-CURRENCY-YTD ROUNDED =
                   CNFEE-COIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CNFEE-CURRENCY-YTD
           END-IF
           ADD CNFEE-CURRENCY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STRAP YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2070-LIMIT-STRAP.
           IF CNFEE-STRAP-YTD < 100.00
               MOVE 100.00 TO CNFEE-STRAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNFEE-STRAP-YTD > 50000.00
               MOVE 50000.00 TO CNFEE-STRAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STRAP GRID TIER FOR THE STRAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2080-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STRAP-GRID-RATE(1) TO CNFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CNFEE-STRAP-AMT NOT < WS-STRAP-GRID-LOW(WS-TX)
                   MOVE WS-STRAP-GRID-RATE(WS-TX) TO CNFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2090-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CNFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE COIN YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2100-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CNFEE-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNFEE-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CNFEE-COIN-YTD *
                       CNFEE-FEE-PCT / 200
               WHEN CNFEE-REASON-CD = 'JL'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN CNFEE-REASON-CD = 'LW'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CNFEE-COIN-YTD *
                       CNFEE-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2110-COUNT-NOTICE.
           IF CNFEE-NOTICE-CNT < 9999999
               ADD 1 TO CNFEE-NOTICE-CNT
           END-IF
           IF CNFEE-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CNFEE-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2120-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E575 DESC REQUIRED
      * E634 FEE AMT NEGATIVE
      * E718 FEE AMT OVER 5000
      * E136 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNFEE-DESC = SPACES
                   MOVE 'E575' TO WS-REJECT-CODE
               WHEN CNFEE-FEE-AMT < 0
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN CNFEE-FEE-AMT > 5000.00
                   MOVE 'E718' TO WS-REJECT-CODE
               WHEN CNFEE-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E136' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-NOTICE.
           IF CNFEE-NOTICE-CNT < 9999999
               ADD 1 TO CNFEE-NOTICE-CNT
           END-IF
           IF CNFEE-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CNFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2150-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = CNFEE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CNFEE-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE CNFEE-STRAP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STRAP YTD.
      *----------------------------------------------------------------*
       2160-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNFEE-STRAP-YTD ROUNDED =
                   CNFEE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CNFEE-STRAP-YTD
           END-IF
           ADD CNFEE-STRAP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2170-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CNFEE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CNFEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2180-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNFEE-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STRAP AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2190-ACCRUE-STRAP.
           IF CNFEE-STRAP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNFEE-STRAP-AMT * CNFEE-RATE / 100 / 360
               ADD WS-WORK-AMT TO CNFEE-VARIANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2200-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE CNFEE-STRAP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E200 REF NO REQUIRED
      * E164 CURRENCY YTD NEGATIVE
      * E309 CURRENCY YTD OVER 50000
      * E462 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2210-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNFEE-REF-NO = SPACES
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN CNFEE-CURRENCY-YTD < 0
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN CNFEE-CURRENCY-YTD > 50000.00
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN CNFEE-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E462' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E983 DESC REQUIRED
      * E891 CURRENCY YTD NEGATIVE
      * E933 CURRENCY YTD OVER 250000
      * E782 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2220-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNFEE-DESC = SPACES
                   MOVE 'E983' TO WS-REJECT-CODE
               WHEN CNFEE-CURRENCY-YTD < 0
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN CNFEE-CURRENCY-YTD > 250000.00
                   MOVE 'E933' TO WS-REJECT-CODE
               WHEN CNFEE-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-CYCLE.
           IF CNFEE-CYCLE-CNT < 9999999
               ADD 1 TO CNFEE-CYCLE-CNT
           END-IF
           IF CNFEE-CYCLE-CNT NOT < 3
               MOVE 'Y' TO CNFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2240-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CNFEE-NOTICE-DATE > 0 AND CNFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNFEE-CURRENCY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CNFEE-CURRENCY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CNFEE-CURRENCY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CNFEE-CURRENCY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNFEE-CURRENCY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E355 REF NO REQUIRED
      * E236 COIN YTD NEGATIVE
      * E487 COIN YTD OVER 25000
      * E156 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2250-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNFEE-REF-NO = SPACES
                   MOVE 'E355' TO WS-REJECT-CODE
               WHEN CNFEE-COIN-YTD < 0
                   MOVE 'E236' TO WS-REJECT-CODE
               WHEN CNFEE-COIN-YTD > 25000.00
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN CNFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VARIANCE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2260-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNFEE-REASON-CD
                   ADD CNFEE-VARIANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNFEE-VARIANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2270-COUNT-CYCLE.
           IF CNFEE-CYCLE-CNT < 9999999
               ADD 1 TO CNFEE-CYCLE-CNT
           END-IF
           IF CNFEE-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CNFEE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2280-DESCRIBE-CARRIER.
           EVALUATE CNFEE-CARRIER-CD
               WHEN 'J'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CNFEE-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNFEE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CNFEE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-CARRIER.
           EVALUATE CNFEE-CARRIER-CD
               WHEN 'J'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CNFEE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2320-DESCRIBE-SOURCE.
           EVALUATE CNFEE-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COIN YTD FEE ON THE VARIANCE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2330-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CNFEE-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CNFEE-COIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE CNFEE-COIN-YTD ROUNDED = CNFEE-VARIANCE-YTD *
                       CNFEE-RATE / 200
               WHEN CNFEE-DENOM-CD = 'B'
                   MOVE 7.00 TO CNFEE-COIN-YTD
               WHEN CNFEE-DENOM-CD = 'D'
                   MOVE 5.00 TO CNFEE-COIN-YTD
               WHEN OTHER
                   COMPUTE CNFEE-COIN-YTD ROUNDED = CNFEE-VARIANCE-YTD *
                       CNFEE-RATE / 100
           END-EVALUATE
           IF CNFEE-COIN-YTD > 0 AND CNFEE-COIN-YTD < 10.00
               MOVE 10.00 TO CNFEE-COIN-YTD
           END-IF
           IF CNFEE-COIN-YTD > 250.00
               MOVE 250.00 TO CNFEE-COIN-YTD
           END-IF
           ADD CNFEE-COIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2340-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CNFEE-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNFEE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CNFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2350-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE CNFEE-CURRENCY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNFEE-NEXT-REVIEW-DATE > 0 AND CNFEE-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNFEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CNFEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CNFEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CNFEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNFEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E978 DESC REQUIRED
      * E422 WORK AMT NEGATIVE
      * E392 WORK AMT OVER 10000
      * E158 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2370-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNFEE-DESC = SPACES
                   MOVE 'E978' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E422' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E392' TO WS-REJECT-CODE
               WHEN CNFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E158' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STRAP GRID TIER FOR THE STRAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2380-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STRAP-GRID-RATE(1) TO CNFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CNFEE-STRAP-AMT NOT < WS-STRAP-GRID-LOW(WS-TX)
                   MOVE WS-STRAP-GRID-RATE(WS-TX) TO CNFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE VARIANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2390-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO CNFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CNFEE-VARIANCE-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO CNFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CFR -> DTX WHEN EFFECTIVE DATE IS PAST
      * DTX -> EEA WHEN EFFECTIVE DATE IS PAST
      * EEA -> EUA WHEN EFFECTIVE DATE IS PAST
      * EUA -> TPB WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2400-SET-SOURCE.
           EVALUATE CNFEE-SOURCE-CD
               WHEN 'CFR'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DTX' TO CNFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DTX'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EEA' TO CNFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EEA'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EUA' TO CNFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EUA'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TPB' TO CNFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E600 DESC REQUIRED
      * E913 WORK AMT NEGATIVE
      * E116 WORK AMT OVER 1000000
      * E470 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNFEE-DESC = SPACES
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E913' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E116' TO WS-REJECT-CODE
               WHEN CNFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E470' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STRAP YTD.
      *----------------------------------------------------------------*
       2420-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNFEE-STRAP-YTD ROUNDED =
                   CNFEE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CNFEE-STRAP-YTD
           END-IF
           ADD CNFEE-STRAP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE VARIANCE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2430-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CNFEE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CNFEE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNFEE-HOLD-FLAG = 'Y'
                   COMPUTE CNFEE-FEE-AMT ROUNDED = CNFEE-VARIANCE-YTD *
                       CNFEE-FEE-PCT / 200
               WHEN CNFEE-CARRIER-CD = 'J'
                   MOVE 15.00 TO CNFEE-FEE-AMT
               WHEN CNFEE-CARRIER-CD = 'L'
                   MOVE 15.00 TO CNFEE-FEE-AMT
               WHEN OTHER
                   COMPUTE CNFEE-FEE-AMT ROUNDED = CNFEE-VARIANCE-YTD *
                       CNFEE-FEE-PCT / 100
           END-EVALUATE
           IF CNFEE-FEE-AMT > 0 AND CNFEE-FEE-AMT < 2.00
               MOVE 2.00 TO CNFEE-FEE-AMT
           END-IF
           IF CNFEE-FEE-AMT > 75.00
               MOVE 75.00 TO CNFEE-FEE-AMT
           END-IF
           ADD CNFEE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2440-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2450-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO CNFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CNFEE-STRAP-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO CNFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CURRENCY YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2460-LIMIT-CURRENCY.
           IF CNFEE-CURRENCY-YTD < 0.00
               MOVE 0.00 TO CNFEE-CURRENCY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNFEE-CURRENCY-YTD > 5000.00
               MOVE 5000.00 TO CNFEE-CURRENCY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO CNFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO CNFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2480-COUNT-CYCLE.
           IF CNFEE-CYCLE-CNT < 9999999
               ADD 1 TO CNFEE-CYCLE-CNT
           END-IF
           IF CNFEE-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CNFEE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP YTD FEE ON THE CURRENCY YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2490-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CNFEE-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CNFEE-STRAP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNFEE-HOLD-FLAG = 'Y'
                   COMPUTE CNFEE-STRAP-YTD ROUNDED = CNFEE-CURRENCY-YTD
                       * CNFEE-FEE-PCT / 200
               WHEN CNFEE-SOURCE-CD = 'CFR'
                   MOVE 15.00 TO CNFEE-STRAP-YTD
               WHEN CNFEE-SOURCE-CD = 'DTX'
                   MOVE 15.00 TO CNFEE-STRAP-YTD
               WHEN OTHER
                   COMPUTE CNFEE-STRAP-YTD ROUNDED = CNFEE-CURRENCY-YTD
                       * CNFEE-FEE-PCT / 100
           END-EVALUATE
           IF CNFEE-STRAP-YTD > 0 AND CNFEE-STRAP-YTD < 10.00
               MOVE 10.00 TO CNFEE-STRAP-YTD
           END-IF
           IF CNFEE-STRAP-YTD > 250.00
               MOVE 250.00 TO CNFEE-STRAP-YTD
           END-IF
           ADD CNFEE-STRAP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2500-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE CNFEE-VARIANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STRAP AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CNFEE-STRAP-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CNFEE-STRAP-AMT - (WS-WORK-AMT * 4)
           COMPUTE CNFEE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STRAP AMT.
      *----------------------------------------------------------------*
       2520-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNFEE-STRAP-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNFEE-STRAP-AMT
           END-IF
           ADD CNFEE-STRAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CURRENCY YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2530-FLAG-CURRENCY.
           IF CNFEE-CURRENCY-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CNFEE-CURRENCY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2540-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2550-DESCRIBE-DENOM.
           EVALUATE CNFEE-DENOM-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CNFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2570-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CNFEE-RATE / 100 / 365
               ADD WS-WORK-AMT TO CNFEE-COIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * B -> D WHEN EFFECTIVE DATE IS PAST
      * D -> P WHEN EFFECTIVE DATE IS PAST
      * P -> S WHEN EFFECTIVE DATE IS PAST
      * S -> W WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2580-SET-DENOM.
           EVALUATE CNFEE-DENOM-CD
               WHEN 'B'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'D' TO CNFEE-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'P' TO CNFEE-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO CNFEE-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'W' TO CNFEE-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2590-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE CNFEE-COIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2600-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNFEE-CARRIER-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNFEE-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2610-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2620-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO CNFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CNFEE-STRAP-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO CNFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STRAP GRID TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2630-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STRAP-GRID-RATE(1) TO CNFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CNFEE-STRAP-YTD NOT < WS-STRAP-GRID-LOW(WS-TX)
                   MOVE WS-STRAP-GRID-RATE(WS-TX) TO CNFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2640-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CNFEE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CNFEE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2650-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CNFEE-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNFEE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CNFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CURRENCY YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2660-ACCRUE-CURRENCY.
           IF CNFEE-CURRENCY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNFEE-CURRENCY-YTD * CNFEE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CNFEE-COIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E341 DESC REQUIRED
      * E698 COIN YTD NEGATIVE
      * E595 COIN YTD OVER 25000
      * E263 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2670-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNFEE-DESC = SPACES
                   MOVE 'E341' TO WS-REJECT-CODE
               WHEN CNFEE-COIN-YTD < 0
                   MOVE 'E698' TO WS-REJECT-CODE
               WHEN CNFEE-COIN-YTD > 25000.00
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN CNFEE-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2680-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNFEE-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNFEE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STRAP GRID TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2690-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STRAP-GRID-RATE(1) TO CNFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CNFEE-STRAP-YTD NOT < WS-STRAP-GRID-LOW(WS-TX)
                   MOVE WS-STRAP-GRID-RATE(WS-TX) TO CNFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E160 REF NO REQUIRED
      * E599 STRAP AMT NEGATIVE
      * E787 STRAP AMT OVER 5000
      * E063 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2700-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNFEE-REF-NO = SPACES
                   MOVE 'E160' TO WS-REJECT-CODE
               WHEN CNFEE-STRAP-AMT < 0
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN CNFEE-STRAP-AMT > 5000.00
                   MOVE 'E787' TO WS-REJECT-CODE
               WHEN CNFEE-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E063' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CFR -> DTX WHEN EFFECTIVE DATE IS PAST
      * DTX -> EEA WHEN EFFECTIVE DATE IS PAST
      * EEA -> EUA WHEN EFFECTIVE DATE IS PAST
      * EUA -> TPB WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2710-SET-SOURCE.
           EVALUATE CNFEE-SOURCE-CD
               WHEN 'CFR'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DTX' TO CNFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DTX'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EEA' TO CNFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EEA'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EUA' TO CNFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EUA'
                   IF CNFEE-EFFECTIVE-DATE > 0 AND CNFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TPB' TO CNFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2720-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNFEE-NEXT-REVIEW-DATE > 0 AND CNFEE-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2730-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CNFEE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CNFEE-VARIANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2740-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CNFEE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CNFEE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2750-DESCRIBE-REASON.
           EVALUATE CNFEE-REASON-CD
               WHEN 'JL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CNFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CNFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CNFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CNFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
