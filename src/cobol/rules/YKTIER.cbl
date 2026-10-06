      *================================================================*
      * PROGRAM  : YKTIER                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DATE WINDOW BRIDGE RULE - DATE RULES. CALLED BY THE *
      *            DATE WINDOW BRIDGE BATCH AND ONLINE PROGRAMS.       *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-08-14 LMB  ORIGINAL                                       *
      * 1994-06-01 PQR  AMOUNT-1 CALCULATION CORRECTED INC7223         *
      * 1994-01-05 GAF  CONTROL CALCULATION CORRECTED REQ7409          *
      * 1996-02-27 MKL  AMOUNT-1 EXCEPTIONS TO OPS QUEUE REQ4783       *
      * 1996-12-19 DPS  NEW AMOUNT-1 REPORT COLUMNS REQ8878            *
      * 1997-02-07 TWH  NEW CONTROL REPORT COLUMNS REQ3624             *
      * 1997-09-27 DPS  AMOUNT-2 CUTOFF MOVED PRB8309                  *
      * 2005-05-28 SKN  PERFORMANCE - AMOUNT-1 READ AHEAD PRB5056      *
      * 2005-08-26 GAF  AMOUNT-2 ROUNDING - HALF UP REQ1969            *
      * 2007-12-05 SKN  AMOUNT-2 LIMIT REVISED PRB4402                 *
      * 2008-11-27 EWS  NEW CONTROL REPORT COLUMNS INC7566             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKTIER.
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
                                           VALUE 8.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 30.
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
      * WS-AMOUNT-1-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-1-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000887500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000531000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000039000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000484000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006200000000116000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008700000000098000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013700000000792000'.
       01  WS-AMOUNT-1-GRID-TBL REDEFINES WS-AMOUNT-1-GRID-VALUES.
           05  WS-AMOUNT-1-GRID-ROW OCCURS 7 TIMES.
               10  WS-AMOUNT-1-GRID-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-1-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONTROL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000878000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000013000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000445000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000696000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000523500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000572500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013650000000469000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013700000000705000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016200000000812500'.
       01  WS-CONTROL-TIER-TBL REDEFINES WS-CONTROL-TIER-VALUES.
           05  WS-CONTROL-TIER-ROW OCCURS 10 TIMES.
               10  WS-CONTROL-TIER-LOW     PIC 9(09)V99.
               10  WS-CONTROL-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-AMOUNT-1-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-1-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000007000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000252000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000174500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000037500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000295000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018250000000092500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020750000000595000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021750000000072000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031750000000089000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032750000000644000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032800000000706500'.
       01  WS-AMOUNT-1-SCHED-TBL REDEFINES WS-AMOUNT-1-SCHED-VALUES.
           05  WS-AMOUNT-1-SCHED-ROW OCCURS 11 TIMES.
               10  WS-AMOUNT-1-SCHED-LOW   PIC 9(09)V99.
               10  WS-AMOUNT-1-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY YKTIERP.

       PROCEDURE DIVISION USING YKTIER-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-AMOUNT-2
           PERFORM 2010-ACCRUE-CONTROL
           PERFORM 2020-ACCRUE-PENALTY-PCT
           PERFORM 2030-COUNT-RETRY
           PERFORM 2040-ASSESS-AUTO-RENEW
           PERFORM 2050-PRORATE-AMOUNT-2
           PERFORM 2060-COUNT-RETRY
           PERFORM 2070-PRORATE-AMOUNT-1
           PERFORM 2080-ACCUM-CYCLE
           PERFORM 2090-ACCUM-PIVOT
           PERFORM 2100-LIMIT-ITEM
           PERFORM 2110-SELECT-DISCOUNT
           PERFORM 2120-AGE-EXPIRY
           PERFORM 2130-LIMIT-AMOUNT-2
           PERFORM 2140-COUNT-ITEM
           PERFORM 2150-SELECT-DISCOUNT
           PERFORM 2160-LIMIT-AMOUNT-1
           PERFORM 2170-PRORATE-CONTROL
           PERFORM 2180-ASSESS-AUTO-RENEW
           PERFORM 2190-FLAG-AMOUNT-2
           PERFORM 2200-COUNT-ITEM
           PERFORM 2210-SELECT-DISCOUNT
           PERFORM 2220-AGE-LAST-ACT
           PERFORM 2230-CHECK-MATURITY
           PERFORM 2240-AGE-EXPIRY
           PERFORM 2250-EDIT-CONTACT
           PERFORM 2260-PRORATE-AMOUNT-1
           PERFORM 2270-EDIT-CONTACT
           PERFORM 2280-SELECT-DISCOUNT
           PERFORM 2290-DESCRIBE-SOURCE
           PERFORM 2300-PRORATE-AMOUNT-1
           PERFORM 2310-ASSESS-PREFERRED
           PERFORM 2320-SPLIT-AMOUNT-1
           PERFORM 2330-AGE-LAST-ACT
           PERFORM 2340-SET-CYCLE
           PERFORM 2350-CHECK-MATURITY
           PERFORM 2360-SET-CYCLE
           PERFORM 2370-SPLIT-CONTROL
           PERFORM 2380-ACCUM-SOURCE
           PERFORM 2390-EDIT-CONTACT
           PERFORM 2400-PRORATE-AMOUNT-2
           PERFORM 2410-CHECK-LAST-ACT
           PERFORM 2420-CHECK-EXPIRY
           PERFORM 2430-ASSESS-PREFERRED
           PERFORM 2440-COUNT-RETRY
           PERFORM 2450-ASSESS-PREFERRED
           PERFORM 2460-CHECK-EXPIRY
           PERFORM 2470-SELECT-DISCOUNT
           PERFORM 2480-SPLIT-AMOUNT-1
           PERFORM 2490-CHECK-EXPIRY
           PERFORM 2500-SPLIT-AMOUNT-2
           PERFORM 2510-ACCUM-CYCLE
           PERFORM 2520-DESCRIBE-PIVOT
           PERFORM 2530-EDIT-CUST-ID
           PERFORM 2540-ACCRUE-PENALTY-PCT
           PERFORM 2550-FLAG-ITEM
           PERFORM 2560-ACCRUE-AMOUNT-2
           PERFORM 2570-AGE-LAST-MAINT
           PERFORM 2580-LIMIT-AMOUNT-2
           PERFORM 2590-DESCRIBE-REGION
           PERFORM 2600-AGE-MATURITY
           PERFORM 2610-PRORATE-AMOUNT-2
           PERFORM 2620-ASSESS-REVIEW
           PERFORM 2630-SET-SOURCE
           PERFORM 2640-LIMIT-AMOUNT-2
           PERFORM 2650-SELECT-PENALTY-PCT
           PERFORM 2660-SET-PIVOT
           PERFORM 2670-COUNT-ITEM
           PERFORM 2680-SET-PIVOT
           PERFORM 2690-AGE-MATURITY
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO YKTIER-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YKTIER-AMOUNT-2-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YKTIER-AMOUNT-2-AMT - (WS-WORK-AMT *
               2)
           COMPUTE YKTIER-AMOUNT-1-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2010-ACCRUE-CONTROL.
           IF YKTIER-CONTROL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKTIER-CONTROL-AMT * YKTIER-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * YKTIER-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO YKTIER-CONTROL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2030-COUNT-RETRY.
           IF YKTIER-RETRY-CNT < 9999999
               ADD 1 TO YKTIER-RETRY-CNT
           END-IF
           IF YKTIER-RETRY-CNT NOT < 12
               MOVE 'Y' TO YKTIER-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2040-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YKTIER-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO YKTIER-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKTIER-PREFERRED-FLAG = 'Y'
                   COMPUTE YKTIER-AMOUNT-2-AMT ROUNDED = WS-WORK-AMT *
                       YKTIER-PENALTY-PCT / 200
               WHEN YKTIER-SOURCE-CD = 'BNX'
                   MOVE 5.00 TO YKTIER-AMOUNT-2-AMT
               WHEN YKTIER-SOURCE-CD = 'FTB'
                   MOVE 10.00 TO YKTIER-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YKTIER-AMOUNT-2-AMT ROUNDED = WS-WORK-AMT *
                       YKTIER-PENALTY-PCT / 100
           END-EVALUATE
           IF YKTIER-AMOUNT-2-AMT > 0 AND YKTIER-AMOUNT-2-AMT < 1.00
               MOVE 1.00 TO YKTIER-AMOUNT-2-AMT
           END-IF
           IF YKTIER-AMOUNT-2-AMT > 75.00
               MOVE 75.00 TO YKTIER-AMOUNT-2-AMT
           END-IF
           ADD YKTIER-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       2050-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKTIER-AMOUNT-2-AMT ROUNDED =
                   YKTIER-CONTROL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YKTIER-AMOUNT-2-AMT
           END-IF
           ADD YKTIER-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2060-COUNT-RETRY.
           IF YKTIER-RETRY-CNT < 9999999
               ADD 1 TO YKTIER-RETRY-CNT
           END-IF
           IF YKTIER-RETRY-CNT NOT < 6
               MOVE 'Y' TO YKTIER-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 1 YTD.
      *----------------------------------------------------------------*
       2070-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKTIER-AMOUNT-1-YTD ROUNDED =
                   YKTIER-CONTROL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YKTIER-AMOUNT-1-YTD
           END-IF
           ADD YKTIER-AMOUNT-1-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2080-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKTIER-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKTIER-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2090-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKTIER-PIVOT-CD
                   ADD YKTIER-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKTIER-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKTIER-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2100-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIER TIER FOR THE AMOUNT 2 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2110-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIER-RATE(1) TO YKTIER-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF YKTIER-AMOUNT-2-YTD NOT < WS-CONTROL-TIER-LOW(WS-TX)
                   MOVE WS-CONTROL-TIER-RATE(WS-TX) TO YKTIER-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKTIER-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKTIER-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2120-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-EXPIRY-DATE > 0 AND YKTIER-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKTIER-AMOUNT-2-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YKTIER-AMOUNT-2-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YKTIER-AMOUNT-2-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD YKTIER-AMOUNT-2-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKTIER-AMOUNT-2-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-AMOUNT-2.
           IF YKTIER-AMOUNT-2-YTD < 50.00
               MOVE 50.00 TO YKTIER-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKTIER-AMOUNT-2-YTD > 50000.00
               MOVE 50000.00 TO YKTIER-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-ITEM.
           IF YKTIER-ITEM-CNT < 9999999
               ADD 1 TO YKTIER-ITEM-CNT
           END-IF
           IF YKTIER-ITEM-CNT NOT < 5
               MOVE 'Y' TO YKTIER-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 GRID TIER FOR THE AMOUNT 2 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2150-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-GRID-RATE(1) TO YKTIER-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF YKTIER-AMOUNT-2-YTD NOT < WS-AMOUNT-1-GRID-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-GRID-RATE(WS-TX) TO YKTIER-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKTIER-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKTIER-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2160-LIMIT-AMOUNT-1.
           IF YKTIER-AMOUNT-1-AMT < 10.00
               MOVE 10.00 TO YKTIER-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKTIER-AMOUNT-1-AMT > 2500.00
               MOVE 2500.00 TO YKTIER-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2170-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKTIER-CONTROL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CONTROL AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2180-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YKTIER-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKTIER-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = YKTIER-CONTROL-AMT *
                       YKTIER-DISCOUNT / 200
               WHEN YKTIER-SOURCE-CD = 'BNX'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN YKTIER-SOURCE-CD = 'FTB'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = YKTIER-CONTROL-AMT *
                       YKTIER-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2190-FLAG-AMOUNT-2.
           IF YKTIER-AMOUNT-2-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YKTIER-AMOUNT-2-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-ITEM.
           IF YKTIER-ITEM-CNT < 9999999
               ADD 1 TO YKTIER-ITEM-CNT
           END-IF
           IF YKTIER-ITEM-CNT NOT < 12
               MOVE 'Y' TO YKTIER-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 GRID TIER FOR THE AMOUNT 2 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2210-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-GRID-RATE(1) TO YKTIER-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF YKTIER-AMOUNT-2-YTD NOT < WS-AMOUNT-1-GRID-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-GRID-RATE(WS-TX) TO YKTIER-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKTIER-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKTIER-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2220-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-LAST-ACT-DATE > 0 AND YKTIER-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKTIER-CONTROL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD YKTIER-CONTROL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD YKTIER-CONTROL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YKTIER-CONTROL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKTIER-CONTROL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2230-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO YKTIER-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2240-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-EXPIRY-DATE > 0 AND YKTIER-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKTIER-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YKTIER-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YKTIER-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YKTIER-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKTIER-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E045 CONTACT REQUIRED
      * E609 WORK AMT 2 NEGATIVE
      * E491 WORK AMT 2 OVER 10000
      * E829 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2250-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKTIER-CONTACT = SPACES
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E609' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E491' TO WS-REJECT-CODE
               WHEN YKTIER-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E829' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       2260-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKTIER-AMOUNT-1-AMT ROUNDED =
                   YKTIER-AMOUNT-2-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YKTIER-AMOUNT-1-AMT
           END-IF
           ADD YKTIER-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E563 CONTACT REQUIRED
      * E867 WORK AMT NEGATIVE
      * E293 WORK AMT OVER 1000000
      * E396 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2270-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKTIER-CONTACT = SPACES
                   MOVE 'E563' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E293' TO WS-REJECT-CODE
               WHEN YKTIER-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 SCHED TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2280-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-SCHED-RATE(1) TO YKTIER-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YKTIER-AMOUNT-2-AMT NOT < WS-AMOUNT-1-SCHED-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-SCHED-RATE(WS-TX) TO YKTIER-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKTIER-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKTIER-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2290-DESCRIBE-SOURCE.
           EVALUATE YKTIER-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTROL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       2300-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKTIER-AMOUNT-1-AMT ROUNDED =
                   YKTIER-CONTROL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YKTIER-AMOUNT-1-AMT
           END-IF
           ADD YKTIER-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL YTD FEE ON THE AMOUNT 2 YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2310-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YKTIER-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO YKTIER-CONTROL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKTIER-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YKTIER-CONTROL-YTD ROUNDED =
                       YKTIER-AMOUNT-2-YTD * YKTIER-DISCOUNT / 200
               WHEN YKTIER-REGION-CD = 'C'
                   MOVE 12.00 TO YKTIER-CONTROL-YTD
               WHEN YKTIER-REGION-CD = 'H'
                   MOVE 7.00 TO YKTIER-CONTROL-YTD
               WHEN OTHER
                   COMPUTE YKTIER-CONTROL-YTD ROUNDED =
                       YKTIER-AMOUNT-2-YTD * YKTIER-DISCOUNT / 100
           END-EVALUATE
           IF YKTIER-CONTROL-YTD > 0 AND YKTIER-CONTROL-YTD < 1.00
               MOVE 1.00 TO YKTIER-CONTROL-YTD
           END-IF
           IF YKTIER-CONTROL-YTD > 50.00
               MOVE 50.00 TO YKTIER-CONTROL-YTD
           END-IF
           ADD YKTIER-CONTROL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YKTIER-AMOUNT-1-AMT / 4
           COMPUTE WS-WORK-AMT-2 = YKTIER-AMOUNT-1-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2330-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-LAST-ACT-DATE > 0 AND YKTIER-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKTIER-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YKTIER-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YKTIER-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD YKTIER-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKTIER-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * FJ -> LA WHEN MATURITY DATE IS PAST
      * LA -> SP WHEN MATURITY DATE IS PAST
      * SP -> UC WHEN MATURITY DATE IS PAST
      * UC -> FJ WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2340-SET-CYCLE.
           EVALUATE YKTIER-CYCLE-CD
               WHEN 'FJ'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LA' TO YKTIER-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LA'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SP' TO YKTIER-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UC' TO YKTIER-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UC'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FJ' TO YKTIER-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO YKTIER-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * FJ -> LA WHEN LAST MAINT DATE IS PAST
      * LA -> SP WHEN LAST MAINT DATE IS PAST
      * SP -> UC WHEN LAST MAINT DATE IS PAST
      * UC -> FJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2360-SET-CYCLE.
           EVALUATE YKTIER-CYCLE-CD
               WHEN 'FJ'
                   IF YKTIER-LAST-MAINT-DATE > 0 AND
                       YKTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'LA' TO YKTIER-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LA'
                   IF YKTIER-LAST-MAINT-DATE > 0 AND
                       YKTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'SP' TO YKTIER-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF YKTIER-LAST-MAINT-DATE > 0 AND
                       YKTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UC' TO YKTIER-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UC'
                   IF YKTIER-LAST-MAINT-DATE > 0 AND
                       YKTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FJ' TO YKTIER-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONTROL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2370-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YKTIER-CONTROL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = YKTIER-CONTROL-AMT - (WS-WORK-AMT *
               3)
           COMPUTE YKTIER-CONTROL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2380-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKTIER-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKTIER-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E776 CONTACT REQUIRED
      * E044 WORK AMT NEGATIVE
      * E791 WORK AMT OVER 99999
      * E721 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2390-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKTIER-CONTACT = SPACES
                   MOVE 'E776' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E791' TO WS-REJECT-CODE
               WHEN YKTIER-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E721' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       2400-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKTIER-AMOUNT-2-AMT ROUNDED =
                   YKTIER-CONTROL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YKTIER-AMOUNT-2-AMT
           END-IF
           ADD YKTIER-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2410-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO YKTIER-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2420-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO YKTIER-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2430-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YKTIER-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO YKTIER-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKTIER-REVIEW-FLAG = 'Y'
                   COMPUTE YKTIER-AMOUNT-2-AMT ROUNDED = WS-WORK-AMT-2 *
                       YKTIER-PENALTY-PCT / 200
               WHEN YKTIER-CYCLE-CD = 'FJ'
                   MOVE 3.00 TO YKTIER-AMOUNT-2-AMT
               WHEN YKTIER-CYCLE-CD = 'LA'
                   MOVE 15.00 TO YKTIER-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YKTIER-AMOUNT-2-AMT ROUNDED = WS-WORK-AMT-2 *
                       YKTIER-PENALTY-PCT / 100
           END-EVALUATE
           IF YKTIER-AMOUNT-2-AMT > 0 AND YKTIER-AMOUNT-2-AMT < 5.00
               MOVE 5.00 TO YKTIER-AMOUNT-2-AMT
           END-IF
           IF YKTIER-AMOUNT-2-AMT > 25.00
               MOVE 25.00 TO YKTIER-AMOUNT-2-AMT
           END-IF
           ADD YKTIER-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2440-COUNT-RETRY.
           IF YKTIER-RETRY-CNT < 9999999
               ADD 1 TO YKTIER-RETRY-CNT
           END-IF
           IF YKTIER-RETRY-CNT NOT < 12
               MOVE 'Y' TO YKTIER-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2450-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YKTIER-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO YKTIER-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKTIER-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YKTIER-CONTROL-AMT ROUNDED =
                       YKTIER-AMOUNT-1-AMT * YKTIER-DISCOUNT / 200
               WHEN YKTIER-SOURCE-CD = 'BNX'
                   MOVE 3.00 TO YKTIER-CONTROL-AMT
               WHEN YKTIER-SOURCE-CD = 'FTB'
                   MOVE 15.00 TO YKTIER-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YKTIER-CONTROL-AMT ROUNDED =
                       YKTIER-AMOUNT-1-AMT * YKTIER-DISCOUNT / 100
           END-EVALUATE
           IF YKTIER-CONTROL-AMT > 0 AND YKTIER-CONTROL-AMT < 1.00
               MOVE 1.00 TO YKTIER-CONTROL-AMT
           END-IF
           IF YKTIER-CONTROL-AMT > 35.00
               MOVE 35.00 TO YKTIER-CONTROL-AMT
           END-IF
           ADD YKTIER-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2460-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO YKTIER-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIER-RATE(1) TO YKTIER-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-CONTROL-TIER-LOW(WS-TX)
                   MOVE WS-CONTROL-TIER-RATE(WS-TX) TO YKTIER-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKTIER-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKTIER-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2480-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YKTIER-AMOUNT-1-AMT / 4
           COMPUTE WS-WORK-AMT-2 = YKTIER-AMOUNT-1-AMT - (WS-WORK-AMT *
               4)
           COMPUTE YKTIER-AMOUNT-2-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO YKTIER-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2500-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YKTIER-AMOUNT-2-YTD / 12
           COMPUTE WS-WORK-AMT-2 = YKTIER-AMOUNT-2-YTD - (WS-WORK-AMT *
               12)
           COMPUTE YKTIER-CONTROL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKTIER-CYCLE-CD
                   ADD YKTIER-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKTIER-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKTIER-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2520-DESCRIBE-PIVOT.
           EVALUATE YKTIER-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E144 CUST ID REQUIRED
      * E644 CONTROL AMT NEGATIVE
      * E334 CONTROL AMT OVER 5000
      * E602 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2530-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKTIER-CUST-ID = SPACES
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN YKTIER-CONTROL-AMT < 0
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN YKTIER-CONTROL-AMT > 5000.00
                   MOVE 'E334' TO WS-REJECT-CODE
               WHEN YKTIER-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2540-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * YKTIER-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO YKTIER-CONTROL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2550-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2560-ACCRUE-AMOUNT-2.
           IF YKTIER-AMOUNT-2-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKTIER-AMOUNT-2-YTD * YKTIER-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO YKTIER-AMOUNT-2-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2570-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-LAST-MAINT-DATE > 0 AND YKTIER-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2580-LIMIT-AMOUNT-2.
           IF YKTIER-AMOUNT-2-YTD < 25.00
               MOVE 25.00 TO YKTIER-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKTIER-AMOUNT-2-YTD > 1000.00
               MOVE 1000.00 TO YKTIER-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2590-DESCRIBE-REGION.
           EVALUATE YKTIER-REGION-CD
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2600-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKTIER-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YKTIER-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD YKTIER-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YKTIER-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKTIER-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2610-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKTIER-AMOUNT-2-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL YTD FEE ON THE AMOUNT 2 YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2620-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN YKTIER-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO YKTIER-CONTROL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKTIER-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YKTIER-CONTROL-YTD ROUNDED =
                       YKTIER-AMOUNT-2-YTD * YKTIER-DISCOUNT / 200
               WHEN YKTIER-CYCLE-CD = 'FJ'
                   MOVE 10.00 TO YKTIER-CONTROL-YTD
               WHEN YKTIER-CYCLE-CD = 'LA'
                   MOVE 10.00 TO YKTIER-CONTROL-YTD
               WHEN OTHER
                   COMPUTE YKTIER-CONTROL-YTD ROUNDED =
                       YKTIER-AMOUNT-2-YTD * YKTIER-DISCOUNT / 100
           END-EVALUATE
           IF YKTIER-CONTROL-YTD > 0 AND YKTIER-CONTROL-YTD < 10.00
               MOVE 10.00 TO YKTIER-CONTROL-YTD
           END-IF
           IF YKTIER-CONTROL-YTD > 250.00
               MOVE 250.00 TO YKTIER-CONTROL-YTD
           END-IF
           ADD YKTIER-CONTROL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN MATURITY DATE IS PAST
      * FTB -> GAP WHEN MATURITY DATE IS PAST
      * GAP -> PAP WHEN MATURITY DATE IS PAST
      * PAP -> VGC WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2630-SET-SOURCE.
           EVALUATE YKTIER-SOURCE-CD
               WHEN 'BNX'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YKTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YKTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YKTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YKTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2640-LIMIT-AMOUNT-2.
           IF YKTIER-AMOUNT-2-YTD < 25.00
               MOVE 25.00 TO YKTIER-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKTIER-AMOUNT-2-YTD > 5000.00
               MOVE 5000.00 TO YKTIER-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 SCHED TIER FOR THE AMOUNT 1 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2650-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-SCHED-RATE(1) TO YKTIER-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YKTIER-AMOUNT-1-AMT NOT < WS-AMOUNT-1-SCHED-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-SCHED-RATE(WS-TX) TO
                       YKTIER-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKTIER-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKTIER-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN MATURITY DATE IS PAST
      * NNG -> PTC WHEN MATURITY DATE IS PAST
      * PTC -> WZW WHEN MATURITY DATE IS PAST
      * WZW -> KGT WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2660-SET-PIVOT.
           EVALUATE YKTIER-PIVOT-CD
               WHEN 'KGT'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YKTIER-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YKTIER-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YKTIER-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YKTIER-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2670-COUNT-ITEM.
           IF YKTIER-ITEM-CNT < 9999999
               ADD 1 TO YKTIER-ITEM-CNT
           END-IF
           IF YKTIER-ITEM-CNT NOT < 12
               MOVE 'Y' TO YKTIER-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN LAST MAINT DATE IS PAST
      * NNG -> PTC WHEN LAST MAINT DATE IS PAST
      * PTC -> WZW WHEN LAST MAINT DATE IS PAST
      * WZW -> KGT WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2680-SET-PIVOT.
           EVALUATE YKTIER-PIVOT-CD
               WHEN 'KGT'
                   IF YKTIER-LAST-MAINT-DATE > 0 AND
                       YKTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YKTIER-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YKTIER-LAST-MAINT-DATE > 0 AND
                       YKTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YKTIER-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YKTIER-LAST-MAINT-DATE > 0 AND
                       YKTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YKTIER-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YKTIER-LAST-MAINT-DATE > 0 AND
                       YKTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YKTIER-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2690-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YKTIER-MATURITY-DATE > 0 AND YKTIER-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKTIER-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKTIER-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YKTIER-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD YKTIER-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YKTIER-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKTIER-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO YKTIER-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO YKTIER-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO YKTIER-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'YKTIER EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
