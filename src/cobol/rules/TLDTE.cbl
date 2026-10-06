      *================================================================*
      * PROGRAM  : TLDTE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TELLER BALANCING RULE - PRICING. CALLED BY THE      *
      *            TELLER BALANCING BATCH AND ONLINE PROGRAMS.         *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-10-13 SKN  ORIGINAL                                       *
      * 1995-09-23 CRP  VAULT-BUY RERUN PROTECTION PRB1109             *
      * 2010-12-20 RJT  VAULT-BUY LIMIT REVISED CHG2623                *
      * 2010-09-09 SKN  AUDIT FINDING - BATCH CONTROL TOTALS PRB4527   *
      * 2011-06-22 TWH  OVERSHORT RERUN PROTECTION CHG8204             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLDTE.
       AUTHOR. M K LOWE.

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
                                           VALUE 11.000000.
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
      * WS-BATCH-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BATCH-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000212000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000834000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000858500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020050000000833000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020550000000406500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030550000000654000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030650000000329000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030750000000580000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033250000000399000'.
       01  WS-BATCH-BAND-TBL REDEFINES WS-BATCH-BAND-VALUES.
           05  WS-BATCH-BAND-ROW OCCURS 9 TIMES.
               10  WS-BATCH-BAND-LOW       PIC 9(09)V99.
               10  WS-BATCH-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BATCH-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BATCH-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000726000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000888500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000516500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000022000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000444000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021050000000331500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031050000000868000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032050000000290500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00037050000000671000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00042050000000281500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00044550000000478500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00044650000000492000'.
       01  WS-BATCH-TIER-TBL REDEFINES WS-BATCH-TIER-VALUES.
           05  WS-BATCH-TIER-ROW OCCURS 12 TIMES.
               10  WS-BATCH-TIER-LOW       PIC 9(09)V99.
               10  WS-BATCH-TIER-RATE      PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TLDTEP.

       PROCEDURE DIVISION USING TLDTE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-COUNT-RETRY
           PERFORM 2010-LIMIT-ITEM
           PERFORM 2020-ACCRUE-CASH-IN
           PERFORM 2030-AGE-LAST-ACT
           PERFORM 2040-ASSESS-SUSPEND
           PERFORM 2050-ACCUM-DRAWER
           PERFORM 2060-ASSESS-HOLD
           PERFORM 2070-SPLIT-VAULT-BUY
           PERFORM 2080-SPLIT-CASH-OUT
           PERFORM 2090-CHECK-LAST-ACT
           PERFORM 2100-EDIT-DESC
           PERFORM 2110-COUNT-RETRY
           PERFORM 2120-LIMIT-VAULT-BUY
           PERFORM 2130-SELECT-FEE-PCT
           PERFORM 2140-COUNT-NOTICE
           PERFORM 2150-PRORATE-BATCH
           PERFORM 2160-ACCRUE-FEE-PCT
           PERFORM 2170-ASSESS-SUSPEND
           PERFORM 2180-CHECK-LAST-ACT
           PERFORM 2190-DESCRIBE-CYCLE
           PERFORM 2200-ACCRUE-CASH-IN
           PERFORM 2210-COUNT-NOTICE
           PERFORM 2220-ACCRUE-CASH-OUT
           PERFORM 2230-COUNT-RETRY
           PERFORM 2240-SET-CHANNEL
           PERFORM 2250-FLAG-CASH-OUT
           PERFORM 2260-EDIT-DESC
           PERFORM 2270-ACCRUE-DISCOUNT
           PERFORM 2280-CHECK-LAST-ACT
           PERFORM 2290-EDIT-DESC
           PERFORM 2300-CHECK-LAST-STMT
           PERFORM 2310-PRORATE-CASH-OUT
           PERFORM 2320-ASSESS-HOLD
           PERFORM 2330-ACCUM-SOURCE
           PERFORM 2340-LIMIT-CASH-IN
           PERFORM 2350-DESCRIBE-DRAWER
           PERFORM 2360-ASSESS-HOLD
           PERFORM 2370-AGE-NOTICE
           PERFORM 2380-LIMIT-VAULT-BUY
           PERFORM 2390-COUNT-RETRY
           PERFORM 2400-DESCRIBE-SOURCE
           PERFORM 2410-PRORATE-CASH-IN
           PERFORM 2420-ACCUM-CYCLE
           PERFORM 2430-SPLIT-CASH-IN
           PERFORM 2440-PRORATE-BATCH
           PERFORM 2450-CHECK-EFFECTIVE
           PERFORM 2460-LIMIT-BATCH
           PERFORM 2470-ACCUM-SOURCE
           PERFORM 2480-COUNT-RETRY
           PERFORM 2490-ACCUM-CYCLE
           PERFORM 2500-DESCRIBE-DRAWER
           PERFORM 2510-SET-SOURCE
           PERFORM 2520-LIMIT-CASH-OUT
           PERFORM 2530-AGE-EFFECTIVE
           PERFORM 2540-ACCRUE-VAULT-BUY
           PERFORM 2550-FLAG-BATCH
           PERFORM 2560-ACCRUE-CASH-IN
           PERFORM 2570-FLAG-CASH-OUT
           PERFORM 2580-AGE-LAST-STMT
           PERFORM 2590-PRORATE-CASH-IN
           PERFORM 2600-PRORATE-CASH-IN
           PERFORM 2610-ASSESS-STOP
           PERFORM 2620-FLAG-CASH-IN
           PERFORM 2630-EDIT-CONTACT
           PERFORM 2640-ACCUM-DRAWER
           PERFORM 2650-ACCUM-DRAWER
           PERFORM 2660-CHECK-NOTICE
           PERFORM 2670-PRORATE-CASH-OUT
           PERFORM 2680-ACCRUE-CASH-OUT
           PERFORM 2690-SELECT-FEE-PCT
           PERFORM 2700-EDIT-CONTACT
           PERFORM 2710-ASSESS-SUSPEND
           PERFORM 2720-PRORATE-VAULT-BUY
           PERFORM 2730-SPLIT-BATCH
           PERFORM 2740-AGE-NOTICE
           PERFORM 2750-CHECK-LAST-ACT
           PERFORM 2760-DESCRIBE-DRAWER
           PERFORM 2770-SELECT-DISCOUNT
           PERFORM 2780-CHECK-EFFECTIVE
           PERFORM 2790-EDIT-DESC
           PERFORM 2800-CHECK-NOTICE
           PERFORM 2810-PRORATE-CASH-OUT
           PERFORM 2820-AGE-LAST-STMT
           PERFORM 2830-LIMIT-CASH-OUT
           PERFORM 2840-LIMIT-BATCH
           PERFORM 2850-CHECK-LAST-ACT
           PERFORM 2860-FLAG-ITEM
           PERFORM 2870-DESCRIBE-SOURCE
           PERFORM 2880-SELECT-DISCOUNT
           PERFORM 2890-CHECK-LAST-STMT
           PERFORM 2900-EDIT-DESC
           PERFORM 2910-DESCRIBE-DRAWER
           PERFORM 2920-ASSESS-STOP
           PERFORM 2930-SPLIT-VAULT-BUY
           PERFORM 2940-ACCUM-SOURCE
           PERFORM 2950-DESCRIBE-DRAWER
           PERFORM 2960-CHECK-LAST-STMT
           PERFORM 2970-ACCUM-CYCLE
           PERFORM 2980-SET-CYCLE
           PERFORM 2990-ASSESS-HOLD
           PERFORM 3000-FLAG-CASH-IN
           PERFORM 3010-SPLIT-CASH-OUT
           PERFORM 3020-FLAG-ITEM
           PERFORM 3030-ASSESS-HOLD
           PERFORM 3040-COUNT-RETRY
           PERFORM 3050-ACCRUE-CASH-IN
           PERFORM 3060-ACCRUE-CASH-IN
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TLDTE-RETURN-CODE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2000-COUNT-RETRY.
           IF TLDTE-RETRY-CNT < 9999999
               ADD 1 TO TLDTE-RETRY-CNT
           END-IF
           IF TLDTE-RETRY-CNT NOT < 10
               MOVE 'Y' TO TLDTE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2010-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-CASH-IN.
           IF TLDTE-CASH-IN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLDTE-CASH-IN-AMT * TLDTE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TLDTE-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-LAST-ACT-DATE > 0 AND TLDTE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLDTE-VAULT-BUY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLDTE-VAULT-BUY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLDTE-VAULT-BUY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TLDTE-VAULT-BUY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLDTE-VAULT-BUY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2040-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLDTE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLDTE-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLDTE-HOLD-FLAG = 'Y'
                   COMPUTE TLDTE-CASH-IN-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLDTE-DISCOUNT / 200
               WHEN TLDTE-CYCLE-CD = 'KX'
                   MOVE 10.00 TO TLDTE-CASH-IN-AMT
               WHEN TLDTE-CYCLE-CD = 'TA'
                   MOVE 10.00 TO TLDTE-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TLDTE-CASH-IN-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLDTE-DISCOUNT / 100
           END-EVALUATE
           IF TLDTE-CASH-IN-AMT > 0 AND TLDTE-CASH-IN-AMT < 5.00
               MOVE 5.00 TO TLDTE-CASH-IN-AMT
           END-IF
           IF TLDTE-CASH-IN-AMT > 250.00
               MOVE 250.00 TO TLDTE-CASH-IN-AMT
           END-IF
           ADD TLDTE-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2050-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLDTE-DRAWER-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLDTE-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT YTD FEE ON THE CASH OUT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2060-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TLDTE-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TLDTE-CASH-OUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLDTE-SUSPEND-FLAG = 'Y'
                   COMPUTE TLDTE-CASH-OUT-YTD ROUNDED =
                       TLDTE-CASH-OUT-AMT * TLDTE-DISCOUNT / 200
               WHEN TLDTE-DRAWER-CD = 'D'
                   MOVE 12.00 TO TLDTE-CASH-OUT-YTD
               WHEN TLDTE-DRAWER-CD = 'F'
                   MOVE 12.00 TO TLDTE-CASH-OUT-YTD
               WHEN OTHER
                   COMPUTE TLDTE-CASH-OUT-YTD ROUNDED =
                       TLDTE-CASH-OUT-AMT * TLDTE-DISCOUNT / 100
           END-EVALUATE
           IF TLDTE-CASH-OUT-YTD > 0 AND TLDTE-CASH-OUT-YTD < 1.00
               MOVE 1.00 TO TLDTE-CASH-OUT-YTD
           END-IF
           IF TLDTE-CASH-OUT-YTD > 50.00
               MOVE 50.00 TO TLDTE-CASH-OUT-YTD
           END-IF
           ADD TLDTE-CASH-OUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TLDTE-VAULT-BUY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLDTE-VAULT-BUY-YTD - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2080-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TLDTE-CASH-OUT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLDTE-CASH-OUT-AMT - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2090-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TLDTE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E292 DESC REQUIRED
      * E842 VAULT BUY YTD NEGATIVE
      * E785 VAULT BUY YTD OVER 10000
      * E225 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLDTE-DESC = SPACES
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN TLDTE-VAULT-BUY-YTD < 0
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN TLDTE-VAULT-BUY-YTD > 10000.00
                   MOVE 'E785' TO WS-REJECT-CODE
               WHEN TLDTE-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2110-COUNT-RETRY.
           IF TLDTE-RETRY-CNT < 9999999
               ADD 1 TO TLDTE-RETRY-CNT
           END-IF
           IF TLDTE-RETRY-CNT NOT < 3
               MOVE 'Y' TO TLDTE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2120-LIMIT-VAULT-BUY.
           IF TLDTE-VAULT-BUY-YTD < 100.00
               MOVE 100.00 TO TLDTE-VAULT-BUY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLDTE-VAULT-BUY-YTD > 1000.00
               MOVE 1000.00 TO TLDTE-VAULT-BUY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BATCH TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2130-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BATCH-TIER-RATE(1) TO TLDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-BATCH-TIER-LOW(WS-TX)
                   MOVE WS-BATCH-TIER-RATE(WS-TX) TO TLDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-NOTICE.
           IF TLDTE-NOTICE-CNT < 9999999
               ADD 1 TO TLDTE-NOTICE-CNT
           END-IF
           IF TLDTE-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TLDTE-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       2150-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLDTE-BATCH-AMT ROUNDED =
                   TLDTE-VAULT-BUY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLDTE-BATCH-AMT
           END-IF
           ADD TLDTE-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2160-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TLDTE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY YTD FEE ON THE CASH OUT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2170-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLDTE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLDTE-VAULT-BUY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLDTE-HOLD-FLAG = 'Y'
                   COMPUTE TLDTE-VAULT-BUY-YTD ROUNDED =
                       TLDTE-CASH-OUT-YTD * TLDTE-DISCOUNT / 200
               WHEN TLDTE-CYCLE-CD = 'KX'
                   MOVE 3.00 TO TLDTE-VAULT-BUY-YTD
               WHEN TLDTE-CYCLE-CD = 'TA'
                   MOVE 7.00 TO TLDTE-VAULT-BUY-YTD
               WHEN OTHER
                   COMPUTE TLDTE-VAULT-BUY-YTD ROUNDED =
                       TLDTE-CASH-OUT-YTD * TLDTE-DISCOUNT / 100
           END-EVALUATE
           IF TLDTE-VAULT-BUY-YTD > 0 AND TLDTE-VAULT-BUY-YTD < 1.00
               MOVE 1.00 TO TLDTE-VAULT-BUY-YTD
           END-IF
           IF TLDTE-VAULT-BUY-YTD > 25.00
               MOVE 25.00 TO TLDTE-VAULT-BUY-YTD
           END-IF
           ADD TLDTE-VAULT-BUY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLDTE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2190-DESCRIBE-CYCLE.
           EVALUATE TLDTE-CYCLE-CD
               WHEN 'KX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2200-ACCRUE-CASH-IN.
           IF TLDTE-CASH-IN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLDTE-CASH-IN-AMT * TLDTE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2210-COUNT-NOTICE.
           IF TLDTE-NOTICE-CNT < 9999999
               ADD 1 TO TLDTE-NOTICE-CNT
           END-IF
           IF TLDTE-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TLDTE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2220-ACCRUE-CASH-OUT.
           IF TLDTE-CASH-OUT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLDTE-CASH-OUT-YTD * TLDTE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-RETRY.
           IF TLDTE-RETRY-CNT < 9999999
               ADD 1 TO TLDTE-RETRY-CNT
           END-IF
           IF TLDTE-RETRY-CNT NOT < 5
               MOVE 'Y' TO TLDTE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN LAST ACT DATE IS PAST
      * F -> M WHEN LAST ACT DATE IS PAST
      * M -> N WHEN LAST ACT DATE IS PAST
      * N -> R WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2240-SET-CHANNEL.
           EVALUATE TLDTE-CHANNEL-CD
               WHEN 'D'
                   IF TLDTE-LAST-ACT-DATE > 0 AND TLDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TLDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TLDTE-LAST-ACT-DATE > 0 AND TLDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLDTE-LAST-ACT-DATE > 0 AND TLDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TLDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TLDTE-LAST-ACT-DATE > 0 AND TLDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TLDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2250-FLAG-CASH-OUT.
           IF TLDTE-CASH-OUT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLDTE-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E872 DESC REQUIRED
      * E408 BATCH AMT NEGATIVE
      * E549 BATCH AMT OVER 1000000
      * E165 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2260-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLDTE-DESC = SPACES
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN TLDTE-BATCH-AMT < 0
                   MOVE 'E408' TO WS-REJECT-CODE
               WHEN TLDTE-BATCH-AMT > 1000000.00
                   MOVE 'E549' TO WS-REJECT-CODE
               WHEN TLDTE-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E165' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TLDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2280-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TLDTE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E770 DESC REQUIRED
      * E519 CASH OUT AMT NEGATIVE
      * E845 CASH OUT AMT OVER 10000
      * E201 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2290-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLDTE-DESC = SPACES
                   MOVE 'E770' TO WS-REJECT-CODE
               WHEN TLDTE-CASH-OUT-AMT < 0
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN TLDTE-CASH-OUT-AMT > 10000.00
                   MOVE 'E845' TO WS-REJECT-CODE
               WHEN TLDTE-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E201' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLDTE-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH IN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       2310-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLDTE-CASH-OUT-AMT ROUNDED =
                   TLDTE-CASH-IN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLDTE-CASH-OUT-AMT
           END-IF
           ADD TLDTE-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE WORK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2320-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TLDTE-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TLDTE-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLDTE-STOP-FLAG = 'Y'
                   COMPUTE TLDTE-CASH-OUT-AMT ROUNDED = WS-WORK-AMT *
                       TLDTE-DISCOUNT / 200
               WHEN TLDTE-CYCLE-CD = 'KX'
                   MOVE 12.00 TO TLDTE-CASH-OUT-AMT
               WHEN TLDTE-CYCLE-CD = 'TA'
                   MOVE 10.00 TO TLDTE-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TLDTE-CASH-OUT-AMT ROUNDED = WS-WORK-AMT *
                       TLDTE-DISCOUNT / 100
           END-EVALUATE
           IF TLDTE-CASH-OUT-AMT > 0 AND TLDTE-CASH-OUT-AMT < 5.00
               MOVE 5.00 TO TLDTE-CASH-OUT-AMT
           END-IF
           IF TLDTE-CASH-OUT-AMT > 250.00
               MOVE 250.00 TO TLDTE-CASH-OUT-AMT
           END-IF
           ADD TLDTE-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2330-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLDTE-SOURCE-CD
                   ADD TLDTE-CASH-OUT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLDTE-CASH-OUT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2340-LIMIT-CASH-IN.
           IF TLDTE-CASH-IN-AMT < 0.00
               MOVE 0.00 TO TLDTE-CASH-IN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLDTE-CASH-IN-AMT > 500.00
               MOVE 500.00 TO TLDTE-CASH-IN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2350-DESCRIBE-DRAWER.
           EVALUATE TLDTE-DRAWER-CD
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'F'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE CASH OUT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2360-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TLDTE-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TLDTE-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLDTE-STOP-FLAG = 'Y'
                   COMPUTE TLDTE-CASH-IN-AMT ROUNDED =
                       TLDTE-CASH-OUT-YTD * TLDTE-FEE-PCT / 200
               WHEN TLDTE-CHANNEL-CD = 'D'
                   MOVE 7.00 TO TLDTE-CASH-IN-AMT
               WHEN TLDTE-CHANNEL-CD = 'F'
                   MOVE 5.00 TO TLDTE-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TLDTE-CASH-IN-AMT ROUNDED =
                       TLDTE-CASH-OUT-YTD * TLDTE-FEE-PCT / 100
           END-EVALUATE
           IF TLDTE-CASH-IN-AMT > 0 AND TLDTE-CASH-IN-AMT < 5.00
               MOVE 5.00 TO TLDTE-CASH-IN-AMT
           END-IF
           IF TLDTE-CASH-IN-AMT > 25.00
               MOVE 25.00 TO TLDTE-CASH-IN-AMT
           END-IF
           ADD TLDTE-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2370-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-NOTICE-DATE > 0 AND TLDTE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT BUY YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2380-LIMIT-VAULT-BUY.
           IF TLDTE-VAULT-BUY-YTD < 100.00
               MOVE 100.00 TO TLDTE-VAULT-BUY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLDTE-VAULT-BUY-YTD > 10000.00
               MOVE 10000.00 TO TLDTE-VAULT-BUY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2390-COUNT-RETRY.
           IF TLDTE-RETRY-CNT < 9999999
               ADD 1 TO TLDTE-RETRY-CNT
           END-IF
           IF TLDTE-RETRY-CNT NOT < 3
               MOVE 'Y' TO TLDTE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2400-DESCRIBE-SOURCE.
           EVALUATE TLDTE-SOURCE-CD
               WHEN 'AF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH IN AMT.
      *----------------------------------------------------------------*
       2410-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLDTE-CASH-IN-AMT ROUNDED =
                   TLDTE-CASH-OUT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLDTE-CASH-IN-AMT
           END-IF
           ADD TLDTE-CASH-IN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2420-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLDTE-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH IN AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2430-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TLDTE-CASH-IN-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLDTE-CASH-IN-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLDTE-CASH-OUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH IN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       2440-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLDTE-BATCH-AMT ROUNDED =
                   TLDTE-CASH-IN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLDTE-BATCH-AMT
           END-IF
           ADD TLDTE-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2450-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TLDTE-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2460-LIMIT-BATCH.
           IF TLDTE-BATCH-AMT < 50.00
               MOVE 50.00 TO TLDTE-BATCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLDTE-BATCH-AMT > 5000.00
               MOVE 5000.00 TO TLDTE-BATCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT BUY YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2470-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLDTE-SOURCE-CD
                   ADD TLDTE-VAULT-BUY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLDTE-VAULT-BUY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2480-COUNT-RETRY.
           IF TLDTE-RETRY-CNT < 9999999
               ADD 1 TO TLDTE-RETRY-CNT
           END-IF
           IF TLDTE-RETRY-CNT NOT < 10
               MOVE 'Y' TO TLDTE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLDTE-CYCLE-CD
                   ADD TLDTE-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLDTE-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2500-DESCRIBE-DRAWER.
           EVALUATE TLDTE-DRAWER-CD
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN EFFECTIVE DATE IS PAST
      * PT -> SZ WHEN EFFECTIVE DATE IS PAST
      * SZ -> VK WHEN EFFECTIVE DATE IS PAST
      * VK -> VW WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2510-SET-SOURCE.
           EVALUATE TLDTE-SOURCE-CD
               WHEN 'AF'
                   IF TLDTE-EFFECTIVE-DATE > 0 AND TLDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLDTE-EFFECTIVE-DATE > 0 AND TLDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TLDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TLDTE-EFFECTIVE-DATE > 0 AND TLDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VK' TO TLDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TLDTE-EFFECTIVE-DATE > 0 AND TLDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VW' TO TLDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH OUT AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2520-LIMIT-CASH-OUT.
           IF TLDTE-CASH-OUT-AMT < 10.00
               MOVE 10.00 TO TLDTE-CASH-OUT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLDTE-CASH-OUT-AMT > 5000.00
               MOVE 5000.00 TO TLDTE-CASH-OUT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2530-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-EFFECTIVE-DATE > 0 AND TLDTE-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLDTE-VAULT-BUY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLDTE-VAULT-BUY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TLDTE-VAULT-BUY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLDTE-VAULT-BUY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLDTE-VAULT-BUY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT BUY YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2540-ACCRUE-VAULT-BUY.
           IF TLDTE-VAULT-BUY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLDTE-VAULT-BUY-YTD * TLDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLDTE-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2550-FLAG-BATCH.
           IF TLDTE-BATCH-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLDTE-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2560-ACCRUE-CASH-IN.
           IF TLDTE-CASH-IN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLDTE-CASH-IN-AMT * TLDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLDTE-CASH-OUT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2570-FLAG-CASH-OUT.
           IF TLDTE-CASH-OUT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLDTE-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2580-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-LAST-STMT-DATE > 0 AND TLDTE-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLDTE-CASH-OUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLDTE-CASH-OUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLDTE-CASH-OUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLDTE-CASH-OUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLDTE-CASH-OUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASH IN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH IN YTD.
      *----------------------------------------------------------------*
       2590-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLDTE-CASH-IN-YTD ROUNDED =
                   TLDTE-CASH-IN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLDTE-CASH-IN-YTD
           END-IF
           ADD TLDTE-CASH-IN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CASH IN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2600-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLDTE-CASH-IN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY YTD FEE ON THE BATCH AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2610-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLDTE-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLDTE-VAULT-BUY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLDTE-SUSPEND-FLAG = 'Y'
                   COMPUTE TLDTE-VAULT-BUY-YTD ROUNDED = TLDTE-BATCH-AMT
                       * TLDTE-DISCOUNT / 200
               WHEN TLDTE-DRAWER-CD = 'D'
                   MOVE 5.00 TO TLDTE-VAULT-BUY-YTD
               WHEN TLDTE-DRAWER-CD = 'F'
                   MOVE 10.00 TO TLDTE-VAULT-BUY-YTD
               WHEN OTHER
                   COMPUTE TLDTE-VAULT-BUY-YTD ROUNDED = TLDTE-BATCH-AMT
                       * TLDTE-DISCOUNT / 100
           END-EVALUATE
           IF TLDTE-VAULT-BUY-YTD > 0 AND TLDTE-VAULT-BUY-YTD < 5.00
               MOVE 5.00 TO TLDTE-VAULT-BUY-YTD
           END-IF
           IF TLDTE-VAULT-BUY-YTD > 150.00
               MOVE 150.00 TO TLDTE-VAULT-BUY-YTD
           END-IF
           ADD TLDTE-VAULT-BUY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH IN YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2620-FLAG-CASH-IN.
           IF TLDTE-CASH-IN-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLDTE-CASH-IN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E945 CONTACT REQUIRED
      * E608 CASH OUT YTD NEGATIVE
      * E710 CASH OUT YTD OVER 50000
      * E192 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2630-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLDTE-CONTACT = SPACES
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN TLDTE-CASH-OUT-YTD < 0
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN TLDTE-CASH-OUT-YTD > 50000.00
                   MOVE 'E710' TO WS-REJECT-CODE
               WHEN TLDTE-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E192' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLDTE-DRAWER-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLDTE-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT BUY YTD BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2650-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLDTE-DRAWER-CD
                   ADD TLDTE-VAULT-BUY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLDTE-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLDTE-VAULT-BUY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2660-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLDTE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT YTD.
      *----------------------------------------------------------------*
       2670-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLDTE-CASH-OUT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLDTE-CASH-OUT-YTD
           END-IF
           ADD TLDTE-CASH-OUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2680-ACCRUE-CASH-OUT.
           IF TLDTE-CASH-OUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLDTE-CASH-OUT-AMT * TLDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLDTE-BATCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BATCH BAND TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2690-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BATCH-BAND-RATE(1) TO TLDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLDTE-CASH-IN-AMT NOT < WS-BATCH-BAND-LOW(WS-TX)
                   MOVE WS-BATCH-BAND-RATE(WS-TX) TO TLDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E024 CONTACT REQUIRED
      * E039 WORK AMT NEGATIVE
      * E593 WORK AMT OVER 250000
      * E892 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2700-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLDTE-CONTACT = SPACES
                   MOVE 'E024' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E039' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E593' TO WS-REJECT-CODE
               WHEN TLDTE-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E892' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CASH OUT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2710-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLDTE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLDTE-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLDTE-CASH-OUT-AMT *
                       TLDTE-DISCOUNT / 200
               WHEN TLDTE-CYCLE-CD = 'KX'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN TLDTE-CYCLE-CD = 'TA'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLDTE-CASH-OUT-AMT *
                       TLDTE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CASH IN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT BUY YTD.
      *----------------------------------------------------------------*
       2720-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLDTE-VAULT-BUY-YTD ROUNDED =
                   TLDTE-CASH-IN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLDTE-VAULT-BUY-YTD
           END-IF
           ADD TLDTE-VAULT-BUY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2730-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TLDTE-BATCH-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLDTE-BATCH-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLDTE-CASH-OUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2740-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-NOTICE-DATE > 0 AND TLDTE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2750-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TLDTE-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2760-DESCRIBE-DRAWER.
           EVALUATE TLDTE-DRAWER-CD
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BATCH TIER TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2770-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-BATCH-TIER-RATE(1) TO TLDTE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TLDTE-CASH-IN-AMT NOT < WS-BATCH-TIER-LOW(WS-TX)
                   MOVE WS-BATCH-TIER-RATE(WS-TX) TO TLDTE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLDTE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLDTE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2780-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TLDTE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E364 DESC REQUIRED
      * E767 WORK AMT NEGATIVE
      * E910 WORK AMT OVER 1000000
      * E316 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2790-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLDTE-DESC = SPACES
                   MOVE 'E364' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN TLDTE-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E316' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2800-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLDTE-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BATCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       2810-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLDTE-CASH-OUT-AMT ROUNDED =
                   TLDTE-BATCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLDTE-CASH-OUT-AMT
           END-IF
           ADD TLDTE-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2820-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-LAST-STMT-DATE > 0 AND TLDTE-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLDTE-CASH-IN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLDTE-CASH-IN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLDTE-CASH-IN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLDTE-CASH-IN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLDTE-CASH-IN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH OUT AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2830-LIMIT-CASH-OUT.
           IF TLDTE-CASH-OUT-AMT < 50.00
               MOVE 50.00 TO TLDTE-CASH-OUT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLDTE-CASH-OUT-AMT > 5000.00
               MOVE 5000.00 TO TLDTE-CASH-OUT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2840-LIMIT-BATCH.
           IF TLDTE-BATCH-AMT < 0.00
               MOVE 0.00 TO TLDTE-BATCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLDTE-BATCH-AMT > 500.00
               MOVE 500.00 TO TLDTE-BATCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2850-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TLDTE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2860-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2870-DESCRIBE-SOURCE.
           EVALUATE TLDTE-SOURCE-CD
               WHEN 'AF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BATCH BAND TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2880-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-BATCH-BAND-RATE(1) TO TLDTE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLDTE-CASH-IN-AMT NOT < WS-BATCH-BAND-LOW(WS-TX)
                   MOVE WS-BATCH-BAND-RATE(WS-TX) TO TLDTE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLDTE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLDTE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2890-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TLDTE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E894 DESC REQUIRED
      * E681 WORK AMT 2 NEGATIVE
      * E744 WORK AMT 2 OVER 10000
      * E235 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2900-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLDTE-DESC = SPACES
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN TLDTE-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E235' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2910-DESCRIBE-DRAWER.
           EVALUATE TLDTE-DRAWER-CD
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2920-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLDTE-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLDTE-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLDTE-HOLD-FLAG = 'Y'
                   COMPUTE TLDTE-CASH-OUT-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLDTE-FEE-PCT / 200
               WHEN TLDTE-SOURCE-CD = 'AF'
                   MOVE 7.00 TO TLDTE-CASH-OUT-AMT
               WHEN TLDTE-SOURCE-CD = 'PT'
                   MOVE 15.00 TO TLDTE-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TLDTE-CASH-OUT-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLDTE-FEE-PCT / 100
           END-EVALUATE
           IF TLDTE-CASH-OUT-AMT > 0 AND TLDTE-CASH-OUT-AMT < 5.00
               MOVE 5.00 TO TLDTE-CASH-OUT-AMT
           END-IF
           IF TLDTE-CASH-OUT-AMT > 25.00
               MOVE 25.00 TO TLDTE-CASH-OUT-AMT
           END-IF
           ADD TLDTE-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2930-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TLDTE-VAULT-BUY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLDTE-VAULT-BUY-YTD - (WS-WORK-AMT *
               3)
           COMPUTE TLDTE-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2940-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLDTE-SOURCE-CD
                   ADD TLDTE-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLDTE-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2950-DESCRIBE-DRAWER.
           EVALUATE TLDTE-DRAWER-CD
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2960-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TLDTE-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLDTE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLDTE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2970-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLDTE-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN NOTICE DATE IS PAST
      * TA -> TG WHEN NOTICE DATE IS PAST
      * TG -> WG WHEN NOTICE DATE IS PAST
      * WG -> KX WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2980-SET-CYCLE.
           EVALUATE TLDTE-CYCLE-CD
               WHEN 'KX'
                   IF TLDTE-NOTICE-DATE > 0 AND TLDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TLDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TLDTE-NOTICE-DATE > 0 AND TLDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TLDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TLDTE-NOTICE-DATE > 0 AND TLDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TLDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TLDTE-NOTICE-DATE > 0 AND TLDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TLDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CASH IN AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2990-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TLDTE-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLDTE-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLDTE-CASH-IN-AMT *
                       TLDTE-FEE-PCT / 200
               WHEN TLDTE-CHANNEL-CD = 'D'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN TLDTE-CHANNEL-CD = 'F'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLDTE-CASH-IN-AMT *
                       TLDTE-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH IN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3000-FLAG-CASH-IN.
           IF TLDTE-CASH-IN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLDTE-CASH-IN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3010-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TLDTE-CASH-OUT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLDTE-CASH-OUT-YTD - (WS-WORK-AMT *
               4)
           COMPUTE TLDTE-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3020-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE BATCH AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3030-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TLDTE-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLDTE-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLDTE-BATCH-AMT *
                       TLDTE-DISCOUNT / 200
               WHEN TLDTE-CYCLE-CD = 'KX'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN TLDTE-CYCLE-CD = 'TA'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLDTE-BATCH-AMT *
                       TLDTE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-RETRY.
           IF TLDTE-RETRY-CNT < 9999999
               ADD 1 TO TLDTE-RETRY-CNT
           END-IF
           IF TLDTE-RETRY-CNT NOT < 3
               MOVE 'Y' TO TLDTE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-CASH-IN.
           IF TLDTE-CASH-IN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLDTE-CASH-IN-YTD * TLDTE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TLDTE-BATCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-CASH-IN.
           IF TLDTE-CASH-IN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLDTE-CASH-IN-AMT * TLDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLDTE-BATCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TLDTE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TLDTE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TLDTE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TLDTE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
