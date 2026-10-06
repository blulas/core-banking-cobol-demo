      *================================================================*
      * PROGRAM  : XCCAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS RULE - DATE RULES. CALLED BY *
      *            THE CHRISTMAS CLUB SAVINGS BATCH AND ONLINE         *
      *            PROGRAMS.                                           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-17 MKL  ORIGINAL                                       *
      * 1992-06-06 MKL  NEW PENALTY REPORT COLUMNS REQ3357             *
      * 1992-07-19 GAF  CONTRIB ROUNDING - HALF UP INC1079             *
      * 1995-08-06 SKN  PAYOUT LIMIT REVISED REQ1551                   *
      * 1999-04-26 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-02-07 MKL  WEEKLY ROUNDING - HALF UP INC7019              *
      * 2004-04-20 TWH  NEW WEEKLY REPORT COLUMNS REQ7795              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCCAL.
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
                                           VALUE 10.000000.
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
      * WS-GOAL-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GOAL-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000298500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000764500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000205500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000671000'.
       01  WS-GOAL-SCHED-TBL REDEFINES WS-GOAL-SCHED-VALUES.
           05  WS-GOAL-SCHED-ROW OCCURS 4 TIMES.
               10  WS-GOAL-SCHED-LOW       PIC 9(09)V99.
               10  WS-GOAL-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WEEKLY-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WEEKLY-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000772000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000362000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000855000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000030500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000143500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000547500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000571000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000822000'.
       01  WS-WEEKLY-GRID-TBL REDEFINES WS-WEEKLY-GRID-VALUES.
           05  WS-WEEKLY-GRID-ROW OCCURS 8 TIMES.
               10  WS-WEEKLY-GRID-LOW      PIC 9(09)V99.
               10  WS-WEEKLY-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYOUT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYOUT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000800500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000477000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000415500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000497000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011150000000805500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016150000000431000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018650000000690000'.
       01  WS-PAYOUT-BAND-TBL REDEFINES WS-PAYOUT-BAND-VALUES.
           05  WS-PAYOUT-BAND-ROW OCCURS 7 TIMES.
               10  WS-PAYOUT-BAND-LOW      PIC 9(09)V99.
               10  WS-PAYOUT-BAND-RATE     PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY XCCALP.

       PROCEDURE DIVISION USING XCCAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PRORATE-WEEKLY
           PERFORM 2010-SPLIT-MISSED
           PERFORM 2020-COUNT-RETRY
           PERFORM 2030-CHECK-LAST-ACT
           PERFORM 2040-ACCRUE-PAYOUT
           PERFORM 2050-DESCRIBE-CYCLE
           PERFORM 2060-FLAG-PENALTY
           PERFORM 2070-ACCRUE-CONTRIB
           PERFORM 2080-AGE-LAST-ACT
           PERFORM 2090-LIMIT-ITEM
           PERFORM 2100-COUNT-CYCLE
           PERFORM 2110-SET-REASON
           PERFORM 2120-ACCUM-PLAN
           PERFORM 2130-COUNT-RETRY
           PERFORM 2140-DESCRIBE-REASON
           PERFORM 2150-AGE-LAST-STMT
           PERFORM 2160-PRORATE-MISSED
           PERFORM 2170-COUNT-RETRY
           PERFORM 2180-FLAG-ITEM
           PERFORM 2190-ACCRUE-PAYOUT
           PERFORM 2200-PRORATE-GOAL
           PERFORM 2210-ACCRUE-CONTRIB
           PERFORM 2220-AGE-NOTICE
           PERFORM 2230-FLAG-ITEM
           PERFORM 2240-ACCUM-PLAN
           PERFORM 2250-LIMIT-GOAL
           PERFORM 2260-ASSESS-OVERRIDE
           PERFORM 2270-COUNT-CYCLE
           PERFORM 2280-COUNT-RETRY
           PERFORM 2290-EDIT-CUST-ID
           PERFORM 2300-SPLIT-GOAL
           PERFORM 2310-SET-PLAN
           PERFORM 2320-COUNT-CYCLE
           PERFORM 2330-LIMIT-PAYOUT
           PERFORM 2340-EDIT-REF-NO
           PERFORM 2350-ACCUM-PLAN
           PERFORM 2360-DESCRIBE-CYCLE
           PERFORM 2370-PRORATE-MISSED
           PERFORM 2380-CHECK-NOTICE
           PERFORM 2390-FLAG-GOAL
           PERFORM 2400-SELECT-FEE-PCT
           PERFORM 2410-ACCUM-CYCLE
           PERFORM 2420-CHECK-MATURITY
           PERFORM 2430-PRORATE-MISSED
           PERFORM 2440-ASSESS-OVERRIDE
           PERFORM 2450-LIMIT-ITEM
           PERFORM 2460-SET-PLAN
           PERFORM 2470-SPLIT-CONTRIB
           PERFORM 2480-FLAG-WEEKLY
           PERFORM 2490-ASSESS-STOP
           PERFORM 2500-SELECT-FEE-PCT
           PERFORM 2510-SPLIT-PAYOUT
           PERFORM 2520-FLAG-WEEKLY
           PERFORM 2530-PRORATE-CONTRIB
           PERFORM 2540-SPLIT-ITEM
           PERFORM 2550-SPLIT-ITEM
           PERFORM 2560-SPLIT-CONTRIB
           PERFORM 2570-SPLIT-CONTRIB
           PERFORM 2580-ACCUM-REGION
           PERFORM 2590-ACCRUE-PAYOUT
           PERFORM 2600-SET-REASON
           PERFORM 2610-PRORATE-GOAL
           PERFORM 2620-LIMIT-PAYOUT
           PERFORM 2630-DESCRIBE-PLAN
           PERFORM 2640-ASSESS-PREFERRED
           PERFORM 2650-FLAG-GOAL
           PERFORM 2660-CHECK-LAST-ACT
           PERFORM 2670-ACCUM-REGION
           PERFORM 2680-SELECT-DISCOUNT
           PERFORM 2690-ASSESS-STOP
           PERFORM 2700-ACCUM-PLAN
           PERFORM 2710-SPLIT-PENALTY
           PERFORM 2720-AGE-NOTICE
           PERFORM 2730-SPLIT-PENALTY
           PERFORM 2740-ACCUM-PLAN
           PERFORM 2750-DESCRIBE-REGION
           PERFORM 2760-SET-REASON
           PERFORM 2770-DESCRIBE-PLAN
           PERFORM 2780-PRORATE-PAYOUT
           PERFORM 2790-LIMIT-GOAL
           PERFORM 2800-SPLIT-MISSED
           PERFORM 2810-PRORATE-WEEKLY
           PERFORM 2820-COUNT-RETRY
           PERFORM 2830-ASSESS-PREFERRED
           PERFORM 2840-AGE-LAST-STMT
           PERFORM 2850-CHECK-NOTICE
           PERFORM 2860-LIMIT-GOAL
           PERFORM 2870-SPLIT-CONTRIB
           PERFORM 2880-COUNT-RETRY
           PERFORM 2890-SPLIT-GOAL
           PERFORM 2900-DESCRIBE-PLAN
           PERFORM 2910-FLAG-ITEM
           PERFORM 2920-EDIT-REF-NO
           PERFORM 2930-SPLIT-MISSED
           PERFORM 2940-SET-CYCLE
           PERFORM 2950-ACCRUE-WEEKLY
           PERFORM 2960-COUNT-RETRY
           PERFORM 2970-SET-REGION
           PERFORM 2980-PRORATE-PAYOUT
           PERFORM 2990-SPLIT-WEEKLY
           PERFORM 3000-SPLIT-GOAL
           PERFORM 3010-LIMIT-WEEKLY
           PERFORM 3020-ACCUM-REGION
           PERFORM 3030-AGE-MATURITY
           PERFORM 3040-SELECT-DISCOUNT
           PERFORM 3050-SET-PLAN
           PERFORM 3060-LIMIT-GOAL
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO XCCAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WEEKLY YTD.
      *----------------------------------------------------------------*
       2000-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCCAL-WEEKLY-YTD ROUNDED =
                   XCCAL-CONTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCCAL-WEEKLY-YTD
           END-IF
           ADD XCCAL-WEEKLY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT MISSED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2010-SPLIT-MISSED.
           COMPUTE WS-WORK-AMT = XCCAL-MISSED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCCAL-MISSED-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCCAL-WEEKLY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2020-COUNT-RETRY.
           IF XCCAL-RETRY-CNT < 9999999
               ADD 1 TO XCCAL-RETRY-CNT
           END-IF
           IF XCCAL-RETRY-CNT NOT < 3
               MOVE 'Y' TO XCCAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2030-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF XCCAL-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO XCCAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2040-ACCRUE-PAYOUT.
           IF XCCAL-PAYOUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCCAL-PAYOUT-AMT * XCCAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2050-DESCRIBE-CYCLE.
           EVALUATE XCCAL-CYCLE-CD
               WHEN 'EM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2060-FLAG-PENALTY.
           IF XCCAL-PENALTY-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCCAL-PENALTY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2070-ACCRUE-CONTRIB.
           IF XCCAL-CONTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCCAL-CONTRIB-YTD * XCCAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2080-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCCAL-GOAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD XCCAL-GOAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCCAL-GOAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCCAL-GOAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCCAL-GOAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2090-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2100-COUNT-CYCLE.
           IF XCCAL-CYCLE-CNT < 9999999
               ADD 1 TO XCCAL-CYCLE-CNT
           END-IF
           IF XCCAL-CYCLE-CNT NOT < 6
               MOVE 'Y' TO XCCAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN NOTICE DATE IS PAST
      * BT -> CY WHEN NOTICE DATE IS PAST
      * CY -> KM WHEN NOTICE DATE IS PAST
      * KM -> MV WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2110-SET-REASON.
           EVALUATE XCCAL-REASON-CD
               WHEN 'AL'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MISSED AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2120-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCCAL-PLAN-CD
                   ADD XCCAL-MISSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCCAL-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCCAL-MISSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2130-COUNT-RETRY.
           IF XCCAL-RETRY-CNT < 9999999
               ADD 1 TO XCCAL-RETRY-CNT
           END-IF
           IF XCCAL-RETRY-CNT NOT < 5
               MOVE 'Y' TO XCCAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2140-DESCRIBE-REASON.
           EVALUATE XCCAL-REASON-CD
               WHEN 'AL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF XCCAL-LAST-STMT-DATE > 0 AND XCCAL-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCCAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCCAL-WEEKLY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCCAL-WEEKLY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCCAL-WEEKLY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCCAL-WEEKLY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCCAL-WEEKLY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MISSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2160-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCCAL-MISSED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2170-COUNT-RETRY.
           IF XCCAL-RETRY-CNT < 9999999
               ADD 1 TO XCCAL-RETRY-CNT
           END-IF
           IF XCCAL-RETRY-CNT NOT < 10
               MOVE 'Y' TO XCCAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2180-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2190-ACCRUE-PAYOUT.
           IF XCCAL-PAYOUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCCAL-PAYOUT-AMT * XCCAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCCAL-GOAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2200-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   XCCAL-GOAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2210-ACCRUE-CONTRIB.
           IF XCCAL-CONTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCCAL-CONTRIB-YTD * XCCAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2220-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCCAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCCAL-MISSED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCCAL-MISSED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCCAL-MISSED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCCAL-MISSED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCCAL-MISSED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2230-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MISSED AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2240-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCCAL-PLAN-CD
                   ADD XCCAL-MISSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCCAL-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCCAL-MISSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * GOAL YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2250-LIMIT-GOAL.
           IF XCCAL-GOAL-YTD < 0.00
               MOVE 0.00 TO XCCAL-GOAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCCAL-GOAL-YTD > 2500.00
               MOVE 2500.00 TO XCCAL-GOAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL YTD FEE ON THE PENALTY YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2260-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCCAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCCAL-GOAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCCAL-PREFERRED-FLAG = 'Y'
                   COMPUTE XCCAL-GOAL-YTD ROUNDED = XCCAL-PENALTY-YTD *
                       XCCAL-FEE-PCT / 200
               WHEN XCCAL-REASON-CD = 'AL'
                   MOVE 20.00 TO XCCAL-GOAL-YTD
               WHEN XCCAL-REASON-CD = 'BT'
                   MOVE 10.00 TO XCCAL-GOAL-YTD
               WHEN OTHER
                   COMPUTE XCCAL-GOAL-YTD ROUNDED = XCCAL-PENALTY-YTD *
                       XCCAL-FEE-PCT / 100
           END-EVALUATE
           IF XCCAL-GOAL-YTD > 0 AND XCCAL-GOAL-YTD < 5.00
               MOVE 5.00 TO XCCAL-GOAL-YTD
           END-IF
           IF XCCAL-GOAL-YTD > 100.00
               MOVE 100.00 TO XCCAL-GOAL-YTD
           END-IF
           ADD XCCAL-GOAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2270-COUNT-CYCLE.
           IF XCCAL-CYCLE-CNT < 9999999
               ADD 1 TO XCCAL-CYCLE-CNT
           END-IF
           IF XCCAL-CYCLE-CNT NOT < 3
               MOVE 'Y' TO XCCAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-RETRY.
           IF XCCAL-RETRY-CNT < 9999999
               ADD 1 TO XCCAL-RETRY-CNT
           END-IF
           IF XCCAL-RETRY-CNT NOT < 6
               MOVE 'Y' TO XCCAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E183 CUST ID REQUIRED
      * E052 WORK AMT NEGATIVE
      * E002 WORK AMT OVER 250000
      * E457 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2290-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCCAL-CUST-ID = SPACES
                   MOVE 'E183' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E052' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E002' TO WS-REJECT-CODE
               WHEN XCCAL-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E457' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GOAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2300-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCCAL-GOAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCCAL-GOAL-YTD - (WS-WORK-AMT * 4)
           COMPUTE XCCAL-PENALTY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN NOTICE DATE IS PAST
      * JT -> KC WHEN NOTICE DATE IS PAST
      * KC -> KW WHEN NOTICE DATE IS PAST
      * KW -> MX WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-PLAN.
           EVALUATE XCCAL-PLAN-CD
               WHEN 'GN'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2320-COUNT-CYCLE.
           IF XCCAL-CYCLE-CNT < 9999999
               ADD 1 TO XCCAL-CYCLE-CNT
           END-IF
           IF XCCAL-CYCLE-CNT NOT < 10
               MOVE 'Y' TO XCCAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYOUT AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2330-LIMIT-PAYOUT.
           IF XCCAL-PAYOUT-AMT < 0.00
               MOVE 0.00 TO XCCAL-PAYOUT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCCAL-PAYOUT-AMT > 500.00
               MOVE 500.00 TO XCCAL-PAYOUT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E760 REF NO REQUIRED
      * E747 WORK AMT 2 NEGATIVE
      * E044 WORK AMT 2 OVER 50000
      * E029 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCCAL-REF-NO = SPACES
                   MOVE 'E760' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E747' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN XCCAL-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E029' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYOUT AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2350-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCCAL-PLAN-CD
                   ADD XCCAL-PAYOUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCCAL-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCCAL-PAYOUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2360-DESCRIBE-CYCLE.
           EVALUATE XCCAL-CYCLE-CD
               WHEN 'EM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WEEKLY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MISSED AMT.
      *----------------------------------------------------------------*
       2370-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCCAL-MISSED-AMT ROUNDED =
                   XCCAL-WEEKLY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCCAL-MISSED-AMT
           END-IF
           ADD XCCAL-MISSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2380-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCCAL-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCCAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO XCCAL-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2390-FLAG-GOAL.
           IF XCCAL-GOAL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCCAL-GOAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT BAND TIER FOR THE PAYOUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2400-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-BAND-RATE(1) TO XCCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF XCCAL-PAYOUT-AMT NOT < WS-PAYOUT-BAND-LOW(WS-TX)
                   MOVE WS-PAYOUT-BAND-RATE(WS-TX) TO XCCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2410-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCCAL-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCCAL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2420-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF XCCAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCCAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO XCCAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MISSED AMT.
      *----------------------------------------------------------------*
       2430-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCCAL-MISSED-AMT ROUNDED =
                   XCCAL-CONTRIB-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCCAL-MISSED-AMT
           END-IF
           ADD XCCAL-MISSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY YTD FEE ON THE WEEKLY YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2440-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCCAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCCAL-PENALTY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCCAL-PREFERRED-FLAG = 'Y'
                   COMPUTE XCCAL-PENALTY-YTD ROUNDED = XCCAL-WEEKLY-YTD
                       * XCCAL-FEE-PCT / 200
               WHEN XCCAL-REGION-CD = 'FLM'
                   MOVE 15.00 TO XCCAL-PENALTY-YTD
               WHEN XCCAL-REGION-CD = 'HLS'
                   MOVE 20.00 TO XCCAL-PENALTY-YTD
               WHEN OTHER
                   COMPUTE XCCAL-PENALTY-YTD ROUNDED = XCCAL-WEEKLY-YTD
                       * XCCAL-FEE-PCT / 100
           END-EVALUATE
           IF XCCAL-PENALTY-YTD > 0 AND XCCAL-PENALTY-YTD < 10.00
               MOVE 10.00 TO XCCAL-PENALTY-YTD
           END-IF
           IF XCCAL-PENALTY-YTD > 150.00
               MOVE 150.00 TO XCCAL-PENALTY-YTD
           END-IF
           ADD XCCAL-PENALTY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2450-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN MATURITY DATE IS PAST
      * JT -> KC WHEN MATURITY DATE IS PAST
      * KC -> KW WHEN MATURITY DATE IS PAST
      * KW -> MX WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2460-SET-PLAN.
           EVALUATE XCCAL-PLAN-CD
               WHEN 'GN'
                   IF XCCAL-MATURITY-DATE > 0 AND XCCAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCCAL-MATURITY-DATE > 0 AND XCCAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCCAL-MATURITY-DATE > 0 AND XCCAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCCAL-MATURITY-DATE > 0 AND XCCAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2470-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCCAL-CONTRIB-YTD / 3
           COMPUTE WS-WORK-AMT-2 = XCCAL-CONTRIB-YTD - (WS-WORK-AMT * 3)
           COMPUTE XCCAL-PENALTY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2480-FLAG-WEEKLY.
           IF XCCAL-WEEKLY-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCCAL-WEEKLY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY YTD FEE ON THE CONTRIB YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2490-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCCAL-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCCAL-PENALTY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCCAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCCAL-PENALTY-YTD ROUNDED = XCCAL-CONTRIB-YTD
                       * XCCAL-FEE-PCT / 200
               WHEN XCCAL-REASON-CD = 'AL'
                   MOVE 10.00 TO XCCAL-PENALTY-YTD
               WHEN XCCAL-REASON-CD = 'BT'
                   MOVE 5.00 TO XCCAL-PENALTY-YTD
               WHEN OTHER
                   COMPUTE XCCAL-PENALTY-YTD ROUNDED = XCCAL-CONTRIB-YTD
                       * XCCAL-FEE-PCT / 100
           END-EVALUATE
           IF XCCAL-PENALTY-YTD > 0 AND XCCAL-PENALTY-YTD < 1.00
               MOVE 1.00 TO XCCAL-PENALTY-YTD
           END-IF
           IF XCCAL-PENALTY-YTD > 250.00
               MOVE 250.00 TO XCCAL-PENALTY-YTD
           END-IF
           ADD XCCAL-PENALTY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE GOAL SCHED TIER FOR THE WEEKLY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2500-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-SCHED-RATE(1) TO XCCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCCAL-WEEKLY-YTD NOT < WS-GOAL-SCHED-LOW(WS-TX)
                   MOVE WS-GOAL-SCHED-RATE(WS-TX) TO XCCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYOUT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-PAYOUT.
           COMPUTE WS-WORK-AMT = XCCAL-PAYOUT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCCAL-PAYOUT-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2520-FLAG-WEEKLY.
           IF XCCAL-WEEKLY-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCCAL-WEEKLY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2530-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCCAL-CONTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2540-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2560-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCCAL-CONTRIB-YTD / 12
           COMPUTE WS-WORK-AMT-2 = XCCAL-CONTRIB-YTD - (WS-WORK-AMT *
               12)
           COMPUTE XCCAL-GOAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2570-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCCAL-CONTRIB-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCCAL-CONTRIB-YTD - (WS-WORK-AMT * 2)
           COMPUTE XCCAL-GOAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCCAL-REGION-CD
                   ADD XCCAL-GOAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCCAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCCAL-GOAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-PAYOUT.
           IF XCCAL-PAYOUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCCAL-PAYOUT-AMT * XCCAL-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCCAL-MISSED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN NOTICE DATE IS PAST
      * BT -> CY WHEN NOTICE DATE IS PAST
      * CY -> KM WHEN NOTICE DATE IS PAST
      * KM -> MV WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2600-SET-REASON.
           EVALUATE XCCAL-REASON-CD
               WHEN 'AL'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PENALTY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GOAL YTD.
      *----------------------------------------------------------------*
       2610-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCCAL-GOAL-YTD ROUNDED =
                   XCCAL-PENALTY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCCAL-GOAL-YTD
           END-IF
           ADD XCCAL-GOAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYOUT AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2620-LIMIT-PAYOUT.
           IF XCCAL-PAYOUT-AMT < 100.00
               MOVE 100.00 TO XCCAL-PAYOUT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCCAL-PAYOUT-AMT > 50000.00
               MOVE 50000.00 TO XCCAL-PAYOUT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2630-DESCRIBE-PLAN.
           EVALUATE XCCAL-PLAN-CD
               WHEN 'GN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2640-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCCAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCCAL-MISSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCCAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCCAL-MISSED-AMT ROUNDED = WS-WORK-AMT *
                       XCCAL-FEE-PCT / 200
               WHEN XCCAL-PLAN-CD = 'GN'
                   MOVE 7.00 TO XCCAL-MISSED-AMT
               WHEN XCCAL-PLAN-CD = 'JT'
                   MOVE 7.00 TO XCCAL-MISSED-AMT
               WHEN OTHER
                   COMPUTE XCCAL-MISSED-AMT ROUNDED = WS-WORK-AMT *
                       XCCAL-FEE-PCT / 100
           END-EVALUATE
           IF XCCAL-MISSED-AMT > 0 AND XCCAL-MISSED-AMT < 1.00
               MOVE 1.00 TO XCCAL-MISSED-AMT
           END-IF
           IF XCCAL-MISSED-AMT > 50.00
               MOVE 50.00 TO XCCAL-MISSED-AMT
           END-IF
           ADD XCCAL-MISSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * GOAL YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2650-FLAG-GOAL.
           IF XCCAL-GOAL-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCCAL-GOAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2660-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF XCCAL-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO XCCAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYOUT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2670-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCCAL-REGION-CD
                   ADD XCCAL-PAYOUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCCAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCCAL-PAYOUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GOAL SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2680-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-SCHED-RATE(1) TO XCCAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-GOAL-SCHED-LOW(WS-TX)
                   MOVE WS-GOAL-SCHED-RATE(WS-TX) TO XCCAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCCAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCCAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYOUT FEE ON THE CONTRIB YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2690-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCCAL-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCCAL-PAYOUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCCAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCCAL-PAYOUT-AMT ROUNDED = XCCAL-CONTRIB-YTD
                       * XCCAL-FEE-PCT / 200
               WHEN XCCAL-CYCLE-CD = 'EM'
                   MOVE 15.00 TO XCCAL-PAYOUT-AMT
               WHEN XCCAL-CYCLE-CD = 'FE'
                   MOVE 15.00 TO XCCAL-PAYOUT-AMT
               WHEN OTHER
                   COMPUTE XCCAL-PAYOUT-AMT ROUNDED = XCCAL-CONTRIB-YTD
                       * XCCAL-FEE-PCT / 100
           END-EVALUATE
           IF XCCAL-PAYOUT-AMT > 0 AND XCCAL-PAYOUT-AMT < 2.00
               MOVE 2.00 TO XCCAL-PAYOUT-AMT
           END-IF
           IF XCCAL-PAYOUT-AMT > 250.00
               MOVE 250.00 TO XCCAL-PAYOUT-AMT
           END-IF
           ADD XCCAL-PAYOUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYOUT AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2700-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCCAL-PLAN-CD
                   ADD XCCAL-PAYOUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCCAL-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCCAL-PAYOUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2710-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = XCCAL-PENALTY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCCAL-PENALTY-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2720-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCCAL-NOTICE-DATE > 0 AND XCCAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCCAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCCAL-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCCAL-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD XCCAL-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCCAL-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCCAL-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PENALTY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2730-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = XCCAL-PENALTY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = XCCAL-PENALTY-YTD - (WS-WORK-AMT * 3)
           COMPUTE XCCAL-GOAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2740-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCCAL-PLAN-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCCAL-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2750-DESCRIBE-REGION.
           EVALUATE XCCAL-REGION-CD
               WHEN 'FLM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN LAST ACT DATE IS PAST
      * BT -> CY WHEN LAST ACT DATE IS PAST
      * CY -> KM WHEN LAST ACT DATE IS PAST
      * KM -> MV WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2760-SET-REASON.
           EVALUATE XCCAL-REASON-CD
               WHEN 'AL'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2770-DESCRIBE-PLAN.
           EVALUATE XCCAL-PLAN-CD
               WHEN 'GN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE GOAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYOUT AMT.
      *----------------------------------------------------------------*
       2780-PRORATE-PAYOUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCCAL-PAYOUT-AMT ROUNDED =
                   XCCAL-GOAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCCAL-PAYOUT-AMT
           END-IF
           ADD XCCAL-PAYOUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * GOAL YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2790-LIMIT-GOAL.
           IF XCCAL-GOAL-YTD < 10.00
               MOVE 10.00 TO XCCAL-GOAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCCAL-GOAL-YTD > 10000.00
               MOVE 10000.00 TO XCCAL-GOAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MISSED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2800-SPLIT-MISSED.
           COMPUTE WS-WORK-AMT = XCCAL-MISSED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCCAL-MISSED-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WEEKLY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2810-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCCAL-WEEKLY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2820-COUNT-RETRY.
           IF XCCAL-RETRY-CNT < 9999999
               ADD 1 TO XCCAL-RETRY-CNT
           END-IF
           IF XCCAL-RETRY-CNT NOT < 3
               MOVE 'Y' TO XCCAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PAYOUT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2830-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCCAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCCAL-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = XCCAL-PAYOUT-AMT *
                       XCCAL-DISCOUNT / 200
               WHEN XCCAL-REASON-CD = 'AL'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN XCCAL-REASON-CD = 'BT'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = XCCAL-PAYOUT-AMT *
                       XCCAL-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2840-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF XCCAL-LAST-STMT-DATE > 0 AND XCCAL-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCCAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2850-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCCAL-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCCAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO XCCAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2860-LIMIT-GOAL.
           IF XCCAL-GOAL-YTD < 10.00
               MOVE 10.00 TO XCCAL-GOAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCCAL-GOAL-YTD > 10000.00
               MOVE 10000.00 TO XCCAL-GOAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2870-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE XCCAL-CONTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2880-COUNT-RETRY.
           IF XCCAL-RETRY-CNT < 9999999
               ADD 1 TO XCCAL-RETRY-CNT
           END-IF
           IF XCCAL-RETRY-CNT NOT < 6
               MOVE 'Y' TO XCCAL-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GOAL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2890-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCCAL-GOAL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = XCCAL-GOAL-YTD - (WS-WORK-AMT * 3)
           COMPUTE XCCAL-CONTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2900-DESCRIBE-PLAN.
           EVALUATE XCCAL-PLAN-CD
               WHEN 'GN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2910-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E781 REF NO REQUIRED
      * E104 CONTRIB YTD NEGATIVE
      * E933 CONTRIB YTD OVER 1000000
      * E789 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2920-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCCAL-REF-NO = SPACES
                   MOVE 'E781' TO WS-REJECT-CODE
               WHEN XCCAL-CONTRIB-YTD < 0
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN XCCAL-CONTRIB-YTD > 1000000.00
                   MOVE 'E933' TO WS-REJECT-CODE
               WHEN XCCAL-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MISSED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2930-SPLIT-MISSED.
           COMPUTE WS-WORK-AMT = XCCAL-MISSED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCCAL-MISSED-AMT - (WS-WORK-AMT * 3)
           COMPUTE XCCAL-PAYOUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN LAST STMT DATE IS PAST
      * FE -> PL WHEN LAST STMT DATE IS PAST
      * PL -> RU WHEN LAST STMT DATE IS PAST
      * RU -> SU WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2940-SET-CYCLE.
           EVALUATE XCCAL-CYCLE-CD
               WHEN 'EM'
                   IF XCCAL-LAST-STMT-DATE > 0 AND XCCAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FE' TO XCCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF XCCAL-LAST-STMT-DATE > 0 AND XCCAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PL' TO XCCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF XCCAL-LAST-STMT-DATE > 0 AND XCCAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RU' TO XCCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF XCCAL-LAST-STMT-DATE > 0 AND XCCAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SU' TO XCCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2950-ACCRUE-WEEKLY.
           IF XCCAL-WEEKLY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCCAL-WEEKLY-YTD * XCCAL-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCCAL-PENALTY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2960-COUNT-RETRY.
           IF XCCAL-RETRY-CNT < 9999999
               ADD 1 TO XCCAL-RETRY-CNT
           END-IF
           IF XCCAL-RETRY-CNT NOT < 5
               MOVE 'Y' TO XCCAL-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN LAST ACT DATE IS PAST
      * HLS -> KMJ WHEN LAST ACT DATE IS PAST
      * KMJ -> PDA WHEN LAST ACT DATE IS PAST
      * PDA -> RVG WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2970-SET-REGION.
           EVALUATE XCCAL-REGION-CD
               WHEN 'FLM'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCCAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCCAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCCAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCCAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYOUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2980-PRORATE-PAYOUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCCAL-PAYOUT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WEEKLY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2990-SPLIT-WEEKLY.
           COMPUTE WS-WORK-AMT = XCCAL-WEEKLY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCCAL-WEEKLY-YTD - (WS-WORK-AMT * 4)
           COMPUTE XCCAL-PENALTY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GOAL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCCAL-GOAL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCCAL-GOAL-YTD - (WS-WORK-AMT * 2)
           COMPUTE XCCAL-PENALTY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-WEEKLY.
           IF XCCAL-WEEKLY-YTD < 10.00
               MOVE 10.00 TO XCCAL-WEEKLY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCCAL-WEEKLY-YTD > 1000.00
               MOVE 1000.00 TO XCCAL-WEEKLY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3020-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCCAL-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCCAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF XCCAL-MATURITY-DATE > 0 AND XCCAL-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCCAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCCAL-MISSED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCCAL-MISSED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCCAL-MISSED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD XCCAL-MISSED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCCAL-MISSED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GOAL SCHED TIER FOR THE PENALTY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-SCHED-RATE(1) TO XCCAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCCAL-PENALTY-YTD NOT < WS-GOAL-SCHED-LOW(WS-TX)
                   MOVE WS-GOAL-SCHED-RATE(WS-TX) TO XCCAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCCAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCCAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN LAST ACT DATE IS PAST
      * JT -> KC WHEN LAST ACT DATE IS PAST
      * KC -> KW WHEN LAST ACT DATE IS PAST
      * KW -> MX WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-PLAN.
           EVALUATE XCCAL-PLAN-CD
               WHEN 'GN'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCCAL-LAST-ACT-DATE > 0 AND XCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCCAL-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GOAL YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-GOAL.
           IF XCCAL-GOAL-YTD < 0.00
               MOVE 0.00 TO XCCAL-GOAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCCAL-GOAL-YTD > 50000.00
               MOVE 50000.00 TO XCCAL-GOAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO XCCAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO XCCAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO XCCAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'XCCAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
