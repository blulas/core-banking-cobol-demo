      *================================================================*
      * PROGRAM  : XCTIER                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS RULE - ELIGIBILITY. CALLED BY*
      *            THE CHRISTMAS CLUB SAVINGS BATCH AND ONLINE         *
      *            PROGRAMS.                                           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-10-19 BHS  ORIGINAL                                       *
      * 1992-10-03 GAF  NEW CONTRIB REPORT COLUMNS PRB7225             *
      * 1993-12-14 BHS  NEW BONUS REPORT COLUMNS INC0145               *
      * 1993-10-10 EWS  WEEKLY ROUNDING - HALF UP CHG7881              *
      * 1995-02-16 TWH  WEEKLY LIMIT REVISED INC6867                   *
      * 1995-11-07 SKN  MISSED FEE SCHEDULE UPDATE REQ9805             *
      * 1997-12-27 LMB  PAYOUT ROUNDING - HALF UP CHG7107              *
      * 1998-04-19 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-04-02 RJT  PERFORMANCE - MISSED READ AHEAD CHG2239        *
      * 2000-11-18 GAF  WEEKLY ROUNDING - HALF UP PRB2186              *
      * 2001-12-24 GAF  AUDIT FINDING - PENALTY CONTROL TOTALS INC3567 *
      * 2001-10-15 TWH  AUDIT FINDING - GOAL CONTROL TOTALS REQ0517    *
      * 2001-09-15 CRP  MISSED STATUS CODES ADDED CHG4528              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCTIER.
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
                                           VALUE 15.000000.
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
      * WS-WEEKLY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WEEKLY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000750500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000495500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000801000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001600000000448500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001650000000152000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000125000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002250000000119000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004750000000172000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000829000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000839500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000347500'.
       01  WS-WEEKLY-TIER-TBL REDEFINES WS-WEEKLY-TIER-VALUES.
           05  WS-WEEKLY-TIER-ROW OCCURS 11 TIMES.
               10  WS-WEEKLY-TIER-LOW      PIC 9(09)V99.
               10  WS-WEEKLY-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYOUT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYOUT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000884000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000613000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000587000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000430000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016100000000578000'.
       01  WS-PAYOUT-BAND-TBL REDEFINES WS-PAYOUT-BAND-VALUES.
           05  WS-PAYOUT-BAND-ROW OCCURS 5 TIMES.
               10  WS-PAYOUT-BAND-LOW      PIC 9(09)V99.
               10  WS-PAYOUT-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PENALTY-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000050500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000353500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000227000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006250000000029000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000624000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016350000000744000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017350000000898000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019850000000392000'.
       01  WS-PENALTY-GRID-TBL REDEFINES WS-PENALTY-GRID-VALUES.
           05  WS-PENALTY-GRID-ROW OCCURS 8 TIMES.
               10  WS-PENALTY-GRID-LOW     PIC 9(09)V99.
               10  WS-PENALTY-GRID-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY XCTIERP.

       PROCEDURE DIVISION USING XCTIER-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CHECK-NOTICE
           PERFORM 2010-ACCUM-PAYOUT-METHOD
           PERFORM 2020-COUNT-CYCLE
           PERFORM 2030-FLAG-ITEM
           PERFORM 2040-AGE-LAST-STMT
           PERFORM 2050-SET-PAYOUT-METHOD
           PERFORM 2060-AGE-LAST-PAY
           PERFORM 2070-AGE-NOTICE
           PERFORM 2080-ASSESS-SUSPEND
           PERFORM 2090-SELECT-RATE
           PERFORM 2100-PRORATE-MISSED
           PERFORM 2110-CHECK-NOTICE
           PERFORM 2120-PRORATE-GOAL
           PERFORM 2130-SPLIT-BONUS
           PERFORM 2140-ASSESS-OVERRIDE
           PERFORM 2150-SET-REGION
           PERFORM 2160-ACCRUE-DISCOUNT
           PERFORM 2170-AGE-EXPIRY
           PERFORM 2180-AGE-LAST-STMT
           PERFORM 2190-CHECK-LAST-STMT
           PERFORM 2200-SPLIT-MISSED
           PERFORM 2210-PRORATE-BONUS
           PERFORM 2220-EDIT-NAME
           PERFORM 2230-EDIT-CONTACT
           PERFORM 2240-DESCRIBE-REGION
           PERFORM 2250-SPLIT-GOAL
           PERFORM 2260-EDIT-NAME
           PERFORM 2270-DESCRIBE-PLAN
           PERFORM 2280-ACCUM-REASON
           PERFORM 2290-CHECK-EXPIRY
           PERFORM 2300-PRORATE-MISSED
           PERFORM 2310-DESCRIBE-PAYOUT-METHOD
           PERFORM 2320-ACCUM-PAYOUT-METHOD
           PERFORM 2330-FLAG-MISSED
           PERFORM 2340-EDIT-CONTACT
           PERFORM 2350-CHECK-EXPIRY
           PERFORM 2360-ACCUM-PLAN
           PERFORM 2370-EDIT-CONTACT
           PERFORM 2380-FLAG-PAYOUT
           PERFORM 2390-LIMIT-ITEM
           PERFORM 2400-EDIT-CONTACT
           PERFORM 2410-PRORATE-ACCRUED
           PERFORM 2420-ACCUM-REASON
           PERFORM 2430-ACCRUE-ACCRUED
           PERFORM 2440-ACCUM-REGION
           PERFORM 2450-SET-REASON
           PERFORM 2460-CHECK-LAST-STMT
           PERFORM 2470-PRORATE-ACCRUED
           PERFORM 2480-PRORATE-ACCRUED
           PERFORM 2490-EDIT-CONTACT
           PERFORM 2500-ACCUM-REASON
           PERFORM 2510-EDIT-CONTACT
           PERFORM 2520-COUNT-RETRY
           PERFORM 2530-SET-PLAN
           PERFORM 2540-SET-PLAN
           PERFORM 2550-SPLIT-ACCRUED
           PERFORM 2560-ACCRUE-GOAL
           PERFORM 2570-CHECK-NOTICE
           PERFORM 2580-SELECT-DISCOUNT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO XCTIER-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2000-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO XCTIER-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCTIER-PAYOUT-METHOD-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCTIER-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2020-COUNT-CYCLE.
           IF XCTIER-CYCLE-CNT < 9999999
               ADD 1 TO XCTIER-CYCLE-CNT
           END-IF
           IF XCTIER-CYCLE-CNT NOT < 5
               MOVE 'Y' TO XCTIER-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2030-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2040-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-LAST-STMT-DATE > 0 AND XCTIER-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCTIER-ACCRUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCTIER-ACCRUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCTIER-ACCRUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCTIER-ACCRUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCTIER-ACCRUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYOUT METHOD CD:
      * B -> H WHEN LAST STMT DATE IS PAST
      * H -> U WHEN LAST STMT DATE IS PAST
      * U -> W WHEN LAST STMT DATE IS PAST
      * W -> B WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2050-SET-PAYOUT-METHOD.
           EVALUATE XCTIER-PAYOUT-METHOD-CD
               WHEN 'B'
                   IF XCTIER-LAST-STMT-DATE > 0 AND
                       XCTIER-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO XCTIER-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF XCTIER-LAST-STMT-DATE > 0 AND
                       XCTIER-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'U' TO XCTIER-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF XCTIER-LAST-STMT-DATE > 0 AND
                       XCTIER-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'W' TO XCTIER-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF XCTIER-LAST-STMT-DATE > 0 AND
                       XCTIER-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'B' TO XCTIER-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-LAST-PAY-DATE > 0 AND XCTIER-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCTIER-ACCRUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCTIER-ACCRUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD XCTIER-ACCRUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCTIER-ACCRUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCTIER-ACCRUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-NOTICE-DATE > 0 AND XCTIER-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYOUT FEE ON THE ACCRUED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2080-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCTIER-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCTIER-PAYOUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCTIER-PREFERRED-FLAG = 'Y'
                   COMPUTE XCTIER-PAYOUT-AMT ROUNDED =
                       XCTIER-ACCRUED-AMT * XCTIER-RATE / 200
               WHEN XCTIER-PLAN-CD = 'GN'
                   MOVE 5.00 TO XCTIER-PAYOUT-AMT
               WHEN XCTIER-PLAN-CD = 'JT'
                   MOVE 7.00 TO XCTIER-PAYOUT-AMT
               WHEN OTHER
                   COMPUTE XCTIER-PAYOUT-AMT ROUNDED =
                       XCTIER-ACCRUED-AMT * XCTIER-RATE / 100
           END-EVALUATE
           IF XCTIER-PAYOUT-AMT > 0 AND XCTIER-PAYOUT-AMT < 2.00
               MOVE 2.00 TO XCTIER-PAYOUT-AMT
           END-IF
           IF XCTIER-PAYOUT-AMT > 25.00
               MOVE 25.00 TO XCTIER-PAYOUT-AMT
           END-IF
           ADD XCTIER-PAYOUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT BAND TIER FOR THE GOAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-BAND-RATE(1) TO XCTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCTIER-GOAL-AMT NOT < WS-PAYOUT-BAND-LOW(WS-TX)
                   MOVE WS-PAYOUT-BAND-RATE(WS-TX) TO XCTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MISSED AMT.
      *----------------------------------------------------------------*
       2100-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCTIER-MISSED-AMT ROUNDED =
                   XCTIER-ACCRUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCTIER-MISSED-AMT
           END-IF
           ADD XCTIER-MISSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2110-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO XCTIER-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GOAL AMT.
      *----------------------------------------------------------------*
       2120-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCTIER-GOAL-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCTIER-GOAL-AMT
           END-IF
           ADD XCTIER-GOAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT BONUS YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCTIER-BONUS-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCTIER-BONUS-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE MISSED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2140-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCTIER-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCTIER-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = XCTIER-MISSED-AMT *
                       XCTIER-DISCOUNT / 200
               WHEN XCTIER-PLAN-CD = 'GN'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN XCTIER-PLAN-CD = 'JT'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = XCTIER-MISSED-AMT *
                       XCTIER-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN LAST STMT DATE IS PAST
      * HLS -> KMJ WHEN LAST STMT DATE IS PAST
      * KMJ -> PDA WHEN LAST STMT DATE IS PAST
      * PDA -> RVG WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2150-SET-REGION.
           EVALUATE XCTIER-REGION-CD
               WHEN 'FLM'
                   IF XCTIER-LAST-STMT-DATE > 0 AND
                       XCTIER-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCTIER-LAST-STMT-DATE > 0 AND
                       XCTIER-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCTIER-LAST-STMT-DATE > 0 AND
                       XCTIER-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCTIER-LAST-STMT-DATE > 0 AND
                       XCTIER-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2160-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * XCTIER-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCTIER-PAYOUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2170-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-EXPIRY-DATE > 0 AND XCTIER-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2180-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-LAST-STMT-DATE > 0 AND XCTIER-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2190-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO XCTIER-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MISSED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2200-SPLIT-MISSED.
           COMPUTE WS-WORK-AMT = XCTIER-MISSED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = XCTIER-MISSED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE XCTIER-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BONUS YTD.
      *----------------------------------------------------------------*
       2210-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCTIER-BONUS-YTD ROUNDED =
                   XCTIER-ACCRUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCTIER-BONUS-YTD
           END-IF
           ADD XCTIER-BONUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E297 NAME REQUIRED
      * E596 ACCRUED AMT NEGATIVE
      * E608 ACCRUED AMT OVER 1000000
      * E347 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2220-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCTIER-NAME = SPACES
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN XCTIER-ACCRUED-AMT < 0
                   MOVE 'E596' TO WS-REJECT-CODE
               WHEN XCTIER-ACCRUED-AMT > 1000000.00
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN XCTIER-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E347' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E965 CONTACT REQUIRED
      * E689 PAYOUT AMT NEGATIVE
      * E869 PAYOUT AMT OVER 25000
      * E957 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCTIER-CONTACT = SPACES
                   MOVE 'E965' TO WS-REJECT-CODE
               WHEN XCTIER-PAYOUT-AMT < 0
                   MOVE 'E689' TO WS-REJECT-CODE
               WHEN XCTIER-PAYOUT-AMT > 25000.00
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN XCTIER-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2240-DESCRIBE-REGION.
           EVALUATE XCTIER-REGION-CD
               WHEN 'FLM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2250-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE XCTIER-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E410 NAME REQUIRED
      * E815 PAYOUT AMT NEGATIVE
      * E251 PAYOUT AMT OVER 99999
      * E330 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2260-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCTIER-NAME = SPACES
                   MOVE 'E410' TO WS-REJECT-CODE
               WHEN XCTIER-PAYOUT-AMT < 0
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN XCTIER-PAYOUT-AMT > 99999.00
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN XCTIER-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2270-DESCRIBE-PLAN.
           EVALUATE XCTIER-PLAN-CD
               WHEN 'GN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUED YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2280-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCTIER-REASON-CD
                   ADD XCTIER-ACCRUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCTIER-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCTIER-ACCRUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO XCTIER-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MISSED AMT.
      *----------------------------------------------------------------*
       2300-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCTIER-MISSED-AMT ROUNDED =
                   XCTIER-GOAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCTIER-MISSED-AMT
           END-IF
           ADD XCTIER-MISSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE PAYOUT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2310-DESCRIBE-PAYOUT-METHOD.
           EVALUATE XCTIER-PAYOUT-METHOD-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUED AMT BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCTIER-PAYOUT-METHOD-CD
                   ADD XCTIER-ACCRUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCTIER-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCTIER-ACCRUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * MISSED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2330-FLAG-MISSED.
           IF XCTIER-MISSED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCTIER-MISSED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E902 CONTACT REQUIRED
      * E705 ACCRUED AMT NEGATIVE
      * E911 ACCRUED AMT OVER 99999
      * E102 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCTIER-CONTACT = SPACES
                   MOVE 'E902' TO WS-REJECT-CODE
               WHEN XCTIER-ACCRUED-AMT < 0
                   MOVE 'E705' TO WS-REJECT-CODE
               WHEN XCTIER-ACCRUED-AMT > 99999.00
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN XCTIER-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO XCTIER-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2360-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCTIER-PLAN-CD
                   ADD XCTIER-GOAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCTIER-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCTIER-GOAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E855 CONTACT REQUIRED
      * E726 WORK AMT NEGATIVE
      * E439 WORK AMT OVER 5000
      * E979 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2370-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCTIER-CONTACT = SPACES
                   MOVE 'E855' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E726' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN XCTIER-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYOUT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2380-FLAG-PAYOUT.
           IF XCTIER-PAYOUT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCTIER-PAYOUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2390-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E251 CONTACT REQUIRED
      * E819 GOAL AMT NEGATIVE
      * E157 GOAL AMT OVER 250000
      * E846 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2400-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCTIER-CONTACT = SPACES
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN XCTIER-GOAL-AMT < 0
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN XCTIER-GOAL-AMT > 250000.00
                   MOVE 'E157' TO WS-REJECT-CODE
               WHEN XCTIER-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E846' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUED AMT.
      *----------------------------------------------------------------*
       2410-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCTIER-ACCRUED-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCTIER-ACCRUED-AMT
           END-IF
           ADD XCTIER-ACCRUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE MISSED AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2420-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCTIER-REASON-CD
                   ADD XCTIER-MISSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCTIER-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCTIER-MISSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUED AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2430-ACCRUE-ACCRUED.
           IF XCTIER-ACCRUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCTIER-ACCRUED-AMT * XCTIER-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCTIER-ACCRUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYOUT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2440-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCTIER-REGION-CD
                   ADD XCTIER-PAYOUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCTIER-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCTIER-PAYOUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN NOTICE DATE IS PAST
      * BT -> CY WHEN NOTICE DATE IS PAST
      * CY -> KM WHEN NOTICE DATE IS PAST
      * KM -> MV WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2450-SET-REASON.
           EVALUATE XCTIER-REASON-CD
               WHEN 'AL'
                   IF XCTIER-NOTICE-DATE > 0 AND XCTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCTIER-NOTICE-DATE > 0 AND XCTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCTIER-NOTICE-DATE > 0 AND XCTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCTIER-NOTICE-DATE > 0 AND XCTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2460-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO XCTIER-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MISSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUED AMT.
      *----------------------------------------------------------------*
       2470-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCTIER-ACCRUED-AMT ROUNDED =
                   XCTIER-MISSED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCTIER-ACCRUED-AMT
           END-IF
           ADD XCTIER-ACCRUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUED YTD.
      *----------------------------------------------------------------*
       2480-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCTIER-ACCRUED-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCTIER-ACCRUED-YTD
           END-IF
           ADD XCTIER-ACCRUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E492 CONTACT REQUIRED
      * E544 PAYOUT AMT NEGATIVE
      * E549 PAYOUT AMT OVER 1000000
      * E976 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2490-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCTIER-CONTACT = SPACES
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN XCTIER-PAYOUT-AMT < 0
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN XCTIER-PAYOUT-AMT > 1000000.00
                   MOVE 'E549' TO WS-REJECT-CODE
               WHEN XCTIER-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCTIER-REASON-CD
                   ADD XCTIER-BONUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCTIER-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCTIER-BONUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E027 CONTACT REQUIRED
      * E380 MISSED AMT NEGATIVE
      * E322 MISSED AMT OVER 50000
      * E379 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2510-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCTIER-CONTACT = SPACES
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN XCTIER-MISSED-AMT < 0
                   MOVE 'E380' TO WS-REJECT-CODE
               WHEN XCTIER-MISSED-AMT > 50000.00
                   MOVE 'E322' TO WS-REJECT-CODE
               WHEN XCTIER-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E379' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2520-COUNT-RETRY.
           IF XCTIER-RETRY-CNT < 9999999
               ADD 1 TO XCTIER-RETRY-CNT
           END-IF
           IF XCTIER-RETRY-CNT NOT < 10
               MOVE 'Y' TO XCTIER-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN EXPIRY DATE IS PAST
      * JT -> KC WHEN EXPIRY DATE IS PAST
      * KC -> KW WHEN EXPIRY DATE IS PAST
      * KW -> MX WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-PLAN.
           EVALUATE XCTIER-PLAN-CD
               WHEN 'GN'
                   IF XCTIER-EXPIRY-DATE > 0 AND XCTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCTIER-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCTIER-EXPIRY-DATE > 0 AND XCTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCTIER-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCTIER-EXPIRY-DATE > 0 AND XCTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCTIER-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCTIER-EXPIRY-DATE > 0 AND XCTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCTIER-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN EXPIRY DATE IS PAST
      * JT -> KC WHEN EXPIRY DATE IS PAST
      * KC -> KW WHEN EXPIRY DATE IS PAST
      * KW -> MX WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2540-SET-PLAN.
           EVALUATE XCTIER-PLAN-CD
               WHEN 'GN'
                   IF XCTIER-EXPIRY-DATE > 0 AND XCTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCTIER-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCTIER-EXPIRY-DATE > 0 AND XCTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCTIER-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCTIER-EXPIRY-DATE > 0 AND XCTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCTIER-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCTIER-EXPIRY-DATE > 0 AND XCTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCTIER-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ACCRUED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-ACCRUED.
           COMPUTE WS-WORK-AMT = XCTIER-ACCRUED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = XCTIER-ACCRUED-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2560-ACCRUE-GOAL.
           IF XCTIER-GOAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCTIER-GOAL-AMT * XCTIER-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCTIER-ACCRUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCTIER-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCTIER-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO XCTIER-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT BAND TIER FOR THE PAYOUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2580-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-BAND-RATE(1) TO XCTIER-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCTIER-PAYOUT-AMT NOT < WS-PAYOUT-BAND-LOW(WS-TX)
                   MOVE WS-PAYOUT-BAND-RATE(WS-TX) TO XCTIER-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCTIER-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCTIER-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO XCTIER-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO XCTIER-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO XCTIER-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'XCTIER EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
