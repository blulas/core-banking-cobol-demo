      *================================================================*
      * PROGRAM  : XCPEN                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS RULE - FEES. CALLED BY THE   *
      *            CHRISTMAS CLUB SAVINGS BATCH AND ONLINE PROGRAMS.   *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-12 RJT  ORIGINAL                                       *
      * 1994-04-24 TWH  GOAL RERUN PROTECTION REQ8647                  *
      * 1998-05-17 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-20 SKN  PAYOUT CUTOFF MOVED INC5723                    *
      * 1999-09-09 GAF  WEEKLY RERUN PROTECTION INC7585                *
      * 2000-12-11 BHS  WEEKLY EXCEPTIONS TO OPS QUEUE INC5854         *
      * 2004-07-19 RJT  NEW GOAL REPORT COLUMNS INC7006                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCPEN.
       AUTHOR. J L MARSH.

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
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 28.
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
      * WS-PAYOUT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYOUT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000558000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000208000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000250000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000016000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000814500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000042500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021100000000208000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022100000000492000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027100000000333000'.
       01  WS-PAYOUT-TIER-TBL REDEFINES WS-PAYOUT-TIER-VALUES.
           05  WS-PAYOUT-TIER-ROW OCCURS 9 TIMES.
               10  WS-PAYOUT-TIER-LOW      PIC 9(09)V99.
               10  WS-PAYOUT-TIER-RATE     PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY XCPENP.

       PROCEDURE DIVISION USING XCPEN-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCUM-PAYOUT-METHOD
           PERFORM 2010-SELECT-RATE
           PERFORM 2020-SELECT-FEE-PCT
           PERFORM 2030-PRORATE-GOAL
           PERFORM 2040-PRORATE-CONTRIB
           PERFORM 2050-ACCRUE-CONTRIB
           PERFORM 2060-SPLIT-BONUS
           PERFORM 2070-EDIT-CUST-ID
           PERFORM 2080-ASSESS-PREFERRED
           PERFORM 2090-EDIT-CONTACT
           PERFORM 2100-ACCUM-TYPE
           PERFORM 2110-ACCUM-REGION
           PERFORM 2120-ACCUM-REGION
           PERFORM 2130-SPLIT-MISSED
           PERFORM 2140-LIMIT-GOAL
           PERFORM 2150-FLAG-ITEM
           PERFORM 2160-PRORATE-WEEKLY
           PERFORM 2170-SET-PAYOUT-METHOD
           PERFORM 2180-SPLIT-BONUS
           PERFORM 2190-DESCRIBE-PLAN
           PERFORM 2200-LIMIT-WEEKLY
           PERFORM 2210-LIMIT-MISSED
           PERFORM 2220-FLAG-GOAL
           PERFORM 2230-FLAG-WEEKLY
           PERFORM 2240-ACCUM-PLAN
           PERFORM 2250-ACCRUE-RATE
           PERFORM 2260-AGE-OPEN
           PERFORM 2270-LIMIT-MISSED
           PERFORM 2280-AGE-MATURITY
           PERFORM 2290-ASSESS-PREFERRED
           PERFORM 2300-CHECK-MATURITY
           PERFORM 2310-ACCUM-PLAN
           PERFORM 2320-SELECT-RATE
           PERFORM 2330-CHECK-NOTICE
           PERFORM 2340-DESCRIBE-PAYOUT-METHOD
           PERFORM 2350-SET-PAYOUT-METHOD
           PERFORM 2360-SET-TYPE
           PERFORM 2370-EDIT-CUST-ID
           PERFORM 2380-PRORATE-GOAL
           PERFORM 2390-PRORATE-MISSED
           PERFORM 2400-SPLIT-GOAL
           PERFORM 2410-SPLIT-WEEKLY
           PERFORM 2420-PRORATE-CONTRIB
           PERFORM 2430-CHECK-NOTICE
           PERFORM 2440-AGE-LAST-PAY
           PERFORM 2450-FLAG-ITEM
           PERFORM 2460-COUNT-CYCLE
           PERFORM 2470-LIMIT-GOAL
           PERFORM 2480-DESCRIBE-PAYOUT-METHOD
           PERFORM 2490-LIMIT-WEEKLY
           PERFORM 2500-ACCUM-REGION
           PERFORM 2510-ASSESS-STOP
           PERFORM 2520-SPLIT-CONTRIB
           PERFORM 2530-PRORATE-CONTRIB
           PERFORM 2540-DESCRIBE-REGION
           PERFORM 2550-LIMIT-CONTRIB
           PERFORM 2560-FLAG-GOAL
           PERFORM 2570-DESCRIBE-REGION
           PERFORM 2580-SET-REGION
           PERFORM 2590-DESCRIBE-REGION
           PERFORM 2600-SPLIT-CONTRIB
           PERFORM 2610-PRORATE-BONUS
           PERFORM 2620-ACCUM-PAYOUT-METHOD
           PERFORM 2630-DESCRIBE-REGION
           PERFORM 2640-ACCUM-PAYOUT-METHOD
           PERFORM 2650-EDIT-CUST-ID
           PERFORM 2660-ACCRUE-CONTRIB
           PERFORM 2670-LIMIT-ITEM
           PERFORM 2680-LIMIT-MISSED
           PERFORM 2690-PRORATE-MISSED
           PERFORM 2700-SPLIT-WEEKLY
           PERFORM 2710-PRORATE-MISSED
           PERFORM 2720-ACCRUE-GOAL
           PERFORM 2730-PRORATE-WEEKLY
           PERFORM 2740-LIMIT-GOAL
           PERFORM 2750-AGE-NOTICE
           PERFORM 2760-DESCRIBE-REGION
           PERFORM 2770-LIMIT-BONUS
           PERFORM 2780-CHECK-OPEN
           PERFORM 2790-SELECT-FEE-PCT
           PERFORM 2800-ASSESS-OVERRIDE
           PERFORM 2810-EDIT-CUST-ID
           PERFORM 2820-SPLIT-GOAL
           PERFORM 2830-FLAG-ITEM
           PERFORM 2840-FLAG-BONUS
           PERFORM 2850-ACCUM-PLAN
           PERFORM 2860-ACCUM-PAYOUT-METHOD
           PERFORM 2870-ACCRUE-BONUS
           PERFORM 2880-ASSESS-STOP
           PERFORM 2890-SET-REGION
           PERFORM 2900-SPLIT-BONUS
           PERFORM 2910-ASSESS-OVERRIDE
           PERFORM 2920-COUNT-NOTICE
           PERFORM 2930-ACCUM-TYPE
           PERFORM 2940-SPLIT-GOAL
           PERFORM 2950-PRORATE-MISSED
           PERFORM 2960-EDIT-CONTACT
           PERFORM 2970-SELECT-RATE
           PERFORM 2980-ASSESS-OVERRIDE
           PERFORM 2990-PRORATE-WEEKLY
           PERFORM 3000-LIMIT-BONUS
           PERFORM 3010-LIMIT-GOAL
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO XCPEN-RETURN-CODE.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB AMT BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2000-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-PAYOUT-METHOD-CD
                   ADD XCPEN-CONTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCPEN-CONTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT TIER TIER FOR THE GOAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2010-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-TIER-RATE(1) TO XCPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF XCPEN-GOAL-AMT NOT < WS-PAYOUT-TIER-LOW(WS-TX)
                   MOVE WS-PAYOUT-TIER-RATE(WS-TX) TO XCPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2020-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-TIER-RATE(1) TO XCPEN-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-PAYOUT-TIER-LOW(WS-TX)
                   MOVE WS-PAYOUT-TIER-RATE(WS-TX) TO XCPEN-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCPEN-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCPEN-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2030-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCPEN-GOAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE MISSED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       2040-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCPEN-CONTRIB-AMT ROUNDED =
                   XCPEN-MISSED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCPEN-CONTRIB-AMT
           END-IF
           ADD XCPEN-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2050-ACCRUE-CONTRIB.
           IF XCPEN-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCPEN-CONTRIB-AMT * XCPEN-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCPEN-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE XCPEN-BONUS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E793 CUST ID REQUIRED
      * E147 WORK AMT NEGATIVE
      * E125 WORK AMT OVER 1000000
      * E715 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2070-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCPEN-CUST-ID = SPACES
                   MOVE 'E793' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E125' TO WS-REJECT-CODE
               WHEN XCPEN-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED YTD FEE ON THE GOAL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2080-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCPEN-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCPEN-MISSED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCPEN-STOP-FLAG = 'Y'
                   COMPUTE XCPEN-MISSED-YTD ROUNDED = XCPEN-GOAL-YTD *
                       XCPEN-RATE / 200
               WHEN XCPEN-TYPE-CD = 'A'
                   MOVE 7.00 TO XCPEN-MISSED-YTD
               WHEN XCPEN-TYPE-CD = 'B'
                   MOVE 3.00 TO XCPEN-MISSED-YTD
               WHEN OTHER
                   COMPUTE XCPEN-MISSED-YTD ROUNDED = XCPEN-GOAL-YTD *
                       XCPEN-RATE / 100
           END-EVALUATE
           IF XCPEN-MISSED-YTD > 0 AND XCPEN-MISSED-YTD < 1.00
               MOVE 1.00 TO XCPEN-MISSED-YTD
           END-IF
           IF XCPEN-MISSED-YTD > 100.00
               MOVE 100.00 TO XCPEN-MISSED-YTD
           END-IF
           ADD XCPEN-MISSED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E697 CONTACT REQUIRED
      * E354 CONTRIB AMT NEGATIVE
      * E057 CONTRIB AMT OVER 5000
      * E161 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2090-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCPEN-CONTACT = SPACES
                   MOVE 'E697' TO WS-REJECT-CODE
               WHEN XCPEN-CONTRIB-AMT < 0
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN XCPEN-CONTRIB-AMT > 5000.00
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN XCPEN-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2100-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-TYPE-CD
                   ADD XCPEN-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCPEN-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-REGION-CD
                   ADD XCPEN-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCPEN-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2120-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MISSED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-MISSED.
           COMPUTE WS-WORK-AMT = XCPEN-MISSED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCPEN-MISSED-YTD - (WS-WORK-AMT * 4)
           COMPUTE XCPEN-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2140-LIMIT-GOAL.
           IF XCPEN-GOAL-AMT < 100.00
               MOVE 100.00 TO XCPEN-GOAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-GOAL-AMT > 500.00
               MOVE 500.00 TO XCPEN-GOAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2150-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WEEKLY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2160-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCPEN-WEEKLY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYOUT METHOD CD:
      * B -> H WHEN MATURITY DATE IS PAST
      * H -> U WHEN MATURITY DATE IS PAST
      * U -> W WHEN MATURITY DATE IS PAST
      * W -> B WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2170-SET-PAYOUT-METHOD.
           EVALUATE XCPEN-PAYOUT-METHOD-CD
               WHEN 'B'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO XCPEN-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO XCPEN-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO XCPEN-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCPEN-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2180-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE XCPEN-BONUS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2190-DESCRIBE-PLAN.
           EVALUATE XCPEN-PLAN-CD
               WHEN 'GN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WEEKLY AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2200-LIMIT-WEEKLY.
           IF XCPEN-WEEKLY-AMT < 0.00
               MOVE 0.00 TO XCPEN-WEEKLY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-WEEKLY-AMT > 2500.00
               MOVE 2500.00 TO XCPEN-WEEKLY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MISSED YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2210-LIMIT-MISSED.
           IF XCPEN-MISSED-YTD < 100.00
               MOVE 100.00 TO XCPEN-MISSED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-MISSED-YTD > 2500.00
               MOVE 2500.00 TO XCPEN-MISSED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2220-FLAG-GOAL.
           IF XCPEN-GOAL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCPEN-GOAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2230-FLAG-WEEKLY.
           IF XCPEN-WEEKLY-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCPEN-WEEKLY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2240-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-PLAN-CD
                   ADD XCPEN-CONTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCPEN-CONTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2250-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * XCPEN-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCPEN-GOAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF XCPEN-OPEN-DATE > 0 AND XCPEN-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCPEN-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCPEN-MISSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCPEN-MISSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCPEN-MISSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCPEN-MISSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCPEN-MISSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MISSED YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2270-LIMIT-MISSED.
           IF XCPEN-MISSED-YTD < 10.00
               MOVE 10.00 TO XCPEN-MISSED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-MISSED-YTD > 2500.00
               MOVE 2500.00 TO XCPEN-MISSED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2280-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCPEN-MATURITY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE CONTRIB AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2290-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCPEN-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCPEN-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCPEN-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCPEN-WEEKLY-AMT ROUNDED = XCPEN-CONTRIB-AMT
                       * XCPEN-FEE-PCT / 200
               WHEN XCPEN-REGION-CD = 'FLM'
                   MOVE 10.00 TO XCPEN-WEEKLY-AMT
               WHEN XCPEN-REGION-CD = 'HLS'
                   MOVE 3.00 TO XCPEN-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCPEN-WEEKLY-AMT ROUNDED = XCPEN-CONTRIB-AMT
                       * XCPEN-FEE-PCT / 100
           END-EVALUATE
           IF XCPEN-WEEKLY-AMT > 0 AND XCPEN-WEEKLY-AMT < 10.00
               MOVE 10.00 TO XCPEN-WEEKLY-AMT
           END-IF
           IF XCPEN-WEEKLY-AMT > 25.00
               MOVE 25.00 TO XCPEN-WEEKLY-AMT
           END-IF
           ADD XCPEN-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF XCPEN-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCPEN-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO XCPEN-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2310-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-PLAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2320-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-TIER-RATE(1) TO XCPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-PAYOUT-TIER-LOW(WS-TX)
                   MOVE WS-PAYOUT-TIER-RATE(WS-TX) TO XCPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2330-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCPEN-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO XCPEN-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYOUT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2340-DESCRIBE-PAYOUT-METHOD.
           EVALUATE XCPEN-PAYOUT-METHOD-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYOUT METHOD CD:
      * B -> H WHEN NOTICE DATE IS PAST
      * H -> U WHEN NOTICE DATE IS PAST
      * U -> W WHEN NOTICE DATE IS PAST
      * W -> B WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2350-SET-PAYOUT-METHOD.
           EVALUATE XCPEN-PAYOUT-METHOD-CD
               WHEN 'B'
                   IF XCPEN-NOTICE-DATE > 0 AND XCPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO XCPEN-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF XCPEN-NOTICE-DATE > 0 AND XCPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO XCPEN-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF XCPEN-NOTICE-DATE > 0 AND XCPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO XCPEN-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF XCPEN-NOTICE-DATE > 0 AND XCPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCPEN-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * A -> B WHEN LAST PAY DATE IS PAST
      * B -> D WHEN LAST PAY DATE IS PAST
      * D -> E WHEN LAST PAY DATE IS PAST
      * E -> T WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2360-SET-TYPE.
           EVALUATE XCPEN-TYPE-CD
               WHEN 'A'
                   IF XCPEN-LAST-PAY-DATE > 0 AND XCPEN-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCPEN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF XCPEN-LAST-PAY-DATE > 0 AND XCPEN-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO XCPEN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF XCPEN-LAST-PAY-DATE > 0 AND XCPEN-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO XCPEN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF XCPEN-LAST-PAY-DATE > 0 AND XCPEN-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO XCPEN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E173 CUST ID REQUIRED
      * E248 CONTRIB AMT NEGATIVE
      * E551 CONTRIB AMT OVER 10000
      * E493 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2370-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCPEN-CUST-ID = SPACES
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN XCPEN-CONTRIB-AMT < 0
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN XCPEN-CONTRIB-AMT > 10000.00
                   MOVE 'E551' TO WS-REJECT-CODE
               WHEN XCPEN-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GOAL YTD.
      *----------------------------------------------------------------*
       2380-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCPEN-GOAL-YTD ROUNDED =
                   XCPEN-GOAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCPEN-GOAL-YTD
           END-IF
           ADD XCPEN-GOAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MISSED YTD.
      *----------------------------------------------------------------*
       2390-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCPEN-MISSED-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCPEN-MISSED-YTD
           END-IF
           ADD XCPEN-MISSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT GOAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2400-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCPEN-GOAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCPEN-GOAL-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCPEN-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WEEKLY AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2410-SPLIT-WEEKLY.
           COMPUTE WS-WORK-AMT = XCPEN-WEEKLY-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCPEN-WEEKLY-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       2420-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCPEN-CONTRIB-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCPEN-CONTRIB-AMT
           END-IF
           ADD XCPEN-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2430-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCPEN-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO XCPEN-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2440-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCPEN-LAST-PAY-DATE > 0 AND XCPEN-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCPEN-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCPEN-MISSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCPEN-MISSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD XCPEN-MISSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCPEN-MISSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCPEN-MISSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2450-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2460-COUNT-CYCLE.
           IF XCPEN-CYCLE-CNT < 9999999
               ADD 1 TO XCPEN-CYCLE-CNT
           END-IF
           IF XCPEN-CYCLE-CNT NOT < 3
               MOVE 'Y' TO XCPEN-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * GOAL YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2470-LIMIT-GOAL.
           IF XCPEN-GOAL-YTD < 50.00
               MOVE 50.00 TO XCPEN-GOAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-GOAL-YTD > 5000.00
               MOVE 5000.00 TO XCPEN-GOAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYOUT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-PAYOUT-METHOD.
           EVALUATE XCPEN-PAYOUT-METHOD-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WEEKLY AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2490-LIMIT-WEEKLY.
           IF XCPEN-WEEKLY-AMT < 100.00
               MOVE 100.00 TO XCPEN-WEEKLY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-WEEKLY-AMT > 10000.00
               MOVE 10000.00 TO XCPEN-WEEKLY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-REGION-CD
                   ADD XCPEN-GOAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCPEN-GOAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED YTD FEE ON THE WEEKLY AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2510-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCPEN-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCPEN-MISSED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE XCPEN-MISSED-YTD ROUNDED = XCPEN-WEEKLY-AMT *
                       XCPEN-FEE-PCT / 200
               WHEN XCPEN-PAYOUT-METHOD-CD = 'B'
                   MOVE 10.00 TO XCPEN-MISSED-YTD
               WHEN XCPEN-PAYOUT-METHOD-CD = 'H'
                   MOVE 12.00 TO XCPEN-MISSED-YTD
               WHEN OTHER
                   COMPUTE XCPEN-MISSED-YTD ROUNDED = XCPEN-WEEKLY-AMT *
                       XCPEN-FEE-PCT / 100
           END-EVALUATE
           IF XCPEN-MISSED-YTD > 0 AND XCPEN-MISSED-YTD < 10.00
               MOVE 10.00 TO XCPEN-MISSED-YTD
           END-IF
           IF XCPEN-MISSED-YTD > 150.00
               MOVE 150.00 TO XCPEN-MISSED-YTD
           END-IF
           ADD XCPEN-MISSED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2520-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCPEN-CONTRIB-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCPEN-CONTRIB-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCPEN-GOAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       2530-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCPEN-CONTRIB-AMT ROUNDED =
                   XCPEN-GOAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCPEN-CONTRIB-AMT
           END-IF
           ADD XCPEN-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2540-DESCRIBE-REGION.
           EVALUATE XCPEN-REGION-CD
               WHEN 'FLM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTRIB AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2550-LIMIT-CONTRIB.
           IF XCPEN-CONTRIB-AMT < 25.00
               MOVE 25.00 TO XCPEN-CONTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-CONTRIB-AMT > 5000.00
               MOVE 5000.00 TO XCPEN-CONTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2560-FLAG-GOAL.
           IF XCPEN-GOAL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCPEN-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2570-DESCRIBE-REGION.
           EVALUATE XCPEN-REGION-CD
               WHEN 'FLM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN MATURITY DATE IS PAST
      * HLS -> KMJ WHEN MATURITY DATE IS PAST
      * KMJ -> PDA WHEN MATURITY DATE IS PAST
      * PDA -> RVG WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2580-SET-REGION.
           EVALUATE XCPEN-REGION-CD
               WHEN 'FLM'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2590-DESCRIBE-REGION.
           EVALUATE XCPEN-REGION-CD
               WHEN 'FLM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2600-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCPEN-CONTRIB-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCPEN-CONTRIB-AMT - (WS-WORK-AMT * 4)
           COMPUTE XCPEN-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BONUS YTD.
      *----------------------------------------------------------------*
       2610-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCPEN-BONUS-YTD ROUNDED =
                   XCPEN-GOAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCPEN-BONUS-YTD
           END-IF
           ADD XCPEN-BONUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS YTD BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2620-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-PAYOUT-METHOD-CD
                   ADD XCPEN-BONUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCPEN-BONUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2630-DESCRIBE-REGION.
           EVALUATE XCPEN-REGION-CD
               WHEN 'FLM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL YTD BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-PAYOUT-METHOD-CD
                   ADD XCPEN-GOAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCPEN-GOAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E650 CUST ID REQUIRED
      * E068 GOAL AMT NEGATIVE
      * E192 GOAL AMT OVER 5000
      * E831 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2650-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCPEN-CUST-ID = SPACES
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN XCPEN-GOAL-AMT < 0
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN XCPEN-GOAL-AMT > 5000.00
                   MOVE 'E192' TO WS-REJECT-CODE
               WHEN XCPEN-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E831' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2660-ACCRUE-CONTRIB.
           IF XCPEN-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCPEN-CONTRIB-AMT * XCPEN-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2670-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MISSED YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2680-LIMIT-MISSED.
           IF XCPEN-MISSED-YTD < 25.00
               MOVE 25.00 TO XCPEN-MISSED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-MISSED-YTD > 1000.00
               MOVE 1000.00 TO XCPEN-MISSED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MISSED YTD.
      *----------------------------------------------------------------*
       2690-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCPEN-MISSED-YTD ROUNDED =
                   XCPEN-CONTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCPEN-MISSED-YTD
           END-IF
           ADD XCPEN-MISSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2700-SPLIT-WEEKLY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE XCPEN-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MISSED YTD.
      *----------------------------------------------------------------*
       2710-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCPEN-MISSED-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCPEN-MISSED-YTD
           END-IF
           ADD XCPEN-MISSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2720-ACCRUE-GOAL.
           IF XCPEN-GOAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCPEN-GOAL-AMT * XCPEN-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCPEN-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MISSED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WEEKLY AMT.
      *----------------------------------------------------------------*
       2730-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCPEN-WEEKLY-AMT ROUNDED =
                   XCPEN-MISSED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCPEN-WEEKLY-AMT
           END-IF
           ADD XCPEN-WEEKLY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * GOAL YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2740-LIMIT-GOAL.
           IF XCPEN-GOAL-YTD < 10.00
               MOVE 10.00 TO XCPEN-GOAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-GOAL-YTD > 50000.00
               MOVE 50000.00 TO XCPEN-GOAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2750-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCPEN-NOTICE-DATE > 0 AND XCPEN-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCPEN-BONUS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCPEN-BONUS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCPEN-BONUS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCPEN-BONUS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCPEN-BONUS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2760-DESCRIBE-REGION.
           EVALUATE XCPEN-REGION-CD
               WHEN 'FLM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BONUS YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2770-LIMIT-BONUS.
           IF XCPEN-BONUS-YTD < 100.00
               MOVE 100.00 TO XCPEN-BONUS-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-BONUS-YTD > 2500.00
               MOVE 2500.00 TO XCPEN-BONUS-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2780-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF XCPEN-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCPEN-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO XCPEN-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2790-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-TIER-RATE(1) TO XCPEN-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-PAYOUT-TIER-LOW(WS-TX)
                   MOVE WS-PAYOUT-TIER-RATE(WS-TX) TO XCPEN-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCPEN-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCPEN-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED YTD FEE ON THE GOAL AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2800-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCPEN-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCPEN-MISSED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCPEN-STOP-FLAG = 'Y'
                   COMPUTE XCPEN-MISSED-YTD ROUNDED = XCPEN-GOAL-AMT *
                       XCPEN-RATE / 200
               WHEN XCPEN-REGION-CD = 'FLM'
                   MOVE 7.00 TO XCPEN-MISSED-YTD
               WHEN XCPEN-REGION-CD = 'HLS'
                   MOVE 3.00 TO XCPEN-MISSED-YTD
               WHEN OTHER
                   COMPUTE XCPEN-MISSED-YTD ROUNDED = XCPEN-GOAL-AMT *
                       XCPEN-RATE / 100
           END-EVALUATE
           IF XCPEN-MISSED-YTD > 0 AND XCPEN-MISSED-YTD < 10.00
               MOVE 10.00 TO XCPEN-MISSED-YTD
           END-IF
           IF XCPEN-MISSED-YTD > 35.00
               MOVE 35.00 TO XCPEN-MISSED-YTD
           END-IF
           ADD XCPEN-MISSED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E256 CUST ID REQUIRED
      * E057 CONTRIB AMT NEGATIVE
      * E436 CONTRIB AMT OVER 50000
      * E072 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2810-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCPEN-CUST-ID = SPACES
                   MOVE 'E256' TO WS-REJECT-CODE
               WHEN XCPEN-CONTRIB-AMT < 0
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN XCPEN-CONTRIB-AMT > 50000.00
                   MOVE 'E436' TO WS-REJECT-CODE
               WHEN XCPEN-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E072' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GOAL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2820-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCPEN-GOAL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = XCPEN-GOAL-YTD - (WS-WORK-AMT * 3)
           COMPUTE XCPEN-BONUS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2830-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * BONUS YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2840-FLAG-BONUS.
           IF XCPEN-BONUS-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCPEN-BONUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS YTD BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2850-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-PLAN-CD
                   ADD XCPEN-BONUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCPEN-BONUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2860-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-PAYOUT-METHOD-CD
                   ADD XCPEN-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCPEN-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BONUS YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2870-ACCRUE-BONUS.
           IF XCPEN-BONUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCPEN-BONUS-YTD * XCPEN-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCPEN-MISSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE GOAL AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2880-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCPEN-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCPEN-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = XCPEN-GOAL-AMT *
                       XCPEN-FEE-PCT / 200
               WHEN XCPEN-PLAN-CD = 'GN'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN XCPEN-PLAN-CD = 'JT'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = XCPEN-GOAL-AMT *
                       XCPEN-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN MATURITY DATE IS PAST
      * HLS -> KMJ WHEN MATURITY DATE IS PAST
      * KMJ -> PDA WHEN MATURITY DATE IS PAST
      * PDA -> RVG WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2890-SET-REGION.
           EVALUATE XCPEN-REGION-CD
               WHEN 'FLM'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCPEN-MATURITY-DATE > 0 AND XCPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BONUS YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2900-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCPEN-BONUS-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCPEN-BONUS-YTD - (WS-WORK-AMT * 2)
           COMPUTE XCPEN-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL FEE ON THE BONUS YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2910-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCPEN-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCPEN-GOAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCPEN-STOP-FLAG = 'Y'
                   COMPUTE XCPEN-GOAL-AMT ROUNDED = XCPEN-BONUS-YTD *
                       XCPEN-FEE-PCT / 200
               WHEN XCPEN-PAYOUT-METHOD-CD = 'B'
                   MOVE 20.00 TO XCPEN-GOAL-AMT
               WHEN XCPEN-PAYOUT-METHOD-CD = 'H'
                   MOVE 7.00 TO XCPEN-GOAL-AMT
               WHEN OTHER
                   COMPUTE XCPEN-GOAL-AMT ROUNDED = XCPEN-BONUS-YTD *
                       XCPEN-FEE-PCT / 100
           END-EVALUATE
           IF XCPEN-GOAL-AMT > 0 AND XCPEN-GOAL-AMT < 1.00
               MOVE 1.00 TO XCPEN-GOAL-AMT
           END-IF
           IF XCPEN-GOAL-AMT > 250.00
               MOVE 250.00 TO XCPEN-GOAL-AMT
           END-IF
           ADD XCPEN-GOAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2920-COUNT-NOTICE.
           IF XCPEN-NOTICE-CNT < 9999999
               ADD 1 TO XCPEN-NOTICE-CNT
           END-IF
           IF XCPEN-NOTICE-CNT NOT < 10
               MOVE 'Y' TO XCPEN-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2930-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCPEN-TYPE-CD
                   ADD XCPEN-GOAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCPEN-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCPEN-GOAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GOAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2940-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCPEN-GOAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCPEN-GOAL-AMT - (WS-WORK-AMT * 4)
           COMPUTE XCPEN-MISSED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MISSED YTD.
      *----------------------------------------------------------------*
       2950-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCPEN-MISSED-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCPEN-MISSED-YTD
           END-IF
           ADD XCPEN-MISSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E652 CONTACT REQUIRED
      * E600 WORK AMT NEGATIVE
      * E063 WORK AMT OVER 25000
      * E939 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2960-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCPEN-CONTACT = SPACES
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E063' TO WS-REJECT-CODE
               WHEN XCPEN-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT TIER TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2970-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-TIER-RATE(1) TO XCPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF XCPEN-CONTRIB-AMT NOT < WS-PAYOUT-TIER-LOW(WS-TX)
                   MOVE WS-PAYOUT-TIER-RATE(WS-TX) TO XCPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL YTD FEE ON THE MISSED YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2980-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCPEN-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCPEN-GOAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCPEN-STOP-FLAG = 'Y'
                   COMPUTE XCPEN-GOAL-YTD ROUNDED = XCPEN-MISSED-YTD *
                       XCPEN-RATE / 200
               WHEN XCPEN-PAYOUT-METHOD-CD = 'B'
                   MOVE 20.00 TO XCPEN-GOAL-YTD
               WHEN XCPEN-PAYOUT-METHOD-CD = 'H'
                   MOVE 10.00 TO XCPEN-GOAL-YTD
               WHEN OTHER
                   COMPUTE XCPEN-GOAL-YTD ROUNDED = XCPEN-MISSED-YTD *
                       XCPEN-RATE / 100
           END-EVALUATE
           IF XCPEN-GOAL-YTD > 0 AND XCPEN-GOAL-YTD < 1.00
               MOVE 1.00 TO XCPEN-GOAL-YTD
           END-IF
           IF XCPEN-GOAL-YTD > 150.00
               MOVE 150.00 TO XCPEN-GOAL-YTD
           END-IF
           ADD XCPEN-GOAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE MISSED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WEEKLY AMT.
      *----------------------------------------------------------------*
       2990-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCPEN-WEEKLY-AMT ROUNDED =
                   XCPEN-MISSED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCPEN-WEEKLY-AMT
           END-IF
           ADD XCPEN-WEEKLY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BONUS YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3000-LIMIT-BONUS.
           IF XCPEN-BONUS-YTD < 100.00
               MOVE 100.00 TO XCPEN-BONUS-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-BONUS-YTD > 2500.00
               MOVE 2500.00 TO XCPEN-BONUS-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-GOAL.
           IF XCPEN-GOAL-AMT < 0.00
               MOVE 0.00 TO XCPEN-GOAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCPEN-GOAL-AMT > 5000.00
               MOVE 5000.00 TO XCPEN-GOAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO XCPEN-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO XCPEN-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO XCPEN-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'XCPEN EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
