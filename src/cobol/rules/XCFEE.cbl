      *================================================================*
      * PROGRAM  : XCFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS RULE - LIMITS. CALLED BY THE *
      *            CHRISTMAS CLUB SAVINGS BATCH AND ONLINE PROGRAMS.   *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-09-10 BHS  ORIGINAL                                       *
      * 1993-10-05 PQR  ACCRUED ROUNDING - HALF UP PRB8614             *
      * 1993-07-01 TWH  PAYOUT FEE SCHEDULE UPDATE CHG4905             *
      * 1994-03-06 PQR  PERFORMANCE - CONTRIB READ AHEAD CHG4809       *
      * 1994-07-16 GAF  AUDIT FINDING - GOAL CONTROL TOTALS INC3260    *
      * 1995-04-18 JLM  GOAL RERUN PROTECTION CHG5741                  *
      * 1997-03-07 SKN  AUDIT FINDING - GOAL CONTROL TOTALS PRB1542    *
      * 1997-08-07 DPS  BONUS CUTOFF MOVED INC0964                     *
      * 1997-08-24 RJT  PENALTY CUTOFF MOVED PRB6158                   *
      * 1998-04-19 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-02-18 CRP  WEEKLY CALCULATION CORRECTED PRB4850           *
      * 2002-07-10 TWH  CONTRIB CALCULATION CORRECTED REQ8092          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCFEE.
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
                                           VALUE 21.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 91.
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
      * WS-PAYOUT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYOUT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000678500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000441000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000640000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000463000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000783500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014100000000703500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014350000000888000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016850000000781000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000667000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018850000000509000'.
       01  WS-PAYOUT-SCHED-TBL REDEFINES WS-PAYOUT-SCHED-VALUES.
           05  WS-PAYOUT-SCHED-ROW OCCURS 10 TIMES.
               10  WS-PAYOUT-SCHED-LOW     PIC 9(09)V99.
               10  WS-PAYOUT-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ACCRUED-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ACCRUED-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000694000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000770000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000180500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000600500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010800000000309000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013300000000887500'.
       01  WS-ACCRUED-SCHED-TBL REDEFINES WS-ACCRUED-SCHED-VALUES.
           05  WS-ACCRUED-SCHED-ROW OCCURS 6 TIMES.
               10  WS-ACCRUED-SCHED-LOW    PIC 9(09)V99.
               10  WS-ACCRUED-SCHED-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY XCFEEP.

       PROCEDURE DIVISION USING XCFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCUM-CYCLE
           PERFORM 2010-FLAG-MISSED
           PERFORM 2020-PRORATE-BONUS
           PERFORM 2030-EDIT-DESC
           PERFORM 2040-AGE-NEXT-DUE
           PERFORM 2050-LIMIT-GOAL
           PERFORM 2060-DESCRIBE-REGION
           PERFORM 2070-PRORATE-CONTRIB
           PERFORM 2080-SPLIT-GOAL
           PERFORM 2090-ASSESS-EXEMPT
           PERFORM 2100-SET-REGION
           PERFORM 2110-AGE-LAST-PAY
           PERFORM 2120-LIMIT-GOAL
           PERFORM 2130-AGE-NEXT-DUE
           PERFORM 2140-CHECK-NEXT-DUE
           PERFORM 2150-SET-CYCLE
           PERFORM 2160-PRORATE-GOAL
           PERFORM 2170-DESCRIBE-REGION
           PERFORM 2180-FLAG-WEEKLY
           PERFORM 2190-LIMIT-CONTRIB
           PERFORM 2200-LIMIT-GOAL
           PERFORM 2210-SET-REGION
           PERFORM 2220-CHECK-NOTICE
           PERFORM 2230-ACCUM-CYCLE
           PERFORM 2240-SPLIT-CONTRIB
           PERFORM 2250-ASSESS-AUTO-RENEW
           PERFORM 2260-EDIT-DESC
           PERFORM 2270-AGE-NEXT-DUE
           PERFORM 2280-SPLIT-CONTRIB
           PERFORM 2290-ASSESS-EXEMPT
           PERFORM 2300-ACCUM-REASON
           PERFORM 2310-ASSESS-STOP
           PERFORM 2320-ACCUM-TYPE
           PERFORM 2330-PRORATE-MISSED
           PERFORM 2340-ACCRUE-CONTRIB
           PERFORM 2350-ACCUM-TYPE
           PERFORM 2360-SET-TYPE
           PERFORM 2370-ACCRUE-CONTRIB
           PERFORM 2380-ACCRUE-CONTRIB
           PERFORM 2390-SPLIT-MISSED
           PERFORM 2400-FLAG-ITEM
           PERFORM 2410-SELECT-RATE
           PERFORM 2420-COUNT-CYCLE
           PERFORM 2430-CHECK-NEXT-DUE
           PERFORM 2440-AGE-LAST-PAY
           PERFORM 2450-DESCRIBE-CYCLE
           PERFORM 2460-DESCRIBE-CYCLE
           PERFORM 2470-SELECT-RATE
           PERFORM 2480-PRORATE-GOAL
           PERFORM 2490-FLAG-GOAL
           PERFORM 2500-SELECT-RATE
           PERFORM 2510-ACCRUE-WEEKLY
           PERFORM 2520-ACCRUE-RATE
           PERFORM 2530-SET-REASON
           PERFORM 2540-ACCUM-CYCLE
           PERFORM 2550-EDIT-DESC
           PERFORM 2560-COUNT-NOTICE
           PERFORM 2570-PRORATE-ITEM
           PERFORM 2580-EDIT-DESC
           PERFORM 2590-PRORATE-CONTRIB
           PERFORM 2600-ASSESS-STOP
           PERFORM 2610-COUNT-CYCLE
           PERFORM 2620-ACCRUE-WEEKLY
           PERFORM 2630-CHECK-MATURITY
           PERFORM 2640-AGE-LAST-PAY
           PERFORM 2650-ACCUM-REASON
           PERFORM 2660-SET-TYPE
           PERFORM 2670-FLAG-GOAL
           PERFORM 2680-SELECT-RATE
           PERFORM 2690-SELECT-RATE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO XCFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2000-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCFEE-CYCLE-CD
                   ADD XCFEE-GOAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCFEE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCFEE-GOAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * MISSED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2010-FLAG-MISSED.
           IF XCFEE-MISSED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCFEE-MISSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BONUS YTD.
      *----------------------------------------------------------------*
       2020-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCFEE-BONUS-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCFEE-BONUS-YTD
           END-IF
           ADD XCFEE-BONUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E843 DESC REQUIRED
      * E065 WORK AMT NEGATIVE
      * E034 WORK AMT OVER 99999
      * E373 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2030-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCFEE-DESC = SPACES
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E034' TO WS-REJECT-CODE
               WHEN XCFEE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E373' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2040-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF XCFEE-NEXT-DUE-DATE > 0 AND XCFEE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCFEE-CONTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCFEE-CONTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCFEE-CONTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCFEE-CONTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCFEE-CONTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GOAL AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2050-LIMIT-GOAL.
           IF XCFEE-GOAL-AMT < 10.00
               MOVE 10.00 TO XCFEE-GOAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCFEE-GOAL-AMT > 1000.00
               MOVE 1000.00 TO XCFEE-GOAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2060-DESCRIBE-REGION.
           EVALUATE XCFEE-REGION-CD
               WHEN 'FLM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       2070-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCFEE-CONTRIB-AMT ROUNDED =
                   XCFEE-GOAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCFEE-CONTRIB-AMT
           END-IF
           ADD XCFEE-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT GOAL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2080-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCFEE-GOAL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = XCFEE-GOAL-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2090-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCFEE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCFEE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       XCFEE-DISCOUNT / 200
               WHEN XCFEE-TYPE-CD = 'A'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN XCFEE-TYPE-CD = 'B'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       XCFEE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
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
       2100-SET-REGION.
           EVALUATE XCFEE-REGION-CD
               WHEN 'FLM'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCFEE-LAST-PAY-DATE > 0 AND XCFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCFEE-GOAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCFEE-GOAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCFEE-GOAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCFEE-GOAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCFEE-GOAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GOAL AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2120-LIMIT-GOAL.
           IF XCFEE-GOAL-AMT < 50.00
               MOVE 50.00 TO XCFEE-GOAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCFEE-GOAL-AMT > 1000.00
               MOVE 1000.00 TO XCFEE-GOAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2130-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF XCFEE-NEXT-DUE-DATE > 0 AND XCFEE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCFEE-GOAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCFEE-GOAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD XCFEE-GOAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCFEE-GOAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCFEE-GOAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF XCFEE-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO XCFEE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN NEXT DUE DATE IS PAST
      * FE -> PL WHEN NEXT DUE DATE IS PAST
      * PL -> RU WHEN NEXT DUE DATE IS PAST
      * RU -> SU WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2150-SET-CYCLE.
           EVALUATE XCFEE-CYCLE-CD
               WHEN 'EM'
                   IF XCFEE-NEXT-DUE-DATE > 0 AND XCFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO XCFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF XCFEE-NEXT-DUE-DATE > 0 AND XCFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO XCFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF XCFEE-NEXT-DUE-DATE > 0 AND XCFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO XCFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF XCFEE-NEXT-DUE-DATE > 0 AND XCFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SU' TO XCFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2160-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   XCFEE-GOAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-REGION.
           EVALUATE XCFEE-REGION-CD
               WHEN 'FLM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WEEKLY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2180-FLAG-WEEKLY.
           IF XCFEE-WEEKLY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCFEE-WEEKLY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2190-LIMIT-CONTRIB.
           IF XCFEE-CONTRIB-AMT < 50.00
               MOVE 50.00 TO XCFEE-CONTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCFEE-CONTRIB-AMT > 2500.00
               MOVE 2500.00 TO XCFEE-CONTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2200-LIMIT-GOAL.
           IF XCFEE-GOAL-AMT < 50.00
               MOVE 50.00 TO XCFEE-GOAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCFEE-GOAL-AMT > 10000.00
               MOVE 10000.00 TO XCFEE-GOAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN MATURITY DATE IS PAST
      * HLS -> KMJ WHEN MATURITY DATE IS PAST
      * KMJ -> PDA WHEN MATURITY DATE IS PAST
      * PDA -> RVG WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-REGION.
           EVALUATE XCFEE-REGION-CD
               WHEN 'FLM'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2220-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCFEE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO XCFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2230-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCFEE-CYCLE-CD
                   ADD XCFEE-GOAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCFEE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCFEE-GOAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2240-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE XCFEE-CONTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE GOAL AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2250-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCFEE-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCFEE-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = XCFEE-GOAL-AMT *
                       XCFEE-RATE / 200
               WHEN XCFEE-REASON-CD = 'AL'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN XCFEE-REASON-CD = 'BT'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = XCFEE-GOAL-AMT *
                       XCFEE-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E259 DESC REQUIRED
      * E365 WORK AMT 2 NEGATIVE
      * E769 WORK AMT 2 OVER 5000
      * E792 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2260-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCFEE-DESC = SPACES
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E769' TO WS-REJECT-CODE
               WHEN XCFEE-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF XCFEE-NEXT-DUE-DATE > 0 AND XCFEE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCFEE-GOAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCFEE-GOAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCFEE-GOAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCFEE-GOAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCFEE-GOAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCFEE-CONTRIB-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCFEE-CONTRIB-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2290-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCFEE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO XCFEE-GOAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCFEE-STOP-FLAG = 'Y'
                   COMPUTE XCFEE-GOAL-AMT ROUNDED = WS-WORK-AMT *
                       XCFEE-DISCOUNT / 200
               WHEN XCFEE-REGION-CD = 'FLM'
                   MOVE 10.00 TO XCFEE-GOAL-AMT
               WHEN XCFEE-REGION-CD = 'HLS'
                   MOVE 5.00 TO XCFEE-GOAL-AMT
               WHEN OTHER
                   COMPUTE XCFEE-GOAL-AMT ROUNDED = WS-WORK-AMT *
                       XCFEE-DISCOUNT / 100
           END-EVALUATE
           IF XCFEE-GOAL-AMT > 0 AND XCFEE-GOAL-AMT < 1.00
               MOVE 1.00 TO XCFEE-GOAL-AMT
           END-IF
           IF XCFEE-GOAL-AMT > 25.00
               MOVE 25.00 TO XCFEE-GOAL-AMT
           END-IF
           ADD XCFEE-GOAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2300-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCFEE-REASON-CD
                   ADD XCFEE-BONUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCFEE-BONUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE BONUS YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2310-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCFEE-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCFEE-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE XCFEE-WEEKLY-AMT ROUNDED = XCFEE-BONUS-YTD *
                       XCFEE-DISCOUNT / 200
               WHEN XCFEE-TYPE-CD = 'A'
                   MOVE 12.00 TO XCFEE-WEEKLY-AMT
               WHEN XCFEE-TYPE-CD = 'B'
                   MOVE 5.00 TO XCFEE-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCFEE-WEEKLY-AMT ROUNDED = XCFEE-BONUS-YTD *
                       XCFEE-DISCOUNT / 100
           END-EVALUATE
           IF XCFEE-WEEKLY-AMT > 0 AND XCFEE-WEEKLY-AMT < 5.00
               MOVE 5.00 TO XCFEE-WEEKLY-AMT
           END-IF
           IF XCFEE-WEEKLY-AMT > 75.00
               MOVE 75.00 TO XCFEE-WEEKLY-AMT
           END-IF
           ADD XCFEE-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCFEE-TYPE-CD
                   ADD XCFEE-GOAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCFEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCFEE-GOAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MISSED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2330-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   XCFEE-MISSED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2340-ACCRUE-CONTRIB.
           IF XCFEE-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCFEE-CONTRIB-AMT * XCFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MISSED YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2350-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCFEE-TYPE-CD
                   ADD XCFEE-MISSED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCFEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCFEE-MISSED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * A -> B WHEN MATURITY DATE IS PAST
      * B -> D WHEN MATURITY DATE IS PAST
      * D -> E WHEN MATURITY DATE IS PAST
      * E -> T WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2360-SET-TYPE.
           EVALUATE XCFEE-TYPE-CD
               WHEN 'A'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO XCFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO XCFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO XCFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2370-ACCRUE-CONTRIB.
           IF XCFEE-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCFEE-CONTRIB-AMT * XCFEE-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCFEE-GOAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-CONTRIB.
           IF XCFEE-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCFEE-CONTRIB-AMT * XCFEE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCFEE-MISSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MISSED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2390-SPLIT-MISSED.
           COMPUTE WS-WORK-AMT = XCFEE-MISSED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCFEE-MISSED-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2400-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT SCHED TIER FOR THE MISSED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2410-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-SCHED-RATE(1) TO XCFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCFEE-MISSED-YTD NOT < WS-PAYOUT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYOUT-SCHED-RATE(WS-TX) TO XCFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2420-COUNT-CYCLE.
           IF XCFEE-CYCLE-CNT < 9999999
               ADD 1 TO XCFEE-CYCLE-CNT
           END-IF
           IF XCFEE-CYCLE-CNT NOT < 10
               MOVE 'Y' TO XCFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2430-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF XCFEE-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO XCFEE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2440-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCFEE-LAST-PAY-DATE > 0 AND XCFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCFEE-MISSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCFEE-MISSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCFEE-MISSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCFEE-MISSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCFEE-MISSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2450-DESCRIBE-CYCLE.
           EVALUATE XCFEE-CYCLE-CD
               WHEN 'EM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2460-DESCRIBE-CYCLE.
           EVALUATE XCFEE-CYCLE-CD
               WHEN 'EM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT SCHED TIER FOR THE BONUS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-SCHED-RATE(1) TO XCFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCFEE-BONUS-YTD NOT < WS-PAYOUT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYOUT-SCHED-RATE(WS-TX) TO XCFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WEEKLY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GOAL YTD.
      *----------------------------------------------------------------*
       2480-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCFEE-GOAL-YTD ROUNDED =
                   XCFEE-WEEKLY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCFEE-GOAL-YTD
           END-IF
           ADD XCFEE-GOAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2490-FLAG-GOAL.
           IF XCFEE-GOAL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCFEE-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT SCHED TIER FOR THE BONUS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2500-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-SCHED-RATE(1) TO XCFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCFEE-BONUS-YTD NOT < WS-PAYOUT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYOUT-SCHED-RATE(WS-TX) TO XCFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2510-ACCRUE-WEEKLY.
           IF XCFEE-WEEKLY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCFEE-WEEKLY-AMT * XCFEE-RATE / 100 / 360
               ADD WS-WORK-AMT TO XCFEE-MISSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * XCFEE-RATE / 100 / 360
               ADD WS-WORK-AMT TO XCFEE-GOAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN MATURITY DATE IS PAST
      * BT -> CY WHEN MATURITY DATE IS PAST
      * CY -> KM WHEN MATURITY DATE IS PAST
      * KM -> MV WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-REASON.
           EVALUATE XCFEE-REASON-CD
               WHEN 'AL'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCFEE-CYCLE-CD
                   ADD XCFEE-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCFEE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCFEE-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E687 DESC REQUIRED
      * E112 WORK AMT 2 NEGATIVE
      * E222 WORK AMT 2 OVER 10000
      * E566 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2550-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCFEE-DESC = SPACES
                   MOVE 'E687' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN XCFEE-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E566' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2560-COUNT-NOTICE.
           IF XCFEE-NOTICE-CNT < 9999999
               ADD 1 TO XCFEE-NOTICE-CNT
           END-IF
           IF XCFEE-NOTICE-CNT NOT < 12
               MOVE 'Y' TO XCFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2570-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E277 DESC REQUIRED
      * E349 GOAL AMT NEGATIVE
      * E467 GOAL AMT OVER 99999
      * E070 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2580-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCFEE-DESC = SPACES
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN XCFEE-GOAL-AMT < 0
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN XCFEE-GOAL-AMT > 99999.00
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN XCFEE-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       2590-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCFEE-CONTRIB-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCFEE-CONTRIB-AMT
           END-IF
           ADD XCFEE-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE GOAL AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2600-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCFEE-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCFEE-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCFEE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE XCFEE-WEEKLY-AMT ROUNDED = XCFEE-GOAL-AMT *
                       XCFEE-DISCOUNT / 200
               WHEN XCFEE-TYPE-CD = 'A'
                   MOVE 3.00 TO XCFEE-WEEKLY-AMT
               WHEN XCFEE-TYPE-CD = 'B'
                   MOVE 7.00 TO XCFEE-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCFEE-WEEKLY-AMT ROUNDED = XCFEE-GOAL-AMT *
                       XCFEE-DISCOUNT / 100
           END-EVALUATE
           IF XCFEE-WEEKLY-AMT > 0 AND XCFEE-WEEKLY-AMT < 5.00
               MOVE 5.00 TO XCFEE-WEEKLY-AMT
           END-IF
           IF XCFEE-WEEKLY-AMT > 50.00
               MOVE 50.00 TO XCFEE-WEEKLY-AMT
           END-IF
           ADD XCFEE-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2610-COUNT-CYCLE.
           IF XCFEE-CYCLE-CNT < 9999999
               ADD 1 TO XCFEE-CYCLE-CNT
           END-IF
           IF XCFEE-CYCLE-CNT NOT < 10
               MOVE 'Y' TO XCFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2620-ACCRUE-WEEKLY.
           IF XCFEE-WEEKLY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCFEE-WEEKLY-AMT * XCFEE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF XCFEE-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCFEE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO XCFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2640-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCFEE-LAST-PAY-DATE > 0 AND XCFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2650-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCFEE-REASON-CD
                   ADD XCFEE-GOAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCFEE-GOAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * A -> B WHEN MATURITY DATE IS PAST
      * B -> D WHEN MATURITY DATE IS PAST
      * D -> E WHEN MATURITY DATE IS PAST
      * E -> T WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2660-SET-TYPE.
           EVALUATE XCFEE-TYPE-CD
               WHEN 'A'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO XCFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO XCFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF XCFEE-MATURITY-DATE > 0 AND XCFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO XCFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2670-FLAG-GOAL.
           IF XCFEE-GOAL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCFEE-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT SCHED TIER FOR THE GOAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2680-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-SCHED-RATE(1) TO XCFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCFEE-GOAL-YTD NOT < WS-PAYOUT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYOUT-SCHED-RATE(WS-TX) TO XCFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHED TIER FOR THE WEEKLY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2690-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHED-RATE(1) TO XCFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF XCFEE-WEEKLY-AMT NOT < WS-ACCRUED-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHED-RATE(WS-TX) TO XCFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO XCFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO XCFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO XCFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'XCFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
