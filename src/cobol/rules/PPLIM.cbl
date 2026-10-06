      *================================================================*
      * PROGRAM  : PPLIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : POSITIVE PAY RULE - FEES. CALLED BY THE POSITIVE PAY*
      *            BATCH AND ONLINE PROGRAMS.                          *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-13 MKL  ORIGINAL                                       *
      * 1996-10-27 GAF  PERFORMANCE - STALE READ AHEAD PRB5891         *
      * 1998-05-27 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-08-21 SKN  AUDIT FINDING - VOID CONTROL TOTALS CHG5207    *
      * 2002-05-20 MKL  EXCEPT STATUS CODES ADDED PRB9280              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPLIM.
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
                                           VALUE 9.000000.
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
      * WS-VOID-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000224000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000204500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000688500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000477000'.
       01  WS-VOID-SCHED-TBL REDEFINES WS-VOID-SCHED-VALUES.
           05  WS-VOID-SCHED-ROW OCCURS 4 TIMES.
               10  WS-VOID-SCHED-LOW       PIC 9(09)V99.
               10  WS-VOID-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STALE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STALE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000340000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000733500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000339000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000048500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000031000'.
       01  WS-STALE-BAND-TBL REDEFINES WS-STALE-BAND-VALUES.
           05  WS-STALE-BAND-ROW OCCURS 5 TIMES.
               10  WS-STALE-BAND-LOW       PIC 9(09)V99.
               10  WS-STALE-BAND-RATE      PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PPLIMP.

       PROCEDURE DIVISION USING PPLIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-PAID
           PERFORM 2010-ACCUM-REASON
           PERFORM 2020-SELECT-DISCOUNT
           PERFORM 2030-COUNT-NOTICE
           PERFORM 2040-DESCRIBE-REGION
           PERFORM 2050-LIMIT-ITEM
           PERFORM 2060-SPLIT-PAID
           PERFORM 2070-FLAG-PAID
           PERFORM 2080-PRORATE-STALE
           PERFORM 2090-AGE-EXPIRY
           PERFORM 2100-AGE-CLOSE
           PERFORM 2110-CHECK-NEXT-REVIEW
           PERFORM 2120-ACCUM-REASON
           PERFORM 2130-PRORATE-STALE
           PERFORM 2140-PRORATE-VOID
           PERFORM 2150-COUNT-CYCLE
           PERFORM 2160-SPLIT-STALE
           PERFORM 2170-ASSESS-EXEMPT
           PERFORM 2180-PRORATE-EXCEPT
           PERFORM 2190-ACCRUE-PAID
           PERFORM 2200-DESCRIBE-REASON
           PERFORM 2210-FLAG-STALE
           PERFORM 2220-SELECT-RATE
           PERFORM 2230-SPLIT-ISSUED
           PERFORM 2240-ASSESS-NOTICE-SENT
           PERFORM 2250-SELECT-DISCOUNT
           PERFORM 2260-PRORATE-PAID
           PERFORM 2270-DESCRIBE-REGION
           PERFORM 2280-ASSESS-NOTICE-SENT
           PERFORM 2290-AGE-NEXT-REVIEW
           PERFORM 2300-PRORATE-STALE
           PERFORM 2310-ACCUM-DECISION
           PERFORM 2320-DESCRIBE-REASON
           PERFORM 2330-COUNT-NOTICE
           PERFORM 2340-SPLIT-VOID
           PERFORM 2350-ASSESS-NOTICE-SENT
           PERFORM 2360-LIMIT-EXCEPT
           PERFORM 2370-DESCRIBE-REASON
           PERFORM 2380-ACCRUE-VOID
           PERFORM 2390-SPLIT-PAID
           PERFORM 2400-EDIT-NAME
           PERFORM 2410-ACCUM-REGION
           PERFORM 2420-CHECK-CLOSE
           PERFORM 2430-DESCRIBE-MATCH-RULE
           PERFORM 2440-SPLIT-VOID
           PERFORM 2450-FLAG-PAID
           PERFORM 2460-COUNT-CYCLE
           PERFORM 2470-EDIT-NAME
           PERFORM 2480-SPLIT-STALE
           PERFORM 2490-SET-REASON
           PERFORM 2500-SELECT-DISCOUNT
           PERFORM 2510-CHECK-CLOSE
           PERFORM 2520-ACCUM-DECISION
           PERFORM 2530-SPLIT-ISSUED
           PERFORM 2540-SET-REGION
           PERFORM 2550-EDIT-NAME
           PERFORM 2560-ASSESS-NOTICE-SENT
           PERFORM 2570-PRORATE-VOID
           PERFORM 2580-CHECK-NEXT-REVIEW
           PERFORM 2590-SELECT-RATE
           PERFORM 2600-FLAG-VOID
           PERFORM 2610-ACCUM-REASON
           PERFORM 2620-AGE-OPEN
           PERFORM 2630-ACCUM-MATCH-RULE
           PERFORM 2640-ASSESS-EXEMPT
           PERFORM 2650-ASSESS-AUTO-RENEW
           PERFORM 2660-ACCUM-REGION
           PERFORM 2670-AGE-CLOSE
           PERFORM 2680-SELECT-DISCOUNT
           PERFORM 2690-PRORATE-PAID
           PERFORM 2700-DESCRIBE-REASON
           PERFORM 2710-ACCUM-REGION
           PERFORM 2720-COUNT-CYCLE
           PERFORM 2730-ACCUM-REASON
           PERFORM 2740-ASSESS-NOTICE-SENT
           PERFORM 2750-SPLIT-VOID
           PERFORM 2760-EDIT-NAME
           PERFORM 2770-SET-REASON
           PERFORM 2780-COUNT-CYCLE
           PERFORM 2790-ACCUM-REASON
           PERFORM 2800-EDIT-REF-NO
           PERFORM 2810-AGE-OPEN
           PERFORM 2820-EDIT-REF-NO
           PERFORM 2830-COUNT-NOTICE
           PERFORM 2840-SET-REASON
           PERFORM 2850-SELECT-RATE
           PERFORM 2860-SPLIT-VOID
           PERFORM 2870-ASSESS-EXEMPT
           PERFORM 2880-AGE-NEXT-REVIEW
           PERFORM 2890-ACCRUE-STALE
           PERFORM 2900-ASSESS-AUTO-RENEW
           PERFORM 2910-ASSESS-EXEMPT
           PERFORM 2920-ACCUM-MATCH-RULE
           PERFORM 2930-ASSESS-EXEMPT
           PERFORM 2940-ACCRUE-PAID
           PERFORM 2950-EDIT-REF-NO
           PERFORM 2960-SELECT-RATE
           PERFORM 2970-LIMIT-STALE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PPLIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPLIM-PAID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPLIM-PAID-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-REASON-CD
                   ADD PPLIM-STALE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPLIM-STALE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2020-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2030-COUNT-NOTICE.
           IF PPLIM-NOTICE-CNT < 9999999
               ADD 1 TO PPLIM-NOTICE-CNT
           END-IF
           IF PPLIM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO PPLIM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2040-DESCRIBE-REGION.
           EVALUATE PPLIM-REGION-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2050-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPLIM-PAID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPLIM-PAID-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPLIM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2070-FLAG-PAID.
           IF PPLIM-PAID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPLIM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STALE YTD.
      *----------------------------------------------------------------*
       2080-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPLIM-STALE-YTD ROUNDED =
                   PPLIM-PAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPLIM-STALE-YTD
           END-IF
           ADD PPLIM-STALE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2090-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PPLIM-EXPIRY-DATE > 0 AND PPLIM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPLIM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPLIM-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPLIM-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPLIM-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPLIM-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPLIM-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2100-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPLIM-CLOSE-DATE > 0 AND PPLIM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2110-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPLIM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPLIM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PPLIM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2120-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-REASON-CD
                   ADD PPLIM-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPLIM-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STALE YTD.
      *----------------------------------------------------------------*
       2130-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPLIM-STALE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPLIM-STALE-YTD
           END-IF
           ADD PPLIM-STALE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID YTD.
      *----------------------------------------------------------------*
       2140-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPLIM-VOID-YTD ROUNDED =
                   PPLIM-PAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPLIM-VOID-YTD
           END-IF
           ADD PPLIM-VOID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2150-COUNT-CYCLE.
           IF PPLIM-CYCLE-CNT < 9999999
               ADD 1 TO PPLIM-CYCLE-CNT
           END-IF
           IF PPLIM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PPLIM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STALE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPLIM-STALE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PPLIM-STALE-YTD - (WS-WORK-AMT * 2)
           COMPUTE PPLIM-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE VOID YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2170-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPLIM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PPLIM-VOID-YTD *
                       PPLIM-DISCOUNT / 200
               WHEN PPLIM-REGION-CD = 'A'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN PPLIM-REGION-CD = 'B'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PPLIM-VOID-YTD *
                       PPLIM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCEPT YTD.
      *----------------------------------------------------------------*
       2180-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPLIM-EXCEPT-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPLIM-EXCEPT-YTD
           END-IF
           ADD PPLIM-EXCEPT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2190-ACCRUE-PAID.
           IF PPLIM-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPLIM-PAID-AMT * PPLIM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPLIM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2200-DESCRIBE-REASON.
           EVALUATE PPLIM-REASON-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STALE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2210-FLAG-STALE.
           IF PPLIM-STALE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPLIM-STALE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE VOID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PPLIM-VOID-YTD NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2230-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPLIM-ISSUED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PPLIM-ISSUED-YTD - (WS-WORK-AMT * 12)
           COMPUTE PPLIM-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE PAID YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2240-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPLIM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PPLIM-PAID-AMT ROUNDED = PPLIM-PAID-YTD *
                       PPLIM-RATE / 200
               WHEN PPLIM-REGION-CD = 'A'
                   MOVE 7.00 TO PPLIM-PAID-AMT
               WHEN PPLIM-REGION-CD = 'B'
                   MOVE 12.00 TO PPLIM-PAID-AMT
               WHEN OTHER
                   COMPUTE PPLIM-PAID-AMT ROUNDED = PPLIM-PAID-YTD *
                       PPLIM-RATE / 100
           END-EVALUATE
           IF PPLIM-PAID-AMT > 0 AND PPLIM-PAID-AMT < 1.00
               MOVE 1.00 TO PPLIM-PAID-AMT
           END-IF
           IF PPLIM-PAID-AMT > 35.00
               MOVE 35.00 TO PPLIM-PAID-AMT
           END-IF
           ADD PPLIM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2250-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       2260-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPLIM-PAID-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPLIM-PAID-AMT
           END-IF
           ADD PPLIM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2270-DESCRIBE-REGION.
           EVALUATE PPLIM-REGION-CD
               WHEN 'A'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2280-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPLIM-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPLIM-EXCEPT-YTD ROUNDED = WS-WORK-AMT-2 *
                       PPLIM-RATE / 200
               WHEN PPLIM-DECISION-CD = 'AND'
                   MOVE 15.00 TO PPLIM-EXCEPT-YTD
               WHEN PPLIM-DECISION-CD = 'BER'
                   MOVE 15.00 TO PPLIM-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE PPLIM-EXCEPT-YTD ROUNDED = WS-WORK-AMT-2 *
                       PPLIM-RATE / 100
           END-EVALUATE
           IF PPLIM-EXCEPT-YTD > 0 AND PPLIM-EXCEPT-YTD < 1.00
               MOVE 1.00 TO PPLIM-EXCEPT-YTD
           END-IF
           IF PPLIM-EXCEPT-YTD > 250.00
               MOVE 250.00 TO PPLIM-EXCEPT-YTD
           END-IF
           ADD PPLIM-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPLIM-NEXT-REVIEW-DATE > 0 AND PPLIM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPLIM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPLIM-PAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPLIM-PAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPLIM-PAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPLIM-PAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPLIM-PAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STALE YTD.
      *----------------------------------------------------------------*
       2300-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPLIM-STALE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPLIM-STALE-YTD
           END-IF
           ADD PPLIM-STALE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY DECISION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2310-ACCUM-DECISION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-DECISION-CD
                   ADD PPLIM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-DECISION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPLIM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2320-DESCRIBE-REASON.
           EVALUATE PPLIM-REASON-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'V'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2330-COUNT-NOTICE.
           IF PPLIM-NOTICE-CNT < 9999999
               ADD 1 TO PPLIM-NOTICE-CNT
           END-IF
           IF PPLIM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO PPLIM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2340-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPLIM-VOID-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PPLIM-VOID-YTD - (WS-WORK-AMT * 2)
           COMPUTE PPLIM-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID YTD FEE ON THE ISSUED YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2350-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPLIM-VOID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPLIM-VOID-YTD ROUNDED = PPLIM-ISSUED-YTD *
                       PPLIM-RATE / 200
               WHEN PPLIM-REASON-CD = 'B'
                   MOVE 12.00 TO PPLIM-VOID-YTD
               WHEN PPLIM-REASON-CD = 'M'
                   MOVE 7.00 TO PPLIM-VOID-YTD
               WHEN OTHER
                   COMPUTE PPLIM-VOID-YTD ROUNDED = PPLIM-ISSUED-YTD *
                       PPLIM-RATE / 100
           END-EVALUATE
           IF PPLIM-VOID-YTD > 0 AND PPLIM-VOID-YTD < 10.00
               MOVE 10.00 TO PPLIM-VOID-YTD
           END-IF
           IF PPLIM-VOID-YTD > 75.00
               MOVE 75.00 TO PPLIM-VOID-YTD
           END-IF
           ADD PPLIM-VOID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2360-LIMIT-EXCEPT.
           IF PPLIM-EXCEPT-YTD < 50.00
               MOVE 50.00 TO PPLIM-EXCEPT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPLIM-EXCEPT-YTD > 5000.00
               MOVE 5000.00 TO PPLIM-EXCEPT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-REASON.
           EVALUATE PPLIM-REASON-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-VOID.
           IF PPLIM-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPLIM-VOID-YTD * PPLIM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPLIM-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2390-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPLIM-PAID-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PPLIM-PAID-AMT - (WS-WORK-AMT * 2)
           COMPUTE PPLIM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E573 NAME REQUIRED
      * E589 WORK AMT 2 NEGATIVE
      * E752 WORK AMT 2 OVER 99999
      * E454 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2400-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPLIM-NAME = SPACES
                   MOVE 'E573' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E589' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E752' TO WS-REJECT-CODE
               WHEN PPLIM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E454' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2410-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-REGION-CD
                   ADD PPLIM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPLIM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2420-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPLIM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PPLIM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2430-DESCRIBE-MATCH-RULE.
           EVALUATE PPLIM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2440-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE PPLIM-VOID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2450-FLAG-PAID.
           IF PPLIM-PAID-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPLIM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2460-COUNT-CYCLE.
           IF PPLIM-CYCLE-CNT < 9999999
               ADD 1 TO PPLIM-CYCLE-CNT
           END-IF
           IF PPLIM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PPLIM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E481 NAME REQUIRED
      * E266 ISSUED YTD NEGATIVE
      * E935 ISSUED YTD OVER 50000
      * E081 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2470-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPLIM-NAME = SPACES
                   MOVE 'E481' TO WS-REJECT-CODE
               WHEN PPLIM-ISSUED-YTD < 0
                   MOVE 'E266' TO WS-REJECT-CODE
               WHEN PPLIM-ISSUED-YTD > 50000.00
                   MOVE 'E935' TO WS-REJECT-CODE
               WHEN PPLIM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2480-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPLIM-STALE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN EXPIRY DATE IS PAST
      * M -> N WHEN EXPIRY DATE IS PAST
      * N -> U WHEN EXPIRY DATE IS PAST
      * U -> V WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-REASON.
           EVALUATE PPLIM-REASON-CD
               WHEN 'B'
                   IF PPLIM-EXPIRY-DATE > 0 AND PPLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPLIM-EXPIRY-DATE > 0 AND PPLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPLIM-EXPIRY-DATE > 0 AND PPLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPLIM-EXPIRY-DATE > 0 AND PPLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE EXCEPT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2500-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PPLIM-EXCEPT-YTD NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2510-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPLIM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PPLIM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY DECISION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2520-ACCUM-DECISION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-DECISION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-DECISION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPLIM-ISSUED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PPLIM-ISSUED-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN EXPIRY DATE IS PAST
      * B -> D WHEN EXPIRY DATE IS PAST
      * D -> H WHEN EXPIRY DATE IS PAST
      * H -> N WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2540-SET-REGION.
           EVALUATE PPLIM-REGION-CD
               WHEN 'A'
                   IF PPLIM-EXPIRY-DATE > 0 AND PPLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPLIM-EXPIRY-DATE > 0 AND PPLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PPLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PPLIM-EXPIRY-DATE > 0 AND PPLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PPLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PPLIM-EXPIRY-DATE > 0 AND PPLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E410 NAME REQUIRED
      * E183 PAID YTD NEGATIVE
      * E763 PAID YTD OVER 25000
      * E777 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2550-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPLIM-NAME = SPACES
                   MOVE 'E410' TO WS-REJECT-CODE
               WHEN PPLIM-PAID-YTD < 0
                   MOVE 'E183' TO WS-REJECT-CODE
               WHEN PPLIM-PAID-YTD > 25000.00
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN PPLIM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E777' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID YTD FEE ON THE STALE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2560-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPLIM-VOID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPLIM-VOID-YTD ROUNDED = PPLIM-STALE-YTD *
                       PPLIM-RATE / 200
               WHEN PPLIM-MATCH-RULE-CD = 'A'
                   MOVE 12.00 TO PPLIM-VOID-YTD
               WHEN PPLIM-MATCH-RULE-CD = 'B'
                   MOVE 20.00 TO PPLIM-VOID-YTD
               WHEN OTHER
                   COMPUTE PPLIM-VOID-YTD ROUNDED = PPLIM-STALE-YTD *
                       PPLIM-RATE / 100
           END-EVALUATE
           IF PPLIM-VOID-YTD > 0 AND PPLIM-VOID-YTD < 2.00
               MOVE 2.00 TO PPLIM-VOID-YTD
           END-IF
           IF PPLIM-VOID-YTD > 50.00
               MOVE 50.00 TO PPLIM-VOID-YTD
           END-IF
           ADD PPLIM-VOID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID YTD.
      *----------------------------------------------------------------*
       2570-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPLIM-VOID-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPLIM-VOID-YTD
           END-IF
           ADD PPLIM-VOID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2580-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPLIM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPLIM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PPLIM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE VOID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2590-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PPLIM-VOID-YTD NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2600-FLAG-VOID.
           IF PPLIM-VOID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPLIM-VOID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2610-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-REASON-CD
                   ADD PPLIM-EXCEPT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPLIM-EXCEPT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2620-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPLIM-OPEN-DATE > 0 AND PPLIM-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPLIM-EXCEPT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPLIM-EXCEPT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPLIM-EXCEPT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPLIM-EXCEPT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPLIM-EXCEPT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT YTD BY MATCH RULE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2630-ACCUM-MATCH-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-MATCH-RULE-CD
                   ADD PPLIM-EXCEPT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-MATCH-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPLIM-EXCEPT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2640-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPLIM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPLIM-DISCOUNT / 200
               WHEN PPLIM-MATCH-RULE-CD = 'A'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN PPLIM-MATCH-RULE-CD = 'B'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPLIM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE STALE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2650-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PPLIM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PPLIM-STALE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPLIM-STALE-YTD ROUNDED = WS-WORK-AMT-2 *
                       PPLIM-RATE / 200
               WHEN PPLIM-REGION-CD = 'A'
                   MOVE 20.00 TO PPLIM-STALE-YTD
               WHEN PPLIM-REGION-CD = 'B'
                   MOVE 15.00 TO PPLIM-STALE-YTD
               WHEN OTHER
                   COMPUTE PPLIM-STALE-YTD ROUNDED = WS-WORK-AMT-2 *
                       PPLIM-RATE / 100
           END-EVALUATE
           IF PPLIM-STALE-YTD > 0 AND PPLIM-STALE-YTD < 5.00
               MOVE 5.00 TO PPLIM-STALE-YTD
           END-IF
           IF PPLIM-STALE-YTD > 250.00
               MOVE 250.00 TO PPLIM-STALE-YTD
           END-IF
           ADD PPLIM-STALE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2660-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-REGION-CD
                   ADD PPLIM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPLIM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2670-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPLIM-CLOSE-DATE > 0 AND PPLIM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPLIM-EXCEPT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPLIM-EXCEPT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPLIM-EXCEPT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PPLIM-EXCEPT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPLIM-EXCEPT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE VOID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2680-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PPLIM-VOID-YTD NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2690-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPLIM-PAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2700-DESCRIBE-REASON.
           EVALUATE PPLIM-REASON-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'U'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'V'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2710-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-REGION-CD
                   ADD PPLIM-ISSUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPLIM-ISSUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2720-COUNT-CYCLE.
           IF PPLIM-CYCLE-CNT < 9999999
               ADD 1 TO PPLIM-CYCLE-CNT
           END-IF
           IF PPLIM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO PPLIM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2730-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-REASON-CD
                   ADD PPLIM-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPLIM-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2740-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPLIM-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPLIM-ISSUED-YTD ROUNDED = WS-WORK-AMT *
                       PPLIM-RATE / 200
               WHEN PPLIM-REASON-CD = 'B'
                   MOVE 3.00 TO PPLIM-ISSUED-YTD
               WHEN PPLIM-REASON-CD = 'M'
                   MOVE 3.00 TO PPLIM-ISSUED-YTD
               WHEN OTHER
                   COMPUTE PPLIM-ISSUED-YTD ROUNDED = WS-WORK-AMT *
                       PPLIM-RATE / 100
           END-EVALUATE
           IF PPLIM-ISSUED-YTD > 0 AND PPLIM-ISSUED-YTD < 10.00
               MOVE 10.00 TO PPLIM-ISSUED-YTD
           END-IF
           IF PPLIM-ISSUED-YTD > 100.00
               MOVE 100.00 TO PPLIM-ISSUED-YTD
           END-IF
           ADD PPLIM-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2750-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE PPLIM-VOID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E545 NAME REQUIRED
      * E913 PAID AMT NEGATIVE
      * E612 PAID AMT OVER 5000
      * E742 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2760-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPLIM-NAME = SPACES
                   MOVE 'E545' TO WS-REJECT-CODE
               WHEN PPLIM-PAID-AMT < 0
                   MOVE 'E913' TO WS-REJECT-CODE
               WHEN PPLIM-PAID-AMT > 5000.00
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN PPLIM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN CLOSE DATE IS PAST
      * M -> N WHEN CLOSE DATE IS PAST
      * N -> U WHEN CLOSE DATE IS PAST
      * U -> V WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2770-SET-REASON.
           EVALUATE PPLIM-REASON-CD
               WHEN 'B'
                   IF PPLIM-CLOSE-DATE > 0 AND PPLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPLIM-CLOSE-DATE > 0 AND PPLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPLIM-CLOSE-DATE > 0 AND PPLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPLIM-CLOSE-DATE > 0 AND PPLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2780-COUNT-CYCLE.
           IF PPLIM-CYCLE-CNT < 9999999
               ADD 1 TO PPLIM-CYCLE-CNT
           END-IF
           IF PPLIM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PPLIM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2790-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-REASON-CD
                   ADD PPLIM-ISSUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPLIM-ISSUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E857 REF NO REQUIRED
      * E837 WORK AMT NEGATIVE
      * E926 WORK AMT OVER 25000
      * E417 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2800-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPLIM-REF-NO = SPACES
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E837' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN PPLIM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2810-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPLIM-OPEN-DATE > 0 AND PPLIM-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E178 REF NO REQUIRED
      * E210 WORK AMT NEGATIVE
      * E230 WORK AMT OVER 50000
      * E284 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2820-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPLIM-REF-NO = SPACES
                   MOVE 'E178' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E210' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN PPLIM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2830-COUNT-NOTICE.
           IF PPLIM-NOTICE-CNT < 9999999
               ADD 1 TO PPLIM-NOTICE-CNT
           END-IF
           IF PPLIM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO PPLIM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN OPEN DATE IS PAST
      * M -> N WHEN OPEN DATE IS PAST
      * N -> U WHEN OPEN DATE IS PAST
      * U -> V WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2840-SET-REASON.
           EVALUATE PPLIM-REASON-CD
               WHEN 'B'
                   IF PPLIM-OPEN-DATE > 0 AND PPLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPLIM-OPEN-DATE > 0 AND PPLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPLIM-OPEN-DATE > 0 AND PPLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPLIM-OPEN-DATE > 0 AND PPLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE STALE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2850-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PPLIM-STALE-YTD NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2860-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE PPLIM-VOID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE STALE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2870-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPLIM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPLIM-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PPLIM-EXCEPT-YTD ROUNDED = PPLIM-STALE-YTD *
                       PPLIM-DISCOUNT / 200
               WHEN PPLIM-REASON-CD = 'B'
                   MOVE 12.00 TO PPLIM-EXCEPT-YTD
               WHEN PPLIM-REASON-CD = 'M'
                   MOVE 15.00 TO PPLIM-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE PPLIM-EXCEPT-YTD ROUNDED = PPLIM-STALE-YTD *
                       PPLIM-DISCOUNT / 100
           END-EVALUATE
           IF PPLIM-EXCEPT-YTD > 0 AND PPLIM-EXCEPT-YTD < 1.00
               MOVE 1.00 TO PPLIM-EXCEPT-YTD
           END-IF
           IF PPLIM-EXCEPT-YTD > 250.00
               MOVE 250.00 TO PPLIM-EXCEPT-YTD
           END-IF
           ADD PPLIM-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2880-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPLIM-NEXT-REVIEW-DATE > 0 AND PPLIM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPLIM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPLIM-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2890-ACCRUE-STALE.
           IF PPLIM-STALE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPLIM-STALE-YTD * PPLIM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE EXCEPT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2900-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PPLIM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PPLIM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPLIM-PAID-AMT ROUNDED = PPLIM-EXCEPT-YTD *
                       PPLIM-DISCOUNT / 200
               WHEN PPLIM-MATCH-RULE-CD = 'A'
                   MOVE 10.00 TO PPLIM-PAID-AMT
               WHEN PPLIM-MATCH-RULE-CD = 'B'
                   MOVE 3.00 TO PPLIM-PAID-AMT
               WHEN OTHER
                   COMPUTE PPLIM-PAID-AMT ROUNDED = PPLIM-EXCEPT-YTD *
                       PPLIM-DISCOUNT / 100
           END-EVALUATE
           IF PPLIM-PAID-AMT > 0 AND PPLIM-PAID-AMT < 1.00
               MOVE 1.00 TO PPLIM-PAID-AMT
           END-IF
           IF PPLIM-PAID-AMT > 75.00
               MOVE 75.00 TO PPLIM-PAID-AMT
           END-IF
           ADD PPLIM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE ISSUED YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2910-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPLIM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPLIM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PPLIM-PAID-AMT ROUNDED = PPLIM-ISSUED-YTD *
                       PPLIM-RATE / 200
               WHEN PPLIM-REGION-CD = 'A'
                   MOVE 3.00 TO PPLIM-PAID-AMT
               WHEN PPLIM-REGION-CD = 'B'
                   MOVE 20.00 TO PPLIM-PAID-AMT
               WHEN OTHER
                   COMPUTE PPLIM-PAID-AMT ROUNDED = PPLIM-ISSUED-YTD *
                       PPLIM-RATE / 100
           END-EVALUATE
           IF PPLIM-PAID-AMT > 0 AND PPLIM-PAID-AMT < 1.00
               MOVE 1.00 TO PPLIM-PAID-AMT
           END-IF
           IF PPLIM-PAID-AMT > 100.00
               MOVE 100.00 TO PPLIM-PAID-AMT
           END-IF
           ADD PPLIM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY MATCH RULE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2920-ACCUM-MATCH-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPLIM-MATCH-RULE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPLIM-MATCH-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE ISSUED YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2930-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPLIM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPLIM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPLIM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PPLIM-PAID-YTD ROUNDED = PPLIM-ISSUED-YTD *
                       PPLIM-RATE / 200
               WHEN PPLIM-DECISION-CD = 'AND'
                   MOVE 20.00 TO PPLIM-PAID-YTD
               WHEN PPLIM-DECISION-CD = 'BER'
                   MOVE 5.00 TO PPLIM-PAID-YTD
               WHEN OTHER
                   COMPUTE PPLIM-PAID-YTD ROUNDED = PPLIM-ISSUED-YTD *
                       PPLIM-RATE / 100
           END-EVALUATE
           IF PPLIM-PAID-YTD > 0 AND PPLIM-PAID-YTD < 10.00
               MOVE 10.00 TO PPLIM-PAID-YTD
           END-IF
           IF PPLIM-PAID-YTD > 250.00
               MOVE 250.00 TO PPLIM-PAID-YTD
           END-IF
           ADD PPLIM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2940-ACCRUE-PAID.
           IF PPLIM-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPLIM-PAID-YTD * PPLIM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPLIM-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E393 REF NO REQUIRED
      * E493 PAID AMT NEGATIVE
      * E475 PAID AMT OVER 50000
      * E824 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2950-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPLIM-REF-NO = SPACES
                   MOVE 'E393' TO WS-REJECT-CODE
               WHEN PPLIM-PAID-AMT < 0
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN PPLIM-PAID-AMT > 50000.00
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN PPLIM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E824' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2960-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PPLIM-PAID-AMT NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2970-LIMIT-STALE.
           IF PPLIM-STALE-YTD < 0.00
               MOVE 0.00 TO PPLIM-STALE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPLIM-STALE-YTD > 5000.00
               MOVE 5000.00 TO PPLIM-STALE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PPLIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PPLIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PPLIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PPLIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
