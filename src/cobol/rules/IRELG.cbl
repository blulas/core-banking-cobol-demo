      *================================================================*
      * PROGRAM  : IRELG                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : IRA CUSTODY RULE - ELIGIBILITY. CALLED BY THE IRA   *
      *            CUSTODY BATCH AND ONLINE PROGRAMS.                  *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-12 DPS  ORIGINAL                                       *
      * 2001-01-08 GAF  CONTRIB ROUNDING - HALF UP INC1789             *
      * 2003-03-08 RJT  EXCESS LIMIT REVISED REQ7808                   *
      * 2005-04-22 GAF  PERFORMANCE - EXCESS READ AHEAD CHG8925        *
      * 2006-05-26 GAF  RMD CUTOFF MOVED CHG2516                       *
      * 2006-10-13 RJT  EXCESS STATUS CODES ADDED INC3812              *
      * 2009-10-01 JLM  WITHHOLD CUTOFF MOVED PRB1717                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRELG.
       AUTHOR. D P SHAW.

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
                                           VALUE 18.000000.
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
      * WS-ROLLOVER-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ROLLOVER-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000277500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000488500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000011000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000685500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011200000000430500'.
       01  WS-ROLLOVER-TIER-TBL REDEFINES WS-ROLLOVER-TIER-VALUES.
           05  WS-ROLLOVER-TIER-ROW OCCURS 5 TIMES.
               10  WS-ROLLOVER-TIER-LOW    PIC 9(09)V99.
               10  WS-ROLLOVER-TIER-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY IRELGP.

       PROCEDURE DIVISION USING IRELG-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-NEXT-REVIEW
           PERFORM 2010-COUNT-CYCLE
           PERFORM 2020-FLAG-ITEM
           PERFORM 2030-FLAG-ROLLOVER
           PERFORM 2040-SET-REGION
           PERFORM 2050-LIMIT-ITEM
           PERFORM 2060-LIMIT-ROLLOVER
           PERFORM 2070-COUNT-RETRY
           PERFORM 2080-CHECK-NEXT-REVIEW
           PERFORM 2090-PRORATE-ROLLOVER
           PERFORM 2100-EDIT-ADDR
           PERFORM 2110-AGE-EXPIRY
           PERFORM 2120-CHECK-NEXT-REVIEW
           PERFORM 2130-PRORATE-ROLLOVER
           PERFORM 2140-ACCRUE-FMV
           PERFORM 2150-CHECK-LAST-STMT
           PERFORM 2160-COUNT-CYCLE
           PERFORM 2170-ASSESS-REVIEW
           PERFORM 2180-CHECK-EXPIRY
           PERFORM 2190-PRORATE-RMD
           PERFORM 2200-AGE-NEXT-REVIEW
           PERFORM 2210-DESCRIBE-REGION
           PERFORM 2220-CHECK-LAST-STMT
           PERFORM 2230-COUNT-RETRY
           PERFORM 2240-CHECK-LAST-ACT
           PERFORM 2250-SET-TYPE
           PERFORM 2260-SELECT-MARGIN
           PERFORM 2270-SELECT-MARGIN
           PERFORM 2280-AGE-EXPIRY
           PERFORM 2290-EDIT-ADDR
           PERFORM 2300-CHECK-NEXT-REVIEW
           PERFORM 2310-EDIT-ADDR
           PERFORM 2320-LIMIT-ROLLOVER
           PERFORM 2330-CHECK-NEXT-REVIEW
           PERFORM 2340-AGE-LAST-STMT
           PERFORM 2350-COUNT-RETRY
           PERFORM 2360-COUNT-RETRY
           PERFORM 2370-LIMIT-EXCESS
           PERFORM 2380-SELECT-FEE-PCT
           PERFORM 2390-COUNT-CYCLE
           PERFORM 2400-SET-CHANNEL
           PERFORM 2410-AGE-NEXT-REVIEW
           PERFORM 2420-FLAG-ROLLOVER
           PERFORM 2430-ASSESS-REVIEW
           PERFORM 2440-AGE-NEXT-REVIEW
           PERFORM 2450-SET-CHANNEL
           PERFORM 2460-SPLIT-RMD
           PERFORM 2470-SET-REASON
           PERFORM 2480-ACCUM-CHANNEL
           PERFORM 2490-FLAG-ITEM
           PERFORM 2500-SPLIT-FMV
           PERFORM 2510-LIMIT-ITEM
           PERFORM 2520-PRORATE-RMD
           PERFORM 2530-ACCUM-TYPE
           PERFORM 2540-LIMIT-FMV
           PERFORM 2550-SPLIT-EXCESS
           PERFORM 2560-PRORATE-RMD
           PERFORM 2570-CHECK-NEXT-REVIEW
           PERFORM 2580-EDIT-ADDR
           PERFORM 2590-SPLIT-ROLLOVER
           PERFORM 2600-CHECK-NEXT-REVIEW
           PERFORM 2610-AGE-LAST-STMT
           PERFORM 2620-ACCRUE-RMD
           PERFORM 2630-LIMIT-EXCESS
           PERFORM 2640-SPLIT-RMD
           PERFORM 2650-ACCRUE-RMD
           PERFORM 2660-PRORATE-FMV
           PERFORM 2670-FLAG-ITEM
           PERFORM 2680-AGE-EXPIRY
           PERFORM 2690-AGE-EXPIRY
           PERFORM 2700-ASSESS-AUTO-RENEW
           PERFORM 2710-SPLIT-ROLLOVER
           PERFORM 2720-ACCUM-REASON
           PERFORM 2730-FLAG-ROLLOVER
           PERFORM 2740-SET-REGION
           PERFORM 2750-ACCRUE-RMD
           PERFORM 2760-PRORATE-RMD
           PERFORM 2770-EDIT-NAME
           PERFORM 2780-EDIT-ADDR
           PERFORM 2790-ASSESS-REVIEW
           PERFORM 2800-ACCUM-REGION
           PERFORM 2810-COUNT-RETRY
           PERFORM 2820-COUNT-CYCLE
           PERFORM 2830-LIMIT-ITEM
           PERFORM 2840-SET-TYPE
           PERFORM 2850-ASSESS-AUTO-RENEW
           PERFORM 2860-SPLIT-ROLLOVER
           PERFORM 2870-SPLIT-EXCESS
           PERFORM 2880-ASSESS-STOP
           PERFORM 2890-AGE-EXPIRY
           PERFORM 2900-ACCUM-REASON
           PERFORM 2910-ACCUM-REASON
           PERFORM 2920-PRORATE-FMV
           PERFORM 2930-COUNT-CYCLE
           PERFORM 2940-SET-REASON
           PERFORM 2950-DESCRIBE-REGION
           PERFORM 2960-SPLIT-RMD
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO IRELG-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRELG-NEXT-REVIEW-DATE > 0 AND IRELG-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRELG-EXCESS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD IRELG-EXCESS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD IRELG-EXCESS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRELG-EXCESS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRELG-EXCESS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-CYCLE.
           IF IRELG-CYCLE-CNT < 9999999
               ADD 1 TO IRELG-CYCLE-CNT
           END-IF
           IF IRELG-CYCLE-CNT NOT < 12
               MOVE 'Y' TO IRELG-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2020-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2030-FLAG-ROLLOVER.
           IF IRELG-ROLLOVER-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRELG-ROLLOVER-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN EXPIRY DATE IS PAST
      * FL -> GY WHEN EXPIRY DATE IS PAST
      * GY -> HU WHEN EXPIRY DATE IS PAST
      * HU -> KA WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2040-SET-REGION.
           EVALUATE IRELG-REGION-CD
               WHEN 'CE'
                   IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRELG-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRELG-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRELG-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRELG-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2050-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2060-LIMIT-ROLLOVER.
           IF IRELG-ROLLOVER-AMT < 100.00
               MOVE 100.00 TO IRELG-ROLLOVER-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRELG-ROLLOVER-AMT > 50000.00
               MOVE 50000.00 TO IRELG-ROLLOVER-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2070-COUNT-RETRY.
           IF IRELG-RETRY-CNT < 9999999
               ADD 1 TO IRELG-RETRY-CNT
           END-IF
           IF IRELG-RETRY-CNT NOT < 5
               MOVE 'Y' TO IRELG-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2080-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRELG-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO IRELG-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2090-PRORATE-ROLLOVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRELG-ROLLOVER-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E250 ADDR REQUIRED
      * E247 EXCESS AMT NEGATIVE
      * E061 EXCESS AMT OVER 250000
      * E168 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRELG-ADDR = SPACES
                   MOVE 'E250' TO WS-REJECT-CODE
               WHEN IRELG-EXCESS-AMT < 0
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN IRELG-EXCESS-AMT > 250000.00
                   MOVE 'E061' TO WS-REJECT-CODE
               WHEN IRELG-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-EXPIRY-DATE TO DTU-DATE-1
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
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2120-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRELG-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO IRELG-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCESS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROLLOVER YTD.
      *----------------------------------------------------------------*
       2130-PRORATE-ROLLOVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRELG-ROLLOVER-YTD ROUNDED =
                   IRELG-EXCESS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRELG-ROLLOVER-YTD
           END-IF
           ADD IRELG-ROLLOVER-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FMV YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2140-ACCRUE-FMV.
           IF IRELG-FMV-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRELG-FMV-YTD * IRELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO IRELG-ROLLOVER-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2150-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRELG-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO IRELG-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-CYCLE.
           IF IRELG-CYCLE-CNT < 9999999
               ADD 1 TO IRELG-CYCLE-CNT
           END-IF
           IF IRELG-CYCLE-CNT NOT < 3
               MOVE 'Y' TO IRELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FMV FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2170-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN IRELG-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO IRELG-FMV-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRELG-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE IRELG-FMV-AMT ROUNDED = IRELG-ROLLOVER-AMT *
                       IRELG-MARGIN / 200
               WHEN IRELG-TYPE-CD = 'EA'
                   MOVE 20.00 TO IRELG-FMV-AMT
               WHEN IRELG-TYPE-CD = 'HR'
                   MOVE 10.00 TO IRELG-FMV-AMT
               WHEN OTHER
                   COMPUTE IRELG-FMV-AMT ROUNDED = IRELG-ROLLOVER-AMT *
                       IRELG-MARGIN / 100
           END-EVALUATE
           IF IRELG-FMV-AMT > 0 AND IRELG-FMV-AMT < 2.00
               MOVE 2.00 TO IRELG-FMV-AMT
           END-IF
           IF IRELG-FMV-AMT > 100.00
               MOVE 100.00 TO IRELG-FMV-AMT
           END-IF
           ADD IRELG-FMV-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRELG-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO IRELG-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RMD AMT.
      *----------------------------------------------------------------*
       2190-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRELG-RMD-AMT ROUNDED =
                   IRELG-ROLLOVER-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRELG-RMD-AMT
           END-IF
           ADD IRELG-RMD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2200-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRELG-NEXT-REVIEW-DATE > 0 AND IRELG-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2210-DESCRIBE-REGION.
           EVALUATE IRELG-REGION-CD
               WHEN 'CE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2220-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRELG-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO IRELG-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-RETRY.
           IF IRELG-RETRY-CNT < 9999999
               ADD 1 TO IRELG-RETRY-CNT
           END-IF
           IF IRELG-RETRY-CNT NOT < 12
               MOVE 'Y' TO IRELG-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2240-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRELG-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO IRELG-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN NEXT REVIEW DATE IS PAST
      * HR -> XZ WHEN NEXT REVIEW DATE IS PAST
      * XZ -> EA WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2250-SET-TYPE.
           EVALUATE IRELG-TYPE-CD
               WHEN 'EA'
                   IF IRELG-NEXT-REVIEW-DATE > 0 AND
                       IRELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRELG-NEXT-REVIEW-DATE > 0 AND
                       IRELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRELG-NEXT-REVIEW-DATE > 0 AND
                       IRELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2260-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-TIER-RATE(1) TO IRELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-ROLLOVER-TIER-LOW(WS-TX)
                   MOVE WS-ROLLOVER-TIER-RATE(WS-TX) TO IRELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER TIER TIER FOR THE EXCESS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2270-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-TIER-RATE(1) TO IRELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF IRELG-EXCESS-AMT NOT < WS-ROLLOVER-TIER-LOW(WS-TX)
                   MOVE WS-ROLLOVER-TIER-RATE(WS-TX) TO IRELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2280-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRELG-RMD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRELG-RMD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRELG-RMD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD IRELG-RMD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRELG-RMD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E229 ADDR REQUIRED
      * E870 FMV YTD NEGATIVE
      * E237 FMV YTD OVER 10000
      * E446 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2290-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRELG-ADDR = SPACES
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN IRELG-FMV-YTD < 0
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN IRELG-FMV-YTD > 10000.00
                   MOVE 'E237' TO WS-REJECT-CODE
               WHEN IRELG-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRELG-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO IRELG-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E592 ADDR REQUIRED
      * E686 WORK AMT 2 NEGATIVE
      * E759 WORK AMT 2 OVER 1000000
      * E262 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2310-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRELG-ADDR = SPACES
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E759' TO WS-REJECT-CODE
               WHEN IRELG-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E262' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2320-LIMIT-ROLLOVER.
           IF IRELG-ROLLOVER-YTD < 50.00
               MOVE 50.00 TO IRELG-ROLLOVER-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRELG-ROLLOVER-YTD > 1000.00
               MOVE 1000.00 TO IRELG-ROLLOVER-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2330-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRELG-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO IRELG-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2340-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRELG-LAST-STMT-DATE > 0 AND IRELG-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRELG-ROLLOVER-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRELG-ROLLOVER-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD IRELG-ROLLOVER-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRELG-ROLLOVER-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRELG-ROLLOVER-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2350-COUNT-RETRY.
           IF IRELG-RETRY-CNT < 9999999
               ADD 1 TO IRELG-RETRY-CNT
           END-IF
           IF IRELG-RETRY-CNT NOT < 10
               MOVE 'Y' TO IRELG-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-RETRY.
           IF IRELG-RETRY-CNT < 9999999
               ADD 1 TO IRELG-RETRY-CNT
           END-IF
           IF IRELG-RETRY-CNT NOT < 10
               MOVE 'Y' TO IRELG-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCESS AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2370-LIMIT-EXCESS.
           IF IRELG-EXCESS-AMT < 100.00
               MOVE 100.00 TO IRELG-EXCESS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRELG-EXCESS-AMT > 2500.00
               MOVE 2500.00 TO IRELG-EXCESS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER TIER TIER FOR THE EXCESS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2380-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-TIER-RATE(1) TO IRELG-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF IRELG-EXCESS-AMT NOT < WS-ROLLOVER-TIER-LOW(WS-TX)
                   MOVE WS-ROLLOVER-TIER-RATE(WS-TX) TO IRELG-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRELG-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRELG-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2390-COUNT-CYCLE.
           IF IRELG-CYCLE-CNT < 9999999
               ADD 1 TO IRELG-CYCLE-CNT
           END-IF
           IF IRELG-CYCLE-CNT NOT < 10
               MOVE 'Y' TO IRELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AS -> AY WHEN LAST STMT DATE IS PAST
      * AY -> JR WHEN LAST STMT DATE IS PAST
      * JR -> TA WHEN LAST STMT DATE IS PAST
      * TA -> UM WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2400-SET-CHANNEL.
           EVALUATE IRELG-CHANNEL-CD
               WHEN 'AS'
                   IF IRELG-LAST-STMT-DATE > 0 AND IRELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AY' TO IRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AY'
                   IF IRELG-LAST-STMT-DATE > 0 AND IRELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JR' TO IRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JR'
                   IF IRELG-LAST-STMT-DATE > 0 AND IRELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TA' TO IRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF IRELG-LAST-STMT-DATE > 0 AND IRELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UM' TO IRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2410-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRELG-NEXT-REVIEW-DATE > 0 AND IRELG-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRELG-FMV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD IRELG-FMV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD IRELG-FMV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRELG-FMV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRELG-FMV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ROLLOVER YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2420-FLAG-ROLLOVER.
           IF IRELG-ROLLOVER-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRELG-ROLLOVER-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER YTD FEE ON THE EXCESS AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2430-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN IRELG-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO IRELG-ROLLOVER-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRELG-STOP-FLAG = 'Y'
                   COMPUTE IRELG-ROLLOVER-YTD ROUNDED = IRELG-EXCESS-AMT
                       * IRELG-FEE-PCT / 200
               WHEN IRELG-REASON-CD = 'CMR'
                   MOVE 5.00 TO IRELG-ROLLOVER-YTD
               WHEN IRELG-REASON-CD = 'LJS'
                   MOVE 12.00 TO IRELG-ROLLOVER-YTD
               WHEN OTHER
                   COMPUTE IRELG-ROLLOVER-YTD ROUNDED = IRELG-EXCESS-AMT
                       * IRELG-FEE-PCT / 100
           END-EVALUATE
           IF IRELG-ROLLOVER-YTD > 0 AND IRELG-ROLLOVER-YTD < 2.00
               MOVE 2.00 TO IRELG-ROLLOVER-YTD
           END-IF
           IF IRELG-ROLLOVER-YTD > 250.00
               MOVE 250.00 TO IRELG-ROLLOVER-YTD
           END-IF
           ADD IRELG-ROLLOVER-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2440-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRELG-NEXT-REVIEW-DATE > 0 AND IRELG-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRELG-ROLLOVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD IRELG-ROLLOVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD IRELG-ROLLOVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRELG-ROLLOVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRELG-ROLLOVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AS -> AY WHEN LAST STMT DATE IS PAST
      * AY -> JR WHEN LAST STMT DATE IS PAST
      * JR -> TA WHEN LAST STMT DATE IS PAST
      * TA -> UM WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2450-SET-CHANNEL.
           EVALUATE IRELG-CHANNEL-CD
               WHEN 'AS'
                   IF IRELG-LAST-STMT-DATE > 0 AND IRELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AY' TO IRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AY'
                   IF IRELG-LAST-STMT-DATE > 0 AND IRELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JR' TO IRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JR'
                   IF IRELG-LAST-STMT-DATE > 0 AND IRELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TA' TO IRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF IRELG-LAST-STMT-DATE > 0 AND IRELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UM' TO IRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RMD AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2460-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = IRELG-RMD-AMT / 2
           COMPUTE WS-WORK-AMT-2 = IRELG-RMD-AMT - (WS-WORK-AMT * 2)
           COMPUTE IRELG-EXCESS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * CMR -> LJS WHEN EXPIRY DATE IS PAST
      * LJS -> NRT WHEN EXPIRY DATE IS PAST
      * NRT -> UGA WHEN EXPIRY DATE IS PAST
      * UGA -> WBS WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-REASON.
           EVALUATE IRELG-REASON-CD
               WHEN 'CMR'
                   IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LJS' TO IRELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LJS'
                   IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NRT' TO IRELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NRT'
                   IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGA' TO IRELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGA'
                   IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WBS' TO IRELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2480-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRELG-CHANNEL-CD
                   ADD IRELG-ROLLOVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRELG-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRELG-ROLLOVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2490-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2500-SPLIT-FMV.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE IRELG-FMV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2510-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD AMT.
      *----------------------------------------------------------------*
       2520-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRELG-RMD-AMT ROUNDED =
                   IRELG-ROLLOVER-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRELG-RMD-AMT
           END-IF
           ADD IRELG-RMD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2530-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRELG-TYPE-CD
                   ADD IRELG-EXCESS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRELG-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRELG-EXCESS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FMV AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2540-LIMIT-FMV.
           IF IRELG-FMV-AMT < 0.00
               MOVE 0.00 TO IRELG-FMV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRELG-FMV-AMT > 500.00
               MOVE 500.00 TO IRELG-FMV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCESS AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRELG-EXCESS-AMT / 4
           COMPUTE WS-WORK-AMT-2 = IRELG-EXCESS-AMT - (WS-WORK-AMT * 4)
           COMPUTE IRELG-RMD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RMD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2560-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   IRELG-RMD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRELG-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO IRELG-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E459 ADDR REQUIRED
      * E034 WORK AMT NEGATIVE
      * E768 WORK AMT OVER 99999
      * E151 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2580-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRELG-ADDR = SPACES
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E034' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E768' TO WS-REJECT-CODE
               WHEN IRELG-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2590-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRELG-ROLLOVER-YTD / 2
           COMPUTE WS-WORK-AMT-2 = IRELG-ROLLOVER-YTD - (WS-WORK-AMT *
               2)
           COMPUTE IRELG-FMV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2600-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRELG-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO IRELG-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2610-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRELG-LAST-STMT-DATE > 0 AND IRELG-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRELG-ROLLOVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRELG-ROLLOVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRELG-ROLLOVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRELG-ROLLOVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRELG-ROLLOVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2620-ACCRUE-RMD.
           IF IRELG-RMD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRELG-RMD-AMT * IRELG-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCESS AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2630-LIMIT-EXCESS.
           IF IRELG-EXCESS-AMT < 10.00
               MOVE 10.00 TO IRELG-EXCESS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRELG-EXCESS-AMT > 2500.00
               MOVE 2500.00 TO IRELG-EXCESS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RMD AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2640-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = IRELG-RMD-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRELG-RMD-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRELG-ROLLOVER-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2650-ACCRUE-RMD.
           IF IRELG-RMD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRELG-RMD-AMT * IRELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO IRELG-EXCESS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FMV AMT.
      *----------------------------------------------------------------*
       2660-PRORATE-FMV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRELG-FMV-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRELG-FMV-AMT
           END-IF
           ADD IRELG-FMV-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2670-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2680-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-EXPIRY-DATE TO DTU-DATE-1
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
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2690-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRELG-ROLLOVER-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRELG-ROLLOVER-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD IRELG-ROLLOVER-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRELG-ROLLOVER-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRELG-ROLLOVER-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE EXCESS FEE ON THE ROLLOVER YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2700-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN IRELG-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO IRELG-EXCESS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRELG-REVIEW-FLAG = 'Y'
                   COMPUTE IRELG-EXCESS-AMT ROUNDED = IRELG-ROLLOVER-YTD
                       * IRELG-FEE-PCT / 200
               WHEN IRELG-REASON-CD = 'CMR'
                   MOVE 10.00 TO IRELG-EXCESS-AMT
               WHEN IRELG-REASON-CD = 'LJS'
                   MOVE 15.00 TO IRELG-EXCESS-AMT
               WHEN OTHER
                   COMPUTE IRELG-EXCESS-AMT ROUNDED = IRELG-ROLLOVER-YTD
                       * IRELG-FEE-PCT / 100
           END-EVALUATE
           IF IRELG-EXCESS-AMT > 0 AND IRELG-EXCESS-AMT < 2.00
               MOVE 2.00 TO IRELG-EXCESS-AMT
           END-IF
           IF IRELG-EXCESS-AMT > 150.00
               MOVE 150.00 TO IRELG-EXCESS-AMT
           END-IF
           ADD IRELG-EXCESS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2710-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRELG-ROLLOVER-YTD / 2
           COMPUTE WS-WORK-AMT-2 = IRELG-ROLLOVER-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2720-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRELG-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRELG-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2730-FLAG-ROLLOVER.
           IF IRELG-ROLLOVER-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRELG-ROLLOVER-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN LAST ACT DATE IS PAST
      * FL -> GY WHEN LAST ACT DATE IS PAST
      * GY -> HU WHEN LAST ACT DATE IS PAST
      * HU -> KA WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2740-SET-REGION.
           EVALUATE IRELG-REGION-CD
               WHEN 'CE'
                   IF IRELG-LAST-ACT-DATE > 0 AND IRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRELG-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRELG-LAST-ACT-DATE > 0 AND IRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRELG-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRELG-LAST-ACT-DATE > 0 AND IRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRELG-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRELG-LAST-ACT-DATE > 0 AND IRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRELG-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2750-ACCRUE-RMD.
           IF IRELG-RMD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRELG-RMD-AMT * IRELG-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO IRELG-EXCESS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FMV YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RMD AMT.
      *----------------------------------------------------------------*
       2760-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRELG-RMD-AMT ROUNDED =
                   IRELG-FMV-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRELG-RMD-AMT
           END-IF
           ADD IRELG-RMD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E083 NAME REQUIRED
      * E921 FMV AMT NEGATIVE
      * E874 FMV AMT OVER 10000
      * E597 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2770-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRELG-NAME = SPACES
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN IRELG-FMV-AMT < 0
                   MOVE 'E921' TO WS-REJECT-CODE
               WHEN IRELG-FMV-AMT > 10000.00
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN IRELG-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E597' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E828 ADDR REQUIRED
      * E450 WORK AMT NEGATIVE
      * E768 WORK AMT OVER 99999
      * E874 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2780-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRELG-ADDR = SPACES
                   MOVE 'E828' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E768' TO WS-REJECT-CODE
               WHEN IRELG-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER FEE ON THE WORK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2790-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN IRELG-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO IRELG-ROLLOVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRELG-STOP-FLAG = 'Y'
                   COMPUTE IRELG-ROLLOVER-AMT ROUNDED = WS-WORK-AMT *
                       IRELG-FEE-PCT / 200
               WHEN IRELG-REGION-CD = 'CE'
                   MOVE 15.00 TO IRELG-ROLLOVER-AMT
               WHEN IRELG-REGION-CD = 'FL'
                   MOVE 12.00 TO IRELG-ROLLOVER-AMT
               WHEN OTHER
                   COMPUTE IRELG-ROLLOVER-AMT ROUNDED = WS-WORK-AMT *
                       IRELG-FEE-PCT / 100
           END-EVALUATE
           IF IRELG-ROLLOVER-AMT > 0 AND IRELG-ROLLOVER-AMT < 5.00
               MOVE 5.00 TO IRELG-ROLLOVER-AMT
           END-IF
           IF IRELG-ROLLOVER-AMT > 35.00
               MOVE 35.00 TO IRELG-ROLLOVER-AMT
           END-IF
           ADD IRELG-ROLLOVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FMV AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2800-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRELG-REGION-CD
                   ADD IRELG-FMV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRELG-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRELG-FMV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2810-COUNT-RETRY.
           IF IRELG-RETRY-CNT < 9999999
               ADD 1 TO IRELG-RETRY-CNT
           END-IF
           IF IRELG-RETRY-CNT NOT < 6
               MOVE 'Y' TO IRELG-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2820-COUNT-CYCLE.
           IF IRELG-CYCLE-CNT < 9999999
               ADD 1 TO IRELG-CYCLE-CNT
           END-IF
           IF IRELG-CYCLE-CNT NOT < 10
               MOVE 'Y' TO IRELG-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2830-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN NEXT REVIEW DATE IS PAST
      * HR -> XZ WHEN NEXT REVIEW DATE IS PAST
      * XZ -> EA WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2840-SET-TYPE.
           EVALUATE IRELG-TYPE-CD
               WHEN 'EA'
                   IF IRELG-NEXT-REVIEW-DATE > 0 AND
                       IRELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRELG-NEXT-REVIEW-DATE > 0 AND
                       IRELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRELG-NEXT-REVIEW-DATE > 0 AND
                       IRELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER YTD FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2850-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN IRELG-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO IRELG-ROLLOVER-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRELG-REVIEW-FLAG = 'Y'
                   COMPUTE IRELG-ROLLOVER-YTD ROUNDED =
                       IRELG-ROLLOVER-AMT * IRELG-MARGIN / 200
               WHEN IRELG-REGION-CD = 'CE'
                   MOVE 12.00 TO IRELG-ROLLOVER-YTD
               WHEN IRELG-REGION-CD = 'FL'
                   MOVE 10.00 TO IRELG-ROLLOVER-YTD
               WHEN OTHER
                   COMPUTE IRELG-ROLLOVER-YTD ROUNDED =
                       IRELG-ROLLOVER-AMT * IRELG-MARGIN / 100
           END-EVALUATE
           IF IRELG-ROLLOVER-YTD > 0 AND IRELG-ROLLOVER-YTD < 5.00
               MOVE 5.00 TO IRELG-ROLLOVER-YTD
           END-IF
           IF IRELG-ROLLOVER-YTD > 50.00
               MOVE 50.00 TO IRELG-ROLLOVER-YTD
           END-IF
           ADD IRELG-ROLLOVER-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2860-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRELG-ROLLOVER-YTD / 12
           COMPUTE WS-WORK-AMT-2 = IRELG-ROLLOVER-YTD - (WS-WORK-AMT *
               12)
           COMPUTE IRELG-FMV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCESS AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2870-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRELG-EXCESS-AMT / 4
           COMPUTE WS-WORK-AMT-2 = IRELG-EXCESS-AMT - (WS-WORK-AMT * 4)
           COMPUTE IRELG-FMV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE EXCESS AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2880-ASSESS-STOP.
           EVALUATE TRUE
               WHEN IRELG-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRELG-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = IRELG-EXCESS-AMT *
                       IRELG-FEE-PCT / 200
               WHEN IRELG-REGION-CD = 'CE'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN IRELG-REGION-CD = 'FL'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = IRELG-EXCESS-AMT *
                       IRELG-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2890-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRELG-EXPIRY-DATE > 0 AND IRELG-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RMD AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2900-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRELG-REASON-CD
                   ADD IRELG-RMD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRELG-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRELG-RMD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2910-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRELG-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRELG-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RMD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FMV YTD.
      *----------------------------------------------------------------*
       2920-PRORATE-FMV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRELG-FMV-YTD ROUNDED =
                   IRELG-RMD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRELG-FMV-YTD
           END-IF
           ADD IRELG-FMV-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2930-COUNT-CYCLE.
           IF IRELG-CYCLE-CNT < 9999999
               ADD 1 TO IRELG-CYCLE-CNT
           END-IF
           IF IRELG-CYCLE-CNT NOT < 6
               MOVE 'Y' TO IRELG-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * CMR -> LJS WHEN NEXT REVIEW DATE IS PAST
      * LJS -> NRT WHEN NEXT REVIEW DATE IS PAST
      * NRT -> UGA WHEN NEXT REVIEW DATE IS PAST
      * UGA -> WBS WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2940-SET-REASON.
           EVALUATE IRELG-REASON-CD
               WHEN 'CMR'
                   IF IRELG-NEXT-REVIEW-DATE > 0 AND
                       IRELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LJS' TO IRELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LJS'
                   IF IRELG-NEXT-REVIEW-DATE > 0 AND
                       IRELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NRT' TO IRELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NRT'
                   IF IRELG-NEXT-REVIEW-DATE > 0 AND
                       IRELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UGA' TO IRELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGA'
                   IF IRELG-NEXT-REVIEW-DATE > 0 AND
                       IRELG-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WBS' TO IRELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2950-DESCRIBE-REGION.
           EVALUATE IRELG-REGION-CD
               WHEN 'CE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RMD AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2960-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = IRELG-RMD-AMT / 12
           COMPUTE WS-WORK-AMT-2 = IRELG-RMD-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO IRELG-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO IRELG-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO IRELG-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'IRELG EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
