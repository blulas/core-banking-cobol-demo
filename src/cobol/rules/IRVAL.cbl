      *================================================================*
      * PROGRAM  : IRVAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : IRA CUSTODY RULE - PRICING. CALLED BY THE IRA       *
      *            CUSTODY BATCH AND ONLINE PROGRAMS.                  *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-05-15 DPS  ORIGINAL                                       *
      * 1997-05-07 JLM  ROLLOVER CUTOFF MOVED REQ8955                  *
      * 1997-10-04 JLM  DISTRIB LIMIT REVISED CHG2883                  *
      * 2006-10-03 DPS  WITHHOLD CUTOFF MOVED INC8825                  *
      * 2009-04-10 TWH  NEW DISTRIB REPORT COLUMNS PRB2106             *
      * 2011-04-21 EWS  WITHHOLD FEE SCHEDULE UPDATE CHG9460           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRVAL.
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
                                           VALUE 24.000000.
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
      * WS-RMD-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RMD-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000817500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000345500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000710500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000178000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000791000'.
       01  WS-RMD-GRID-TBL REDEFINES WS-RMD-GRID-VALUES.
           05  WS-RMD-GRID-ROW OCCURS 5 TIMES.
               10  WS-RMD-GRID-LOW         PIC 9(09)V99.
               10  WS-RMD-GRID-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONTRIB-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTRIB-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000513500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000896500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000045500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000638000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008550000000759000'.
       01  WS-CONTRIB-SCHED-TBL REDEFINES WS-CONTRIB-SCHED-VALUES.
           05  WS-CONTRIB-SCHED-ROW OCCURS 5 TIMES.
               10  WS-CONTRIB-SCHED-LOW    PIC 9(09)V99.
               10  WS-CONTRIB-SCHED-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY IRVALP.

       PROCEDURE DIVISION USING IRVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CHECK-MATURITY
           PERFORM 2010-ACCUM-REGION
           PERFORM 2020-COUNT-ITEM
           PERFORM 2030-SET-DIST-CODE
           PERFORM 2040-FLAG-WITHHOLD
           PERFORM 2050-DESCRIBE-DIST-CODE
           PERFORM 2060-PRORATE-EXCESS
           PERFORM 2070-ACCUM-TYPE
           PERFORM 2080-SET-REGION
           PERFORM 2090-ACCUM-CHANNEL
           PERFORM 2100-CHECK-MATURITY
           PERFORM 2110-ASSESS-HOLD
           PERFORM 2120-ACCUM-REGION
           PERFORM 2130-LIMIT-ITEM
           PERFORM 2140-COUNT-ITEM
           PERFORM 2150-ASSESS-OVERRIDE
           PERFORM 2160-COUNT-CYCLE
           PERFORM 2170-FLAG-FMV
           PERFORM 2180-FLAG-ITEM
           PERFORM 2190-AGE-MATURITY
           PERFORM 2200-SET-CHANNEL
           PERFORM 2210-ASSESS-STOP
           PERFORM 2220-SELECT-MARGIN
           PERFORM 2230-DESCRIBE-REGION
           PERFORM 2240-COUNT-CYCLE
           PERFORM 2250-SPLIT-ITEM
           PERFORM 2260-DESCRIBE-CHANNEL
           PERFORM 2270-FLAG-EXCESS
           PERFORM 2280-SET-CHANNEL
           PERFORM 2290-ACCUM-CHANNEL
           PERFORM 2300-ACCUM-CHANNEL
           PERFORM 2310-PRORATE-FMV
           PERFORM 2320-EDIT-CONTACT
           PERFORM 2330-PRORATE-FMV
           PERFORM 2340-AGE-EFFECTIVE
           PERFORM 2350-SELECT-RATE
           PERFORM 2360-SET-DIST-CODE
           PERFORM 2370-DESCRIBE-DIST-CODE
           PERFORM 2380-ACCUM-CHANNEL
           PERFORM 2390-SELECT-RATE
           PERFORM 2400-DESCRIBE-CHANNEL
           PERFORM 2410-ACCRUE-FMV
           PERFORM 2420-EDIT-CONTACT
           PERFORM 2430-PRORATE-FMV
           PERFORM 2440-PRORATE-EXCESS
           PERFORM 2450-SPLIT-ROLLOVER
           PERFORM 2460-FLAG-FMV
           PERFORM 2470-CHECK-EFFECTIVE
           PERFORM 2480-COUNT-CYCLE
           PERFORM 2490-PRORATE-FMV
           PERFORM 2500-PRORATE-WITHHOLD
           PERFORM 2510-SET-DIST-CODE
           PERFORM 2520-ACCUM-DIST-CODE
           PERFORM 2530-SET-REGION
           PERFORM 2540-LIMIT-FMV
           PERFORM 2550-SET-DIST-CODE
           PERFORM 2560-PRORATE-EXCESS
           PERFORM 2570-DESCRIBE-CHANNEL
           PERFORM 2580-ACCRUE-RATE
           PERFORM 2590-SELECT-RATE
           PERFORM 2600-ACCRUE-FMV
           PERFORM 2610-SELECT-MARGIN
           PERFORM 2620-SPLIT-RMD
           PERFORM 2630-EDIT-CONTACT
           PERFORM 2640-EDIT-ADDR
           PERFORM 2650-FLAG-ITEM
           PERFORM 2660-CHECK-MATURITY
           PERFORM 2670-SET-REGION
           PERFORM 2680-PRORATE-WITHHOLD
           PERFORM 2690-CHECK-MATURITY
           PERFORM 2700-SPLIT-ROLLOVER
           PERFORM 2710-DESCRIBE-TYPE
           PERFORM 2720-EDIT-ADDR
           PERFORM 2730-LIMIT-WITHHOLD
           PERFORM 2740-COUNT-ITEM
           PERFORM 2750-ASSESS-OVERRIDE
           PERFORM 2760-SET-REGION
           PERFORM 2770-ACCUM-DIST-CODE
           PERFORM 2780-EDIT-CONTACT
           PERFORM 2790-LIMIT-ITEM
           PERFORM 2800-SELECT-RATE
           PERFORM 2810-ASSESS-OVERRIDE
           PERFORM 2820-ASSESS-OVERRIDE
           PERFORM 2830-EDIT-ADDR
           PERFORM 2840-DESCRIBE-TYPE
           PERFORM 2850-COUNT-CYCLE
           PERFORM 2860-COUNT-ITEM
           PERFORM 2870-CHECK-MATURITY
           PERFORM 2880-SPLIT-FMV
           PERFORM 2890-ACCUM-REGION
           PERFORM 2900-CHECK-MATURITY
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO IRVAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2000-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRVAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO IRVAL-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RMD YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRVAL-REGION-CD
                   ADD IRVAL-RMD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRVAL-RMD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2020-COUNT-ITEM.
           IF IRVAL-ITEM-CNT < 9999999
               ADD 1 TO IRVAL-ITEM-CNT
           END-IF
           IF IRVAL-ITEM-CNT NOT < 12
               MOVE 'Y' TO IRVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST CODE CD:
      * C -> J WHEN NEXT DUE DATE IS PAST
      * J -> U WHEN NEXT DUE DATE IS PAST
      * U -> X WHEN NEXT DUE DATE IS PAST
      * X -> C WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2030-SET-DIST-CODE.
           EVALUATE IRVAL-DIST-CODE-CD
               WHEN 'C'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WITHHOLD YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2040-FLAG-WITHHOLD.
           IF IRVAL-WITHHOLD-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRVAL-WITHHOLD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2050-DESCRIBE-DIST-CODE.
           EVALUATE IRVAL-DIST-CODE-CD
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'J'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCESS AMT.
      *----------------------------------------------------------------*
       2060-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRVAL-EXCESS-AMT ROUNDED =
                   IRVAL-RMD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRVAL-EXCESS-AMT
           END-IF
           ADD IRVAL-EXCESS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FMV YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2070-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRVAL-TYPE-CD
                   ADD IRVAL-FMV-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRVAL-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRVAL-FMV-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN LAST STMT DATE IS PAST
      * FL -> GY WHEN LAST STMT DATE IS PAST
      * GY -> HU WHEN LAST STMT DATE IS PAST
      * HU -> KA WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2080-SET-REGION.
           EVALUATE IRVAL-REGION-CD
               WHEN 'CE'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RMD YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2090-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRVAL-CHANNEL-CD
                   ADD IRVAL-RMD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRVAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRVAL-RMD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2100-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRVAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO IRVAL-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RMD YTD FEE ON THE WORK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2110-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRVAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRVAL-RMD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRVAL-STOP-FLAG = 'Y'
                   COMPUTE IRVAL-RMD-YTD ROUNDED = WS-WORK-AMT *
                       IRVAL-MARGIN / 200
               WHEN IRVAL-REGION-CD = 'CE'
                   MOVE 10.00 TO IRVAL-RMD-YTD
               WHEN IRVAL-REGION-CD = 'FL'
                   MOVE 15.00 TO IRVAL-RMD-YTD
               WHEN OTHER
                   COMPUTE IRVAL-RMD-YTD ROUNDED = WS-WORK-AMT *
                       IRVAL-MARGIN / 100
           END-EVALUATE
           IF IRVAL-RMD-YTD > 0 AND IRVAL-RMD-YTD < 1.00
               MOVE 1.00 TO IRVAL-RMD-YTD
           END-IF
           IF IRVAL-RMD-YTD > 250.00
               MOVE 250.00 TO IRVAL-RMD-YTD
           END-IF
           ADD IRVAL-RMD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2120-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRVAL-REGION-CD
                   ADD IRVAL-WITHHOLD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRVAL-WITHHOLD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-ITEM.
           IF IRVAL-ITEM-CNT < 9999999
               ADD 1 TO IRVAL-ITEM-CNT
           END-IF
           IF IRVAL-ITEM-CNT NOT < 5
               MOVE 'Y' TO IRVAL-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RMD YTD FEE ON THE EXCESS AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2150-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN IRVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO IRVAL-RMD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRVAL-HOLD-FLAG = 'Y'
                   COMPUTE IRVAL-RMD-YTD ROUNDED = IRVAL-EXCESS-AMT *
                       IRVAL-RATE / 200
               WHEN IRVAL-DIST-CODE-CD = 'C'
                   MOVE 3.00 TO IRVAL-RMD-YTD
               WHEN IRVAL-DIST-CODE-CD = 'J'
                   MOVE 10.00 TO IRVAL-RMD-YTD
               WHEN OTHER
                   COMPUTE IRVAL-RMD-YTD ROUNDED = IRVAL-EXCESS-AMT *
                       IRVAL-RATE / 100
           END-EVALUATE
           IF IRVAL-RMD-YTD > 0 AND IRVAL-RMD-YTD < 2.00
               MOVE 2.00 TO IRVAL-RMD-YTD
           END-IF
           IF IRVAL-RMD-YTD > 250.00
               MOVE 250.00 TO IRVAL-RMD-YTD
           END-IF
           ADD IRVAL-RMD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-CYCLE.
           IF IRVAL-CYCLE-CNT < 9999999
               ADD 1 TO IRVAL-CYCLE-CNT
           END-IF
           IF IRVAL-CYCLE-CNT NOT < 6
               MOVE 'Y' TO IRVAL-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FMV YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2170-FLAG-FMV.
           IF IRVAL-FMV-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRVAL-FMV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2180-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2190-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRVAL-MATURITY-DATE > 0 AND IRVAL-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRVAL-EXCESS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRVAL-EXCESS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD IRVAL-EXCESS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD IRVAL-EXCESS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRVAL-EXCESS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AS -> AY WHEN LAST STMT DATE IS PAST
      * AY -> JR WHEN LAST STMT DATE IS PAST
      * JR -> TA WHEN LAST STMT DATE IS PAST
      * TA -> UM WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2200-SET-CHANNEL.
           EVALUATE IRVAL-CHANNEL-CD
               WHEN 'AS'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AY' TO IRVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AY'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JR' TO IRVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JR'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TA' TO IRVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UM' TO IRVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RMD YTD FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2210-ASSESS-STOP.
           EVALUATE TRUE
               WHEN IRVAL-STOP-FLAG = 'Y'
                   MOVE ZERO TO IRVAL-RMD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRVAL-HOLD-FLAG = 'Y'
                   COMPUTE IRVAL-RMD-YTD ROUNDED = WS-WORK-AMT *
                       IRVAL-RATE / 200
               WHEN IRVAL-REGION-CD = 'CE'
                   MOVE 15.00 TO IRVAL-RMD-YTD
               WHEN IRVAL-REGION-CD = 'FL'
                   MOVE 20.00 TO IRVAL-RMD-YTD
               WHEN OTHER
                   COMPUTE IRVAL-RMD-YTD ROUNDED = WS-WORK-AMT *
                       IRVAL-RATE / 100
           END-EVALUATE
           IF IRVAL-RMD-YTD > 0 AND IRVAL-RMD-YTD < 5.00
               MOVE 5.00 TO IRVAL-RMD-YTD
           END-IF
           IF IRVAL-RMD-YTD > 150.00
               MOVE 150.00 TO IRVAL-RMD-YTD
           END-IF
           ADD IRVAL-RMD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB SCHED TIER FOR THE FMV AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-SCHED-RATE(1) TO IRVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF IRVAL-FMV-AMT NOT < WS-CONTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-CONTRIB-SCHED-RATE(WS-TX) TO IRVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2230-DESCRIBE-REGION.
           EVALUATE IRVAL-REGION-CD
               WHEN 'CE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-CYCLE.
           IF IRVAL-CYCLE-CNT < 9999999
               ADD 1 TO IRVAL-CYCLE-CNT
           END-IF
           IF IRVAL-CYCLE-CNT NOT < 10
               MOVE 'Y' TO IRVAL-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2250-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2260-DESCRIBE-CHANNEL.
           EVALUATE IRVAL-CHANNEL-CD
               WHEN 'AS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'AY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCESS AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2270-FLAG-EXCESS.
           IF IRVAL-EXCESS-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRVAL-EXCESS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AS -> AY WHEN MATURITY DATE IS PAST
      * AY -> JR WHEN MATURITY DATE IS PAST
      * JR -> TA WHEN MATURITY DATE IS PAST
      * TA -> UM WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2280-SET-CHANNEL.
           EVALUATE IRVAL-CHANNEL-CD
               WHEN 'AS'
                   IF IRVAL-MATURITY-DATE > 0 AND IRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AY' TO IRVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AY'
                   IF IRVAL-MATURITY-DATE > 0 AND IRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JR' TO IRVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JR'
                   IF IRVAL-MATURITY-DATE > 0 AND IRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO IRVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF IRVAL-MATURITY-DATE > 0 AND IRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO IRVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FMV YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRVAL-CHANNEL-CD
                   ADD IRVAL-FMV-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRVAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRVAL-FMV-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2300-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRVAL-CHANNEL-CD
                   ADD IRVAL-ROLLOVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRVAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRVAL-ROLLOVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FMV YTD.
      *----------------------------------------------------------------*
       2310-PRORATE-FMV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRVAL-FMV-YTD ROUNDED =
                   IRVAL-WITHHOLD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRVAL-FMV-YTD
           END-IF
           ADD IRVAL-FMV-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E265 CONTACT REQUIRED
      * E063 WORK AMT 2 NEGATIVE
      * E409 WORK AMT 2 OVER 5000
      * E447 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2320-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRVAL-CONTACT = SPACES
                   MOVE 'E265' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E063' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN IRVAL-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FMV AMT.
      *----------------------------------------------------------------*
       2330-PRORATE-FMV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRVAL-FMV-AMT ROUNDED =
                   IRVAL-RMD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRVAL-FMV-AMT
           END-IF
           ADD IRVAL-FMV-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2340-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF IRVAL-EFFECTIVE-DATE > 0 AND IRVAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRVAL-EFFECTIVE-DATE TO DTU-DATE-1
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
      * SELECT THE CONTRIB SCHED TIER FOR THE ROLLOVER AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2350-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-SCHED-RATE(1) TO IRVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF IRVAL-ROLLOVER-AMT NOT < WS-CONTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-CONTRIB-SCHED-RATE(WS-TX) TO IRVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST CODE CD:
      * C -> J WHEN LAST STMT DATE IS PAST
      * J -> U WHEN LAST STMT DATE IS PAST
      * U -> X WHEN LAST STMT DATE IS PAST
      * X -> C WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2360-SET-DIST-CODE.
           EVALUATE IRVAL-DIST-CODE-CD
               WHEN 'C'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'J' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'U' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'X' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'C' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DIST CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-DIST-CODE.
           EVALUATE IRVAL-DIST-CODE-CD
               WHEN 'C'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'J'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2380-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRVAL-CHANNEL-CD
                   ADD IRVAL-EXCESS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRVAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRVAL-EXCESS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RMD GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2390-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RMD-GRID-RATE(1) TO IRVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-RMD-GRID-LOW(WS-TX)
                   MOVE WS-RMD-GRID-RATE(WS-TX) TO IRVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2400-DESCRIBE-CHANNEL.
           EVALUATE IRVAL-CHANNEL-CD
               WHEN 'AS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'AY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FMV YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2410-ACCRUE-FMV.
           IF IRVAL-FMV-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRVAL-FMV-YTD * IRVAL-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO IRVAL-RMD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E478 CONTACT REQUIRED
      * E966 FMV YTD NEGATIVE
      * E330 FMV YTD OVER 50000
      * E906 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2420-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRVAL-CONTACT = SPACES
                   MOVE 'E478' TO WS-REJECT-CODE
               WHEN IRVAL-FMV-YTD < 0
                   MOVE 'E966' TO WS-REJECT-CODE
               WHEN IRVAL-FMV-YTD > 50000.00
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN IRVAL-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FMV YTD.
      *----------------------------------------------------------------*
       2430-PRORATE-FMV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRVAL-FMV-YTD ROUNDED =
                   IRVAL-RMD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRVAL-FMV-YTD
           END-IF
           ADD IRVAL-FMV-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCESS AMT.
      *----------------------------------------------------------------*
       2440-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRVAL-EXCESS-AMT ROUNDED =
                   IRVAL-ROLLOVER-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRVAL-EXCESS-AMT
           END-IF
           ADD IRVAL-EXCESS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2450-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRVAL-ROLLOVER-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRVAL-ROLLOVER-AMT - (WS-WORK-AMT *
               3)
           COMPUTE IRVAL-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FMV AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2460-FLAG-FMV.
           IF IRVAL-FMV-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRVAL-FMV-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2470-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF IRVAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO IRVAL-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2480-COUNT-CYCLE.
           IF IRVAL-CYCLE-CNT < 9999999
               ADD 1 TO IRVAL-CYCLE-CNT
           END-IF
           IF IRVAL-CYCLE-CNT NOT < 12
               MOVE 'Y' TO IRVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FMV YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2490-PRORATE-FMV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   IRVAL-FMV-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE EXCESS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WITHHOLD YTD.
      *----------------------------------------------------------------*
       2500-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRVAL-WITHHOLD-YTD ROUNDED =
                   IRVAL-EXCESS-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRVAL-WITHHOLD-YTD
           END-IF
           ADD IRVAL-WITHHOLD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST CODE CD:
      * C -> J WHEN NEXT DUE DATE IS PAST
      * J -> U WHEN NEXT DUE DATE IS PAST
      * U -> X WHEN NEXT DUE DATE IS PAST
      * X -> C WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2510-SET-DIST-CODE.
           EVALUATE IRVAL-DIST-CODE-CD
               WHEN 'C'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS AMT BY DIST CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2520-ACCUM-DIST-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRVAL-DIST-CODE-CD
                   ADD IRVAL-EXCESS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRVAL-DIST-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRVAL-EXCESS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN MATURITY DATE IS PAST
      * FL -> GY WHEN MATURITY DATE IS PAST
      * GY -> HU WHEN MATURITY DATE IS PAST
      * HU -> KA WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-REGION.
           EVALUATE IRVAL-REGION-CD
               WHEN 'CE'
                   IF IRVAL-MATURITY-DATE > 0 AND IRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRVAL-MATURITY-DATE > 0 AND IRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRVAL-MATURITY-DATE > 0 AND IRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRVAL-MATURITY-DATE > 0 AND IRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FMV YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2540-LIMIT-FMV.
           IF IRVAL-FMV-YTD < 100.00
               MOVE 100.00 TO IRVAL-FMV-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRVAL-FMV-YTD > 2500.00
               MOVE 2500.00 TO IRVAL-FMV-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST CODE CD:
      * C -> J WHEN LAST STMT DATE IS PAST
      * J -> U WHEN LAST STMT DATE IS PAST
      * U -> X WHEN LAST STMT DATE IS PAST
      * X -> C WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2550-SET-DIST-CODE.
           EVALUATE IRVAL-DIST-CODE-CD
               WHEN 'C'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'J' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'U' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'X' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF IRVAL-LAST-STMT-DATE > 0 AND IRVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'C' TO IRVAL-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FMV AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCESS AMT.
      *----------------------------------------------------------------*
       2560-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRVAL-EXCESS-AMT ROUNDED =
                   IRVAL-FMV-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRVAL-EXCESS-AMT
           END-IF
           ADD IRVAL-EXCESS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2570-DESCRIBE-CHANNEL.
           EVALUATE IRVAL-CHANNEL-CD
               WHEN 'AS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'AY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * IRVAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO IRVAL-EXCESS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RMD GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2590-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RMD-GRID-RATE(1) TO IRVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-RMD-GRID-LOW(WS-TX)
                   MOVE WS-RMD-GRID-RATE(WS-TX) TO IRVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FMV YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2600-ACCRUE-FMV.
           IF IRVAL-FMV-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRVAL-FMV-YTD * IRVAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO IRVAL-EXCESS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB SCHED TIER FOR THE WITHHOLD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2610-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-SCHED-RATE(1) TO IRVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF IRVAL-WITHHOLD-YTD NOT < WS-CONTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-CONTRIB-SCHED-RATE(WS-TX) TO IRVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RMD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2620-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = IRVAL-RMD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = IRVAL-RMD-YTD - (WS-WORK-AMT * 2)
           COMPUTE IRVAL-EXCESS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E587 CONTACT REQUIRED
      * E506 RMD YTD NEGATIVE
      * E110 RMD YTD OVER 10000
      * E475 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2630-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRVAL-CONTACT = SPACES
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN IRVAL-RMD-YTD < 0
                   MOVE 'E506' TO WS-REJECT-CODE
               WHEN IRVAL-RMD-YTD > 10000.00
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN IRVAL-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E137 ADDR REQUIRED
      * E324 ROLLOVER AMT NEGATIVE
      * E529 ROLLOVER AMT OVER 50000
      * E764 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2640-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRVAL-ADDR = SPACES
                   MOVE 'E137' TO WS-REJECT-CODE
               WHEN IRVAL-ROLLOVER-AMT < 0
                   MOVE 'E324' TO WS-REJECT-CODE
               WHEN IRVAL-ROLLOVER-AMT > 50000.00
                   MOVE 'E529' TO WS-REJECT-CODE
               WHEN IRVAL-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2650-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2660-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRVAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO IRVAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN EFFECTIVE DATE IS PAST
      * FL -> GY WHEN EFFECTIVE DATE IS PAST
      * GY -> HU WHEN EFFECTIVE DATE IS PAST
      * HU -> KA WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2670-SET-REGION.
           EVALUATE IRVAL-REGION-CD
               WHEN 'CE'
                   IF IRVAL-EFFECTIVE-DATE > 0 AND IRVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRVAL-EFFECTIVE-DATE > 0 AND IRVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRVAL-EFFECTIVE-DATE > 0 AND IRVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRVAL-EFFECTIVE-DATE > 0 AND IRVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WITHHOLD YTD.
      *----------------------------------------------------------------*
       2680-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRVAL-WITHHOLD-YTD ROUNDED =
                   IRVAL-RMD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRVAL-WITHHOLD-YTD
           END-IF
           ADD IRVAL-WITHHOLD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2690-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRVAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO IRVAL-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2700-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE IRVAL-ROLLOVER-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2710-DESCRIBE-TYPE.
           EVALUATE IRVAL-TYPE-CD
               WHEN 'EA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E692 ADDR REQUIRED
      * E196 WITHHOLD YTD NEGATIVE
      * E825 WITHHOLD YTD OVER 5000
      * E066 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2720-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRVAL-ADDR = SPACES
                   MOVE 'E692' TO WS-REJECT-CODE
               WHEN IRVAL-WITHHOLD-YTD < 0
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN IRVAL-WITHHOLD-YTD > 5000.00
                   MOVE 'E825' TO WS-REJECT-CODE
               WHEN IRVAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2730-LIMIT-WITHHOLD.
           IF IRVAL-WITHHOLD-YTD < 100.00
               MOVE 100.00 TO IRVAL-WITHHOLD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRVAL-WITHHOLD-YTD > 50000.00
               MOVE 50000.00 TO IRVAL-WITHHOLD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2740-COUNT-ITEM.
           IF IRVAL-ITEM-CNT < 9999999
               ADD 1 TO IRVAL-ITEM-CNT
           END-IF
           IF IRVAL-ITEM-CNT NOT < 12
               MOVE 'Y' TO IRVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WITHHOLD YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2750-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN IRVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRVAL-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = IRVAL-WITHHOLD-YTD *
                       IRVAL-MARGIN / 200
               WHEN IRVAL-REGION-CD = 'CE'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN IRVAL-REGION-CD = 'FL'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = IRVAL-WITHHOLD-YTD *
                       IRVAL-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN NEXT DUE DATE IS PAST
      * FL -> GY WHEN NEXT DUE DATE IS PAST
      * GY -> HU WHEN NEXT DUE DATE IS PAST
      * HU -> KA WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2760-SET-REGION.
           EVALUATE IRVAL-REGION-CD
               WHEN 'CE'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRVAL-NEXT-DUE-DATE > 0 AND IRVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD YTD BY DIST CODE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2770-ACCUM-DIST-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRVAL-DIST-CODE-CD
                   ADD IRVAL-WITHHOLD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRVAL-DIST-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRVAL-WITHHOLD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E788 CONTACT REQUIRED
      * E535 RMD YTD NEGATIVE
      * E002 RMD YTD OVER 25000
      * E300 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2780-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRVAL-CONTACT = SPACES
                   MOVE 'E788' TO WS-REJECT-CODE
               WHEN IRVAL-RMD-YTD < 0
                   MOVE 'E535' TO WS-REJECT-CODE
               WHEN IRVAL-RMD-YTD > 25000.00
                   MOVE 'E002' TO WS-REJECT-CODE
               WHEN IRVAL-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E300' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2790-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RMD GRID TIER FOR THE ROLLOVER AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2800-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RMD-GRID-RATE(1) TO IRVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF IRVAL-ROLLOVER-AMT NOT < WS-RMD-GRID-LOW(WS-TX)
                   MOVE WS-RMD-GRID-RATE(WS-TX) TO IRVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCESS FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2810-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN IRVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO IRVAL-EXCESS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRVAL-HOLD-FLAG = 'Y'
                   COMPUTE IRVAL-EXCESS-AMT ROUNDED = WS-WORK-AMT-2 *
                       IRVAL-RATE / 200
               WHEN IRVAL-REGION-CD = 'CE'
                   MOVE 7.00 TO IRVAL-EXCESS-AMT
               WHEN IRVAL-REGION-CD = 'FL'
                   MOVE 12.00 TO IRVAL-EXCESS-AMT
               WHEN OTHER
                   COMPUTE IRVAL-EXCESS-AMT ROUNDED = WS-WORK-AMT-2 *
                       IRVAL-RATE / 100
           END-EVALUATE
           IF IRVAL-EXCESS-AMT > 0 AND IRVAL-EXCESS-AMT < 10.00
               MOVE 10.00 TO IRVAL-EXCESS-AMT
           END-IF
           IF IRVAL-EXCESS-AMT > 25.00
               MOVE 25.00 TO IRVAL-EXCESS-AMT
           END-IF
           ADD IRVAL-EXCESS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE RMD YTD FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2820-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN IRVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO IRVAL-RMD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRVAL-HOLD-FLAG = 'Y'
                   COMPUTE IRVAL-RMD-YTD ROUNDED = IRVAL-ROLLOVER-AMT *
                       IRVAL-RATE / 200
               WHEN IRVAL-CHANNEL-CD = 'AS'
                   MOVE 12.00 TO IRVAL-RMD-YTD
               WHEN IRVAL-CHANNEL-CD = 'AY'
                   MOVE 5.00 TO IRVAL-RMD-YTD
               WHEN OTHER
                   COMPUTE IRVAL-RMD-YTD ROUNDED = IRVAL-ROLLOVER-AMT *
                       IRVAL-RATE / 100
           END-EVALUATE
           IF IRVAL-RMD-YTD > 0 AND IRVAL-RMD-YTD < 10.00
               MOVE 10.00 TO IRVAL-RMD-YTD
           END-IF
           IF IRVAL-RMD-YTD > 35.00
               MOVE 35.00 TO IRVAL-RMD-YTD
           END-IF
           ADD IRVAL-RMD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E870 ADDR REQUIRED
      * E198 ROLLOVER AMT NEGATIVE
      * E221 ROLLOVER AMT OVER 250000
      * E197 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2830-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRVAL-ADDR = SPACES
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN IRVAL-ROLLOVER-AMT < 0
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN IRVAL-ROLLOVER-AMT > 250000.00
                   MOVE 'E221' TO WS-REJECT-CODE
               WHEN IRVAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2840-DESCRIBE-TYPE.
           EVALUATE IRVAL-TYPE-CD
               WHEN 'EA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2850-COUNT-CYCLE.
           IF IRVAL-CYCLE-CNT < 9999999
               ADD 1 TO IRVAL-CYCLE-CNT
           END-IF
           IF IRVAL-CYCLE-CNT NOT < 3
               MOVE 'Y' TO IRVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2860-COUNT-ITEM.
           IF IRVAL-ITEM-CNT < 9999999
               ADD 1 TO IRVAL-ITEM-CNT
           END-IF
           IF IRVAL-ITEM-CNT NOT < 5
               MOVE 'Y' TO IRVAL-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2870-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRVAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO IRVAL-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FMV YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2880-SPLIT-FMV.
           COMPUTE WS-WORK-AMT = IRVAL-FMV-YTD / 4
           COMPUTE WS-WORK-AMT-2 = IRVAL-FMV-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FMV AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2890-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRVAL-REGION-CD
                   ADD IRVAL-FMV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRVAL-FMV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2900-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRVAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO IRVAL-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO IRVAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO IRVAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO IRVAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'IRVAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
