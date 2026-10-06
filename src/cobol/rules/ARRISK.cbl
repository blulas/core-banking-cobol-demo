      *================================================================*
      * PROGRAM  : ARRISK                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : ACCOUNT RECONCILEMENT RULE - VALIDATION. CALLED BY  *
      *            THE ACCOUNT RECONCILEMENT BATCH AND ONLINE PROGRAMS.*
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-09-14 CRP  ORIGINAL                                       *
      * 1995-06-24 LMB  VOID CUTOFF MOVED INC4230                      *
      * 1995-01-26 BHS  NEW VOID REPORT COLUMNS PRB4275                *
      * 1996-07-02 PQR  STOP FEE SCHEDULE UPDATE PRB6457               *
      * 1999-12-11 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-09-08 GAF  VOID CALCULATION CORRECTED REQ5513             *
      * 2003-02-07 TWH  STOP FEE SCHEDULE UPDATE REQ3814               *
      * 2005-10-16 SKN  NEW FEE REPORT COLUMNS PRB7027                 *
      * 2006-06-27 SKN  OUTSTAND CALCULATION CORRECTED REQ3353         *
      * 2006-06-02 PQR  FEE STATUS CODES ADDED REQ2877                 *
      * 2009-12-01 PQR  STOP STATUS CODES ADDED REQ1374                *
      * 2012-10-15 DPS  FEE LIMIT REVISED INC1425                      *
      * 2013-12-27 DPS  FEE CUTOFF MOVED REQ0222                       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARRISK.
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
                                           VALUE 14.000000.
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
      * WS-OUTSTAND-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OUTSTAND-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000081500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000670000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000892500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000532000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000212500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000323000'.
       01  WS-OUTSTAND-BAND-TBL REDEFINES WS-OUTSTAND-BAND-VALUES.
           05  WS-OUTSTAND-BAND-ROW OCCURS 6 TIMES.
               10  WS-OUTSTAND-BAND-LOW    PIC 9(09)V99.
               10  WS-OUTSTAND-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ARRISKP.

       PROCEDURE DIVISION USING ARRISK-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PRORATE-ITEM
           PERFORM 2010-ACCRUE-DISCOUNT
           PERFORM 2020-PRORATE-ISSUED
           PERFORM 2030-CHECK-NEXT-DUE
           PERFORM 2040-ACCUM-REGION
           PERFORM 2050-ACCUM-REGION
           PERFORM 2060-DESCRIBE-REGION
           PERFORM 2070-DESCRIBE-REGION
           PERFORM 2080-SELECT-DISCOUNT
           PERFORM 2090-CHECK-NEXT-DUE
           PERFORM 2100-FLAG-STOP
           PERFORM 2110-FLAG-STOP
           PERFORM 2120-PRORATE-OUTSTAND
           PERFORM 2130-PRORATE-OUTSTAND
           PERFORM 2140-AGE-NEXT-DUE
           PERFORM 2150-COUNT-CYCLE
           PERFORM 2160-ACCUM-REGION
           PERFORM 2170-SPLIT-STOP
           PERFORM 2180-COUNT-CYCLE
           PERFORM 2190-EDIT-DESC
           PERFORM 2200-ASSESS-NOTICE-SENT
           PERFORM 2210-FLAG-PAID
           PERFORM 2220-SELECT-DISCOUNT
           PERFORM 2230-LIMIT-ISSUED
           PERFORM 2240-SPLIT-PAID
           PERFORM 2250-AGE-LAST-PAY
           PERFORM 2260-LIMIT-STOP
           PERFORM 2270-ACCRUE-VOID
           PERFORM 2280-COUNT-ITEM
           PERFORM 2290-LIMIT-PAID
           PERFORM 2300-SPLIT-OUTSTAND
           PERFORM 2310-SET-CYCLE
           PERFORM 2320-SPLIT-STOP
           PERFORM 2330-EDIT-ADDR
           PERFORM 2340-SET-CHANNEL
           PERFORM 2350-AGE-NEXT-REVIEW
           PERFORM 2360-ACCUM-BRANCH
           PERFORM 2370-LIMIT-ISSUED
           PERFORM 2380-SET-BRANCH
           PERFORM 2390-ASSESS-HOLD
           PERFORM 2400-CHECK-LAST-STMT
           PERFORM 2410-SET-CHANNEL
           PERFORM 2420-ASSESS-NOTICE-SENT
           PERFORM 2430-SELECT-DISCOUNT
           PERFORM 2440-FLAG-ITEM
           PERFORM 2450-AGE-NEXT-REVIEW
           PERFORM 2460-PRORATE-PAID
           PERFORM 2470-SELECT-DISCOUNT
           PERFORM 2480-SPLIT-ISSUED
           PERFORM 2490-LIMIT-ITEM
           PERFORM 2500-ACCRUE-ISSUED
           PERFORM 2510-PRORATE-ISSUED
           PERFORM 2520-EDIT-ADDR
           PERFORM 2530-FLAG-VOID
           PERFORM 2540-PRORATE-ISSUED
           PERFORM 2550-SELECT-DISCOUNT
           PERFORM 2560-SPLIT-ISSUED
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ARRISK-RETURN-CODE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2000-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2010-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ARRISK-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARRISK-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED YTD.
      *----------------------------------------------------------------*
       2020-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARRISK-ISSUED-YTD ROUNDED =
                   ARRISK-OUTSTAND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARRISK-ISSUED-YTD
           END-IF
           ADD ARRISK-ISSUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2030-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARRISK-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARRISK-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2040-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARRISK-REGION-CD
                   ADD ARRISK-ISSUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARRISK-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARRISK-ISSUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2050-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARRISK-REGION-CD
                   ADD ARRISK-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARRISK-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARRISK-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2060-DESCRIBE-REGION.
           EVALUATE ARRISK-REGION-CD
               WHEN 'AD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2070-DESCRIBE-REGION.
           EVALUATE ARRISK-REGION-CD
               WHEN 'AD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2080-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARRISK-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARRISK-PAID-YTD NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO ARRISK-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARRISK-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARRISK-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2090-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARRISK-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ARRISK-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STOP YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2100-FLAG-STOP.
           IF ARRISK-STOP-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARRISK-STOP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STOP YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2110-FLAG-STOP.
           IF ARRISK-STOP-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARRISK-STOP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       2120-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARRISK-OUTSTAND-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARRISK-OUTSTAND-YTD
           END-IF
           ADD ARRISK-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE VOID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       2130-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARRISK-OUTSTAND-YTD ROUNDED =
                   ARRISK-VOID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARRISK-OUTSTAND-YTD
           END-IF
           ADD ARRISK-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARRISK-NEXT-DUE-DATE > 0 AND ARRISK-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARRISK-OUTSTAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ARRISK-OUTSTAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARRISK-OUTSTAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARRISK-OUTSTAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARRISK-OUTSTAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2150-COUNT-CYCLE.
           IF ARRISK-CYCLE-CNT < 9999999
               ADD 1 TO ARRISK-CYCLE-CNT
           END-IF
           IF ARRISK-CYCLE-CNT NOT < 6
               MOVE 'Y' TO ARRISK-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2160-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARRISK-REGION-CD
                   ADD ARRISK-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARRISK-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARRISK-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STOP YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2170-SPLIT-STOP.
           COMPUTE WS-WORK-AMT = ARRISK-STOP-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARRISK-STOP-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARRISK-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2180-COUNT-CYCLE.
           IF ARRISK-CYCLE-CNT < 9999999
               ADD 1 TO ARRISK-CYCLE-CNT
           END-IF
           IF ARRISK-CYCLE-CNT NOT < 12
               MOVE 'Y' TO ARRISK-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E120 DESC REQUIRED
      * E988 OUTSTAND YTD NEGATIVE
      * E696 OUTSTAND YTD OVER 25000
      * E007 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2190-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARRISK-DESC = SPACES
                   MOVE 'E120' TO WS-REJECT-CODE
               WHEN ARRISK-OUTSTAND-YTD < 0
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN ARRISK-OUTSTAND-YTD > 25000.00
                   MOVE 'E696' TO WS-REJECT-CODE
               WHEN ARRISK-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ISSUED YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2200-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARRISK-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARRISK-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ARRISK-ISSUED-YTD *
                       ARRISK-PENALTY-PCT / 200
               WHEN ARRISK-BRANCH-CD = 'FN'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN ARRISK-BRANCH-CD = 'GM'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ARRISK-ISSUED-YTD *
                       ARRISK-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAID YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2210-FLAG-PAID.
           IF ARRISK-PAID-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARRISK-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE ISSUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARRISK-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARRISK-ISSUED-YTD NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO ARRISK-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARRISK-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARRISK-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2230-LIMIT-ISSUED.
           IF ARRISK-ISSUED-YTD < 10.00
               MOVE 10.00 TO ARRISK-ISSUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARRISK-ISSUED-YTD > 10000.00
               MOVE 10000.00 TO ARRISK-ISSUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2240-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = ARRISK-PAID-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ARRISK-PAID-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2250-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ARRISK-LAST-PAY-DATE > 0 AND ARRISK-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARRISK-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARRISK-VOID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ARRISK-VOID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARRISK-VOID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARRISK-VOID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARRISK-VOID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STOP YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2260-LIMIT-STOP.
           IF ARRISK-STOP-YTD < 50.00
               MOVE 50.00 TO ARRISK-STOP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARRISK-STOP-YTD > 1000.00
               MOVE 1000.00 TO ARRISK-STOP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-VOID.
           IF ARRISK-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARRISK-VOID-YTD * ARRISK-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARRISK-STOP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-ITEM.
           IF ARRISK-ITEM-CNT < 9999999
               ADD 1 TO ARRISK-ITEM-CNT
           END-IF
           IF ARRISK-ITEM-CNT NOT < 10
               MOVE 'Y' TO ARRISK-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2290-LIMIT-PAID.
           IF ARRISK-PAID-YTD < 100.00
               MOVE 100.00 TO ARRISK-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARRISK-PAID-YTD > 1000.00
               MOVE 1000.00 TO ARRISK-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2300-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE ARRISK-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CP -> MR WHEN LAST STMT DATE IS PAST
      * MR -> PY WHEN LAST STMT DATE IS PAST
      * PY -> SP WHEN LAST STMT DATE IS PAST
      * SP -> TP WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-CYCLE.
           EVALUATE ARRISK-CYCLE-CD
               WHEN 'CP'
                   IF ARRISK-LAST-STMT-DATE > 0 AND
                       ARRISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'MR' TO ARRISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF ARRISK-LAST-STMT-DATE > 0 AND
                       ARRISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'PY' TO ARRISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ARRISK-LAST-STMT-DATE > 0 AND
                       ARRISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARRISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARRISK-LAST-STMT-DATE > 0 AND
                       ARRISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'TP' TO ARRISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STOP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-STOP.
           COMPUTE WS-WORK-AMT = ARRISK-STOP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ARRISK-STOP-YTD - (WS-WORK-AMT * 3)
           COMPUTE ARRISK-VOID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E528 ADDR REQUIRED
      * E566 VOID YTD NEGATIVE
      * E991 VOID YTD OVER 5000
      * E939 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2330-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARRISK-ADDR = SPACES
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN ARRISK-VOID-YTD < 0
                   MOVE 'E566' TO WS-REJECT-CODE
               WHEN ARRISK-VOID-YTD > 5000.00
                   MOVE 'E991' TO WS-REJECT-CODE
               WHEN ARRISK-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN NEXT DUE DATE IS PAST
      * SW -> UA WHEN NEXT DUE DATE IS PAST
      * UA -> WB WHEN NEXT DUE DATE IS PAST
      * WB -> ME WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2340-SET-CHANNEL.
           EVALUATE ARRISK-CHANNEL-CD
               WHEN 'ME'
                   IF ARRISK-NEXT-DUE-DATE > 0 AND ARRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARRISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARRISK-NEXT-DUE-DATE > 0 AND ARRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARRISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARRISK-NEXT-DUE-DATE > 0 AND ARRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARRISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARRISK-NEXT-DUE-DATE > 0 AND ARRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARRISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2350-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARRISK-NEXT-REVIEW-DATE > 0 AND ARRISK-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARRISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARRISK-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARRISK-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ARRISK-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARRISK-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARRISK-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2360-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARRISK-BRANCH-CD
                   ADD ARRISK-ISSUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARRISK-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARRISK-ISSUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2370-LIMIT-ISSUED.
           IF ARRISK-ISSUED-AMT < 50.00
               MOVE 50.00 TO ARRISK-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARRISK-ISSUED-AMT > 10000.00
               MOVE 10000.00 TO ARRISK-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN NEXT REVIEW DATE IS PAST
      * GM -> GV WHEN NEXT REVIEW DATE IS PAST
      * GV -> HK WHEN NEXT REVIEW DATE IS PAST
      * HK -> VX WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2380-SET-BRANCH.
           EVALUATE ARRISK-BRANCH-CD
               WHEN 'FN'
                   IF ARRISK-NEXT-REVIEW-DATE > 0 AND
                       ARRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARRISK-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARRISK-NEXT-REVIEW-DATE > 0 AND
                       ARRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARRISK-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARRISK-NEXT-REVIEW-DATE > 0 AND
                       ARRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARRISK-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARRISK-NEXT-REVIEW-DATE > 0 AND
                       ARRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARRISK-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2390-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARRISK-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARRISK-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARRISK-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARRISK-ISSUED-AMT ROUNDED = WS-WORK-AMT-2 *
                       ARRISK-DISCOUNT / 200
               WHEN ARRISK-REGION-CD = 'AD'
                   MOVE 5.00 TO ARRISK-ISSUED-AMT
               WHEN ARRISK-REGION-CD = 'BU'
                   MOVE 7.00 TO ARRISK-ISSUED-AMT
               WHEN OTHER
                   COMPUTE ARRISK-ISSUED-AMT ROUNDED = WS-WORK-AMT-2 *
                       ARRISK-DISCOUNT / 100
           END-EVALUATE
           IF ARRISK-ISSUED-AMT > 0 AND ARRISK-ISSUED-AMT < 5.00
               MOVE 5.00 TO ARRISK-ISSUED-AMT
           END-IF
           IF ARRISK-ISSUED-AMT > 50.00
               MOVE 50.00 TO ARRISK-ISSUED-AMT
           END-IF
           ADD ARRISK-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2400-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARRISK-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARRISK-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ARRISK-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN LAST STMT DATE IS PAST
      * SW -> UA WHEN LAST STMT DATE IS PAST
      * UA -> WB WHEN LAST STMT DATE IS PAST
      * WB -> ME WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2410-SET-CHANNEL.
           EVALUATE ARRISK-CHANNEL-CD
               WHEN 'ME'
                   IF ARRISK-LAST-STMT-DATE > 0 AND
                       ARRISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARRISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARRISK-LAST-STMT-DATE > 0 AND
                       ARRISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARRISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARRISK-LAST-STMT-DATE > 0 AND
                       ARRISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARRISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARRISK-LAST-STMT-DATE > 0 AND
                       ARRISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARRISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2420-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARRISK-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ARRISK-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARRISK-HOLD-FLAG = 'Y'
                   COMPUTE ARRISK-ISSUED-AMT ROUNDED =
                       ARRISK-OUTSTAND-YTD * ARRISK-DISCOUNT / 200
               WHEN ARRISK-CHANNEL-CD = 'ME'
                   MOVE 7.00 TO ARRISK-ISSUED-AMT
               WHEN ARRISK-CHANNEL-CD = 'SW'
                   MOVE 15.00 TO ARRISK-ISSUED-AMT
               WHEN OTHER
                   COMPUTE ARRISK-ISSUED-AMT ROUNDED =
                       ARRISK-OUTSTAND-YTD * ARRISK-DISCOUNT / 100
           END-EVALUATE
           IF ARRISK-ISSUED-AMT > 0 AND ARRISK-ISSUED-AMT < 5.00
               MOVE 5.00 TO ARRISK-ISSUED-AMT
           END-IF
           IF ARRISK-ISSUED-AMT > 50.00
               MOVE 50.00 TO ARRISK-ISSUED-AMT
           END-IF
           ADD ARRISK-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE STOP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2430-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARRISK-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARRISK-STOP-YTD NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO ARRISK-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARRISK-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARRISK-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2440-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2450-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARRISK-NEXT-REVIEW-DATE > 0 AND ARRISK-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARRISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARRISK-ISSUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARRISK-ISSUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ARRISK-ISSUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ARRISK-ISSUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARRISK-ISSUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       2460-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARRISK-PAID-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARRISK-PAID-YTD
           END-IF
           ADD ARRISK-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE VOID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARRISK-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARRISK-VOID-YTD NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO ARRISK-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARRISK-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARRISK-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2480-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARRISK-ISSUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARRISK-ISSUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2490-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-ISSUED.
           IF ARRISK-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARRISK-ISSUED-AMT * ARRISK-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARRISK-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED YTD.
      *----------------------------------------------------------------*
       2510-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARRISK-ISSUED-YTD ROUNDED =
                   ARRISK-PAID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARRISK-ISSUED-YTD
           END-IF
           ADD ARRISK-ISSUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E277 ADDR REQUIRED
      * E993 ISSUED YTD NEGATIVE
      * E439 ISSUED YTD OVER 250000
      * E096 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2520-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARRISK-ADDR = SPACES
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN ARRISK-ISSUED-YTD < 0
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN ARRISK-ISSUED-YTD > 250000.00
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN ARRISK-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2530-FLAG-VOID.
           IF ARRISK-VOID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARRISK-VOID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       2540-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARRISK-ISSUED-AMT ROUNDED =
                   ARRISK-OUTSTAND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARRISK-ISSUED-AMT
           END-IF
           ADD ARRISK-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE ISSUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2550-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARRISK-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARRISK-ISSUED-YTD NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO ARRISK-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARRISK-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARRISK-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2560-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARRISK-ISSUED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARRISK-ISSUED-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARRISK-STOP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ARRISK-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ARRISK-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ARRISK-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ARRISK EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
