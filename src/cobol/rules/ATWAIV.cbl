      *================================================================*
      * PROGRAM  : ATWAIV                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : ATM NETWORK SETTLEMENT RULE - PRICING. CALLED BY THE*
      *            ATM NETWORK SETTLEMENT BATCH AND ONLINE PROGRAMS.   *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-02 PQR  ORIGINAL                                       *
      * 1994-01-01 PQR  PERFORMANCE - INTERCHG READ AHEAD PRB2017      *
      * 1996-11-08 TWH  INTERCHG ROUNDING - HALF UP INC9716            *
      * 2005-04-27 SKN  PERFORMANCE - ADJUST READ AHEAD INC0134        *
      * 2007-11-20 GAF  ADJUST ROUNDING - HALF UP INC3034              *
      * 2008-01-23 BHS  AUDIT FINDING - INTERCHG CONTROL TOTALS INC3064*
      * 2009-05-27 PQR  AUDIT FINDING - SURCHG CONTROL TOTALS REQ7391  *
      * 2010-06-23 LMB  INTERCHG FEE SCHEDULE UPDATE CHG4657           *
      * 2011-09-13 LMB  DISPENSED ROUNDING - HALF UP INC8127           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATWAIV.
       AUTHOR. T W HOLT.

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
      * WS-INTERCHG-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTERCHG-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000458500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000132000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000718000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000253500'.
       01  WS-INTERCHG-GRID-TBL REDEFINES WS-INTERCHG-GRID-VALUES.
           05  WS-INTERCHG-GRID-ROW OCCURS 4 TIMES.
               10  WS-INTERCHG-GRID-LOW    PIC 9(09)V99.
               10  WS-INTERCHG-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASSETTE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASSETTE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000583000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000621000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000834000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000701500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013750000000371500'.
       01  WS-CASSETTE-BAND-TBL REDEFINES WS-CASSETTE-BAND-VALUES.
           05  WS-CASSETTE-BAND-ROW OCCURS 5 TIMES.
               10  WS-CASSETTE-BAND-LOW    PIC 9(09)V99.
               10  WS-CASSETTE-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ATWAIVP.

       PROCEDURE DIVISION USING ATWAIV-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SELECT-RATE
           PERFORM 2010-PRORATE-DISPENSED
           PERFORM 2020-CHECK-EFFECTIVE
           PERFORM 2030-ACCRUE-SETTLE
           PERFORM 2040-ACCUM-BRANCH
           PERFORM 2050-PRORATE-ADJUST
           PERFORM 2060-PRORATE-ITEM
           PERFORM 2070-PRORATE-SETTLE
           PERFORM 2080-PRORATE-ITEM
           PERFORM 2090-LIMIT-ITEM
           PERFORM 2100-DESCRIBE-TERMINAL
           PERFORM 2110-ACCUM-TERMINAL
           PERFORM 2120-ACCUM-TERMINAL
           PERFORM 2130-EDIT-NAME
           PERFORM 2140-ACCUM-BRANCH
           PERFORM 2150-SELECT-FEE-PCT
           PERFORM 2160-CHECK-LAST-PAY
           PERFORM 2170-ASSESS-REVIEW
           PERFORM 2180-SELECT-RATE
           PERFORM 2190-ACCRUE-INTERCHG
           PERFORM 2200-ACCRUE-ADJUST
           PERFORM 2210-FLAG-INTERCHG
           PERFORM 2220-LIMIT-ITEM
           PERFORM 2230-SET-STATUS
           PERFORM 2240-ASSESS-SUSPEND
           PERFORM 2250-EDIT-DESC
           PERFORM 2260-SPLIT-ADJUST
           PERFORM 2270-AGE-NEXT-REVIEW
           PERFORM 2280-SPLIT-SETTLE
           PERFORM 2290-ASSESS-NOTICE-SENT
           PERFORM 2300-SPLIT-INTERCHG
           PERFORM 2310-PRORATE-DISPENSED
           PERFORM 2320-ACCUM-NETWORK
           PERFORM 2330-LIMIT-ITEM
           PERFORM 2340-EDIT-DESC
           PERFORM 2350-SELECT-FEE-PCT
           PERFORM 2360-FLAG-ADJUST
           PERFORM 2370-ASSESS-REVIEW
           PERFORM 2380-PRORATE-ADJUST
           PERFORM 2390-SPLIT-DISPENSED
           PERFORM 2400-EDIT-DESC
           PERFORM 2410-ACCRUE-ADJUST
           PERFORM 2420-FLAG-DISPENSED
           PERFORM 2430-LIMIT-ITEM
           PERFORM 2440-ASSESS-SUSPEND
           PERFORM 2450-CHECK-NEXT-REVIEW
           PERFORM 2460-ACCUM-BRANCH
           PERFORM 2470-DESCRIBE-TERMINAL
           PERFORM 2480-FLAG-SETTLE
           PERFORM 2490-COUNT-ITEM
           PERFORM 2500-SPLIT-SETTLE
           PERFORM 2510-ACCUM-BRANCH
           PERFORM 2520-SELECT-RATE
           PERFORM 2530-SET-BRANCH
           PERFORM 2540-AGE-EFFECTIVE
           PERFORM 2550-AGE-LAST-PAY
           PERFORM 2560-PRORATE-SETTLE
           PERFORM 2570-FLAG-ITEM
           PERFORM 2580-LIMIT-ITEM
           PERFORM 2590-LIMIT-ADJUST
           PERFORM 2600-AGE-NEXT-REVIEW
           PERFORM 2610-AGE-NEXT-REVIEW
           PERFORM 2620-COUNT-PAYMENT
           PERFORM 2630-SELECT-RATE
           PERFORM 2640-ACCUM-TERMINAL
           PERFORM 2650-COUNT-ITEM
           PERFORM 2660-ACCUM-TERMINAL
           PERFORM 2670-LIMIT-SETTLE
           PERFORM 2680-EDIT-NAME
           PERFORM 2690-FLAG-ITEM
           PERFORM 2700-DESCRIBE-BRANCH
           PERFORM 2710-SELECT-RATE
           PERFORM 2720-SELECT-RATE
           PERFORM 2730-ACCUM-TERMINAL
           PERFORM 2740-COUNT-ITEM
           PERFORM 2750-SPLIT-ADJUST
           PERFORM 2760-SET-NETWORK
           PERFORM 2770-SET-NETWORK
           PERFORM 2780-FLAG-INTERCHG
           PERFORM 2790-DESCRIBE-STATUS
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ATWAIV-RETURN-CODE.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE ADJUST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2000-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ATWAIV-ADJUST-AMT NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADJUST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISPENSED YTD.
      *----------------------------------------------------------------*
       2010-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATWAIV-DISPENSED-YTD ROUNDED =
                   ATWAIV-ADJUST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATWAIV-DISPENSED-YTD
           END-IF
           ADD ATWAIV-DISPENSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2020-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ATWAIV-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATWAIV-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ATWAIV-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2030-ACCRUE-SETTLE.
           IF ATWAIV-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATWAIV-SETTLE-AMT * ATWAIV-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2040-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATWAIV-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATWAIV-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADJUST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADJUST YTD.
      *----------------------------------------------------------------*
       2050-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATWAIV-ADJUST-YTD ROUNDED =
                   ATWAIV-ADJUST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATWAIV-ADJUST-YTD
           END-IF
           ADD ATWAIV-ADJUST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2060-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE ADJUST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE YTD.
      *----------------------------------------------------------------*
       2070-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATWAIV-SETTLE-YTD ROUNDED =
                   ATWAIV-ADJUST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATWAIV-SETTLE-YTD
           END-IF
           ADD ATWAIV-SETTLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2080-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2090-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TERMINAL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2100-DESCRIBE-TERMINAL.
           EVALUATE ATWAIV-TERMINAL-CD
               WHEN 'BJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ADJUST AMT BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATWAIV-TERMINAL-CD
                   ADD ATWAIV-ADJUST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATWAIV-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATWAIV-ADJUST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTERCHG YTD BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2120-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATWAIV-TERMINAL-CD
                   ADD ATWAIV-INTERCHG-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATWAIV-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATWAIV-INTERCHG-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E930 NAME REQUIRED
      * E363 SETTLE AMT NEGATIVE
      * E422 SETTLE AMT OVER 250000
      * E900 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATWAIV-NAME = SPACES
                   MOVE 'E930' TO WS-REJECT-CODE
               WHEN ATWAIV-SETTLE-AMT < 0
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN ATWAIV-SETTLE-AMT > 250000.00
                   MOVE 'E422' TO WS-REJECT-CODE
               WHEN ATWAIV-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E900' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADJUST YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2140-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATWAIV-BRANCH-CD
                   ADD ATWAIV-ADJUST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATWAIV-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATWAIV-ADJUST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2150-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ATWAIV-SETTLE-YTD NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2160-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ATWAIV-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ATWAIV-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE INTERCHG YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2170-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATWAIV-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATWAIV-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATWAIV-INTERCHG-YTD *
                       ATWAIV-RATE / 200
               WHEN ATWAIV-TERMINAL-CD = 'BJ'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN ATWAIV-TERMINAL-CD = 'JJ'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATWAIV-INTERCHG-YTD *
                       ATWAIV-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE INTERCHG GRID TIER FOR THE ADJUST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2180-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INTERCHG-GRID-RATE(1) TO ATWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATWAIV-ADJUST-AMT NOT < WS-INTERCHG-GRID-LOW(WS-TX)
                   MOVE WS-INTERCHG-GRID-RATE(WS-TX) TO ATWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTERCHG YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2190-ACCRUE-INTERCHG.
           IF ATWAIV-INTERCHG-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATWAIV-INTERCHG-YTD * ATWAIV-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ATWAIV-DISPENSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2200-ACCRUE-ADJUST.
           IF ATWAIV-ADJUST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATWAIV-ADJUST-YTD * ATWAIV-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATWAIV-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTERCHG YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2210-FLAG-INTERCHG.
           IF ATWAIV-INTERCHG-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATWAIV-INTERCHG-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2220-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN MATURITY DATE IS PAST
      * DP -> FJ WHEN MATURITY DATE IS PAST
      * FJ -> NA WHEN MATURITY DATE IS PAST
      * NA -> DF WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2230-SET-STATUS.
           EVALUATE ATWAIV-STATUS-CD
               WHEN 'DF'
                   IF ATWAIV-MATURITY-DATE > 0 AND ATWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DP' TO ATWAIV-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF ATWAIV-MATURITY-DATE > 0 AND ATWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FJ' TO ATWAIV-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF ATWAIV-MATURITY-DATE > 0 AND ATWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NA' TO ATWAIV-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NA'
                   IF ATWAIV-MATURITY-DATE > 0 AND ATWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DF' TO ATWAIV-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2240-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATWAIV-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATWAIV-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATWAIV-SETTLE-AMT ROUNDED = WS-WORK-AMT-2 *
                       ATWAIV-RATE / 200
               WHEN ATWAIV-BRANCH-CD = 'DU'
                   MOVE 7.00 TO ATWAIV-SETTLE-AMT
               WHEN ATWAIV-BRANCH-CD = 'HX'
                   MOVE 7.00 TO ATWAIV-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATWAIV-SETTLE-AMT ROUNDED = WS-WORK-AMT-2 *
                       ATWAIV-RATE / 100
           END-EVALUATE
           IF ATWAIV-SETTLE-AMT > 0 AND ATWAIV-SETTLE-AMT < 1.00
               MOVE 1.00 TO ATWAIV-SETTLE-AMT
           END-IF
           IF ATWAIV-SETTLE-AMT > 75.00
               MOVE 75.00 TO ATWAIV-SETTLE-AMT
           END-IF
           ADD ATWAIV-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E181 DESC REQUIRED
      * E153 SETTLE YTD NEGATIVE
      * E326 SETTLE YTD OVER 99999
      * E050 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2250-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATWAIV-DESC = SPACES
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN ATWAIV-SETTLE-YTD < 0
                   MOVE 'E153' TO WS-REJECT-CODE
               WHEN ATWAIV-SETTLE-YTD > 99999.00
                   MOVE 'E326' TO WS-REJECT-CODE
               WHEN ATWAIV-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E050' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2260-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATWAIV-ADJUST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATWAIV-ADJUST-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATWAIV-DISPENSED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATWAIV-NEXT-REVIEW-DATE > 0 AND ATWAIV-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATWAIV-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATWAIV-ADJUST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATWAIV-ADJUST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATWAIV-ADJUST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATWAIV-ADJUST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATWAIV-ADJUST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SETTLE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = ATWAIV-SETTLE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ATWAIV-SETTLE-YTD - (WS-WORK-AMT * 3)
           COMPUTE ATWAIV-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG YTD FEE ON THE ADJUST YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2290-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATWAIV-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATWAIV-INTERCHG-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATWAIV-REVIEW-FLAG = 'Y'
                   COMPUTE ATWAIV-INTERCHG-YTD ROUNDED =
                       ATWAIV-ADJUST-YTD * ATWAIV-FEE-PCT / 200
               WHEN ATWAIV-STATUS-CD = 'DF'
                   MOVE 3.00 TO ATWAIV-INTERCHG-YTD
               WHEN ATWAIV-STATUS-CD = 'DP'
                   MOVE 7.00 TO ATWAIV-INTERCHG-YTD
               WHEN OTHER
                   COMPUTE ATWAIV-INTERCHG-YTD ROUNDED =
                       ATWAIV-ADJUST-YTD * ATWAIV-FEE-PCT / 100
           END-EVALUATE
           IF ATWAIV-INTERCHG-YTD > 0 AND ATWAIV-INTERCHG-YTD < 1.00
               MOVE 1.00 TO ATWAIV-INTERCHG-YTD
           END-IF
           IF ATWAIV-INTERCHG-YTD > 75.00
               MOVE 75.00 TO ATWAIV-INTERCHG-YTD
           END-IF
           ADD ATWAIV-INTERCHG-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT INTERCHG YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2300-SPLIT-INTERCHG.
           COMPUTE WS-WORK-AMT = ATWAIV-INTERCHG-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ATWAIV-INTERCHG-YTD - (WS-WORK-AMT *
               3)
           COMPUTE ATWAIV-ADJUST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISPENSED YTD.
      *----------------------------------------------------------------*
       2310-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATWAIV-DISPENSED-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATWAIV-DISPENSED-YTD
           END-IF
           ADD ATWAIV-DISPENSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY NETWORK CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-NETWORK.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATWAIV-NETWORK-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATWAIV-NETWORK-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2330-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E364 DESC REQUIRED
      * E523 ADJUST AMT NEGATIVE
      * E659 ADJUST AMT OVER 5000
      * E770 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATWAIV-DESC = SPACES
                   MOVE 'E364' TO WS-REJECT-CODE
               WHEN ATWAIV-ADJUST-AMT < 0
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN ATWAIV-ADJUST-AMT > 5000.00
                   MOVE 'E659' TO WS-REJECT-CODE
               WHEN ATWAIV-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E770' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTERCHG GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2350-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTERCHG-GRID-RATE(1) TO ATWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-INTERCHG-GRID-LOW(WS-TX)
                   MOVE WS-INTERCHG-GRID-RATE(WS-TX) TO ATWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADJUST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2360-FLAG-ADJUST.
           IF ATWAIV-ADJUST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATWAIV-ADJUST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE YTD FEE ON THE INTERCHG YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2370-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATWAIV-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ATWAIV-SETTLE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATWAIV-SUSPEND-FLAG = 'Y'
                   COMPUTE ATWAIV-SETTLE-YTD ROUNDED =
                       ATWAIV-INTERCHG-YTD * ATWAIV-RATE / 200
               WHEN ATWAIV-NETWORK-CD = 'A'
                   MOVE 3.00 TO ATWAIV-SETTLE-YTD
               WHEN ATWAIV-NETWORK-CD = 'C'
                   MOVE 12.00 TO ATWAIV-SETTLE-YTD
               WHEN OTHER
                   COMPUTE ATWAIV-SETTLE-YTD ROUNDED =
                       ATWAIV-INTERCHG-YTD * ATWAIV-RATE / 100
           END-EVALUATE
           IF ATWAIV-SETTLE-YTD > 0 AND ATWAIV-SETTLE-YTD < 5.00
               MOVE 5.00 TO ATWAIV-SETTLE-YTD
           END-IF
           IF ATWAIV-SETTLE-YTD > 35.00
               MOVE 35.00 TO ATWAIV-SETTLE-YTD
           END-IF
           ADD ATWAIV-SETTLE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADJUST YTD.
      *----------------------------------------------------------------*
       2380-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATWAIV-ADJUST-YTD ROUNDED =
                   ATWAIV-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATWAIV-ADJUST-YTD
           END-IF
           ADD ATWAIV-ADJUST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT DISPENSED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2390-SPLIT-DISPENSED.
           COMPUTE WS-WORK-AMT = ATWAIV-DISPENSED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ATWAIV-DISPENSED-YTD - (WS-WORK-AMT *
               2)
           COMPUTE ATWAIV-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E398 DESC REQUIRED
      * E830 DISPENSED YTD NEGATIVE
      * E168 DISPENSED YTD OVER 5000
      * E775 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2400-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATWAIV-DESC = SPACES
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN ATWAIV-DISPENSED-YTD < 0
                   MOVE 'E830' TO WS-REJECT-CODE
               WHEN ATWAIV-DISPENSED-YTD > 5000.00
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN ATWAIV-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E775' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2410-ACCRUE-ADJUST.
           IF ATWAIV-ADJUST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATWAIV-ADJUST-AMT * ATWAIV-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATWAIV-INTERCHG-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2420-FLAG-DISPENSED.
           IF ATWAIV-DISPENSED-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATWAIV-DISPENSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2430-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ADJUST YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2440-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATWAIV-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATWAIV-ADJUST-YTD *
                       ATWAIV-RATE / 200
               WHEN ATWAIV-STATUS-CD = 'DF'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN ATWAIV-STATUS-CD = 'DP'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATWAIV-ADJUST-YTD *
                       ATWAIV-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2450-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATWAIV-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATWAIV-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ATWAIV-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2460-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATWAIV-BRANCH-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATWAIV-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TERMINAL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2470-DESCRIBE-TERMINAL.
           EVALUATE ATWAIV-TERMINAL-CD
               WHEN 'BJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2480-FLAG-SETTLE.
           IF ATWAIV-SETTLE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATWAIV-SETTLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2490-COUNT-ITEM.
           IF ATWAIV-ITEM-CNT < 9999999
               ADD 1 TO ATWAIV-ITEM-CNT
           END-IF
           IF ATWAIV-ITEM-CNT NOT < 12
               MOVE 'Y' TO ATWAIV-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2500-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = ATWAIV-SETTLE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ATWAIV-SETTLE-YTD - (WS-WORK-AMT * 4)
           COMPUTE ATWAIV-ADJUST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATWAIV-BRANCH-CD
                   ADD ATWAIV-SETTLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATWAIV-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATWAIV-SETTLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTERCHG GRID TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2520-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INTERCHG-GRID-RATE(1) TO ATWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATWAIV-SETTLE-AMT NOT < WS-INTERCHG-GRID-LOW(WS-TX)
                   MOVE WS-INTERCHG-GRID-RATE(WS-TX) TO ATWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN NEXT REVIEW DATE IS PAST
      * HX -> YC WHEN NEXT REVIEW DATE IS PAST
      * YC -> ZB WHEN NEXT REVIEW DATE IS PAST
      * ZB -> DU WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-BRANCH.
           EVALUATE ATWAIV-BRANCH-CD
               WHEN 'DU'
                   IF ATWAIV-NEXT-REVIEW-DATE > 0 AND
                       ATWAIV-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATWAIV-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATWAIV-NEXT-REVIEW-DATE > 0 AND
                       ATWAIV-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATWAIV-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATWAIV-NEXT-REVIEW-DATE > 0 AND
                       ATWAIV-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATWAIV-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATWAIV-NEXT-REVIEW-DATE > 0 AND
                       ATWAIV-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATWAIV-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ATWAIV-EFFECTIVE-DATE > 0 AND ATWAIV-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATWAIV-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATWAIV-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATWAIV-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATWAIV-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ATWAIV-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATWAIV-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2550-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ATWAIV-LAST-PAY-DATE > 0 AND ATWAIV-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATWAIV-ADJUST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATWAIV-ADJUST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ATWAIV-ADJUST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATWAIV-ADJUST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATWAIV-ADJUST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SETTLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2560-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATWAIV-SETTLE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2570-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2580-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADJUST YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2590-LIMIT-ADJUST.
           IF ATWAIV-ADJUST-YTD < 25.00
               MOVE 25.00 TO ATWAIV-ADJUST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATWAIV-ADJUST-YTD > 500.00
               MOVE 500.00 TO ATWAIV-ADJUST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2600-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATWAIV-NEXT-REVIEW-DATE > 0 AND ATWAIV-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATWAIV-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2610-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATWAIV-NEXT-REVIEW-DATE > 0 AND ATWAIV-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATWAIV-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATWAIV-INTERCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATWAIV-INTERCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATWAIV-INTERCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATWAIV-INTERCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATWAIV-INTERCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2620-COUNT-PAYMENT.
           IF ATWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO ATWAIV-PAYMENT-CNT
           END-IF
           IF ATWAIV-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO ATWAIV-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTERCHG GRID TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2630-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INTERCHG-GRID-RATE(1) TO ATWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATWAIV-SETTLE-AMT NOT < WS-INTERCHG-GRID-LOW(WS-TX)
                   MOVE WS-INTERCHG-GRID-RATE(WS-TX) TO ATWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADJUST AMT BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATWAIV-TERMINAL-CD
                   ADD ATWAIV-ADJUST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATWAIV-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATWAIV-ADJUST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2650-COUNT-ITEM.
           IF ATWAIV-ITEM-CNT < 9999999
               ADD 1 TO ATWAIV-ITEM-CNT
           END-IF
           IF ATWAIV-ITEM-CNT NOT < 10
               MOVE 'Y' TO ATWAIV-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2660-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATWAIV-TERMINAL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATWAIV-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2670-LIMIT-SETTLE.
           IF ATWAIV-SETTLE-YTD < 25.00
               MOVE 25.00 TO ATWAIV-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATWAIV-SETTLE-YTD > 1000.00
               MOVE 1000.00 TO ATWAIV-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E651 NAME REQUIRED
      * E212 WORK AMT 2 NEGATIVE
      * E254 WORK AMT 2 OVER 10000
      * E930 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2680-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATWAIV-NAME = SPACES
                   MOVE 'E651' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E212' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E254' TO WS-REJECT-CODE
               WHEN ATWAIV-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E930' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2690-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2700-DESCRIBE-BRANCH.
           EVALUATE ATWAIV-BRANCH-CD
               WHEN 'DU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTERCHG GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2710-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INTERCHG-GRID-RATE(1) TO ATWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-INTERCHG-GRID-LOW(WS-TX)
                   MOVE WS-INTERCHG-GRID-RATE(WS-TX) TO ATWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTERCHG GRID TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2720-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INTERCHG-GRID-RATE(1) TO ATWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATWAIV-INTERCHG-YTD NOT < WS-INTERCHG-GRID-LOW(WS-TX)
                   MOVE WS-INTERCHG-GRID-RATE(WS-TX) TO ATWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2730-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATWAIV-TERMINAL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATWAIV-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2740-COUNT-ITEM.
           IF ATWAIV-ITEM-CNT < 9999999
               ADD 1 TO ATWAIV-ITEM-CNT
           END-IF
           IF ATWAIV-ITEM-CNT NOT < 10
               MOVE 'Y' TO ATWAIV-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2750-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATWAIV-ADJUST-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ATWAIV-ADJUST-YTD - (WS-WORK-AMT *
               12)
           COMPUTE ATWAIV-INTERCHG-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR NETWORK CD:
      * A -> C WHEN MATURITY DATE IS PAST
      * C -> H WHEN MATURITY DATE IS PAST
      * H -> R WHEN MATURITY DATE IS PAST
      * R -> S WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2760-SET-NETWORK.
           EVALUATE ATWAIV-NETWORK-CD
               WHEN 'A'
                   IF ATWAIV-MATURITY-DATE > 0 AND ATWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'C' TO ATWAIV-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF ATWAIV-MATURITY-DATE > 0 AND ATWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO ATWAIV-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATWAIV-MATURITY-DATE > 0 AND ATWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO ATWAIV-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ATWAIV-MATURITY-DATE > 0 AND ATWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO ATWAIV-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR NETWORK CD:
      * A -> C WHEN EFFECTIVE DATE IS PAST
      * C -> H WHEN EFFECTIVE DATE IS PAST
      * H -> R WHEN EFFECTIVE DATE IS PAST
      * R -> S WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2770-SET-NETWORK.
           EVALUATE ATWAIV-NETWORK-CD
               WHEN 'A'
                   IF ATWAIV-EFFECTIVE-DATE > 0 AND
                       ATWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'C' TO ATWAIV-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF ATWAIV-EFFECTIVE-DATE > 0 AND
                       ATWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO ATWAIV-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATWAIV-EFFECTIVE-DATE > 0 AND
                       ATWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO ATWAIV-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ATWAIV-EFFECTIVE-DATE > 0 AND
                       ATWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO ATWAIV-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTERCHG YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2780-FLAG-INTERCHG.
           IF ATWAIV-INTERCHG-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATWAIV-INTERCHG-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2790-DESCRIBE-STATUS.
           EVALUATE ATWAIV-STATUS-CD
               WHEN 'DF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ATWAIV-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ATWAIV-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ATWAIV-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ATWAIV EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
