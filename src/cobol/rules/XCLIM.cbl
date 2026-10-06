      *================================================================*
      * PROGRAM  : XCLIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS RULE - DATE RULES. CALLED BY *
      *            THE CHRISTMAS CLUB SAVINGS BATCH AND ONLINE         *
      *            PROGRAMS.                                           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-10-05 PQR  ORIGINAL                                       *
      * 1992-01-16 LMB  CONTRIB STATUS CODES ADDED CHG0703             *
      * 1993-06-08 SKN  BONUS LIMIT REVISED PRB2234                    *
      * 1993-11-08 TWH  NEW WEEKLY REPORT COLUMNS REQ5938              *
      * 1994-09-09 BHS  PENALTY RERUN PROTECTION PRB1390               *
      * 1994-04-08 PQR  NEW MISSED REPORT COLUMNS REQ0180              *
      * 1994-06-13 TWH  CONTRIB TABLE EXPANDED REQ5319                 *
      * 1998-09-17 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-04 TWH  CONTRIB CALCULATION CORRECTED REQ9921          *
      * 2003-06-19 TWH  PENALTY FEE SCHEDULE UPDATE INC4299            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCLIM.
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
                                           VALUE 14.000000.
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
      * WS-PENALTY-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000645500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000213500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000155000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000439500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000473000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000254000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021750000000336000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024250000000641000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024350000000536500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024850000000193500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034850000000726000'.
       01  WS-PENALTY-GRID-TBL REDEFINES WS-PENALTY-GRID-VALUES.
           05  WS-PENALTY-GRID-ROW OCCURS 11 TIMES.
               10  WS-PENALTY-GRID-LOW     PIC 9(09)V99.
               10  WS-PENALTY-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PENALTY-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000284500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000395500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000016000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000166000'.
       01  WS-PENALTY-BAND-TBL REDEFINES WS-PENALTY-BAND-VALUES.
           05  WS-PENALTY-BAND-ROW OCCURS 4 TIMES.
               10  WS-PENALTY-BAND-LOW     PIC 9(09)V99.
               10  WS-PENALTY-BAND-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY XCLIMP.

       PROCEDURE DIVISION USING XCLIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ASSESS-PREFERRED
           PERFORM 2010-COUNT-PAYMENT
           PERFORM 2020-COUNT-PAYMENT
           PERFORM 2030-EDIT-ADDR
           PERFORM 2040-PRORATE-PAYOUT
           PERFORM 2050-EDIT-ADDR
           PERFORM 2060-SPLIT-PENALTY
           PERFORM 2070-SELECT-FEE-PCT
           PERFORM 2080-EDIT-REF-NO
           PERFORM 2090-FLAG-ACCRUED
           PERFORM 2100-SET-PLAN
           PERFORM 2110-LIMIT-WEEKLY
           PERFORM 2120-AGE-NOTICE
           PERFORM 2130-FLAG-ITEM
           PERFORM 2140-COUNT-CYCLE
           PERFORM 2150-AGE-LAST-PAY
           PERFORM 2160-CHECK-OPEN
           PERFORM 2170-ASSESS-SUSPEND
           PERFORM 2180-PRORATE-PAYOUT
           PERFORM 2190-DESCRIBE-CYCLE
           PERFORM 2200-ASSESS-SUSPEND
           PERFORM 2210-SET-PLAN
           PERFORM 2220-ACCRUE-FEE-PCT
           PERFORM 2230-SELECT-DISCOUNT
           PERFORM 2240-SPLIT-GOAL
           PERFORM 2250-ACCRUE-PAYOUT
           PERFORM 2260-SET-CYCLE
           PERFORM 2270-EDIT-REF-NO
           PERFORM 2280-CHECK-NOTICE
           PERFORM 2290-FLAG-WEEKLY
           PERFORM 2300-PRORATE-PAYOUT
           PERFORM 2310-SPLIT-PAYOUT
           PERFORM 2320-FLAG-PENALTY
           PERFORM 2330-CHECK-NOTICE
           PERFORM 2340-CHECK-LAST-PAY
           PERFORM 2350-ACCRUE-DISCOUNT
           PERFORM 2360-CHECK-NOTICE
           PERFORM 2370-ACCRUE-PAYOUT
           PERFORM 2380-AGE-NOTICE
           PERFORM 2390-COUNT-PAYMENT
           PERFORM 2400-ACCUM-PLAN
           PERFORM 2410-ASSESS-STOP
           PERFORM 2420-SET-REGION
           PERFORM 2430-SPLIT-PAYOUT
           PERFORM 2440-PRORATE-ACCRUED
           PERFORM 2450-PRORATE-PAYOUT
           PERFORM 2460-SET-REGION
           PERFORM 2470-SELECT-DISCOUNT
           PERFORM 2480-DESCRIBE-REASON
           PERFORM 2490-CHECK-OPEN
           PERFORM 2500-ASSESS-STOP
           PERFORM 2510-LIMIT-PAYOUT
           PERFORM 2520-AGE-LAST-PAY
           PERFORM 2530-SELECT-FEE-PCT
           PERFORM 2540-SPLIT-GOAL
           PERFORM 2550-FLAG-ITEM
           PERFORM 2560-COUNT-PAYMENT
           PERFORM 2570-CHECK-NOTICE
           PERFORM 2580-SELECT-DISCOUNT
           PERFORM 2590-COUNT-CYCLE
           PERFORM 2600-EDIT-REF-NO
           PERFORM 2610-EDIT-ADDR
           PERFORM 2620-FLAG-PAYOUT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO XCLIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ACCRUED AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2000-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLIM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLIM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLIM-ACCRUED-AMT *
                       XCLIM-DISCOUNT / 200
               WHEN XCLIM-PLAN-CD = 'GN'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN XCLIM-PLAN-CD = 'JT'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLIM-ACCRUED-AMT *
                       XCLIM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-PAYMENT.
           IF XCLIM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLIM-PAYMENT-CNT
           END-IF
           IF XCLIM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO XCLIM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2020-COUNT-PAYMENT.
           IF XCLIM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLIM-PAYMENT-CNT
           END-IF
           IF XCLIM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO XCLIM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E031 ADDR REQUIRED
      * E587 WORK AMT NEGATIVE
      * E881 WORK AMT OVER 25000
      * E607 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2030-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLIM-ADDR = SPACES
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN XCLIM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WEEKLY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYOUT AMT.
      *----------------------------------------------------------------*
       2040-PRORATE-PAYOUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLIM-PAYOUT-AMT ROUNDED =
                   XCLIM-WEEKLY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLIM-PAYOUT-AMT
           END-IF
           ADD XCLIM-PAYOUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E178 ADDR REQUIRED
      * E594 PAYOUT YTD NEGATIVE
      * E481 PAYOUT YTD OVER 10000
      * E185 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLIM-ADDR = SPACES
                   MOVE 'E178' TO WS-REJECT-CODE
               WHEN XCLIM-PAYOUT-YTD < 0
                   MOVE 'E594' TO WS-REJECT-CODE
               WHEN XCLIM-PAYOUT-YTD > 10000.00
                   MOVE 'E481' TO WS-REJECT-CODE
               WHEN XCLIM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E185' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = XCLIM-PENALTY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCLIM-PENALTY-YTD - (WS-WORK-AMT * 4)
           COMPUTE XCLIM-PAYOUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY BAND TIER FOR THE WEEKLY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2070-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-BAND-RATE(1) TO XCLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLIM-WEEKLY-AMT NOT < WS-PENALTY-BAND-LOW(WS-TX)
                   MOVE WS-PENALTY-BAND-RATE(WS-TX) TO XCLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E658 REF NO REQUIRED
      * E031 WORK AMT NEGATIVE
      * E968 WORK AMT OVER 10000
      * E727 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2080-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLIM-REF-NO = SPACES
                   MOVE 'E658' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E968' TO WS-REJECT-CODE
               WHEN XCLIM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E727' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2090-FLAG-ACCRUED.
           IF XCLIM-ACCRUED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLIM-ACCRUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN NOTICE DATE IS PAST
      * JT -> KC WHEN NOTICE DATE IS PAST
      * KC -> KW WHEN NOTICE DATE IS PAST
      * KW -> MX WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2100-SET-PLAN.
           EVALUATE XCLIM-PLAN-CD
               WHEN 'GN'
                   IF XCLIM-NOTICE-DATE > 0 AND XCLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCLIM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCLIM-NOTICE-DATE > 0 AND XCLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCLIM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCLIM-NOTICE-DATE > 0 AND XCLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCLIM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCLIM-NOTICE-DATE > 0 AND XCLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCLIM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WEEKLY AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2110-LIMIT-WEEKLY.
           IF XCLIM-WEEKLY-AMT < 50.00
               MOVE 50.00 TO XCLIM-WEEKLY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLIM-WEEKLY-AMT > 1000.00
               MOVE 1000.00 TO XCLIM-WEEKLY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2120-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCLIM-NOTICE-DATE > 0 AND XCLIM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2130-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-CYCLE.
           IF XCLIM-CYCLE-CNT < 9999999
               ADD 1 TO XCLIM-CYCLE-CNT
           END-IF
           IF XCLIM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO XCLIM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCLIM-LAST-PAY-DATE > 0 AND XCLIM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLIM-WEEKLY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLIM-WEEKLY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLIM-WEEKLY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLIM-WEEKLY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLIM-WEEKLY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2160-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF XCLIM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO XCLIM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PAYOUT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2170-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLIM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLIM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = XCLIM-PAYOUT-AMT *
                       XCLIM-FEE-PCT / 200
               WHEN XCLIM-PLAN-CD = 'GN'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN XCLIM-PLAN-CD = 'JT'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = XCLIM-PAYOUT-AMT *
                       XCLIM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PENALTY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYOUT AMT.
      *----------------------------------------------------------------*
       2180-PRORATE-PAYOUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLIM-PAYOUT-AMT ROUNDED =
                   XCLIM-PENALTY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLIM-PAYOUT-AMT
           END-IF
           ADD XCLIM-PAYOUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2190-DESCRIBE-CYCLE.
           EVALUATE XCLIM-CYCLE-CD
               WHEN 'EM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE PAYOUT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2200-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLIM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLIM-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLIM-STOP-FLAG = 'Y'
                   COMPUTE XCLIM-WEEKLY-AMT ROUNDED = XCLIM-PAYOUT-YTD *
                       XCLIM-DISCOUNT / 200
               WHEN XCLIM-PLAN-CD = 'GN'
                   MOVE 20.00 TO XCLIM-WEEKLY-AMT
               WHEN XCLIM-PLAN-CD = 'JT'
                   MOVE 7.00 TO XCLIM-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLIM-WEEKLY-AMT ROUNDED = XCLIM-PAYOUT-YTD *
                       XCLIM-DISCOUNT / 100
           END-EVALUATE
           IF XCLIM-WEEKLY-AMT > 0 AND XCLIM-WEEKLY-AMT < 10.00
               MOVE 10.00 TO XCLIM-WEEKLY-AMT
           END-IF
           IF XCLIM-WEEKLY-AMT > 35.00
               MOVE 35.00 TO XCLIM-WEEKLY-AMT
           END-IF
           ADD XCLIM-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN LAST PAY DATE IS PAST
      * JT -> KC WHEN LAST PAY DATE IS PAST
      * KC -> KW WHEN LAST PAY DATE IS PAST
      * KW -> MX WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-PLAN.
           EVALUATE XCLIM-PLAN-CD
               WHEN 'GN'
                   IF XCLIM-LAST-PAY-DATE > 0 AND XCLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCLIM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCLIM-LAST-PAY-DATE > 0 AND XCLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCLIM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCLIM-LAST-PAY-DATE > 0 AND XCLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCLIM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCLIM-LAST-PAY-DATE > 0 AND XCLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCLIM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2220-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * XCLIM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLIM-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY BAND TIER FOR THE WEEKLY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2230-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-BAND-RATE(1) TO XCLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLIM-WEEKLY-AMT NOT < WS-PENALTY-BAND-LOW(WS-TX)
                   MOVE WS-PENALTY-BAND-RATE(WS-TX) TO XCLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GOAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2240-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCLIM-GOAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCLIM-GOAL-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCLIM-ACCRUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2250-ACCRUE-PAYOUT.
           IF XCLIM-PAYOUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLIM-PAYOUT-AMT * XCLIM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLIM-PENALTY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN LAST STMT DATE IS PAST
      * FE -> PL WHEN LAST STMT DATE IS PAST
      * PL -> RU WHEN LAST STMT DATE IS PAST
      * RU -> SU WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2260-SET-CYCLE.
           EVALUATE XCLIM-CYCLE-CD
               WHEN 'EM'
                   IF XCLIM-LAST-STMT-DATE > 0 AND XCLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FE' TO XCLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF XCLIM-LAST-STMT-DATE > 0 AND XCLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PL' TO XCLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF XCLIM-LAST-STMT-DATE > 0 AND XCLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RU' TO XCLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF XCLIM-LAST-STMT-DATE > 0 AND XCLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SU' TO XCLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E094 REF NO REQUIRED
      * E267 PENALTY YTD NEGATIVE
      * E144 PENALTY YTD OVER 5000
      * E329 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2270-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLIM-REF-NO = SPACES
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN XCLIM-PENALTY-YTD < 0
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN XCLIM-PENALTY-YTD > 5000.00
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN XCLIM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E329' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2280-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO XCLIM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2290-FLAG-WEEKLY.
           IF XCLIM-WEEKLY-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLIM-WEEKLY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYOUT YTD.
      *----------------------------------------------------------------*
       2300-PRORATE-PAYOUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLIM-PAYOUT-YTD ROUNDED =
                   XCLIM-ACCRUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLIM-PAYOUT-YTD
           END-IF
           ADD XCLIM-PAYOUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PAYOUT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2310-SPLIT-PAYOUT.
           COMPUTE WS-WORK-AMT = XCLIM-PAYOUT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = XCLIM-PAYOUT-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2320-FLAG-PENALTY.
           IF XCLIM-PENALTY-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLIM-PENALTY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2330-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO XCLIM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2340-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCLIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO XCLIM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2350-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * XCLIM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLIM-ACCRUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2360-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO XCLIM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2370-ACCRUE-PAYOUT.
           IF XCLIM-PAYOUT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLIM-PAYOUT-YTD * XCLIM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLIM-ACCRUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2380-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCLIM-NOTICE-DATE > 0 AND XCLIM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLIM-GOAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD XCLIM-GOAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD XCLIM-GOAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLIM-GOAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLIM-GOAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2390-COUNT-PAYMENT.
           IF XCLIM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLIM-PAYMENT-CNT
           END-IF
           IF XCLIM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO XCLIM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2400-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLIM-PLAN-CD
                   ADD XCLIM-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLIM-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLIM-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE ACCRUED AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2410-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLIM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLIM-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLIM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLIM-WEEKLY-AMT ROUNDED = XCLIM-ACCRUED-AMT
                       * XCLIM-DISCOUNT / 200
               WHEN XCLIM-CYCLE-CD = 'EM'
                   MOVE 15.00 TO XCLIM-WEEKLY-AMT
               WHEN XCLIM-CYCLE-CD = 'FE'
                   MOVE 20.00 TO XCLIM-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLIM-WEEKLY-AMT ROUNDED = XCLIM-ACCRUED-AMT
                       * XCLIM-DISCOUNT / 100
           END-EVALUATE
           IF XCLIM-WEEKLY-AMT > 0 AND XCLIM-WEEKLY-AMT < 10.00
               MOVE 10.00 TO XCLIM-WEEKLY-AMT
           END-IF
           IF XCLIM-WEEKLY-AMT > 25.00
               MOVE 25.00 TO XCLIM-WEEKLY-AMT
           END-IF
           ADD XCLIM-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN LAST PAY DATE IS PAST
      * HLS -> KMJ WHEN LAST PAY DATE IS PAST
      * KMJ -> PDA WHEN LAST PAY DATE IS PAST
      * PDA -> RVG WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2420-SET-REGION.
           EVALUATE XCLIM-REGION-CD
               WHEN 'FLM'
                   IF XCLIM-LAST-PAY-DATE > 0 AND XCLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCLIM-LAST-PAY-DATE > 0 AND XCLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCLIM-LAST-PAY-DATE > 0 AND XCLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCLIM-LAST-PAY-DATE > 0 AND XCLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYOUT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2430-SPLIT-PAYOUT.
           COMPUTE WS-WORK-AMT = XCLIM-PAYOUT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = XCLIM-PAYOUT-YTD - (WS-WORK-AMT * 12)
           COMPUTE XCLIM-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2440-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLIM-ACCRUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYOUT YTD.
      *----------------------------------------------------------------*
       2450-PRORATE-PAYOUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLIM-PAYOUT-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLIM-PAYOUT-YTD
           END-IF
           ADD XCLIM-PAYOUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN NOTICE DATE IS PAST
      * HLS -> KMJ WHEN NOTICE DATE IS PAST
      * KMJ -> PDA WHEN NOTICE DATE IS PAST
      * PDA -> RVG WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2460-SET-REGION.
           EVALUATE XCLIM-REGION-CD
               WHEN 'FLM'
                   IF XCLIM-NOTICE-DATE > 0 AND XCLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCLIM-NOTICE-DATE > 0 AND XCLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCLIM-NOTICE-DATE > 0 AND XCLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCLIM-NOTICE-DATE > 0 AND XCLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY GRID TIER FOR THE WEEKLY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-GRID-RATE(1) TO XCLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF XCLIM-WEEKLY-AMT NOT < WS-PENALTY-GRID-LOW(WS-TX)
                   MOVE WS-PENALTY-GRID-RATE(WS-TX) TO XCLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-REASON.
           EVALUATE XCLIM-REASON-CD
               WHEN 'AL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF XCLIM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLIM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE PAYOUT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2500-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLIM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLIM-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLIM-PREFERRED-FLAG = 'Y'
                   COMPUTE XCLIM-WEEKLY-AMT ROUNDED = XCLIM-PAYOUT-AMT *
                       XCLIM-DISCOUNT / 200
               WHEN XCLIM-REGION-CD = 'FLM'
                   MOVE 15.00 TO XCLIM-WEEKLY-AMT
               WHEN XCLIM-REGION-CD = 'HLS'
                   MOVE 15.00 TO XCLIM-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLIM-WEEKLY-AMT ROUNDED = XCLIM-PAYOUT-AMT *
                       XCLIM-DISCOUNT / 100
           END-EVALUATE
           IF XCLIM-WEEKLY-AMT > 0 AND XCLIM-WEEKLY-AMT < 1.00
               MOVE 1.00 TO XCLIM-WEEKLY-AMT
           END-IF
           IF XCLIM-WEEKLY-AMT > 100.00
               MOVE 100.00 TO XCLIM-WEEKLY-AMT
           END-IF
           ADD XCLIM-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYOUT YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2510-LIMIT-PAYOUT.
           IF XCLIM-PAYOUT-YTD < 50.00
               MOVE 50.00 TO XCLIM-PAYOUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLIM-PAYOUT-YTD > 50000.00
               MOVE 50000.00 TO XCLIM-PAYOUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2520-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCLIM-LAST-PAY-DATE > 0 AND XCLIM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLIM-LAST-PAY-DATE TO DTU-DATE-1
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
      * SELECT THE PENALTY GRID TIER FOR THE PAYOUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-GRID-RATE(1) TO XCLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF XCLIM-PAYOUT-AMT NOT < WS-PENALTY-GRID-LOW(WS-TX)
                   MOVE WS-PENALTY-GRID-RATE(WS-TX) TO XCLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2540-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCLIM-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2550-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2560-COUNT-PAYMENT.
           IF XCLIM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLIM-PAYMENT-CNT
           END-IF
           IF XCLIM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO XCLIM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO XCLIM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY GRID TIER FOR THE PENALTY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2580-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-GRID-RATE(1) TO XCLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF XCLIM-PENALTY-YTD NOT < WS-PENALTY-GRID-LOW(WS-TX)
                   MOVE WS-PENALTY-GRID-RATE(WS-TX) TO XCLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-CYCLE.
           IF XCLIM-CYCLE-CNT < 9999999
               ADD 1 TO XCLIM-CYCLE-CNT
           END-IF
           IF XCLIM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO XCLIM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E870 REF NO REQUIRED
      * E135 PAYOUT AMT NEGATIVE
      * E682 PAYOUT AMT OVER 5000
      * E976 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2600-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLIM-REF-NO = SPACES
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN XCLIM-PAYOUT-AMT < 0
                   MOVE 'E135' TO WS-REJECT-CODE
               WHEN XCLIM-PAYOUT-AMT > 5000.00
                   MOVE 'E682' TO WS-REJECT-CODE
               WHEN XCLIM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E016 ADDR REQUIRED
      * E820 WEEKLY AMT NEGATIVE
      * E862 WEEKLY AMT OVER 99999
      * E587 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLIM-ADDR = SPACES
                   MOVE 'E016' TO WS-REJECT-CODE
               WHEN XCLIM-WEEKLY-AMT < 0
                   MOVE 'E820' TO WS-REJECT-CODE
               WHEN XCLIM-WEEKLY-AMT > 99999.00
                   MOVE 'E862' TO WS-REJECT-CODE
               WHEN XCLIM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYOUT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2620-FLAG-PAYOUT.
           IF XCLIM-PAYOUT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLIM-PAYOUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO XCLIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO XCLIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO XCLIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'XCLIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
