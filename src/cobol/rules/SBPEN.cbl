      *================================================================*
      * PROGRAM  : SBPEN                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : SAVINGS BOND SERVICES RULE - VALIDATION. CALLED BY  *
      *            THE SAVINGS BOND SERVICES BATCH AND ONLINE PROGRAMS.*
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-10-10 MKL  ORIGINAL                                       *
      * 1992-03-02 PQR  TAXABLE LIMIT REVISED CHG6503                  *
      * 1993-04-06 SKN  PURCHASE EXCEPTIONS TO OPS QUEUE REQ0104       *
      * 1993-12-06 TWH  FACE FEE SCHEDULE UPDATE CHG1599               *
      * 1996-06-05 GAF  ACCRUAL CUTOFF MOVED CHG0955                   *
      * 1998-05-09 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-02-09 MKL  FACE RERUN PROTECTION CHG0503                  *
      * 1999-10-15 LMB  PERFORMANCE - ISSUE-PRICE READ AHEAD REQ7483   *
      * 2001-12-11 DPS  FACE TABLE EXPANDED CHG3057                    *
      * 2002-02-24 PQR  PURCHASE CUTOFF MOVED INC1584                  *
      * 2003-10-21 DPS  ACCRUAL LIMIT REVISED REQ0506                  *
      * 2006-06-23 CRP  INTEREST STATUS CODES ADDED REQ9276            *
      * 2008-01-27 DPS  PERFORMANCE - FACE READ AHEAD INC6139          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBPEN.
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
                                           VALUE 12.000000.
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
      * WS-ACCRUAL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ACCRUAL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000204000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000194000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000626000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000271500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015750000000864000'.
       01  WS-ACCRUAL-TIER-TBL REDEFINES WS-ACCRUAL-TIER-VALUES.
           05  WS-ACCRUAL-TIER-ROW OCCURS 5 TIMES.
               10  WS-ACCRUAL-TIER-LOW     PIC 9(09)V99.
               10  WS-ACCRUAL-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000817500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000454500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000528000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000847500'.
       01  WS-INTEREST-SCHED-TBL REDEFINES WS-INTEREST-SCHED-VALUES.
           05  WS-INTEREST-SCHED-ROW OCCURS 4 TIMES.
               10  WS-INTEREST-SCHED-LOW   PIC 9(09)V99.
               10  WS-INTEREST-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TAXABLE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAXABLE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000356000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000327500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000397000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000489000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000802500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012250000000277500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000269500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000081000'.
       01  WS-TAXABLE-BAND-TBL REDEFINES WS-TAXABLE-BAND-VALUES.
           05  WS-TAXABLE-BAND-ROW OCCURS 8 TIMES.
               10  WS-TAXABLE-BAND-LOW     PIC 9(09)V99.
               10  WS-TAXABLE-BAND-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SBPENP.

       PROCEDURE DIVISION USING SBPEN-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-COUNT-CYCLE
           PERFORM 2010-PRORATE-PURCHASE
           PERFORM 2020-FLAG-ITEM
           PERFORM 2030-CHECK-NEXT-REVIEW
           PERFORM 2040-PRORATE-ITEM
           PERFORM 2050-ASSESS-OVERRIDE
           PERFORM 2060-SPLIT-REDEEM
           PERFORM 2070-SET-SOURCE
           PERFORM 2080-PRORATE-REDEEM
           PERFORM 2090-ASSESS-PREFERRED
           PERFORM 2100-PRORATE-INTEREST
           PERFORM 2110-SET-BRANCH
           PERFORM 2120-ACCRUE-MARGIN
           PERFORM 2130-AGE-NOTICE
           PERFORM 2140-ASSESS-PREFERRED
           PERFORM 2150-CHECK-LAST-MAINT
           PERFORM 2160-ACCUM-BRANCH
           PERFORM 2170-FLAG-ACCRUAL
           PERFORM 2180-PRORATE-REDEEM
           PERFORM 2190-AGE-LAST-MAINT
           PERFORM 2200-COUNT-PAYMENT
           PERFORM 2210-ACCRUE-PURCHASE
           PERFORM 2220-SET-BRANCH
           PERFORM 2230-LIMIT-PURCHASE
           PERFORM 2240-EDIT-REF-NO
           PERFORM 2250-ACCUM-SERIES
           PERFORM 2260-SET-BRANCH
           PERFORM 2270-CHECK-LAST-MAINT
           PERFORM 2280-ASSESS-HOLD
           PERFORM 2290-EDIT-ADDR
           PERFORM 2300-EDIT-ADDR
           PERFORM 2310-ACCUM-BRANCH
           PERFORM 2320-LIMIT-PURCHASE
           PERFORM 2330-FLAG-ACCRUAL
           PERFORM 2340-ACCUM-STATUS
           PERFORM 2350-EDIT-REF-NO
           PERFORM 2360-EDIT-ADDR
           PERFORM 2370-PRORATE-ACCRUAL
           PERFORM 2380-DESCRIBE-SOURCE
           PERFORM 2390-SELECT-FEE-PCT
           PERFORM 2400-ACCUM-SERIES
           PERFORM 2410-COUNT-PAYMENT
           PERFORM 2420-FLAG-ITEM
           PERFORM 2430-PRORATE-INTEREST
           PERFORM 2440-CHECK-NEXT-DUE
           PERFORM 2450-SET-BRANCH
           PERFORM 2460-ACCRUE-FEE-PCT
           PERFORM 2470-ASSESS-HOLD
           PERFORM 2480-CHECK-NOTICE
           PERFORM 2490-AGE-NOTICE
           PERFORM 2500-LIMIT-ITEM
           PERFORM 2510-ACCUM-SERIES
           PERFORM 2520-ACCRUE-INTEREST
           PERFORM 2530-DESCRIBE-STATUS
           PERFORM 2540-LIMIT-ACCRUAL
           PERFORM 2550-ACCRUE-REDEEM
           PERFORM 2560-ACCUM-STATUS
           PERFORM 2570-ACCUM-STATUS
           PERFORM 2580-SPLIT-PURCHASE
           PERFORM 2590-DESCRIBE-SOURCE
           PERFORM 2600-ACCUM-SOURCE
           PERFORM 2610-EDIT-REF-NO
           PERFORM 2620-CHECK-NOTICE
           PERFORM 2630-CHECK-NEXT-REVIEW
           PERFORM 2640-PRORATE-ACCRUAL
           PERFORM 2650-PRORATE-ACCRUAL
           PERFORM 2660-COUNT-PAYMENT
           PERFORM 2670-DESCRIBE-SERIES
           PERFORM 2680-COUNT-CYCLE
           PERFORM 2690-LIMIT-ACCRUAL
           PERFORM 2700-DESCRIBE-SERIES
           PERFORM 2710-PRORATE-ITEM
           PERFORM 2720-ACCRUE-ACCRUAL
           PERFORM 2730-LIMIT-ACCRUAL
           PERFORM 2740-SELECT-FEE-PCT
           PERFORM 2750-LIMIT-ITEM
           PERFORM 2760-SET-SERIES
           PERFORM 2770-ASSESS-OVERRIDE
           PERFORM 2780-EDIT-ADDR
           PERFORM 2790-PRORATE-REDEEM
           PERFORM 2800-DESCRIBE-STATUS
           PERFORM 2810-COUNT-CYCLE
           PERFORM 2820-PRORATE-REDEEM
           PERFORM 2830-ASSESS-PREFERRED
           PERFORM 2840-SPLIT-INTEREST
           PERFORM 2850-SET-SOURCE
           PERFORM 2860-PRORATE-ACCRUAL
           PERFORM 2870-FLAG-ACCRUAL
           PERFORM 2880-LIMIT-ACCRUAL
           PERFORM 2890-FLAG-ACCRUAL
           PERFORM 2900-FLAG-PURCHASE
           PERFORM 2910-SPLIT-REDEEM
           PERFORM 2920-AGE-NEXT-DUE
           PERFORM 2930-PRORATE-ACCRUAL
           PERFORM 2940-SET-STATUS
           PERFORM 2950-LIMIT-INTEREST
           PERFORM 2960-AGE-LAST-MAINT
           PERFORM 2970-SELECT-MARGIN
           PERFORM 2980-PRORATE-INTEREST
           PERFORM 2990-FLAG-PURCHASE
           PERFORM 3000-LIMIT-REDEEM
           PERFORM 3010-ASSESS-OVERRIDE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SBPEN-RETURN-CODE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2000-COUNT-CYCLE.
           IF SBPEN-CYCLE-CNT < 9999999
               ADD 1 TO SBPEN-CYCLE-CNT
           END-IF
           IF SBPEN-CYCLE-CNT NOT < 5
               MOVE 'Y' TO SBPEN-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PURCHASE YTD.
      *----------------------------------------------------------------*
       2010-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-PURCHASE-YTD ROUNDED =
                   SBPEN-ACCRUAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBPEN-PURCHASE-YTD
           END-IF
           ADD SBPEN-PURCHASE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2020-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2030-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SBPEN-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2040-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2050-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SBPEN-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SBPEN-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBPEN-HOLD-FLAG = 'Y'
                   COMPUTE SBPEN-INTEREST-YTD ROUNDED = WS-WORK-AMT *
                       SBPEN-MARGIN / 200
               WHEN SBPEN-BRANCH-CD = 'BD'
                   MOVE 7.00 TO SBPEN-INTEREST-YTD
               WHEN SBPEN-BRANCH-CD = 'FC'
                   MOVE 15.00 TO SBPEN-INTEREST-YTD
               WHEN OTHER
                   COMPUTE SBPEN-INTEREST-YTD ROUNDED = WS-WORK-AMT *
                       SBPEN-MARGIN / 100
           END-EVALUATE
           IF SBPEN-INTEREST-YTD > 0 AND SBPEN-INTEREST-YTD < 2.00
               MOVE 2.00 TO SBPEN-INTEREST-YTD
           END-IF
           IF SBPEN-INTEREST-YTD > 35.00
               MOVE 35.00 TO SBPEN-INTEREST-YTD
           END-IF
           ADD SBPEN-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT REDEEM AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBPEN-REDEEM-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBPEN-REDEEM-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBPEN-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN NOTICE DATE IS PAST
      * J -> L WHEN NOTICE DATE IS PAST
      * L -> U WHEN NOTICE DATE IS PAST
      * U -> E WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2070-SET-SOURCE.
           EVALUATE SBPEN-SOURCE-CD
               WHEN 'E'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO SBPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SBPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SBPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO SBPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ACCRUAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REDEEM AMT.
      *----------------------------------------------------------------*
       2080-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-REDEEM-AMT ROUNDED =
                   SBPEN-ACCRUAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBPEN-REDEEM-AMT
           END-IF
           ADD SBPEN-REDEEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL YTD FEE ON THE INTEREST YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2090-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SBPEN-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SBPEN-ACCRUAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBPEN-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBPEN-ACCRUAL-YTD ROUNDED =
                       SBPEN-INTEREST-YTD * SBPEN-FEE-PCT / 200
               WHEN SBPEN-STATUS-CD = 'CS'
                   MOVE 15.00 TO SBPEN-ACCRUAL-YTD
               WHEN SBPEN-STATUS-CD = 'HF'
                   MOVE 12.00 TO SBPEN-ACCRUAL-YTD
               WHEN OTHER
                   COMPUTE SBPEN-ACCRUAL-YTD ROUNDED =
                       SBPEN-INTEREST-YTD * SBPEN-FEE-PCT / 100
           END-EVALUATE
           IF SBPEN-ACCRUAL-YTD > 0 AND SBPEN-ACCRUAL-YTD < 5.00
               MOVE 5.00 TO SBPEN-ACCRUAL-YTD
           END-IF
           IF SBPEN-ACCRUAL-YTD > 250.00
               MOVE 250.00 TO SBPEN-ACCRUAL-YTD
           END-IF
           ADD SBPEN-ACCRUAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE ACCRUAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       2100-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-INTEREST-YTD ROUNDED =
                   SBPEN-ACCRUAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBPEN-INTEREST-YTD
           END-IF
           ADD SBPEN-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN NEXT DUE DATE IS PAST
      * FC -> KR WHEN NEXT DUE DATE IS PAST
      * KR -> MP WHEN NEXT DUE DATE IS PAST
      * MP -> VJ WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2110-SET-BRANCH.
           EVALUATE SBPEN-BRANCH-CD
               WHEN 'BD'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MP' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MP'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2120-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SBPEN-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO SBPEN-ACCRUAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2130-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBPEN-ACCRUAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBPEN-ACCRUAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBPEN-ACCRUAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBPEN-ACCRUAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBPEN-ACCRUAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REDEEM FEE ON THE INTEREST YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2140-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SBPEN-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SBPEN-REDEEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBPEN-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBPEN-REDEEM-AMT ROUNDED = SBPEN-INTEREST-YTD
                       * SBPEN-FEE-PCT / 200
               WHEN SBPEN-SERIES-CD = 'DD'
                   MOVE 5.00 TO SBPEN-REDEEM-AMT
               WHEN SBPEN-SERIES-CD = 'DF'
                   MOVE 12.00 TO SBPEN-REDEEM-AMT
               WHEN OTHER
                   COMPUTE SBPEN-REDEEM-AMT ROUNDED = SBPEN-INTEREST-YTD
                       * SBPEN-FEE-PCT / 100
           END-EVALUATE
           IF SBPEN-REDEEM-AMT > 0 AND SBPEN-REDEEM-AMT < 5.00
               MOVE 5.00 TO SBPEN-REDEEM-AMT
           END-IF
           IF SBPEN-REDEEM-AMT > 250.00
               MOVE 250.00 TO SBPEN-REDEEM-AMT
           END-IF
           ADD SBPEN-REDEEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2150-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SBPEN-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2160-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBPEN-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBPEN-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2170-FLAG-ACCRUAL.
           IF SBPEN-ACCRUAL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBPEN-ACCRUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REDEEM AMT.
      *----------------------------------------------------------------*
       2180-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-REDEEM-AMT ROUNDED =
                   SBPEN-ACCRUAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBPEN-REDEEM-AMT
           END-IF
           ADD SBPEN-REDEEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2190-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-LAST-MAINT-DATE > 0 AND SBPEN-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBPEN-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBPEN-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBPEN-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBPEN-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBPEN-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-PAYMENT.
           IF SBPEN-PAYMENT-CNT < 9999999
               ADD 1 TO SBPEN-PAYMENT-CNT
           END-IF
           IF SBPEN-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO SBPEN-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2210-ACCRUE-PURCHASE.
           IF SBPEN-PURCHASE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBPEN-PURCHASE-AMT * SBPEN-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN NEXT DUE DATE IS PAST
      * FC -> KR WHEN NEXT DUE DATE IS PAST
      * KR -> MP WHEN NEXT DUE DATE IS PAST
      * MP -> VJ WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2220-SET-BRANCH.
           EVALUATE SBPEN-BRANCH-CD
               WHEN 'BD'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MP' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MP'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PURCHASE YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2230-LIMIT-PURCHASE.
           IF SBPEN-PURCHASE-YTD < 25.00
               MOVE 25.00 TO SBPEN-PURCHASE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBPEN-PURCHASE-YTD > 2500.00
               MOVE 2500.00 TO SBPEN-PURCHASE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E417 REF NO REQUIRED
      * E147 PURCHASE AMT NEGATIVE
      * E526 PURCHASE AMT OVER 10000
      * E567 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2240-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBPEN-REF-NO = SPACES
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN SBPEN-PURCHASE-AMT < 0
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN SBPEN-PURCHASE-AMT > 10000.00
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN SBPEN-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E567' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM AMT BY SERIES CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-SERIES.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBPEN-SERIES-CD
                   ADD SBPEN-REDEEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBPEN-SERIES-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBPEN-REDEEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN NEXT DUE DATE IS PAST
      * FC -> KR WHEN NEXT DUE DATE IS PAST
      * KR -> MP WHEN NEXT DUE DATE IS PAST
      * MP -> VJ WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2260-SET-BRANCH.
           EVALUATE SBPEN-BRANCH-CD
               WHEN 'BD'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MP' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MP'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2270-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SBPEN-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE PURCHASE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2280-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBPEN-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBPEN-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE SBPEN-PURCHASE-YTD ROUNDED =
                       SBPEN-PURCHASE-AMT * SBPEN-MARGIN / 200
               WHEN SBPEN-SOURCE-CD = 'E'
                   MOVE 7.00 TO SBPEN-PURCHASE-YTD
               WHEN SBPEN-SOURCE-CD = 'J'
                   MOVE 5.00 TO SBPEN-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE SBPEN-PURCHASE-YTD ROUNDED =
                       SBPEN-PURCHASE-AMT * SBPEN-MARGIN / 100
           END-EVALUATE
           IF SBPEN-PURCHASE-YTD > 0 AND SBPEN-PURCHASE-YTD < 2.00
               MOVE 2.00 TO SBPEN-PURCHASE-YTD
           END-IF
           IF SBPEN-PURCHASE-YTD > 100.00
               MOVE 100.00 TO SBPEN-PURCHASE-YTD
           END-IF
           ADD SBPEN-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E144 ADDR REQUIRED
      * E315 ACCRUAL YTD NEGATIVE
      * E838 ACCRUAL YTD OVER 5000
      * E066 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2290-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBPEN-ADDR = SPACES
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN SBPEN-ACCRUAL-YTD < 0
                   MOVE 'E315' TO WS-REJECT-CODE
               WHEN SBPEN-ACCRUAL-YTD > 5000.00
                   MOVE 'E838' TO WS-REJECT-CODE
               WHEN SBPEN-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E146 ADDR REQUIRED
      * E698 PURCHASE AMT NEGATIVE
      * E518 PURCHASE AMT OVER 250000
      * E196 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2300-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBPEN-ADDR = SPACES
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN SBPEN-PURCHASE-AMT < 0
                   MOVE 'E698' TO WS-REJECT-CODE
               WHEN SBPEN-PURCHASE-AMT > 250000.00
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN SBPEN-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2310-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBPEN-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBPEN-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2320-LIMIT-PURCHASE.
           IF SBPEN-PURCHASE-AMT < 0.00
               MOVE 0.00 TO SBPEN-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBPEN-PURCHASE-AMT > 5000.00
               MOVE 5000.00 TO SBPEN-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2330-FLAG-ACCRUAL.
           IF SBPEN-ACCRUAL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBPEN-ACCRUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBPEN-STATUS-CD
                   ADD SBPEN-PURCHASE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBPEN-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBPEN-PURCHASE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E108 REF NO REQUIRED
      * E269 ACCRUAL YTD NEGATIVE
      * E993 ACCRUAL YTD OVER 99999
      * E068 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2350-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBPEN-REF-NO = SPACES
                   MOVE 'E108' TO WS-REJECT-CODE
               WHEN SBPEN-ACCRUAL-YTD < 0
                   MOVE 'E269' TO WS-REJECT-CODE
               WHEN SBPEN-ACCRUAL-YTD > 99999.00
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN SBPEN-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E813 ADDR REQUIRED
      * E053 REDEEM AMT NEGATIVE
      * E494 REDEEM AMT OVER 250000
      * E743 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2360-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBPEN-ADDR = SPACES
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN SBPEN-REDEEM-AMT < 0
                   MOVE 'E053' TO WS-REJECT-CODE
               WHEN SBPEN-REDEEM-AMT > 250000.00
                   MOVE 'E494' TO WS-REJECT-CODE
               WHEN SBPEN-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E743' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUAL AMT.
      *----------------------------------------------------------------*
       2370-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-ACCRUAL-AMT ROUNDED =
                   SBPEN-INTEREST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBPEN-ACCRUAL-AMT
           END-IF
           ADD SBPEN-ACCRUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-SOURCE.
           EVALUATE SBPEN-SOURCE-CD
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'J'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TAXABLE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2390-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAXABLE-BAND-RATE(1) TO SBPEN-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-TAXABLE-BAND-LOW(WS-TX)
                   MOVE WS-TAXABLE-BAND-RATE(WS-TX) TO SBPEN-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBPEN-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBPEN-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUAL YTD BY SERIES CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2400-ACCUM-SERIES.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBPEN-SERIES-CD
                   ADD SBPEN-ACCRUAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBPEN-SERIES-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBPEN-ACCRUAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-PAYMENT.
           IF SBPEN-PAYMENT-CNT < 9999999
               ADD 1 TO SBPEN-PAYMENT-CNT
           END-IF
           IF SBPEN-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO SBPEN-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2420-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       2430-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-INTEREST-YTD ROUNDED =
                   SBPEN-PURCHASE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBPEN-INTEREST-YTD
           END-IF
           ADD SBPEN-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SBPEN-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN LAST MAINT DATE IS PAST
      * FC -> KR WHEN LAST MAINT DATE IS PAST
      * KR -> MP WHEN LAST MAINT DATE IS PAST
      * MP -> VJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2450-SET-BRANCH.
           EVALUATE SBPEN-BRANCH-CD
               WHEN 'BD'
                   IF SBPEN-LAST-MAINT-DATE > 0 AND
                       SBPEN-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FC' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF SBPEN-LAST-MAINT-DATE > 0 AND
                       SBPEN-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KR' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF SBPEN-LAST-MAINT-DATE > 0 AND
                       SBPEN-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'MP' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MP'
                   IF SBPEN-LAST-MAINT-DATE > 0 AND
                       SBPEN-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBPEN-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SBPEN-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBPEN-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE ACCRUAL AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2470-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBPEN-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBPEN-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE SBPEN-PURCHASE-YTD ROUNDED =
                       SBPEN-ACCRUAL-AMT * SBPEN-FEE-PCT / 200
               WHEN SBPEN-BRANCH-CD = 'BD'
                   MOVE 5.00 TO SBPEN-PURCHASE-YTD
               WHEN SBPEN-BRANCH-CD = 'FC'
                   MOVE 5.00 TO SBPEN-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE SBPEN-PURCHASE-YTD ROUNDED =
                       SBPEN-ACCRUAL-AMT * SBPEN-FEE-PCT / 100
           END-EVALUATE
           IF SBPEN-PURCHASE-YTD > 0 AND SBPEN-PURCHASE-YTD < 5.00
               MOVE 5.00 TO SBPEN-PURCHASE-YTD
           END-IF
           IF SBPEN-PURCHASE-YTD > 250.00
               MOVE 250.00 TO SBPEN-PURCHASE-YTD
           END-IF
           ADD SBPEN-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2480-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SBPEN-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2490-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBPEN-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD SBPEN-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SBPEN-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBPEN-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBPEN-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2500-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM AMT BY SERIES CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-SERIES.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBPEN-SERIES-CD
                   ADD SBPEN-REDEEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBPEN-SERIES-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBPEN-REDEEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-INTEREST.
           IF SBPEN-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBPEN-INTEREST-YTD * SBPEN-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2530-DESCRIBE-STATUS.
           EVALUATE SBPEN-STATUS-CD
               WHEN 'CS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCRUAL AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2540-LIMIT-ACCRUAL.
           IF SBPEN-ACCRUAL-AMT < 100.00
               MOVE 100.00 TO SBPEN-ACCRUAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBPEN-ACCRUAL-AMT > 500.00
               MOVE 500.00 TO SBPEN-ACCRUAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2550-ACCRUE-REDEEM.
           IF SBPEN-REDEEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBPEN-REDEEM-AMT * SBPEN-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBPEN-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBPEN-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBPEN-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2570-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBPEN-STATUS-CD
                   ADD SBPEN-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBPEN-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBPEN-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2580-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = SBPEN-PURCHASE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SBPEN-PURCHASE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE SBPEN-ACCRUAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2590-DESCRIBE-SOURCE.
           EVALUATE SBPEN-SOURCE-CD
               WHEN 'E'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2600-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBPEN-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBPEN-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E684 REF NO REQUIRED
      * E305 PURCHASE AMT NEGATIVE
      * E162 PURCHASE AMT OVER 1000000
      * E686 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBPEN-REF-NO = SPACES
                   MOVE 'E684' TO WS-REJECT-CODE
               WHEN SBPEN-PURCHASE-AMT < 0
                   MOVE 'E305' TO WS-REJECT-CODE
               WHEN SBPEN-PURCHASE-AMT > 1000000.00
                   MOVE 'E162' TO WS-REJECT-CODE
               WHEN SBPEN-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2620-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SBPEN-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SBPEN-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUAL YTD.
      *----------------------------------------------------------------*
       2640-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-ACCRUAL-YTD ROUNDED =
                   SBPEN-ACCRUAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBPEN-ACCRUAL-YTD
           END-IF
           ADD SBPEN-ACCRUAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ACCRUAL AMT.
      *----------------------------------------------------------------*
       2650-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-ACCRUAL-AMT ROUNDED =
                   SBPEN-PURCHASE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBPEN-ACCRUAL-AMT
           END-IF
           ADD SBPEN-ACCRUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2660-COUNT-PAYMENT.
           IF SBPEN-PAYMENT-CNT < 9999999
               ADD 1 TO SBPEN-PAYMENT-CNT
           END-IF
           IF SBPEN-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO SBPEN-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2670-DESCRIBE-SERIES.
           EVALUATE SBPEN-SERIES-CD
               WHEN 'DD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2680-COUNT-CYCLE.
           IF SBPEN-CYCLE-CNT < 9999999
               ADD 1 TO SBPEN-CYCLE-CNT
           END-IF
           IF SBPEN-CYCLE-CNT NOT < 6
               MOVE 'Y' TO SBPEN-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2690-LIMIT-ACCRUAL.
           IF SBPEN-ACCRUAL-AMT < 50.00
               MOVE 50.00 TO SBPEN-ACCRUAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBPEN-ACCRUAL-AMT > 50000.00
               MOVE 50000.00 TO SBPEN-ACCRUAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2700-DESCRIBE-SERIES.
           EVALUATE SBPEN-SERIES-CD
               WHEN 'DD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2710-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUAL AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2720-ACCRUE-ACCRUAL.
           IF SBPEN-ACCRUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBPEN-ACCRUAL-AMT * SBPEN-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBPEN-PURCHASE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2730-LIMIT-ACCRUAL.
           IF SBPEN-ACCRUAL-YTD < 25.00
               MOVE 25.00 TO SBPEN-ACCRUAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBPEN-ACCRUAL-YTD > 500.00
               MOVE 500.00 TO SBPEN-ACCRUAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUAL TIER TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2740-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUAL-TIER-RATE(1) TO SBPEN-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBPEN-INTEREST-YTD NOT < WS-ACCRUAL-TIER-LOW(WS-TX)
                   MOVE WS-ACCRUAL-TIER-RATE(WS-TX) TO SBPEN-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBPEN-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBPEN-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2750-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN NOTICE DATE IS PAST
      * DF -> SS WHEN NOTICE DATE IS PAST
      * SS -> VJ WHEN NOTICE DATE IS PAST
      * VJ -> VX WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2760-SET-SERIES.
           EVALUATE SBPEN-SERIES-CD
               WHEN 'DD'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO SBPEN-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SS' TO SBPEN-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SS'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBPEN-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJ'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO SBPEN-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE REDEEM AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2770-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SBPEN-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SBPEN-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE SBPEN-INTEREST-YTD ROUNDED = SBPEN-REDEEM-AMT
                       * SBPEN-MARGIN / 200
               WHEN SBPEN-SERIES-CD = 'DD'
                   MOVE 7.00 TO SBPEN-INTEREST-YTD
               WHEN SBPEN-SERIES-CD = 'DF'
                   MOVE 15.00 TO SBPEN-INTEREST-YTD
               WHEN OTHER
                   COMPUTE SBPEN-INTEREST-YTD ROUNDED = SBPEN-REDEEM-AMT
                       * SBPEN-MARGIN / 100
           END-EVALUATE
           IF SBPEN-INTEREST-YTD > 0 AND SBPEN-INTEREST-YTD < 2.00
               MOVE 2.00 TO SBPEN-INTEREST-YTD
           END-IF
           IF SBPEN-INTEREST-YTD > 150.00
               MOVE 150.00 TO SBPEN-INTEREST-YTD
           END-IF
           ADD SBPEN-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E512 ADDR REQUIRED
      * E529 ACCRUAL YTD NEGATIVE
      * E922 ACCRUAL YTD OVER 25000
      * E481 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2780-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBPEN-ADDR = SPACES
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN SBPEN-ACCRUAL-YTD < 0
                   MOVE 'E529' TO WS-REJECT-CODE
               WHEN SBPEN-ACCRUAL-YTD > 25000.00
                   MOVE 'E922' TO WS-REJECT-CODE
               WHEN SBPEN-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E481' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REDEEM AMT.
      *----------------------------------------------------------------*
       2790-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-REDEEM-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBPEN-REDEEM-AMT
           END-IF
           ADD SBPEN-REDEEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2800-DESCRIBE-STATUS.
           EVALUATE SBPEN-STATUS-CD
               WHEN 'CS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2810-COUNT-CYCLE.
           IF SBPEN-CYCLE-CNT < 9999999
               ADD 1 TO SBPEN-CYCLE-CNT
           END-IF
           IF SBPEN-CYCLE-CNT NOT < 12
               MOVE 'Y' TO SBPEN-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REDEEM AMT.
      *----------------------------------------------------------------*
       2820-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-REDEEM-AMT ROUNDED =
                   SBPEN-ACCRUAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBPEN-REDEEM-AMT
           END-IF
           ADD SBPEN-REDEEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE REDEEM FEE ON THE ACCRUAL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2830-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SBPEN-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SBPEN-REDEEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBPEN-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBPEN-REDEEM-AMT ROUNDED = SBPEN-ACCRUAL-YTD
                       * SBPEN-MARGIN / 200
               WHEN SBPEN-BRANCH-CD = 'BD'
                   MOVE 10.00 TO SBPEN-REDEEM-AMT
               WHEN SBPEN-BRANCH-CD = 'FC'
                   MOVE 15.00 TO SBPEN-REDEEM-AMT
               WHEN OTHER
                   COMPUTE SBPEN-REDEEM-AMT ROUNDED = SBPEN-ACCRUAL-YTD
                       * SBPEN-MARGIN / 100
           END-EVALUATE
           IF SBPEN-REDEEM-AMT > 0 AND SBPEN-REDEEM-AMT < 2.00
               MOVE 2.00 TO SBPEN-REDEEM-AMT
           END-IF
           IF SBPEN-REDEEM-AMT > 35.00
               MOVE 35.00 TO SBPEN-REDEEM-AMT
           END-IF
           ADD SBPEN-REDEEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2840-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = SBPEN-INTEREST-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SBPEN-INTEREST-YTD - (WS-WORK-AMT *
               3)
           COMPUTE SBPEN-ACCRUAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN NOTICE DATE IS PAST
      * J -> L WHEN NOTICE DATE IS PAST
      * L -> U WHEN NOTICE DATE IS PAST
      * U -> E WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2850-SET-SOURCE.
           EVALUATE SBPEN-SOURCE-CD
               WHEN 'E'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO SBPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SBPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SBPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBPEN-NOTICE-DATE > 0 AND SBPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO SBPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUAL YTD.
      *----------------------------------------------------------------*
       2860-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-ACCRUAL-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBPEN-ACCRUAL-YTD
           END-IF
           ADD SBPEN-ACCRUAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCRUAL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2870-FLAG-ACCRUAL.
           IF SBPEN-ACCRUAL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBPEN-ACCRUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2880-LIMIT-ACCRUAL.
           IF SBPEN-ACCRUAL-AMT < 0.00
               MOVE 0.00 TO SBPEN-ACCRUAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBPEN-ACCRUAL-AMT > 50000.00
               MOVE 50000.00 TO SBPEN-ACCRUAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2890-FLAG-ACCRUAL.
           IF SBPEN-ACCRUAL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBPEN-ACCRUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2900-FLAG-PURCHASE.
           IF SBPEN-PURCHASE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBPEN-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2910-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE SBPEN-REDEEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2920-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBPEN-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBPEN-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBPEN-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBPEN-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBPEN-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUAL AMT.
      *----------------------------------------------------------------*
       2930-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-ACCRUAL-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBPEN-ACCRUAL-AMT
           END-IF
           ADD SBPEN-ACCRUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CS -> HF WHEN NEXT DUE DATE IS PAST
      * HF -> JZ WHEN NEXT DUE DATE IS PAST
      * JZ -> CS WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2940-SET-STATUS.
           EVALUATE SBPEN-STATUS-CD
               WHEN 'CS'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO SBPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SBPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SBPEN-NEXT-DUE-DATE > 0 AND SBPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CS' TO SBPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2950-LIMIT-INTEREST.
           IF SBPEN-INTEREST-YTD < 100.00
               MOVE 100.00 TO SBPEN-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBPEN-INTEREST-YTD > 10000.00
               MOVE 10000.00 TO SBPEN-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2960-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBPEN-LAST-MAINT-DATE > 0 AND SBPEN-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBPEN-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBPEN-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBPEN-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBPEN-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBPEN-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBPEN-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2970-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO SBPEN-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO SBPEN-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBPEN-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBPEN-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       2980-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBPEN-INTEREST-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBPEN-INTEREST-YTD
           END-IF
           ADD SBPEN-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PURCHASE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2990-FLAG-PURCHASE.
           IF SBPEN-PURCHASE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBPEN-PURCHASE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * REDEEM AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-REDEEM.
           IF SBPEN-REDEEM-AMT < 50.00
               MOVE 50.00 TO SBPEN-REDEEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBPEN-REDEEM-AMT > 5000.00
               MOVE 5000.00 TO SBPEN-REDEEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL YTD FEE ON THE PURCHASE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3010-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SBPEN-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SBPEN-ACCRUAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE SBPEN-ACCRUAL-YTD ROUNDED =
                       SBPEN-PURCHASE-YTD * SBPEN-FEE-PCT / 200
               WHEN SBPEN-STATUS-CD = 'CS'
                   MOVE 7.00 TO SBPEN-ACCRUAL-YTD
               WHEN SBPEN-STATUS-CD = 'HF'
                   MOVE 5.00 TO SBPEN-ACCRUAL-YTD
               WHEN OTHER
                   COMPUTE SBPEN-ACCRUAL-YTD ROUNDED =
                       SBPEN-PURCHASE-YTD * SBPEN-FEE-PCT / 100
           END-EVALUATE
           IF SBPEN-ACCRUAL-YTD > 0 AND SBPEN-ACCRUAL-YTD < 5.00
               MOVE 5.00 TO SBPEN-ACCRUAL-YTD
           END-IF
           IF SBPEN-ACCRUAL-YTD > 100.00
               MOVE 100.00 TO SBPEN-ACCRUAL-YTD
           END-IF
           ADD SBPEN-ACCRUAL-YTD TO WS-TOTAL-FEES.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SBPEN-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SBPEN-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SBPEN-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SBPEN EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
