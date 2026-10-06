      *================================================================*
      * PROGRAM  : ESVAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MORTGAGE ESCROW RULE - ELIGIBILITY. CALLED BY THE   *
      *            MORTGAGE ESCROW BATCH AND ONLINE PROGRAMS.          *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-04-26 RJT  ORIGINAL                                       *
      * 1999-09-06 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-10-28 SKN  INSURANCE LIMIT REVISED PRB2226                *
      * 2013-01-04 RJT  CUSHION NOTICE TEXT PER LEGAL PRB2474          *
      * 2013-07-08 RJT  SHORTAGE EXCEPTIONS TO OPS QUEUE INC7617       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESVAL.
       AUTHOR. B H SANDERS.

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
      * WS-PAYMENT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000325000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000407000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000403000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000366500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000186000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015300000000565000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017800000000251000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017900000000117000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027900000000238500'.
       01  WS-PAYMENT-GRID-TBL REDEFINES WS-PAYMENT-GRID-VALUES.
           05  WS-PAYMENT-GRID-ROW OCCURS 9 TIMES.
               10  WS-PAYMENT-GRID-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISBURSE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISBURSE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000885000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000769000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000063500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000868000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000215500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025250000000012000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025300000000499000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030300000000260500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030550000000767000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031550000000461000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032550000000450000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00037550000000136000'.
       01  WS-DISBURSE-BAND-TBL REDEFINES WS-DISBURSE-BAND-VALUES.
           05  WS-DISBURSE-BAND-ROW OCCURS 12 TIMES.
               10  WS-DISBURSE-BAND-LOW    PIC 9(09)V99.
               10  WS-DISBURSE-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ESVALP.

       PROCEDURE DIVISION USING ESVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SET-REGION
           PERFORM 2010-CHECK-NEXT-DUE
           PERFORM 2020-LIMIT-INSURANCE
           PERFORM 2030-ASSESS-HOLD
           PERFORM 2040-FLAG-DISBURSE
           PERFORM 2050-PRORATE-SHORTAGE
           PERFORM 2060-AGE-NOTICE
           PERFORM 2070-LIMIT-PAYMENT
           PERFORM 2080-ACCUM-REGION
           PERFORM 2090-ACCRUE-SURPLUS
           PERFORM 2100-FLAG-PAYMENT
           PERFORM 2110-AGE-EFFECTIVE
           PERFORM 2120-SET-REGION
           PERFORM 2130-DESCRIBE-STATUS
           PERFORM 2140-SPLIT-INSURANCE
           PERFORM 2150-DESCRIBE-ANALYSIS
           PERFORM 2160-SET-REASON
           PERFORM 2170-COUNT-RETRY
           PERFORM 2180-CHECK-NEXT-DUE
           PERFORM 2190-DESCRIBE-STATUS
           PERFORM 2200-AGE-NEXT-DUE
           PERFORM 2210-AGE-NOTICE
           PERFORM 2220-CHECK-CLOSE
           PERFORM 2230-EDIT-CONTACT
           PERFORM 2240-ASSESS-PREFERRED
           PERFORM 2250-EDIT-CUST-ID
           PERFORM 2260-DESCRIBE-STATUS
           PERFORM 2270-ACCUM-REGION
           PERFORM 2280-COUNT-CYCLE
           PERFORM 2290-COUNT-CYCLE
           PERFORM 2300-SPLIT-SHORTAGE
           PERFORM 2310-AGE-EFFECTIVE
           PERFORM 2320-EDIT-CONTACT
           PERFORM 2330-PRORATE-DISBURSE
           PERFORM 2340-LIMIT-CUSHION
           PERFORM 2350-DESCRIBE-REGION
           PERFORM 2360-SELECT-DISCOUNT
           PERFORM 2370-DESCRIBE-STATUS
           PERFORM 2380-ASSESS-SUSPEND
           PERFORM 2390-ASSESS-PREFERRED
           PERFORM 2400-EDIT-CUST-ID
           PERFORM 2410-SET-REGION
           PERFORM 2420-SET-ANALYSIS
           PERFORM 2430-SPLIT-CUSHION
           PERFORM 2440-SET-REGION
           PERFORM 2450-ASSESS-SUSPEND
           PERFORM 2460-CHECK-NEXT-DUE
           PERFORM 2470-COUNT-RETRY
           PERFORM 2480-ASSESS-HOLD
           PERFORM 2490-CHECK-EFFECTIVE
           PERFORM 2500-COUNT-RETRY
           PERFORM 2510-EDIT-CUST-ID
           PERFORM 2520-SELECT-DISCOUNT
           PERFORM 2530-COUNT-RETRY
           PERFORM 2540-FLAG-PAYMENT
           PERFORM 2550-SPLIT-INSURANCE
           PERFORM 2560-SELECT-DISCOUNT
           PERFORM 2570-LIMIT-PAYMENT
           PERFORM 2580-ACCRUE-RATE
           PERFORM 2590-SELECT-RATE
           PERFORM 2600-DESCRIBE-ANALYSIS
           PERFORM 2610-COUNT-RETRY
           PERFORM 2620-SET-REASON
           PERFORM 2630-ACCRUE-SHORTAGE
           PERFORM 2640-SET-ANALYSIS
           PERFORM 2650-SPLIT-SHORTAGE
           PERFORM 2660-SPLIT-PAYMENT
           PERFORM 2670-ASSESS-HOLD
           PERFORM 2680-DESCRIBE-STATUS
           PERFORM 2690-SPLIT-DISBURSE
           PERFORM 2700-COUNT-RETRY
           PERFORM 2710-SPLIT-SURPLUS
           PERFORM 2720-PRORATE-SHORTAGE
           PERFORM 2730-ACCUM-REGION
           PERFORM 2740-FLAG-ITEM
           PERFORM 2750-DESCRIBE-REGION
           PERFORM 2760-LIMIT-ITEM
           PERFORM 2770-DESCRIBE-REASON
           PERFORM 2780-SELECT-DISCOUNT
           PERFORM 2790-ASSESS-SUSPEND
           PERFORM 2800-SET-REASON
           PERFORM 2810-SELECT-RATE
           PERFORM 2820-COUNT-RETRY
           PERFORM 2830-COUNT-CYCLE
           PERFORM 2840-LIMIT-SURPLUS
           PERFORM 2850-CHECK-NEXT-DUE
           PERFORM 2860-FLAG-INSURANCE
           PERFORM 2870-DESCRIBE-ANALYSIS
           PERFORM 2880-ACCRUE-SURPLUS
           PERFORM 2890-AGE-NOTICE
           PERFORM 2900-CHECK-EFFECTIVE
           PERFORM 2910-SET-ANALYSIS
           PERFORM 2920-SELECT-RATE
           PERFORM 2930-SPLIT-INSURANCE
           PERFORM 2940-ACCRUE-RATE
           PERFORM 2950-ASSESS-PREFERRED
           PERFORM 2960-COUNT-CYCLE
           PERFORM 2970-LIMIT-ITEM
           PERFORM 2980-CHECK-EFFECTIVE
           PERFORM 2990-ACCUM-ANALYSIS
           PERFORM 3000-FLAG-INSURANCE
           PERFORM 3010-ACCUM-ANALYSIS
           PERFORM 3020-AGE-NEXT-DUE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ESVAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * B -> G WHEN EFFECTIVE DATE IS PAST
      * G -> Y WHEN EFFECTIVE DATE IS PAST
      * Y -> B WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2000-SET-REGION.
           EVALUATE ESVAL-REGION-CD
               WHEN 'B'
                   IF ESVAL-EFFECTIVE-DATE > 0 AND ESVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'G' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ESVAL-EFFECTIVE-DATE > 0 AND ESVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'Y' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF ESVAL-EFFECTIVE-DATE > 0 AND ESVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'B' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2010-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ESVAL-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2020-LIMIT-INSURANCE.
           IF ESVAL-INSURANCE-AMT < 50.00
               MOVE 50.00 TO ESVAL-INSURANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESVAL-INSURANCE-AMT > 50000.00
               MOVE 50000.00 TO ESVAL-INSURANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INSURANCE FEE ON THE SURPLUS YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2030-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESVAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ESVAL-INSURANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE ESVAL-INSURANCE-AMT ROUNDED =
                       ESVAL-SURPLUS-YTD * ESVAL-RATE / 200
               WHEN ESVAL-STATUS-CD = 'A'
                   MOVE 5.00 TO ESVAL-INSURANCE-AMT
               WHEN ESVAL-STATUS-CD = 'E'
                   MOVE 12.00 TO ESVAL-INSURANCE-AMT
               WHEN OTHER
                   COMPUTE ESVAL-INSURANCE-AMT ROUNDED =
                       ESVAL-SURPLUS-YTD * ESVAL-RATE / 100
           END-EVALUATE
           IF ESVAL-INSURANCE-AMT > 0 AND ESVAL-INSURANCE-AMT < 2.00
               MOVE 2.00 TO ESVAL-INSURANCE-AMT
           END-IF
           IF ESVAL-INSURANCE-AMT > 75.00
               MOVE 75.00 TO ESVAL-INSURANCE-AMT
           END-IF
           ADD ESVAL-INSURANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2040-FLAG-DISBURSE.
           IF ESVAL-DISBURSE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESVAL-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHORTAGE YTD.
      *----------------------------------------------------------------*
       2050-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESVAL-SHORTAGE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESVAL-SHORTAGE-YTD
           END-IF
           ADD ESVAL-SHORTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2070-LIMIT-PAYMENT.
           IF ESVAL-PAYMENT-YTD < 50.00
               MOVE 50.00 TO ESVAL-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESVAL-PAYMENT-YTD > 50000.00
               MOVE 50000.00 TO ESVAL-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2080-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESVAL-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2090-ACCRUE-SURPLUS.
           IF ESVAL-SURPLUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESVAL-SURPLUS-YTD * ESVAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESVAL-CUSHION-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2100-FLAG-PAYMENT.
           IF ESVAL-PAYMENT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESVAL-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-EFFECTIVE-DATE > 0 AND ESVAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESVAL-SURPLUS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESVAL-SURPLUS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESVAL-SURPLUS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESVAL-SURPLUS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESVAL-SURPLUS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * B -> G WHEN CLOSE DATE IS PAST
      * G -> Y WHEN CLOSE DATE IS PAST
      * Y -> B WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2120-SET-REGION.
           EVALUATE ESVAL-REGION-CD
               WHEN 'B'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2130-DESCRIBE-STATUS.
           EVALUATE ESVAL-STATUS-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2140-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESVAL-INSURANCE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESVAL-INSURANCE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE ESVAL-SURPLUS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2150-DESCRIBE-ANALYSIS.
           EVALUATE ESVAL-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN NOTICE DATE IS PAST
      * F -> K WHEN NOTICE DATE IS PAST
      * K -> L WHEN NOTICE DATE IS PAST
      * L -> S WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-REASON.
           EVALUATE ESVAL-REASON-CD
               WHEN 'C'
                   IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2170-COUNT-RETRY.
           IF ESVAL-RETRY-CNT < 9999999
               ADD 1 TO ESVAL-RETRY-CNT
           END-IF
           IF ESVAL-RETRY-CNT NOT < 12
               MOVE 'Y' TO ESVAL-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ESVAL-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2190-DESCRIBE-STATUS.
           EVALUATE ESVAL-STATUS-CD
               WHEN 'A'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'E'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2200-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-NEXT-DUE-DATE > 0 AND ESVAL-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESVAL-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESVAL-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESVAL-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ESVAL-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESVAL-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2210-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESVAL-SHORTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESVAL-SHORTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ESVAL-SHORTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD ESVAL-SHORTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESVAL-SHORTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2220-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ESVAL-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E194 CONTACT REQUIRED
      * E675 CUSHION YTD NEGATIVE
      * E072 CUSHION YTD OVER 25000
      * E819 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESVAL-CONTACT = SPACES
                   MOVE 'E194' TO WS-REJECT-CODE
               WHEN ESVAL-CUSHION-YTD < 0
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN ESVAL-CUSHION-YTD > 25000.00
                   MOVE 'E072' TO WS-REJECT-CODE
               WHEN ESVAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE DISBURSE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2240-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESVAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESVAL-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESVAL-HOLD-FLAG = 'Y'
                   COMPUTE ESVAL-PAYMENT-YTD ROUNDED =
                       ESVAL-DISBURSE-YTD * ESVAL-RATE / 200
               WHEN ESVAL-STATUS-CD = 'A'
                   MOVE 20.00 TO ESVAL-PAYMENT-YTD
               WHEN ESVAL-STATUS-CD = 'E'
                   MOVE 10.00 TO ESVAL-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE ESVAL-PAYMENT-YTD ROUNDED =
                       ESVAL-DISBURSE-YTD * ESVAL-RATE / 100
           END-EVALUATE
           IF ESVAL-PAYMENT-YTD > 0 AND ESVAL-PAYMENT-YTD < 2.00
               MOVE 2.00 TO ESVAL-PAYMENT-YTD
           END-IF
           IF ESVAL-PAYMENT-YTD > 25.00
               MOVE 25.00 TO ESVAL-PAYMENT-YTD
           END-IF
           ADD ESVAL-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E634 CUST ID REQUIRED
      * E147 CUSHION YTD NEGATIVE
      * E036 CUSHION YTD OVER 25000
      * E600 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2250-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESVAL-CUST-ID = SPACES
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN ESVAL-CUSHION-YTD < 0
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN ESVAL-CUSHION-YTD > 25000.00
                   MOVE 'E036' TO WS-REJECT-CODE
               WHEN ESVAL-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2260-DESCRIBE-STATUS.
           EVALUATE ESVAL-STATUS-CD
               WHEN 'A'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'E'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'U'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Z'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SURPLUS YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2270-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESVAL-REGION-CD
                   ADD ESVAL-SURPLUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESVAL-SURPLUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-CYCLE.
           IF ESVAL-CYCLE-CNT < 9999999
               ADD 1 TO ESVAL-CYCLE-CNT
           END-IF
           IF ESVAL-CYCLE-CNT NOT < 6
               MOVE 'Y' TO ESVAL-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2290-COUNT-CYCLE.
           IF ESVAL-CYCLE-CNT < 9999999
               ADD 1 TO ESVAL-CYCLE-CNT
           END-IF
           IF ESVAL-CYCLE-CNT NOT < 5
               MOVE 'Y' TO ESVAL-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2300-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = ESVAL-SHORTAGE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ESVAL-SHORTAGE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE ESVAL-INSURANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2310-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-EFFECTIVE-DATE > 0 AND ESVAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-EFFECTIVE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E527 CONTACT REQUIRED
      * E267 WORK AMT 2 NEGATIVE
      * E749 WORK AMT 2 OVER 25000
      * E437 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2320-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESVAL-CONTACT = SPACES
                   MOVE 'E527' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN ESVAL-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       2330-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESVAL-DISBURSE-YTD ROUNDED =
                   ESVAL-SHORTAGE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESVAL-DISBURSE-YTD
           END-IF
           ADD ESVAL-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CUSHION YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2340-LIMIT-CUSHION.
           IF ESVAL-CUSHION-YTD < 50.00
               MOVE 50.00 TO ESVAL-CUSHION-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESVAL-CUSHION-YTD > 10000.00
               MOVE 10000.00 TO ESVAL-CUSHION-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2350-DESCRIBE-REGION.
           EVALUATE ESVAL-REGION-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2360-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO ESVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESVAL-INSURANCE-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO ESVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-STATUS.
           EVALUATE ESVAL-STATUS-CD
               WHEN 'A'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'E'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE YTD FEE ON THE INSURANCE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2380-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESVAL-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ESVAL-SHORTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESVAL-HOLD-FLAG = 'Y'
                   COMPUTE ESVAL-SHORTAGE-YTD ROUNDED =
                       ESVAL-INSURANCE-AMT * ESVAL-RATE / 200
               WHEN ESVAL-ANALYSIS-CD = 'GD'
                   MOVE 5.00 TO ESVAL-SHORTAGE-YTD
               WHEN ESVAL-ANALYSIS-CD = 'LY'
                   MOVE 12.00 TO ESVAL-SHORTAGE-YTD
               WHEN OTHER
                   COMPUTE ESVAL-SHORTAGE-YTD ROUNDED =
                       ESVAL-INSURANCE-AMT * ESVAL-RATE / 100
           END-EVALUATE
           IF ESVAL-SHORTAGE-YTD > 0 AND ESVAL-SHORTAGE-YTD < 5.00
               MOVE 5.00 TO ESVAL-SHORTAGE-YTD
           END-IF
           IF ESVAL-SHORTAGE-YTD > 100.00
               MOVE 100.00 TO ESVAL-SHORTAGE-YTD
           END-IF
           ADD ESVAL-SHORTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE YTD FEE ON THE SURPLUS YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2390-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESVAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESVAL-DISBURSE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE ESVAL-DISBURSE-YTD ROUNDED =
                       ESVAL-SURPLUS-YTD * ESVAL-RATE / 200
               WHEN ESVAL-REASON-CD = 'C'
                   MOVE 15.00 TO ESVAL-DISBURSE-YTD
               WHEN ESVAL-REASON-CD = 'F'
                   MOVE 12.00 TO ESVAL-DISBURSE-YTD
               WHEN OTHER
                   COMPUTE ESVAL-DISBURSE-YTD ROUNDED =
                       ESVAL-SURPLUS-YTD * ESVAL-RATE / 100
           END-EVALUATE
           IF ESVAL-DISBURSE-YTD > 0 AND ESVAL-DISBURSE-YTD < 5.00
               MOVE 5.00 TO ESVAL-DISBURSE-YTD
           END-IF
           IF ESVAL-DISBURSE-YTD > 50.00
               MOVE 50.00 TO ESVAL-DISBURSE-YTD
           END-IF
           ADD ESVAL-DISBURSE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E089 CUST ID REQUIRED
      * E883 SHORTAGE YTD NEGATIVE
      * E592 SHORTAGE YTD OVER 5000
      * E599 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2400-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESVAL-CUST-ID = SPACES
                   MOVE 'E089' TO WS-REJECT-CODE
               WHEN ESVAL-SHORTAGE-YTD < 0
                   MOVE 'E883' TO WS-REJECT-CODE
               WHEN ESVAL-SHORTAGE-YTD > 5000.00
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN ESVAL-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * B -> G WHEN CLOSE DATE IS PAST
      * G -> Y WHEN CLOSE DATE IS PAST
      * Y -> B WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2410-SET-REGION.
           EVALUATE ESVAL-REGION-CD
               WHEN 'B'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ANALYSIS CD:
      * GD -> LY WHEN CLOSE DATE IS PAST
      * LY -> XJ WHEN CLOSE DATE IS PAST
      * XJ -> GD WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2420-SET-ANALYSIS.
           EVALUATE ESVAL-ANALYSIS-CD
               WHEN 'GD'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LY' TO ESVAL-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LY'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO ESVAL-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XJ'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GD' TO ESVAL-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2430-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESVAL-CUSHION-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * B -> G WHEN CLOSE DATE IS PAST
      * G -> Y WHEN CLOSE DATE IS PAST
      * Y -> B WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2440-SET-REGION.
           EVALUATE ESVAL-REGION-CD
               WHEN 'B'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO ESVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS YTD FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2450-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESVAL-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ESVAL-SURPLUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESVAL-HOLD-FLAG = 'Y'
                   COMPUTE ESVAL-SURPLUS-YTD ROUNDED =
                       ESVAL-SHORTAGE-YTD * ESVAL-RATE / 200
               WHEN ESVAL-REASON-CD = 'C'
                   MOVE 15.00 TO ESVAL-SURPLUS-YTD
               WHEN ESVAL-REASON-CD = 'F'
                   MOVE 10.00 TO ESVAL-SURPLUS-YTD
               WHEN OTHER
                   COMPUTE ESVAL-SURPLUS-YTD ROUNDED =
                       ESVAL-SHORTAGE-YTD * ESVAL-RATE / 100
           END-EVALUATE
           IF ESVAL-SURPLUS-YTD > 0 AND ESVAL-SURPLUS-YTD < 10.00
               MOVE 10.00 TO ESVAL-SURPLUS-YTD
           END-IF
           IF ESVAL-SURPLUS-YTD > 50.00
               MOVE 50.00 TO ESVAL-SURPLUS-YTD
           END-IF
           ADD ESVAL-SURPLUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2460-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ESVAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2470-COUNT-RETRY.
           IF ESVAL-RETRY-CNT < 9999999
               ADD 1 TO ESVAL-RETRY-CNT
           END-IF
           IF ESVAL-RETRY-CNT NOT < 12
               MOVE 'Y' TO ESVAL-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SURPLUS YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2480-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESVAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESVAL-SURPLUS-YTD *
                       ESVAL-RATE / 200
               WHEN ESVAL-REGION-CD = 'B'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN ESVAL-REGION-CD = 'G'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESVAL-SURPLUS-YTD *
                       ESVAL-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ESVAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2500-COUNT-RETRY.
           IF ESVAL-RETRY-CNT < 9999999
               ADD 1 TO ESVAL-RETRY-CNT
           END-IF
           IF ESVAL-RETRY-CNT NOT < 10
               MOVE 'Y' TO ESVAL-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E681 CUST ID REQUIRED
      * E204 INSURANCE AMT NEGATIVE
      * E913 INSURANCE AMT OVER 50000
      * E217 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2510-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESVAL-CUST-ID = SPACES
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN ESVAL-INSURANCE-AMT < 0
                   MOVE 'E204' TO WS-REJECT-CODE
               WHEN ESVAL-INSURANCE-AMT > 50000.00
                   MOVE 'E913' TO WS-REJECT-CODE
               WHEN ESVAL-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2520-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ESVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ESVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2530-COUNT-RETRY.
           IF ESVAL-RETRY-CNT < 9999999
               ADD 1 TO ESVAL-RETRY-CNT
           END-IF
           IF ESVAL-RETRY-CNT NOT < 3
               MOVE 'Y' TO ESVAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2540-FLAG-PAYMENT.
           IF ESVAL-PAYMENT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESVAL-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESVAL-INSURANCE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ESVAL-INSURANCE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE SURPLUS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2560-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO ESVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESVAL-SURPLUS-YTD NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO ESVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2570-LIMIT-PAYMENT.
           IF ESVAL-PAYMENT-YTD < 100.00
               MOVE 100.00 TO ESVAL-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESVAL-PAYMENT-YTD > 10000.00
               MOVE 10000.00 TO ESVAL-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ESVAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESVAL-DISBURSE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE SHORTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2590-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ESVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ESVAL-SHORTAGE-YTD NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ESVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2600-DESCRIBE-ANALYSIS.
           EVALUATE ESVAL-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2610-COUNT-RETRY.
           IF ESVAL-RETRY-CNT < 9999999
               ADD 1 TO ESVAL-RETRY-CNT
           END-IF
           IF ESVAL-RETRY-CNT NOT < 6
               MOVE 'Y' TO ESVAL-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN NOTICE DATE IS PAST
      * F -> K WHEN NOTICE DATE IS PAST
      * K -> L WHEN NOTICE DATE IS PAST
      * L -> S WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2620-SET-REASON.
           EVALUATE ESVAL-REASON-CD
               WHEN 'C'
                   IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2630-ACCRUE-SHORTAGE.
           IF ESVAL-SHORTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESVAL-SHORTAGE-YTD * ESVAL-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESVAL-INSURANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ANALYSIS CD:
      * GD -> LY WHEN NOTICE DATE IS PAST
      * LY -> XJ WHEN NOTICE DATE IS PAST
      * XJ -> GD WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2640-SET-ANALYSIS.
           EVALUATE ESVAL-ANALYSIS-CD
               WHEN 'GD'
                   IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LY' TO ESVAL-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LY'
                   IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO ESVAL-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XJ'
                   IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GD' TO ESVAL-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE ESVAL-SHORTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2660-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ESVAL-PAYMENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ESVAL-PAYMENT-YTD - (WS-WORK-AMT * 2)
           COMPUTE ESVAL-SURPLUS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DISBURSE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2670-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESVAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ESVAL-DISBURSE-YTD *
                       ESVAL-RATE / 200
               WHEN ESVAL-REASON-CD = 'C'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN ESVAL-REASON-CD = 'F'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ESVAL-DISBURSE-YTD *
                       ESVAL-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2680-DESCRIBE-STATUS.
           EVALUATE ESVAL-STATUS-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2690-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = ESVAL-DISBURSE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESVAL-DISBURSE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE ESVAL-CUSHION-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2700-COUNT-RETRY.
           IF ESVAL-RETRY-CNT < 9999999
               ADD 1 TO ESVAL-RETRY-CNT
           END-IF
           IF ESVAL-RETRY-CNT NOT < 3
               MOVE 'Y' TO ESVAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURPLUS YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2710-SPLIT-SURPLUS.
           COMPUTE WS-WORK-AMT = ESVAL-SURPLUS-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ESVAL-SURPLUS-YTD - (WS-WORK-AMT * 4)
           COMPUTE ESVAL-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHORTAGE YTD.
      *----------------------------------------------------------------*
       2720-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESVAL-SHORTAGE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESVAL-SHORTAGE-YTD
           END-IF
           ADD ESVAL-SHORTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE SURPLUS YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2730-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESVAL-REGION-CD
                   ADD ESVAL-SURPLUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESVAL-SURPLUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2740-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2750-DESCRIBE-REGION.
           EVALUATE ESVAL-REGION-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'G'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2760-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2770-DESCRIBE-REASON.
           EVALUATE ESVAL-REASON-CD
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2780-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ESVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ESVAL-INSURANCE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ESVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CUSHION YTD FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2790-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESVAL-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ESVAL-CUSHION-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESVAL-PREFERRED-FLAG = 'Y'
                   COMPUTE ESVAL-CUSHION-YTD ROUNDED =
                       ESVAL-SHORTAGE-YTD * ESVAL-RATE / 200
               WHEN ESVAL-STATUS-CD = 'A'
                   MOVE 20.00 TO ESVAL-CUSHION-YTD
               WHEN ESVAL-STATUS-CD = 'E'
                   MOVE 7.00 TO ESVAL-CUSHION-YTD
               WHEN OTHER
                   COMPUTE ESVAL-CUSHION-YTD ROUNDED =
                       ESVAL-SHORTAGE-YTD * ESVAL-RATE / 100
           END-EVALUATE
           IF ESVAL-CUSHION-YTD > 0 AND ESVAL-CUSHION-YTD < 2.00
               MOVE 2.00 TO ESVAL-CUSHION-YTD
           END-IF
           IF ESVAL-CUSHION-YTD > 25.00
               MOVE 25.00 TO ESVAL-CUSHION-YTD
           END-IF
           ADD ESVAL-CUSHION-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN EFFECTIVE DATE IS PAST
      * F -> K WHEN EFFECTIVE DATE IS PAST
      * K -> L WHEN EFFECTIVE DATE IS PAST
      * L -> S WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2800-SET-REASON.
           EVALUATE ESVAL-REASON-CD
               WHEN 'C'
                   IF ESVAL-EFFECTIVE-DATE > 0 AND ESVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'F' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESVAL-EFFECTIVE-DATE > 0 AND ESVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESVAL-EFFECTIVE-DATE > 0 AND ESVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESVAL-EFFECTIVE-DATE > 0 AND ESVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO ESVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE SHORTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2810-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ESVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ESVAL-SHORTAGE-YTD NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ESVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2820-COUNT-RETRY.
           IF ESVAL-RETRY-CNT < 9999999
               ADD 1 TO ESVAL-RETRY-CNT
           END-IF
           IF ESVAL-RETRY-CNT NOT < 3
               MOVE 'Y' TO ESVAL-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2830-COUNT-CYCLE.
           IF ESVAL-CYCLE-CNT < 9999999
               ADD 1 TO ESVAL-CYCLE-CNT
           END-IF
           IF ESVAL-CYCLE-CNT NOT < 3
               MOVE 'Y' TO ESVAL-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2840-LIMIT-SURPLUS.
           IF ESVAL-SURPLUS-YTD < 25.00
               MOVE 25.00 TO ESVAL-SURPLUS-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESVAL-SURPLUS-YTD > 500.00
               MOVE 500.00 TO ESVAL-SURPLUS-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2850-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ESVAL-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2860-FLAG-INSURANCE.
           IF ESVAL-INSURANCE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESVAL-INSURANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2870-DESCRIBE-ANALYSIS.
           EVALUATE ESVAL-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2880-ACCRUE-SURPLUS.
           IF ESVAL-SURPLUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESVAL-SURPLUS-YTD * ESVAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESVAL-INSURANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2890-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-NOTICE-DATE > 0 AND ESVAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2900-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ESVAL-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ANALYSIS CD:
      * GD -> LY WHEN CLOSE DATE IS PAST
      * LY -> XJ WHEN CLOSE DATE IS PAST
      * XJ -> GD WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2910-SET-ANALYSIS.
           EVALUATE ESVAL-ANALYSIS-CD
               WHEN 'GD'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LY' TO ESVAL-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LY'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO ESVAL-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XJ'
                   IF ESVAL-CLOSE-DATE > 0 AND ESVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GD' TO ESVAL-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2920-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO ESVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESVAL-INSURANCE-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO ESVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2930-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE ESVAL-INSURANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2940-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ESVAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESVAL-INSURANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2950-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESVAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESVAL-SHORTAGE-YTD *
                       ESVAL-DISCOUNT / 200
               WHEN ESVAL-STATUS-CD = 'A'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN ESVAL-STATUS-CD = 'E'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESVAL-SHORTAGE-YTD *
                       ESVAL-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2960-COUNT-CYCLE.
           IF ESVAL-CYCLE-CNT < 9999999
               ADD 1 TO ESVAL-CYCLE-CNT
           END-IF
           IF ESVAL-CYCLE-CNT NOT < 6
               MOVE 'Y' TO ESVAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2970-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2980-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ESVAL-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE YTD BY ANALYSIS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2990-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESVAL-ANALYSIS-CD
                   ADD ESVAL-DISBURSE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESVAL-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESVAL-DISBURSE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3000-FLAG-INSURANCE.
           IF ESVAL-INSURANCE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESVAL-INSURANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CUSHION YTD BY ANALYSIS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESVAL-ANALYSIS-CD
                   ADD ESVAL-CUSHION-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESVAL-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESVAL-CUSHION-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESVAL-NEXT-DUE-DATE > 0 AND ESVAL-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESVAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESVAL-CUSHION-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESVAL-CUSHION-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESVAL-CUSHION-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ESVAL-CUSHION-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESVAL-CUSHION-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ESVAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ESVAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ESVAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ESVAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
