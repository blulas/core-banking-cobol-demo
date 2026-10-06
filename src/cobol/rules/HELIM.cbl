      *================================================================*
      * PROGRAM  : HELIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : HOME EQUITY LINES RULE - VALIDATION. CALLED BY THE  *
      *            HOME EQUITY LINES BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-22 CRP  ORIGINAL                                       *
      * 1998-09-21 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-11-14 BHS  ANNUAL-FEE RERUN PROTECTION CHG4982            *
      * 2004-07-19 SKN  PERFORMANCE - BALANCE READ AHEAD PRB9129       *
      * 2005-10-08 RJT  MIN-PAY ROUNDING - HALF UP PRB5862             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELIM.
       AUTHOR. R J TRAVERS.

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
      * WS-MIN-PAY-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-PAY-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000131000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000040500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000714000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000417000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000392000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006300000000216000'.
       01  WS-MIN-PAY-BAND-TBL REDEFINES WS-MIN-PAY-BAND-VALUES.
           05  WS-MIN-PAY-BAND-ROW OCCURS 6 TIMES.
               10  WS-MIN-PAY-BAND-LOW     PIC 9(09)V99.
               10  WS-MIN-PAY-BAND-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY HELIMP.

       PROCEDURE DIVISION USING HELIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ASSESS-OVERRIDE
           PERFORM 2010-ACCRUE-MIN-PAY
           PERFORM 2020-SELECT-PENALTY-PCT
           PERFORM 2030-DESCRIBE-TYPE
           PERFORM 2040-ASSESS-OVERRIDE
           PERFORM 2050-EDIT-REF-NO
           PERFORM 2060-COUNT-EXCEPT
           PERFORM 2070-CHECK-MATURITY
           PERFORM 2080-CHECK-EFFECTIVE
           PERFORM 2090-ACCUM-TYPE
           PERFORM 2100-CHECK-LAST-PAY
           PERFORM 2110-PRORATE-APPRAISE
           PERFORM 2120-COUNT-PAYMENT
           PERFORM 2130-FLAG-ITEM
           PERFORM 2140-ASSESS-HOLD
           PERFORM 2150-PRORATE-APPRAISE
           PERFORM 2160-ACCUM-LIEN-POS
           PERFORM 2170-ASSESS-AUTO-RENEW
           PERFORM 2180-LIMIT-INTEREST
           PERFORM 2190-SELECT-DISCOUNT
           PERFORM 2200-DESCRIBE-DRAW-PERIOD
           PERFORM 2210-SET-DRAW-PERIOD
           PERFORM 2220-LIMIT-ITEM
           PERFORM 2230-ACCUM-STATUS
           PERFORM 2240-FLAG-APPRAISE
           PERFORM 2250-DESCRIBE-LIEN-POS
           PERFORM 2260-SET-DRAW-PERIOD
           PERFORM 2270-SPLIT-APPRAISE
           PERFORM 2280-ACCRUE-APPRAISE
           PERFORM 2290-SPLIT-APPRAISE
           PERFORM 2300-ACCRUE-ANNUAL-FEE
           PERFORM 2310-ACCUM-TYPE
           PERFORM 2320-SPLIT-ANNUAL-FEE
           PERFORM 2330-DESCRIBE-TYPE
           PERFORM 2340-EDIT-DESC
           PERFORM 2350-DESCRIBE-STATUS
           PERFORM 2360-COUNT-PAYMENT
           PERFORM 2370-LIMIT-ITEM
           PERFORM 2380-AGE-MATURITY
           PERFORM 2390-DESCRIBE-TYPE
           PERFORM 2400-PRORATE-MIN-PAY
           PERFORM 2410-SET-TYPE
           PERFORM 2420-SELECT-DISCOUNT
           PERFORM 2430-COUNT-PAYMENT
           PERFORM 2440-EDIT-DESC
           PERFORM 2450-COUNT-PAYMENT
           PERFORM 2460-SELECT-DISCOUNT
           PERFORM 2470-SELECT-PENALTY-PCT
           PERFORM 2480-DESCRIBE-LIEN-POS
           PERFORM 2490-ACCUM-LIEN-POS
           PERFORM 2500-ASSESS-AUTO-RENEW
           PERFORM 2510-DESCRIBE-LIEN-POS
           PERFORM 2520-ASSESS-OVERRIDE
           PERFORM 2530-ACCUM-TYPE
           PERFORM 2540-FLAG-APPRAISE
           PERFORM 2550-SPLIT-APPRAISE
           PERFORM 2560-ACCRUE-BALANCE
           PERFORM 2570-SELECT-DISCOUNT
           PERFORM 2580-CHECK-NEXT-DUE
           PERFORM 2590-ACCUM-TYPE
           PERFORM 2600-SET-DRAW-PERIOD
           PERFORM 2610-COUNT-EXCEPT
           PERFORM 2620-FLAG-ANNUAL-FEE
           PERFORM 2630-CHECK-LAST-PAY
           PERFORM 2640-ACCUM-TYPE
           PERFORM 2650-FLAG-MIN-PAY
           PERFORM 2660-EDIT-DESC
           PERFORM 2670-ACCRUE-DISCOUNT
           PERFORM 2680-SPLIT-BALANCE
           PERFORM 2690-SELECT-PENALTY-PCT
           PERFORM 2700-EDIT-DESC
           PERFORM 2710-DESCRIBE-DRAW-PERIOD
           PERFORM 2720-EDIT-DESC
           PERFORM 2730-ACCRUE-ANNUAL-FEE
           PERFORM 2740-AGE-EFFECTIVE
           PERFORM 2750-SELECT-DISCOUNT
           PERFORM 2760-CHECK-MATURITY
           PERFORM 2770-DESCRIBE-DRAW-PERIOD
           PERFORM 2780-PRORATE-APPRAISE
           PERFORM 2790-COUNT-PAYMENT
           PERFORM 2800-COUNT-EXCEPT
           PERFORM 2810-CHECK-EFFECTIVE
           PERFORM 2820-SET-DRAW-PERIOD
           PERFORM 2830-DESCRIBE-STATUS
           PERFORM 2840-PRORATE-INTEREST
           PERFORM 2850-AGE-EFFECTIVE
           PERFORM 2860-CHECK-EFFECTIVE
           PERFORM 2870-ACCRUE-BALANCE
           PERFORM 2880-SPLIT-INTEREST
           PERFORM 2890-LIMIT-ITEM
           PERFORM 2900-ACCUM-STATUS
           PERFORM 2910-DESCRIBE-TYPE
           PERFORM 2920-EDIT-DESC
           PERFORM 2930-FLAG-APPRAISE
           PERFORM 2940-SPLIT-MIN-PAY
           PERFORM 2950-COUNT-PAYMENT
           PERFORM 2960-SET-DRAW-PERIOD
           PERFORM 2970-SELECT-DISCOUNT
           PERFORM 2980-PRORATE-BALANCE
           PERFORM 2990-CHECK-EFFECTIVE
           PERFORM 3000-FLAG-APPRAISE
           PERFORM 3010-ASSESS-OVERRIDE
           PERFORM 3020-EDIT-DESC
           PERFORM 3030-SET-STATUS
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO HELIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2000-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HELIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HELIM-APPRAISE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELIM-HOLD-FLAG = 'Y'
                   COMPUTE HELIM-APPRAISE-YTD ROUNDED = WS-WORK-AMT-2 *
                       HELIM-PENALTY-PCT / 200
               WHEN HELIM-TYPE-CD = 'CD'
                   MOVE 15.00 TO HELIM-APPRAISE-YTD
               WHEN HELIM-TYPE-CD = 'JW'
                   MOVE 7.00 TO HELIM-APPRAISE-YTD
               WHEN OTHER
                   COMPUTE HELIM-APPRAISE-YTD ROUNDED = WS-WORK-AMT-2 *
                       HELIM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELIM-APPRAISE-YTD > 0 AND HELIM-APPRAISE-YTD < 1.00
               MOVE 1.00 TO HELIM-APPRAISE-YTD
           END-IF
           IF HELIM-APPRAISE-YTD > 100.00
               MOVE 100.00 TO HELIM-APPRAISE-YTD
           END-IF
           ADD HELIM-APPRAISE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN PAY AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2010-ACCRUE-MIN-PAY.
           IF HELIM-MIN-PAY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELIM-MIN-PAY-AMT * HELIM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO HELIM-APPRAISE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE APPRAISE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2020-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELIM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELIM-APPRAISE-YTD NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELIM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELIM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELIM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2030-DESCRIBE-TYPE.
           EVALUATE HELIM-TYPE-CD
               WHEN 'CD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE FEE ON THE APPRAISE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2040-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HELIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HELIM-APPRAISE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELIM-HOLD-FLAG = 'Y'
                   COMPUTE HELIM-APPRAISE-AMT ROUNDED =
                       HELIM-APPRAISE-YTD * HELIM-DISCOUNT / 200
               WHEN HELIM-STATUS-CD = 'GR'
                   MOVE 12.00 TO HELIM-APPRAISE-AMT
               WHEN HELIM-STATUS-CD = 'JC'
                   MOVE 12.00 TO HELIM-APPRAISE-AMT
               WHEN OTHER
                   COMPUTE HELIM-APPRAISE-AMT ROUNDED =
                       HELIM-APPRAISE-YTD * HELIM-DISCOUNT / 100
           END-EVALUATE
           IF HELIM-APPRAISE-AMT > 0 AND HELIM-APPRAISE-AMT < 10.00
               MOVE 10.00 TO HELIM-APPRAISE-AMT
           END-IF
           IF HELIM-APPRAISE-AMT > 100.00
               MOVE 100.00 TO HELIM-APPRAISE-AMT
           END-IF
           ADD HELIM-APPRAISE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E637 REF NO REQUIRED
      * E402 WORK AMT 2 NEGATIVE
      * E190 WORK AMT 2 OVER 10000
      * E318 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELIM-REF-NO = SPACES
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E402' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E190' TO WS-REJECT-CODE
               WHEN HELIM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E318' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2060-COUNT-EXCEPT.
           IF HELIM-EXCEPT-CNT < 9999999
               ADD 1 TO HELIM-EXCEPT-CNT
           END-IF
           IF HELIM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO HELIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2070-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELIM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO HELIM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2080-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELIM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO HELIM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN PAY AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2090-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELIM-TYPE-CD
                   ADD HELIM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELIM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELIM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2100-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HELIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO HELIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO APPRAISE YTD.
      *----------------------------------------------------------------*
       2110-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELIM-APPRAISE-YTD ROUNDED =
                   HELIM-ANNUAL-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELIM-APPRAISE-YTD
           END-IF
           ADD HELIM-APPRAISE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-PAYMENT.
           IF HELIM-PAYMENT-CNT < 9999999
               ADD 1 TO HELIM-PAYMENT-CNT
           END-IF
           IF HELIM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO HELIM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2130-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE FEE ON THE INTEREST YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2140-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HELIM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO HELIM-APPRAISE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELIM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HELIM-APPRAISE-AMT ROUNDED =
                       HELIM-INTEREST-YTD * HELIM-PENALTY-PCT / 200
               WHEN HELIM-DRAW-PERIOD-CD = 'CCV'
                   MOVE 3.00 TO HELIM-APPRAISE-AMT
               WHEN HELIM-DRAW-PERIOD-CD = 'JSU'
                   MOVE 15.00 TO HELIM-APPRAISE-AMT
               WHEN OTHER
                   COMPUTE HELIM-APPRAISE-AMT ROUNDED =
                       HELIM-INTEREST-YTD * HELIM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELIM-APPRAISE-AMT > 0 AND HELIM-APPRAISE-AMT < 5.00
               MOVE 5.00 TO HELIM-APPRAISE-AMT
           END-IF
           IF HELIM-APPRAISE-AMT > 150.00
               MOVE 150.00 TO HELIM-APPRAISE-AMT
           END-IF
           ADD HELIM-APPRAISE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO APPRAISE AMT.
      *----------------------------------------------------------------*
       2150-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELIM-APPRAISE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELIM-APPRAISE-AMT
           END-IF
           ADD HELIM-APPRAISE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE AMT BY LIEN POS CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2160-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELIM-LIEN-POS-CD
                   ADD HELIM-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELIM-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELIM-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE FEE ON THE APPRAISE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2170-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELIM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELIM-ANNUAL-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELIM-OVERRIDE-FLAG = 'Y'
                   COMPUTE HELIM-ANNUAL-FEE-AMT ROUNDED =
                       HELIM-APPRAISE-YTD * HELIM-PENALTY-PCT / 200
               WHEN HELIM-TYPE-CD = 'CD'
                   MOVE 12.00 TO HELIM-ANNUAL-FEE-AMT
               WHEN HELIM-TYPE-CD = 'JW'
                   MOVE 12.00 TO HELIM-ANNUAL-FEE-AMT
               WHEN OTHER
                   COMPUTE HELIM-ANNUAL-FEE-AMT ROUNDED =
                       HELIM-APPRAISE-YTD * HELIM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELIM-ANNUAL-FEE-AMT > 0 AND HELIM-ANNUAL-FEE-AMT < 1.00
               MOVE 1.00 TO HELIM-ANNUAL-FEE-AMT
           END-IF
           IF HELIM-ANNUAL-FEE-AMT > 25.00
               MOVE 25.00 TO HELIM-ANNUAL-FEE-AMT
           END-IF
           ADD HELIM-ANNUAL-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2180-LIMIT-INTEREST.
           IF HELIM-INTEREST-YTD < 25.00
               MOVE 25.00 TO HELIM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELIM-INTEREST-YTD > 5000.00
               MOVE 5000.00 TO HELIM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE APPRAISE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2190-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELIM-APPRAISE-YTD NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2200-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELIM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAW PERIOD CD:
      * CCV -> JSU WHEN NEXT DUE DATE IS PAST
      * JSU -> KXA WHEN NEXT DUE DATE IS PAST
      * KXA -> VMR WHEN NEXT DUE DATE IS PAST
      * VMR -> WLJ WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-DRAW-PERIOD.
           EVALUATE HELIM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   IF HELIM-NEXT-DUE-DATE > 0 AND HELIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JSU' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JSU'
                   IF HELIM-NEXT-DUE-DATE > 0 AND HELIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KXA' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KXA'
                   IF HELIM-NEXT-DUE-DATE > 0 AND HELIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VMR' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VMR'
                   IF HELIM-NEXT-DUE-DATE > 0 AND HELIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLJ' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2220-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2230-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELIM-STATUS-CD
                   ADD HELIM-APPRAISE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELIM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELIM-APPRAISE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2240-FLAG-APPRAISE.
           IF HELIM-APPRAISE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELIM-APPRAISE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2250-DESCRIBE-LIEN-POS.
           EVALUATE HELIM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAW PERIOD CD:
      * CCV -> JSU WHEN EFFECTIVE DATE IS PAST
      * JSU -> KXA WHEN EFFECTIVE DATE IS PAST
      * KXA -> VMR WHEN EFFECTIVE DATE IS PAST
      * VMR -> WLJ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2260-SET-DRAW-PERIOD.
           EVALUATE HELIM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JSU' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JSU'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KXA' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KXA'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VMR' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VMR'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WLJ' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2270-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELIM-APPRAISE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = HELIM-APPRAISE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE HELIM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON APPRAISE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-APPRAISE.
           IF HELIM-APPRAISE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELIM-APPRAISE-YTD * HELIM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2290-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE HELIM-APPRAISE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2300-ACCRUE-ANNUAL-FEE.
           IF HELIM-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELIM-ANNUAL-FEE-AMT * HELIM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO HELIM-BALANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2310-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELIM-TYPE-CD
                   ADD HELIM-APPRAISE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELIM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELIM-APPRAISE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ANNUAL FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-ANNUAL-FEE.
           COMPUTE WS-WORK-AMT = HELIM-ANNUAL-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELIM-ANNUAL-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE HELIM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2330-DESCRIBE-TYPE.
           EVALUATE HELIM-TYPE-CD
               WHEN 'CD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E560 DESC REQUIRED
      * E182 APPRAISE YTD NEGATIVE
      * E505 APPRAISE YTD OVER 25000
      * E162 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELIM-DESC = SPACES
                   MOVE 'E560' TO WS-REJECT-CODE
               WHEN HELIM-APPRAISE-YTD < 0
                   MOVE 'E182' TO WS-REJECT-CODE
               WHEN HELIM-APPRAISE-YTD > 25000.00
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN HELIM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E162' TO WS-REJECT-CODE
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
       2350-DESCRIBE-STATUS.
           EVALUATE HELIM-STATUS-CD
               WHEN 'GR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-PAYMENT.
           IF HELIM-PAYMENT-CNT < 9999999
               ADD 1 TO HELIM-PAYMENT-CNT
           END-IF
           IF HELIM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO HELIM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2370-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2380-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELIM-MATURITY-DATE > 0 AND HELIM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELIM-APPRAISE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELIM-APPRAISE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HELIM-APPRAISE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD HELIM-APPRAISE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELIM-APPRAISE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2390-DESCRIBE-TYPE.
           EVALUATE HELIM-TYPE-CD
               WHEN 'CD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN PAY AMT.
      *----------------------------------------------------------------*
       2400-PRORATE-MIN-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELIM-MIN-PAY-AMT ROUNDED =
                   HELIM-ANNUAL-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELIM-MIN-PAY-AMT
           END-IF
           ADD HELIM-MIN-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN LAST PAY DATE IS PAST
      * JW -> KV WHEN LAST PAY DATE IS PAST
      * KV -> WS WHEN LAST PAY DATE IS PAST
      * WS -> XL WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2410-SET-TYPE.
           EVALUATE HELIM-TYPE-CD
               WHEN 'CD'
                   IF HELIM-LAST-PAY-DATE > 0 AND HELIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JW' TO HELIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JW'
                   IF HELIM-LAST-PAY-DATE > 0 AND HELIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO HELIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF HELIM-LAST-PAY-DATE > 0 AND HELIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WS' TO HELIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WS'
                   IF HELIM-LAST-PAY-DATE > 0 AND HELIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO HELIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE MIN PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELIM-MIN-PAY-AMT NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-PAYMENT.
           IF HELIM-PAYMENT-CNT < 9999999
               ADD 1 TO HELIM-PAYMENT-CNT
           END-IF
           IF HELIM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO HELIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E923 DESC REQUIRED
      * E329 APPRAISE YTD NEGATIVE
      * E056 APPRAISE YTD OVER 25000
      * E154 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2440-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELIM-DESC = SPACES
                   MOVE 'E923' TO WS-REJECT-CODE
               WHEN HELIM-APPRAISE-YTD < 0
                   MOVE 'E329' TO WS-REJECT-CODE
               WHEN HELIM-APPRAISE-YTD > 25000.00
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN HELIM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2450-COUNT-PAYMENT.
           IF HELIM-PAYMENT-CNT < 9999999
               ADD 1 TO HELIM-PAYMENT-CNT
           END-IF
           IF HELIM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO HELIM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELIM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELIM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELIM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELIM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-LIEN-POS.
           EVALUATE HELIM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE AMT BY LIEN POS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELIM-LIEN-POS-CD
                   ADD HELIM-APPRAISE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELIM-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELIM-APPRAISE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE FEE ON THE MIN PAY AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2500-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELIM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELIM-APPRAISE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELIM-HOLD-FLAG = 'Y'
                   COMPUTE HELIM-APPRAISE-AMT ROUNDED =
                       HELIM-MIN-PAY-AMT * HELIM-PENALTY-PCT / 200
               WHEN HELIM-DRAW-PERIOD-CD = 'CCV'
                   MOVE 5.00 TO HELIM-APPRAISE-AMT
               WHEN HELIM-DRAW-PERIOD-CD = 'JSU'
                   MOVE 10.00 TO HELIM-APPRAISE-AMT
               WHEN OTHER
                   COMPUTE HELIM-APPRAISE-AMT ROUNDED =
                       HELIM-MIN-PAY-AMT * HELIM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELIM-APPRAISE-AMT > 0 AND HELIM-APPRAISE-AMT < 5.00
               MOVE 5.00 TO HELIM-APPRAISE-AMT
           END-IF
           IF HELIM-APPRAISE-AMT > 35.00
               MOVE 35.00 TO HELIM-APPRAISE-AMT
           END-IF
           ADD HELIM-APPRAISE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2510-DESCRIBE-LIEN-POS.
           EVALUATE HELIM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MIN PAY FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2520-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HELIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HELIM-MIN-PAY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELIM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HELIM-MIN-PAY-AMT ROUNDED = WS-WORK-AMT-2 *
                       HELIM-DISCOUNT / 200
               WHEN HELIM-TYPE-CD = 'CD'
                   MOVE 7.00 TO HELIM-MIN-PAY-AMT
               WHEN HELIM-TYPE-CD = 'JW'
                   MOVE 5.00 TO HELIM-MIN-PAY-AMT
               WHEN OTHER
                   COMPUTE HELIM-MIN-PAY-AMT ROUNDED = WS-WORK-AMT-2 *
                       HELIM-DISCOUNT / 100
           END-EVALUATE
           IF HELIM-MIN-PAY-AMT > 0 AND HELIM-MIN-PAY-AMT < 5.00
               MOVE 5.00 TO HELIM-MIN-PAY-AMT
           END-IF
           IF HELIM-MIN-PAY-AMT > 35.00
               MOVE 35.00 TO HELIM-MIN-PAY-AMT
           END-IF
           ADD HELIM-MIN-PAY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN PAY AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2530-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELIM-TYPE-CD
                   ADD HELIM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELIM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELIM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2540-FLAG-APPRAISE.
           IF HELIM-APPRAISE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELIM-APPRAISE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELIM-APPRAISE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = HELIM-APPRAISE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2560-ACCRUE-BALANCE.
           IF HELIM-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELIM-BALANCE-YTD * HELIM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELIM-MIN-PAY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE APPRAISE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2570-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELIM-APPRAISE-YTD NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2580-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF HELIM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO HELIM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2590-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELIM-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELIM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAW PERIOD CD:
      * CCV -> JSU WHEN EFFECTIVE DATE IS PAST
      * JSU -> KXA WHEN EFFECTIVE DATE IS PAST
      * KXA -> VMR WHEN EFFECTIVE DATE IS PAST
      * VMR -> WLJ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2600-SET-DRAW-PERIOD.
           EVALUATE HELIM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JSU' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JSU'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KXA' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KXA'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VMR' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VMR'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WLJ' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2610-COUNT-EXCEPT.
           IF HELIM-EXCEPT-CNT < 9999999
               ADD 1 TO HELIM-EXCEPT-CNT
           END-IF
           IF HELIM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO HELIM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2620-FLAG-ANNUAL-FEE.
           IF HELIM-ANNUAL-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELIM-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HELIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO HELIM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELIM-TYPE-CD
                   ADD HELIM-APPRAISE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELIM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELIM-APPRAISE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2650-FLAG-MIN-PAY.
           IF HELIM-MIN-PAY-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELIM-MIN-PAY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E493 DESC REQUIRED
      * E950 ANNUAL FEE AMT NEGATIVE
      * E646 ANNUAL FEE AMT OVER 99999
      * E331 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2660-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELIM-DESC = SPACES
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN HELIM-ANNUAL-FEE-AMT < 0
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN HELIM-ANNUAL-FEE-AMT > 99999.00
                   MOVE 'E646' TO WS-REJECT-CODE
               WHEN HELIM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2670-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * HELIM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO HELIM-BALANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2680-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE HELIM-BALANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE MIN PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2690-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELIM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELIM-MIN-PAY-AMT NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELIM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELIM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELIM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E517 DESC REQUIRED
      * E120 WORK AMT NEGATIVE
      * E925 WORK AMT OVER 50000
      * E230 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2700-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELIM-DESC = SPACES
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E120' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN HELIM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2710-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELIM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E859 DESC REQUIRED
      * E206 APPRAISE AMT NEGATIVE
      * E924 APPRAISE AMT OVER 99999
      * E827 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2720-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELIM-DESC = SPACES
                   MOVE 'E859' TO WS-REJECT-CODE
               WHEN HELIM-APPRAISE-AMT < 0
                   MOVE 'E206' TO WS-REJECT-CODE
               WHEN HELIM-APPRAISE-AMT > 99999.00
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN HELIM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2730-ACCRUE-ANNUAL-FEE.
           IF HELIM-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELIM-ANNUAL-FEE-AMT * HELIM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO HELIM-BALANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2740-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-EFFECTIVE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE APPRAISE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2750-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELIM-APPRAISE-YTD NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2760-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELIM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO HELIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2770-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELIM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MIN PAY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO APPRAISE YTD.
      *----------------------------------------------------------------*
       2780-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELIM-APPRAISE-YTD ROUNDED =
                   HELIM-MIN-PAY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELIM-APPRAISE-YTD
           END-IF
           ADD HELIM-APPRAISE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2790-COUNT-PAYMENT.
           IF HELIM-PAYMENT-CNT < 9999999
               ADD 1 TO HELIM-PAYMENT-CNT
           END-IF
           IF HELIM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO HELIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2800-COUNT-EXCEPT.
           IF HELIM-EXCEPT-CNT < 9999999
               ADD 1 TO HELIM-EXCEPT-CNT
           END-IF
           IF HELIM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO HELIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2810-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELIM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO HELIM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAW PERIOD CD:
      * CCV -> JSU WHEN NEXT DUE DATE IS PAST
      * JSU -> KXA WHEN NEXT DUE DATE IS PAST
      * KXA -> VMR WHEN NEXT DUE DATE IS PAST
      * VMR -> WLJ WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2820-SET-DRAW-PERIOD.
           EVALUATE HELIM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   IF HELIM-NEXT-DUE-DATE > 0 AND HELIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JSU' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JSU'
                   IF HELIM-NEXT-DUE-DATE > 0 AND HELIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KXA' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KXA'
                   IF HELIM-NEXT-DUE-DATE > 0 AND HELIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VMR' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VMR'
                   IF HELIM-NEXT-DUE-DATE > 0 AND HELIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLJ' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2830-DESCRIBE-STATUS.
           EVALUATE HELIM-STATUS-CD
               WHEN 'GR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       2840-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELIM-INTEREST-YTD ROUNDED =
                   HELIM-ANNUAL-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELIM-INTEREST-YTD
           END-IF
           ADD HELIM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2850-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELIM-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD HELIM-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELIM-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELIM-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELIM-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2860-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELIM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO HELIM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2870-ACCRUE-BALANCE.
           IF HELIM-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELIM-BALANCE-YTD * HELIM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HELIM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2880-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = HELIM-INTEREST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = HELIM-INTEREST-YTD - (WS-WORK-AMT *
               4)
           COMPUTE HELIM-ANNUAL-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2890-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2900-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELIM-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELIM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2910-DESCRIBE-TYPE.
           EVALUATE HELIM-TYPE-CD
               WHEN 'CD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E579 DESC REQUIRED
      * E030 INTEREST YTD NEGATIVE
      * E488 INTEREST YTD OVER 50000
      * E540 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2920-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELIM-DESC = SPACES
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN HELIM-INTEREST-YTD < 0
                   MOVE 'E030' TO WS-REJECT-CODE
               WHEN HELIM-INTEREST-YTD > 50000.00
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN HELIM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2930-FLAG-APPRAISE.
           IF HELIM-APPRAISE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELIM-APPRAISE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MIN PAY AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2940-SPLIT-MIN-PAY.
           COMPUTE WS-WORK-AMT = HELIM-MIN-PAY-AMT / 2
           COMPUTE WS-WORK-AMT-2 = HELIM-MIN-PAY-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2950-COUNT-PAYMENT.
           IF HELIM-PAYMENT-CNT < 9999999
               ADD 1 TO HELIM-PAYMENT-CNT
           END-IF
           IF HELIM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO HELIM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAW PERIOD CD:
      * CCV -> JSU WHEN LAST PAY DATE IS PAST
      * JSU -> KXA WHEN LAST PAY DATE IS PAST
      * KXA -> VMR WHEN LAST PAY DATE IS PAST
      * VMR -> WLJ WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2960-SET-DRAW-PERIOD.
           EVALUATE HELIM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   IF HELIM-LAST-PAY-DATE > 0 AND HELIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JSU' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JSU'
                   IF HELIM-LAST-PAY-DATE > 0 AND HELIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KXA' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KXA'
                   IF HELIM-LAST-PAY-DATE > 0 AND HELIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VMR' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VMR'
                   IF HELIM-LAST-PAY-DATE > 0 AND HELIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLJ' TO HELIM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE APPRAISE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2970-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELIM-APPRAISE-AMT NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2980-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELIM-BALANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2990-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELIM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELIM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO HELIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3000-FLAG-APPRAISE.
           IF HELIM-APPRAISE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELIM-APPRAISE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN PAY FEE ON THE INTEREST YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3010-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HELIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HELIM-MIN-PAY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELIM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HELIM-MIN-PAY-AMT ROUNDED =
                       HELIM-INTEREST-YTD * HELIM-PENALTY-PCT / 200
               WHEN HELIM-TYPE-CD = 'CD'
                   MOVE 5.00 TO HELIM-MIN-PAY-AMT
               WHEN HELIM-TYPE-CD = 'JW'
                   MOVE 5.00 TO HELIM-MIN-PAY-AMT
               WHEN OTHER
                   COMPUTE HELIM-MIN-PAY-AMT ROUNDED =
                       HELIM-INTEREST-YTD * HELIM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELIM-MIN-PAY-AMT > 0 AND HELIM-MIN-PAY-AMT < 2.00
               MOVE 2.00 TO HELIM-MIN-PAY-AMT
           END-IF
           IF HELIM-MIN-PAY-AMT > 75.00
               MOVE 75.00 TO HELIM-MIN-PAY-AMT
           END-IF
           ADD HELIM-MIN-PAY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E255 DESC REQUIRED
      * E576 ANNUAL FEE AMT NEGATIVE
      * E990 ANNUAL FEE AMT OVER 250000
      * E639 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELIM-DESC = SPACES
                   MOVE 'E255' TO WS-REJECT-CODE
               WHEN HELIM-ANNUAL-FEE-AMT < 0
                   MOVE 'E576' TO WS-REJECT-CODE
               WHEN HELIM-ANNUAL-FEE-AMT > 250000.00
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN HELIM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E639' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * GR -> JC WHEN EFFECTIVE DATE IS PAST
      * JC -> KH WHEN EFFECTIVE DATE IS PAST
      * KH -> KN WHEN EFFECTIVE DATE IS PAST
      * KN -> PC WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-STATUS.
           EVALUATE HELIM-STATUS-CD
               WHEN 'GR'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JC' TO HELIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KH' TO HELIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KH'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KN' TO HELIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KN'
                   IF HELIM-EFFECTIVE-DATE > 0 AND HELIM-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PC' TO HELIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO HELIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO HELIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO HELIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'HELIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
