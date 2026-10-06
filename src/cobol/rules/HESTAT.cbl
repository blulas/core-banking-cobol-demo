      *================================================================*
      * PROGRAM  : HESTAT                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : HOME EQUITY LINES RULE - STATUS. CALLED BY THE HOME *
      *            EQUITY LINES BATCH AND ONLINE PROGRAMS.             *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-02-02 MKL  ORIGINAL                                       *
      * 1995-03-14 JLM  ANNUAL-FEE EXCEPTIONS TO OPS QUEUE CHG2871     *
      * 1999-05-16 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-11-21 JLM  ANNUAL-FEE FEE SCHEDULE UPDATE PRB7421         *
      * 2003-02-19 CRP  NEW MIN-PAY REPORT COLUMNS INC7136             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HESTAT.
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
                                           VALUE 20.000000.
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
      * WS-MIN-PAY-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-PAY-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000692000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000359500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000102000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000782000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000900000000456000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000950000000521500'.
       01  WS-MIN-PAY-GRID-TBL REDEFINES WS-MIN-PAY-GRID-VALUES.
           05  WS-MIN-PAY-GRID-ROW OCCURS 6 TIMES.
               10  WS-MIN-PAY-GRID-LOW     PIC 9(09)V99.
               10  WS-MIN-PAY-GRID-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY HESTATP.

       PROCEDURE DIVISION USING HESTAT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-DRAW
           PERFORM 2010-ASSESS-OVERRIDE
           PERFORM 2020-ACCRUE-MARGIN
           PERFORM 2030-ACCRUE-DISCOUNT
           PERFORM 2040-COUNT-RETRY
           PERFORM 2050-CHECK-LAST-MAINT
           PERFORM 2060-CHECK-CLOSE
           PERFORM 2070-ASSESS-OVERRIDE
           PERFORM 2080-DESCRIBE-TYPE
           PERFORM 2090-ASSESS-AUTO-RENEW
           PERFORM 2100-LIMIT-ITEM
           PERFORM 2110-ACCRUE-DISCOUNT
           PERFORM 2120-ASSESS-OVERRIDE
           PERFORM 2130-SPLIT-BALANCE
           PERFORM 2140-ACCRUE-ANNUAL-FEE
           PERFORM 2150-ASSESS-AUTO-RENEW
           PERFORM 2160-PRORATE-APPRAISE
           PERFORM 2170-DESCRIBE-REASON
           PERFORM 2180-DESCRIBE-REGION
           PERFORM 2190-SET-REASON
           PERFORM 2200-LIMIT-BALANCE
           PERFORM 2210-LIMIT-ITEM
           PERFORM 2220-AGE-OPEN
           PERFORM 2230-COUNT-RETRY
           PERFORM 2240-AGE-EFFECTIVE
           PERFORM 2250-FLAG-BALANCE
           PERFORM 2260-ASSESS-AUTO-RENEW
           PERFORM 2270-LIMIT-DRAW
           PERFORM 2280-FLAG-ANNUAL-FEE
           PERFORM 2290-COUNT-PAYMENT
           PERFORM 2300-DESCRIBE-REASON
           PERFORM 2310-SELECT-MARGIN
           PERFORM 2320-LIMIT-BALANCE
           PERFORM 2330-ASSESS-PREFERRED
           PERFORM 2340-SELECT-MARGIN
           PERFORM 2350-ASSESS-OVERRIDE
           PERFORM 2360-AGE-LAST-MAINT
           PERFORM 2370-ASSESS-PREFERRED
           PERFORM 2380-DESCRIBE-REASON
           PERFORM 2390-ASSESS-PREFERRED
           PERFORM 2400-ACCUM-REGION
           PERFORM 2410-AGE-OPEN
           PERFORM 2420-ASSESS-AUTO-RENEW
           PERFORM 2430-SPLIT-APPRAISE
           PERFORM 2440-ACCRUE-ANNUAL-FEE
           PERFORM 2450-FLAG-ANNUAL-FEE
           PERFORM 2460-CHECK-CLOSE
           PERFORM 2470-PRORATE-DRAW
           PERFORM 2480-CHECK-CLOSE
           PERFORM 2490-ASSESS-PREFERRED
           PERFORM 2500-FLAG-BALANCE
           PERFORM 2510-COUNT-RETRY
           PERFORM 2520-PRORATE-DRAW
           PERFORM 2530-AGE-LAST-MAINT
           PERFORM 2540-AGE-CLOSE
           PERFORM 2550-AGE-EFFECTIVE
           PERFORM 2560-COUNT-PAYMENT
           PERFORM 2570-SPLIT-ANNUAL-FEE
           PERFORM 2580-SET-REASON
           PERFORM 2590-AGE-EFFECTIVE
           PERFORM 2600-CHECK-OPEN
           PERFORM 2610-SET-TYPE
           PERFORM 2620-PRORATE-BALANCE
           PERFORM 2630-ACCRUE-MARGIN
           PERFORM 2640-LIMIT-DRAW
           PERFORM 2650-SELECT-MARGIN
           PERFORM 2660-DESCRIBE-REASON
           PERFORM 2670-EDIT-CONTACT
           PERFORM 2680-AGE-LAST-MAINT
           PERFORM 2690-PRORATE-DRAW
           PERFORM 2700-FLAG-ITEM
           PERFORM 2710-ACCRUE-ANNUAL-FEE
           PERFORM 2720-EDIT-CUST-ID
           PERFORM 2730-ACCUM-TYPE
           PERFORM 2740-FLAG-ANNUAL-FEE
           PERFORM 2750-COUNT-PAYMENT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO HESTAT-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE HESTAT-DRAW-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE YTD FEE ON THE BALANCE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2010-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HESTAT-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HESTAT-APPRAISE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HESTAT-APPRAISE-YTD ROUNDED =
                       HESTAT-BALANCE-YTD * HESTAT-MARGIN / 200
               WHEN HESTAT-TYPE-CD = 'CD'
                   MOVE 20.00 TO HESTAT-APPRAISE-YTD
               WHEN HESTAT-TYPE-CD = 'JW'
                   MOVE 7.00 TO HESTAT-APPRAISE-YTD
               WHEN OTHER
                   COMPUTE HESTAT-APPRAISE-YTD ROUNDED =
                       HESTAT-BALANCE-YTD * HESTAT-MARGIN / 100
           END-EVALUATE
           IF HESTAT-APPRAISE-YTD > 0 AND HESTAT-APPRAISE-YTD < 5.00
               MOVE 5.00 TO HESTAT-APPRAISE-YTD
           END-IF
           IF HESTAT-APPRAISE-YTD > 50.00
               MOVE 50.00 TO HESTAT-APPRAISE-YTD
           END-IF
           ADD HESTAT-APPRAISE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * HESTAT-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HESTAT-ANNUAL-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2030-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * HESTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO HESTAT-BALANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-RETRY.
           IF HESTAT-RETRY-CNT < 9999999
               ADD 1 TO HESTAT-RETRY-CNT
           END-IF
           IF HESTAT-RETRY-CNT NOT < 6
               MOVE 'Y' TO HESTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2050-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO HESTAT-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2060-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO HESTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE YTD FEE ON THE DRAW AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2070-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HESTAT-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HESTAT-ANNUAL-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HESTAT-ANNUAL-FEE-YTD ROUNDED =
                       HESTAT-DRAW-AMT * HESTAT-DISCOUNT / 200
               WHEN HESTAT-REASON-CD = 'KST'
                   MOVE 12.00 TO HESTAT-ANNUAL-FEE-YTD
               WHEN HESTAT-REASON-CD = 'NWS'
                   MOVE 7.00 TO HESTAT-ANNUAL-FEE-YTD
               WHEN OTHER
                   COMPUTE HESTAT-ANNUAL-FEE-YTD ROUNDED =
                       HESTAT-DRAW-AMT * HESTAT-DISCOUNT / 100
           END-EVALUATE
           IF HESTAT-ANNUAL-FEE-YTD > 0 AND HESTAT-ANNUAL-FEE-YTD < 2.00
               MOVE 2.00 TO HESTAT-ANNUAL-FEE-YTD
           END-IF
           IF HESTAT-ANNUAL-FEE-YTD > 25.00
               MOVE 25.00 TO HESTAT-ANNUAL-FEE-YTD
           END-IF
           ADD HESTAT-ANNUAL-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2080-DESCRIBE-TYPE.
           EVALUATE HESTAT-TYPE-CD
               WHEN 'CD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE DRAW AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2090-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HESTAT-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HESTAT-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE HESTAT-BALANCE-AMT ROUNDED = HESTAT-DRAW-AMT
                       * HESTAT-DISCOUNT / 200
               WHEN HESTAT-TYPE-CD = 'CD'
                   MOVE 10.00 TO HESTAT-BALANCE-AMT
               WHEN HESTAT-TYPE-CD = 'JW'
                   MOVE 7.00 TO HESTAT-BALANCE-AMT
               WHEN OTHER
                   COMPUTE HESTAT-BALANCE-AMT ROUNDED = HESTAT-DRAW-AMT
                       * HESTAT-DISCOUNT / 100
           END-EVALUATE
           IF HESTAT-BALANCE-AMT > 0 AND HESTAT-BALANCE-AMT < 1.00
               MOVE 1.00 TO HESTAT-BALANCE-AMT
           END-IF
           IF HESTAT-BALANCE-AMT > 50.00
               MOVE 50.00 TO HESTAT-BALANCE-AMT
           END-IF
           ADD HESTAT-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2100-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2110-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * HESTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRAW FEE ON THE BALANCE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2120-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HESTAT-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HESTAT-DRAW-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HESTAT-DRAW-AMT ROUNDED = HESTAT-BALANCE-AMT
                       * HESTAT-MARGIN / 200
               WHEN HESTAT-REGION-CD = 'BSD'
                   MOVE 12.00 TO HESTAT-DRAW-AMT
               WHEN HESTAT-REGION-CD = 'HKW'
                   MOVE 15.00 TO HESTAT-DRAW-AMT
               WHEN OTHER
                   COMPUTE HESTAT-DRAW-AMT ROUNDED = HESTAT-BALANCE-AMT
                       * HESTAT-MARGIN / 100
           END-EVALUATE
           IF HESTAT-DRAW-AMT > 0 AND HESTAT-DRAW-AMT < 1.00
               MOVE 1.00 TO HESTAT-DRAW-AMT
           END-IF
           IF HESTAT-DRAW-AMT > 250.00
               MOVE 250.00 TO HESTAT-DRAW-AMT
           END-IF
           ADD HESTAT-DRAW-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT BALANCE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = HESTAT-BALANCE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = HESTAT-BALANCE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE HESTAT-APPRAISE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2140-ACCRUE-ANNUAL-FEE.
           IF HESTAT-ANNUAL-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HESTAT-ANNUAL-FEE-YTD * HESTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ANNUAL FEE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2150-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HESTAT-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = HESTAT-ANNUAL-FEE-AMT
                       * HESTAT-DISCOUNT / 200
               WHEN HESTAT-DRAW-PERIOD-CD = 'CCV'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN HESTAT-DRAW-PERIOD-CD = 'JSU'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = HESTAT-ANNUAL-FEE-AMT
                       * HESTAT-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO APPRAISE YTD.
      *----------------------------------------------------------------*
       2160-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HESTAT-APPRAISE-YTD ROUNDED =
                   HESTAT-ANNUAL-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HESTAT-APPRAISE-YTD
           END-IF
           ADD HESTAT-APPRAISE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-REASON.
           EVALUATE HESTAT-REASON-CD
               WHEN 'KST'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2180-DESCRIBE-REGION.
           EVALUATE HESTAT-REGION-CD
               WHEN 'BSD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HKW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HPB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ULJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN EFFECTIVE DATE IS PAST
      * NWS -> ZJM WHEN EFFECTIVE DATE IS PAST
      * ZJM -> KST WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2190-SET-REASON.
           EVALUATE HESTAT-REASON-CD
               WHEN 'KST'
                   IF HESTAT-EFFECTIVE-DATE > 0 AND
                       HESTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HESTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HESTAT-EFFECTIVE-DATE > 0 AND
                       HESTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HESTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HESTAT-EFFECTIVE-DATE > 0 AND
                       HESTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'KST' TO HESTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BALANCE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2200-LIMIT-BALANCE.
           IF HESTAT-BALANCE-AMT < 100.00
               MOVE 100.00 TO HESTAT-BALANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HESTAT-BALANCE-AMT > 2500.00
               MOVE 2500.00 TO HESTAT-BALANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2210-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2220-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-OPEN-DATE > 0 AND HESTAT-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HESTAT-DRAW-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HESTAT-DRAW-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HESTAT-DRAW-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HESTAT-DRAW-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HESTAT-DRAW-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-RETRY.
           IF HESTAT-RETRY-CNT < 9999999
               ADD 1 TO HESTAT-RETRY-CNT
           END-IF
           IF HESTAT-RETRY-CNT NOT < 3
               MOVE 'Y' TO HESTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2240-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-EFFECTIVE-DATE > 0 AND HESTAT-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HESTAT-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HESTAT-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HESTAT-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HESTAT-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HESTAT-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BALANCE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2250-FLAG-BALANCE.
           IF HESTAT-BALANCE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HESTAT-BALANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE YTD FEE ON THE BALANCE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2260-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HESTAT-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HESTAT-APPRAISE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-PREFERRED-FLAG = 'Y'
                   COMPUTE HESTAT-APPRAISE-YTD ROUNDED =
                       HESTAT-BALANCE-AMT * HESTAT-MARGIN / 200
               WHEN HESTAT-REGION-CD = 'BSD'
                   MOVE 7.00 TO HESTAT-APPRAISE-YTD
               WHEN HESTAT-REGION-CD = 'HKW'
                   MOVE 15.00 TO HESTAT-APPRAISE-YTD
               WHEN OTHER
                   COMPUTE HESTAT-APPRAISE-YTD ROUNDED =
                       HESTAT-BALANCE-AMT * HESTAT-MARGIN / 100
           END-EVALUATE
           IF HESTAT-APPRAISE-YTD > 0 AND HESTAT-APPRAISE-YTD < 10.00
               MOVE 10.00 TO HESTAT-APPRAISE-YTD
           END-IF
           IF HESTAT-APPRAISE-YTD > 250.00
               MOVE 250.00 TO HESTAT-APPRAISE-YTD
           END-IF
           ADD HESTAT-APPRAISE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DRAW AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2270-LIMIT-DRAW.
           IF HESTAT-DRAW-AMT < 25.00
               MOVE 25.00 TO HESTAT-DRAW-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HESTAT-DRAW-AMT > 50000.00
               MOVE 50000.00 TO HESTAT-DRAW-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2280-FLAG-ANNUAL-FEE.
           IF HESTAT-ANNUAL-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HESTAT-ANNUAL-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2290-COUNT-PAYMENT.
           IF HESTAT-PAYMENT-CNT < 9999999
               ADD 1 TO HESTAT-PAYMENT-CNT
           END-IF
           IF HESTAT-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO HESTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-REASON.
           EVALUATE HESTAT-REASON-CD
               WHEN 'KST'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY GRID TIER FOR THE BALANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2310-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-GRID-RATE(1) TO HESTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HESTAT-BALANCE-AMT NOT < WS-MIN-PAY-GRID-LOW(WS-TX)
                   MOVE WS-MIN-PAY-GRID-RATE(WS-TX) TO HESTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HESTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HESTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2320-LIMIT-BALANCE.
           IF HESTAT-BALANCE-YTD < 0.00
               MOVE 0.00 TO HESTAT-BALANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HESTAT-BALANCE-YTD > 2500.00
               MOVE 2500.00 TO HESTAT-BALANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE YTD FEE ON THE BALANCE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2330-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HESTAT-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HESTAT-ANNUAL-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HESTAT-ANNUAL-FEE-YTD ROUNDED =
                       HESTAT-BALANCE-AMT * HESTAT-DISCOUNT / 200
               WHEN HESTAT-TYPE-CD = 'CD'
                   MOVE 5.00 TO HESTAT-ANNUAL-FEE-YTD
               WHEN HESTAT-TYPE-CD = 'JW'
                   MOVE 12.00 TO HESTAT-ANNUAL-FEE-YTD
               WHEN OTHER
                   COMPUTE HESTAT-ANNUAL-FEE-YTD ROUNDED =
                       HESTAT-BALANCE-AMT * HESTAT-DISCOUNT / 100
           END-EVALUATE
           IF HESTAT-ANNUAL-FEE-YTD > 0 AND HESTAT-ANNUAL-FEE-YTD < 1.00
               MOVE 1.00 TO HESTAT-ANNUAL-FEE-YTD
           END-IF
           IF HESTAT-ANNUAL-FEE-YTD > 25.00
               MOVE 25.00 TO HESTAT-ANNUAL-FEE-YTD
           END-IF
           ADD HESTAT-ANNUAL-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY GRID TIER FOR THE APPRAISE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2340-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-GRID-RATE(1) TO HESTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HESTAT-APPRAISE-YTD NOT < WS-MIN-PAY-GRID-LOW(WS-TX)
                   MOVE WS-MIN-PAY-GRID-RATE(WS-TX) TO HESTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HESTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HESTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE ANNUAL FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2350-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HESTAT-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HESTAT-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-PREFERRED-FLAG = 'Y'
                   COMPUTE HESTAT-BALANCE-AMT ROUNDED =
                       HESTAT-ANNUAL-FEE-AMT * HESTAT-MARGIN / 200
               WHEN HESTAT-TYPE-CD = 'CD'
                   MOVE 15.00 TO HESTAT-BALANCE-AMT
               WHEN HESTAT-TYPE-CD = 'JW'
                   MOVE 10.00 TO HESTAT-BALANCE-AMT
               WHEN OTHER
                   COMPUTE HESTAT-BALANCE-AMT ROUNDED =
                       HESTAT-ANNUAL-FEE-AMT * HESTAT-MARGIN / 100
           END-EVALUATE
           IF HESTAT-BALANCE-AMT > 0 AND HESTAT-BALANCE-AMT < 1.00
               MOVE 1.00 TO HESTAT-BALANCE-AMT
           END-IF
           IF HESTAT-BALANCE-AMT > 50.00
               MOVE 50.00 TO HESTAT-BALANCE-AMT
           END-IF
           ADD HESTAT-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-LAST-MAINT-DATE > 0 AND HESTAT-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE DRAW AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2370-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HESTAT-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = HESTAT-DRAW-AMT *
                       HESTAT-DISCOUNT / 200
               WHEN HESTAT-DRAW-PERIOD-CD = 'CCV'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN HESTAT-DRAW-PERIOD-CD = 'JSU'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = HESTAT-DRAW-AMT *
                       HESTAT-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-REASON.
           EVALUATE HESTAT-REASON-CD
               WHEN 'KST'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE FEE ON THE ANNUAL FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2390-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HESTAT-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HESTAT-ANNUAL-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE HESTAT-ANNUAL-FEE-AMT ROUNDED =
                       HESTAT-ANNUAL-FEE-YTD * HESTAT-MARGIN / 200
               WHEN HESTAT-TYPE-CD = 'CD'
                   MOVE 5.00 TO HESTAT-ANNUAL-FEE-AMT
               WHEN HESTAT-TYPE-CD = 'JW'
                   MOVE 3.00 TO HESTAT-ANNUAL-FEE-AMT
               WHEN OTHER
                   COMPUTE HESTAT-ANNUAL-FEE-AMT ROUNDED =
                       HESTAT-ANNUAL-FEE-YTD * HESTAT-MARGIN / 100
           END-EVALUATE
           IF HESTAT-ANNUAL-FEE-AMT > 0 AND HESTAT-ANNUAL-FEE-AMT < 2.00
               MOVE 2.00 TO HESTAT-ANNUAL-FEE-AMT
           END-IF
           IF HESTAT-ANNUAL-FEE-AMT > 25.00
               MOVE 25.00 TO HESTAT-ANNUAL-FEE-AMT
           END-IF
           ADD HESTAT-ANNUAL-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2400-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HESTAT-REGION-CD
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HESTAT-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HESTAT-ANNUAL-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2410-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-OPEN-DATE > 0 AND HESTAT-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HESTAT-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HESTAT-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD HESTAT-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HESTAT-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HESTAT-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE FEE ON THE APPRAISE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2420-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HESTAT-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HESTAT-ANNUAL-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-PREFERRED-FLAG = 'Y'
                   COMPUTE HESTAT-ANNUAL-FEE-AMT ROUNDED =
                       HESTAT-APPRAISE-YTD * HESTAT-MARGIN / 200
               WHEN HESTAT-REASON-CD = 'KST'
                   MOVE 7.00 TO HESTAT-ANNUAL-FEE-AMT
               WHEN HESTAT-REASON-CD = 'NWS'
                   MOVE 5.00 TO HESTAT-ANNUAL-FEE-AMT
               WHEN OTHER
                   COMPUTE HESTAT-ANNUAL-FEE-AMT ROUNDED =
                       HESTAT-APPRAISE-YTD * HESTAT-MARGIN / 100
           END-EVALUATE
           IF HESTAT-ANNUAL-FEE-AMT > 0 AND HESTAT-ANNUAL-FEE-AMT < 2.00
               MOVE 2.00 TO HESTAT-ANNUAL-FEE-AMT
           END-IF
           IF HESTAT-ANNUAL-FEE-AMT > 25.00
               MOVE 25.00 TO HESTAT-ANNUAL-FEE-AMT
           END-IF
           ADD HESTAT-ANNUAL-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2430-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HESTAT-APPRAISE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = HESTAT-APPRAISE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2440-ACCRUE-ANNUAL-FEE.
           IF HESTAT-ANNUAL-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HESTAT-ANNUAL-FEE-YTD * HESTAT-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HESTAT-BALANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2450-FLAG-ANNUAL-FEE.
           IF HESTAT-ANNUAL-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HESTAT-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2460-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO HESTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2470-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HESTAT-DRAW-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2480-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO HESTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2490-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HESTAT-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HESTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       HESTAT-MARGIN / 200
               WHEN HESTAT-REGION-CD = 'BSD'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN HESTAT-REGION-CD = 'HKW'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       HESTAT-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BALANCE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2500-FLAG-BALANCE.
           IF HESTAT-BALANCE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HESTAT-BALANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2510-COUNT-RETRY.
           IF HESTAT-RETRY-CNT < 9999999
               ADD 1 TO HESTAT-RETRY-CNT
           END-IF
           IF HESTAT-RETRY-CNT NOT < 5
               MOVE 'Y' TO HESTAT-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DRAW AMT.
      *----------------------------------------------------------------*
       2520-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HESTAT-DRAW-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HESTAT-DRAW-AMT
           END-IF
           ADD HESTAT-DRAW-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2530-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-LAST-MAINT-DATE > 0 AND HESTAT-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-CLOSE-DATE > 0 AND HESTAT-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2550-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-EFFECTIVE-DATE > 0 AND HESTAT-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HESTAT-APPRAISE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HESTAT-APPRAISE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HESTAT-APPRAISE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HESTAT-APPRAISE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HESTAT-APPRAISE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2560-COUNT-PAYMENT.
           IF HESTAT-PAYMENT-CNT < 9999999
               ADD 1 TO HESTAT-PAYMENT-CNT
           END-IF
           IF HESTAT-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO HESTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ANNUAL FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2570-SPLIT-ANNUAL-FEE.
           COMPUTE WS-WORK-AMT = HESTAT-ANNUAL-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = HESTAT-ANNUAL-FEE-YTD - (WS-WORK-AMT
               * 3)
           COMPUTE HESTAT-BALANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN OPEN DATE IS PAST
      * NWS -> ZJM WHEN OPEN DATE IS PAST
      * ZJM -> KST WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2580-SET-REASON.
           EVALUATE HESTAT-REASON-CD
               WHEN 'KST'
                   IF HESTAT-OPEN-DATE > 0 AND HESTAT-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HESTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HESTAT-OPEN-DATE > 0 AND HESTAT-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HESTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HESTAT-OPEN-DATE > 0 AND HESTAT-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KST' TO HESTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2590-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-EFFECTIVE-DATE > 0 AND HESTAT-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2600-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO HESTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN LAST MAINT DATE IS PAST
      * JW -> KV WHEN LAST MAINT DATE IS PAST
      * KV -> WS WHEN LAST MAINT DATE IS PAST
      * WS -> XL WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2610-SET-TYPE.
           EVALUATE HESTAT-TYPE-CD
               WHEN 'CD'
                   IF HESTAT-LAST-MAINT-DATE > 0 AND
                       HESTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JW' TO HESTAT-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JW'
                   IF HESTAT-LAST-MAINT-DATE > 0 AND
                       HESTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KV' TO HESTAT-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF HESTAT-LAST-MAINT-DATE > 0 AND
                       HESTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'WS' TO HESTAT-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WS'
                   IF HESTAT-LAST-MAINT-DATE > 0 AND
                       HESTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XL' TO HESTAT-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2620-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HESTAT-BALANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2630-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * HESTAT-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HESTAT-ANNUAL-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRAW AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2640-LIMIT-DRAW.
           IF HESTAT-DRAW-AMT < 10.00
               MOVE 10.00 TO HESTAT-DRAW-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HESTAT-DRAW-AMT > 1000.00
               MOVE 1000.00 TO HESTAT-DRAW-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2650-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-GRID-RATE(1) TO HESTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-MIN-PAY-GRID-LOW(WS-TX)
                   MOVE WS-MIN-PAY-GRID-RATE(WS-TX) TO HESTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HESTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HESTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2660-DESCRIBE-REASON.
           EVALUATE HESTAT-REASON-CD
               WHEN 'KST'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E039 CONTACT REQUIRED
      * E361 ANNUAL FEE AMT NEGATIVE
      * E342 ANNUAL FEE AMT OVER 1000000
      * E647 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2670-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HESTAT-CONTACT = SPACES
                   MOVE 'E039' TO WS-REJECT-CODE
               WHEN HESTAT-ANNUAL-FEE-AMT < 0
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN HESTAT-ANNUAL-FEE-AMT > 1000000.00
                   MOVE 'E342' TO WS-REJECT-CODE
               WHEN HESTAT-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2680-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HESTAT-LAST-MAINT-DATE > 0 AND HESTAT-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HESTAT-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HESTAT-ANNUAL-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2690-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HESTAT-DRAW-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2700-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2710-ACCRUE-ANNUAL-FEE.
           IF HESTAT-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HESTAT-ANNUAL-FEE-AMT * HESTAT-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HESTAT-BALANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E540 CUST ID REQUIRED
      * E194 WORK AMT 2 NEGATIVE
      * E293 WORK AMT 2 OVER 25000
      * E738 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2720-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HESTAT-CUST-ID = SPACES
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E194' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E293' TO WS-REJECT-CODE
               WHEN HESTAT-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E738' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2730-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HESTAT-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HESTAT-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2740-FLAG-ANNUAL-FEE.
           IF HESTAT-ANNUAL-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HESTAT-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2750-COUNT-PAYMENT.
           IF HESTAT-PAYMENT-CNT < 9999999
               ADD 1 TO HESTAT-PAYMENT-CNT
           END-IF
           IF HESTAT-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO HESTAT-OVERRIDE-FLAG
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO HESTAT-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO HESTAT-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO HESTAT-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'HESTAT EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
