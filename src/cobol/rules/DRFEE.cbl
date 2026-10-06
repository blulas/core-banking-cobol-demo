      *================================================================*
      * PROGRAM  : DRFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DIVIDEND REINVESTMENT RULE - DATE RULES. CALLED BY  *
      *            THE DIVIDEND REINVESTMENT BATCH AND ONLINE PROGRAMS.*
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-04-04 BHS  ORIGINAL                                       *
      * 1989-12-01 RJT  PERFORMANCE - DIVIDEND READ AHEAD INC9758      *
      * 1995-05-21 MKL  SHARES-AMT FEE SCHEDULE UPDATE INC9610         *
      * 1997-11-09 EWS  REINVEST STATUS CODES ADDED INC7584            *
      * 1999-02-21 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-12-24 BHS  REINVEST NOTICE TEXT PER LEGAL PRB8474         *
      * 2002-09-18 CRP  PERFORMANCE - CASH-PAY READ AHEAD CHG3259      *
      * 2005-11-08 DPS  FEE ROUNDING - HALF UP CHG5866                 *
      * 2010-01-09 TWH  FEE FEE SCHEDULE UPDATE REQ0241                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRFEE.
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
      * WS-CASH-PAY-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-PAY-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000539000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000700500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000875500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000766500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000553000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000626000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015750000000607000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000168500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000606500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017000000000257000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017250000000677000'.
       01  WS-CASH-PAY-SCHED-TBL REDEFINES WS-CASH-PAY-SCHED-VALUES.
           05  WS-CASH-PAY-SCHED-ROW OCCURS 11 TIMES.
               10  WS-CASH-PAY-SCHED-LOW   PIC 9(09)V99.
               10  WS-CASH-PAY-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY DRFEEP.

       PROCEDURE DIVISION USING DRFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-FLAG-FEE
           PERFORM 2010-ACCUM-REASON
           PERFORM 2020-CHECK-OPEN
           PERFORM 2030-FLAG-SHARES-AMT
           PERFORM 2040-COUNT-PAYMENT
           PERFORM 2050-FLAG-ITEM
           PERFORM 2060-DESCRIBE-CYCLE
           PERFORM 2070-AGE-NOTICE
           PERFORM 2080-FLAG-SHARES-AMT
           PERFORM 2090-CHECK-LAST-PAY
           PERFORM 2100-AGE-NOTICE
           PERFORM 2110-ASSESS-SUSPEND
           PERFORM 2120-COUNT-NOTICE
           PERFORM 2130-ACCRUE-WITHHOLD
           PERFORM 2140-COUNT-PAYMENT
           PERFORM 2150-SPLIT-CASH-PAY
           PERFORM 2160-CHECK-NOTICE
           PERFORM 2170-EDIT-DESC
           PERFORM 2180-COUNT-PAYMENT
           PERFORM 2190-PRORATE-FEE
           PERFORM 2200-DESCRIBE-HOLDER-TYPE
           PERFORM 2210-COUNT-PAYMENT
           PERFORM 2220-SPLIT-REINVEST
           PERFORM 2230-EDIT-ADDR
           PERFORM 2240-AGE-NOTICE
           PERFORM 2250-COUNT-PAYMENT
           PERFORM 2260-CHECK-NEXT-REVIEW
           PERFORM 2270-SELECT-RATE
           PERFORM 2280-COUNT-NOTICE
           PERFORM 2290-PRORATE-WITHHOLD
           PERFORM 2300-SET-CYCLE
           PERFORM 2310-CHECK-NEXT-REVIEW
           PERFORM 2320-SELECT-DISCOUNT
           PERFORM 2330-COUNT-PAYMENT
           PERFORM 2340-SET-PLAN-OPTION
           PERFORM 2350-ACCRUE-SHARES-AMT
           PERFORM 2360-COUNT-PAYMENT
           PERFORM 2370-AGE-LAST-PAY
           PERFORM 2380-ASSESS-OVERRIDE
           PERFORM 2390-SET-PLAN-OPTION
           PERFORM 2400-AGE-NOTICE
           PERFORM 2410-SPLIT-CASH-PAY
           PERFORM 2420-ACCRUE-RATE
           PERFORM 2430-COUNT-PAYMENT
           PERFORM 2440-SET-CYCLE
           PERFORM 2450-CHECK-OPEN
           PERFORM 2460-AGE-NEXT-REVIEW
           PERFORM 2470-ASSESS-PREFERRED
           PERFORM 2480-CHECK-LAST-PAY
           PERFORM 2490-PRORATE-WITHHOLD
           PERFORM 2500-SELECT-DISCOUNT
           PERFORM 2510-AGE-NOTICE
           PERFORM 2520-PRORATE-SHARES-AMT
           PERFORM 2530-SPLIT-FEE
           PERFORM 2540-LIMIT-REINVEST
           PERFORM 2550-ACCUM-CYCLE
           PERFORM 2560-DESCRIBE-CYCLE
           PERFORM 2570-SELECT-RATE
           PERFORM 2580-SELECT-RATE
           PERFORM 2590-FLAG-CASH-PAY
           PERFORM 2600-PRORATE-FEE
           PERFORM 2610-COUNT-NOTICE
           PERFORM 2620-SPLIT-REINVEST
           PERFORM 2630-SET-REASON
           PERFORM 2640-AGE-NEXT-REVIEW
           PERFORM 2650-SELECT-DISCOUNT
           PERFORM 2660-FLAG-SHARES-AMT
           PERFORM 2670-ACCRUE-DISCOUNT
           PERFORM 2680-SPLIT-SHARES-AMT
           PERFORM 2690-COUNT-NOTICE
           PERFORM 2700-SELECT-DISCOUNT
           PERFORM 2710-LIMIT-FEE
           PERFORM 2720-CHECK-NEXT-REVIEW
           PERFORM 2730-COUNT-PAYMENT
           PERFORM 2740-ASSESS-OVERRIDE
           PERFORM 2750-FLAG-SHARES-AMT
           PERFORM 2760-ACCRUE-SHARES-AMT
           PERFORM 2770-LIMIT-ITEM
           PERFORM 2780-CHECK-NEXT-REVIEW
           PERFORM 2790-ACCUM-PLAN-OPTION
           PERFORM 2800-PRORATE-SHARES-AMT
           PERFORM 2810-SPLIT-FEE
           PERFORM 2820-SPLIT-WITHHOLD
           PERFORM 2830-COUNT-NOTICE
           PERFORM 2840-FLAG-ITEM
           PERFORM 2850-SPLIT-FEE
           PERFORM 2860-AGE-LAST-PAY
           PERFORM 2870-ASSESS-PREFERRED
           PERFORM 2880-LIMIT-ITEM
           PERFORM 2890-SET-HOLDER-TYPE
           PERFORM 2900-EDIT-ADDR
           PERFORM 2910-ASSESS-OVERRIDE
           PERFORM 2920-FLAG-SHARES-AMT
           PERFORM 2930-ACCRUE-FEE
           PERFORM 2940-PRORATE-SHARES-AMT
           PERFORM 2950-ASSESS-OVERRIDE
           PERFORM 2960-PRORATE-REINVEST
           PERFORM 2970-ACCRUE-DISCOUNT
           PERFORM 2980-ASSESS-OVERRIDE
           PERFORM 2990-LIMIT-SHARES-AMT
           PERFORM 3000-SELECT-DISCOUNT
           PERFORM 3010-LIMIT-FEE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO DRFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2000-FLAG-FEE.
           IF DRFEE-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRFEE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRFEE-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2020-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO DRFEE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2030-FLAG-SHARES-AMT.
           IF DRFEE-SHARES-AMT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRFEE-SHARES-AMT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-PAYMENT.
           IF DRFEE-PAYMENT-CNT < 9999999
               ADD 1 TO DRFEE-PAYMENT-CNT
           END-IF
           IF DRFEE-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO DRFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2050-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2060-DESCRIBE-CYCLE.
           EVALUATE DRFEE-CYCLE-CD
               WHEN 'DE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NOTICE-DATE > 0 AND DRFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRFEE-SHARES-AMT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRFEE-SHARES-AMT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD DRFEE-SHARES-AMT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRFEE-SHARES-AMT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRFEE-SHARES-AMT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHARES AMT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2080-FLAG-SHARES-AMT.
           IF DRFEE-SHARES-AMT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRFEE-SHARES-AMT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2090-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO DRFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2100-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NOTICE-DATE > 0 AND DRFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NOTICE-DATE TO DTU-DATE-1
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
      * ASSESS THE WITHHOLD YTD FEE ON THE SHARES AMT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2110-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRFEE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRFEE-WITHHOLD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE DRFEE-WITHHOLD-YTD ROUNDED =
                       DRFEE-SHARES-AMT-AMT * DRFEE-DISCOUNT / 200
               WHEN DRFEE-REASON-CD = 'B'
                   MOVE 7.00 TO DRFEE-WITHHOLD-YTD
               WHEN DRFEE-REASON-CD = 'G'
                   MOVE 15.00 TO DRFEE-WITHHOLD-YTD
               WHEN OTHER
                   COMPUTE DRFEE-WITHHOLD-YTD ROUNDED =
                       DRFEE-SHARES-AMT-AMT * DRFEE-DISCOUNT / 100
           END-EVALUATE
           IF DRFEE-WITHHOLD-YTD > 0 AND DRFEE-WITHHOLD-YTD < 10.00
               MOVE 10.00 TO DRFEE-WITHHOLD-YTD
           END-IF
           IF DRFEE-WITHHOLD-YTD > 50.00
               MOVE 50.00 TO DRFEE-WITHHOLD-YTD
           END-IF
           ADD DRFEE-WITHHOLD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-NOTICE.
           IF DRFEE-NOTICE-CNT < 9999999
               ADD 1 TO DRFEE-NOTICE-CNT
           END-IF
           IF DRFEE-NOTICE-CNT NOT < 12
               MOVE 'Y' TO DRFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WITHHOLD YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2130-ACCRUE-WITHHOLD.
           IF DRFEE-WITHHOLD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRFEE-WITHHOLD-YTD * DRFEE-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-PAYMENT.
           IF DRFEE-PAYMENT-CNT < 9999999
               ADD 1 TO DRFEE-PAYMENT-CNT
           END-IF
           IF DRFEE-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO DRFEE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2150-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRFEE-CASH-PAY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRFEE-CASH-PAY-YTD - (WS-WORK-AMT *
               4)
           COMPUTE DRFEE-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2160-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E240 DESC REQUIRED
      * E363 CASH PAY YTD NEGATIVE
      * E002 CASH PAY YTD OVER 10000
      * E065 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2170-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRFEE-DESC = SPACES
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN DRFEE-CASH-PAY-YTD < 0
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN DRFEE-CASH-PAY-YTD > 10000.00
                   MOVE 'E002' TO WS-REJECT-CODE
               WHEN DRFEE-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2180-COUNT-PAYMENT.
           IF DRFEE-PAYMENT-CNT < 9999999
               ADD 1 TO DRFEE-PAYMENT-CNT
           END-IF
           IF DRFEE-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO DRFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       2190-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRFEE-FEE-AMT ROUNDED =
                   DRFEE-REINVEST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRFEE-FEE-AMT
           END-IF
           ADD DRFEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2200-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRFEE-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2210-COUNT-PAYMENT.
           IF DRFEE-PAYMENT-CNT < 9999999
               ADD 1 TO DRFEE-PAYMENT-CNT
           END-IF
           IF DRFEE-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO DRFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2220-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRFEE-REINVEST-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRFEE-REINVEST-YTD - (WS-WORK-AMT *
               12)
           COMPUTE DRFEE-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E761 ADDR REQUIRED
      * E017 SHARES AMT YTD NEGATIVE
      * E447 SHARES AMT YTD OVER 25000
      * E357 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRFEE-ADDR = SPACES
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN DRFEE-SHARES-AMT-YTD < 0
                   MOVE 'E017' TO WS-REJECT-CODE
               WHEN DRFEE-SHARES-AMT-YTD > 25000.00
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN DRFEE-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E357' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2240-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NOTICE-DATE > 0 AND DRFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2250-COUNT-PAYMENT.
           IF DRFEE-PAYMENT-CNT < 9999999
               ADD 1 TO DRFEE-PAYMENT-CNT
           END-IF
           IF DRFEE-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO DRFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2260-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO DRFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY SCHED TIER FOR THE CASH PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2270-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-SCHED-RATE(1) TO DRFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRFEE-CASH-PAY-YTD NOT < WS-CASH-PAY-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-PAY-SCHED-RATE(WS-TX) TO DRFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-NOTICE.
           IF DRFEE-NOTICE-CNT < 9999999
               ADD 1 TO DRFEE-NOTICE-CNT
           END-IF
           IF DRFEE-NOTICE-CNT NOT < 5
               MOVE 'Y' TO DRFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WITHHOLD YTD.
      *----------------------------------------------------------------*
       2290-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRFEE-WITHHOLD-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRFEE-WITHHOLD-YTD
           END-IF
           ADD DRFEE-WITHHOLD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN NEXT REVIEW DATE IS PAST
      * FB -> PC WHEN NEXT REVIEW DATE IS PAST
      * PC -> WY WHEN NEXT REVIEW DATE IS PAST
      * WY -> XD WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2300-SET-CYCLE.
           EVALUATE DRFEE-CYCLE-CD
               WHEN 'DE'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRFEE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY SCHED TIER FOR THE SHARES AMT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2320-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-SCHED-RATE(1) TO DRFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRFEE-SHARES-AMT-AMT NOT <
                   WS-CASH-PAY-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-PAY-SCHED-RATE(WS-TX) TO DRFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2330-COUNT-PAYMENT.
           IF DRFEE-PAYMENT-CNT < 9999999
               ADD 1 TO DRFEE-PAYMENT-CNT
           END-IF
           IF DRFEE-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO DRFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN OPTION CD:
      * E -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> T WHEN NEXT REVIEW DATE IS PAST
      * T -> Z WHEN NEXT REVIEW DATE IS PAST
      * Z -> E WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2340-SET-PLAN-OPTION.
           EVALUATE DRFEE-PLAN-OPTION-CD
               WHEN 'E'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO DRFEE-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'T' TO DRFEE-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'Z' TO DRFEE-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'E' TO DRFEE-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2350-ACCRUE-SHARES-AMT.
           IF DRFEE-SHARES-AMT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRFEE-SHARES-AMT-YTD * DRFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-PAYMENT.
           IF DRFEE-PAYMENT-CNT < 9999999
               ADD 1 TO DRFEE-PAYMENT-CNT
           END-IF
           IF DRFEE-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO DRFEE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2370-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-LAST-PAY-DATE > 0 AND DRFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE REINVEST YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2380-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRFEE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE DRFEE-FEE-AMT ROUNDED = DRFEE-REINVEST-YTD *
                       DRFEE-RATE / 200
               WHEN DRFEE-CYCLE-CD = 'DE'
                   MOVE 12.00 TO DRFEE-FEE-AMT
               WHEN DRFEE-CYCLE-CD = 'FB'
                   MOVE 5.00 TO DRFEE-FEE-AMT
               WHEN OTHER
                   COMPUTE DRFEE-FEE-AMT ROUNDED = DRFEE-REINVEST-YTD *
                       DRFEE-RATE / 100
           END-EVALUATE
           IF DRFEE-FEE-AMT > 0 AND DRFEE-FEE-AMT < 5.00
               MOVE 5.00 TO DRFEE-FEE-AMT
           END-IF
           IF DRFEE-FEE-AMT > 100.00
               MOVE 100.00 TO DRFEE-FEE-AMT
           END-IF
           ADD DRFEE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN OPTION CD:
      * E -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> T WHEN NEXT REVIEW DATE IS PAST
      * T -> Z WHEN NEXT REVIEW DATE IS PAST
      * Z -> E WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2390-SET-PLAN-OPTION.
           EVALUATE DRFEE-PLAN-OPTION-CD
               WHEN 'E'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO DRFEE-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'T' TO DRFEE-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'Z' TO DRFEE-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF DRFEE-NEXT-REVIEW-DATE > 0 AND
                       DRFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'E' TO DRFEE-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2400-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NOTICE-DATE > 0 AND DRFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRFEE-REINVEST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2410-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRFEE-CASH-PAY-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRFEE-CASH-PAY-YTD - (WS-WORK-AMT *
               12)
           COMPUTE DRFEE-SHARES-AMT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2420-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRFEE-RATE / 100 / 360
               ADD WS-WORK-AMT TO DRFEE-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-PAYMENT.
           IF DRFEE-PAYMENT-CNT < 9999999
               ADD 1 TO DRFEE-PAYMENT-CNT
           END-IF
           IF DRFEE-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO DRFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN LAST PAY DATE IS PAST
      * FB -> PC WHEN LAST PAY DATE IS PAST
      * PC -> WY WHEN LAST PAY DATE IS PAST
      * WY -> XD WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2440-SET-CYCLE.
           EVALUATE DRFEE-CYCLE-CD
               WHEN 'DE'
                   IF DRFEE-LAST-PAY-DATE > 0 AND DRFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRFEE-LAST-PAY-DATE > 0 AND DRFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRFEE-LAST-PAY-DATE > 0 AND DRFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRFEE-LAST-PAY-DATE > 0 AND DRFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRFEE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2450-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2460-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NEXT-REVIEW-DATE > 0 AND DRFEE-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2470-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRFEE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO DRFEE-REINVEST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRFEE-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRFEE-REINVEST-YTD ROUNDED = WS-WORK-AMT-2 *
                       DRFEE-RATE / 200
               WHEN DRFEE-REASON-CD = 'B'
                   MOVE 12.00 TO DRFEE-REINVEST-YTD
               WHEN DRFEE-REASON-CD = 'G'
                   MOVE 3.00 TO DRFEE-REINVEST-YTD
               WHEN OTHER
                   COMPUTE DRFEE-REINVEST-YTD ROUNDED = WS-WORK-AMT-2 *
                       DRFEE-RATE / 100
           END-EVALUATE
           IF DRFEE-REINVEST-YTD > 0 AND DRFEE-REINVEST-YTD < 1.00
               MOVE 1.00 TO DRFEE-REINVEST-YTD
           END-IF
           IF DRFEE-REINVEST-YTD > 50.00
               MOVE 50.00 TO DRFEE-REINVEST-YTD
           END-IF
           ADD DRFEE-REINVEST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2480-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRFEE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2490-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   DRFEE-WITHHOLD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY SCHED TIER FOR THE REINVEST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2500-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-SCHED-RATE(1) TO DRFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRFEE-REINVEST-YTD NOT < WS-CASH-PAY-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-PAY-SCHED-RATE(WS-TX) TO DRFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2510-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NOTICE-DATE > 0 AND DRFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRFEE-WITHHOLD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD DRFEE-WITHHOLD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRFEE-WITHHOLD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRFEE-WITHHOLD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRFEE-WITHHOLD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2520-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   DRFEE-SHARES-AMT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE DRFEE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2540-LIMIT-REINVEST.
           IF DRFEE-REINVEST-YTD < 25.00
               MOVE 25.00 TO DRFEE-REINVEST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRFEE-REINVEST-YTD > 10000.00
               MOVE 10000.00 TO DRFEE-REINVEST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2550-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRFEE-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRFEE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2560-DESCRIBE-CYCLE.
           EVALUATE DRFEE-CYCLE-CD
               WHEN 'DE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY SCHED TIER FOR THE WITHHOLD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2570-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-SCHED-RATE(1) TO DRFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRFEE-WITHHOLD-YTD NOT < WS-CASH-PAY-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-PAY-SCHED-RATE(WS-TX) TO DRFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY SCHED TIER FOR THE SHARES AMT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2580-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-SCHED-RATE(1) TO DRFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRFEE-SHARES-AMT-AMT NOT <
                   WS-CASH-PAY-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-PAY-SCHED-RATE(WS-TX) TO DRFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2590-FLAG-CASH-PAY.
           IF DRFEE-CASH-PAY-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRFEE-CASH-PAY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       2600-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRFEE-FEE-AMT ROUNDED =
                   DRFEE-REINVEST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRFEE-FEE-AMT
           END-IF
           ADD DRFEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2610-COUNT-NOTICE.
           IF DRFEE-NOTICE-CNT < 9999999
               ADD 1 TO DRFEE-NOTICE-CNT
           END-IF
           IF DRFEE-NOTICE-CNT NOT < 12
               MOVE 'Y' TO DRFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2620-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRFEE-REINVEST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRFEE-REINVEST-YTD - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> G WHEN OPEN DATE IS PAST
      * G -> J WHEN OPEN DATE IS PAST
      * J -> K WHEN OPEN DATE IS PAST
      * K -> L WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2630-SET-REASON.
           EVALUATE DRFEE-REASON-CD
               WHEN 'B'
                   IF DRFEE-OPEN-DATE > 0 AND DRFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO DRFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF DRFEE-OPEN-DATE > 0 AND DRFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO DRFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF DRFEE-OPEN-DATE > 0 AND DRFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRFEE-OPEN-DATE > 0 AND DRFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2640-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NEXT-REVIEW-DATE > 0 AND DRFEE-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRFEE-WITHHOLD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRFEE-WITHHOLD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRFEE-WITHHOLD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD DRFEE-WITHHOLD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRFEE-WITHHOLD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY SCHED TIER FOR THE SHARES AMT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2650-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-SCHED-RATE(1) TO DRFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRFEE-SHARES-AMT-AMT NOT <
                   WS-CASH-PAY-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-PAY-SCHED-RATE(WS-TX) TO DRFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2660-FLAG-SHARES-AMT.
           IF DRFEE-SHARES-AMT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRFEE-SHARES-AMT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2670-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRFEE-SHARES-AMT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2680-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE DRFEE-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2690-COUNT-NOTICE.
           IF DRFEE-NOTICE-CNT < 9999999
               ADD 1 TO DRFEE-NOTICE-CNT
           END-IF
           IF DRFEE-NOTICE-CNT NOT < 10
               MOVE 'Y' TO DRFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY SCHED TIER FOR THE SHARES AMT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2700-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-SCHED-RATE(1) TO DRFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRFEE-SHARES-AMT-YTD NOT <
                   WS-CASH-PAY-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-PAY-SCHED-RATE(WS-TX) TO DRFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2710-LIMIT-FEE.
           IF DRFEE-FEE-AMT < 0.00
               MOVE 0.00 TO DRFEE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRFEE-FEE-AMT > 50000.00
               MOVE 50000.00 TO DRFEE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2720-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2730-COUNT-PAYMENT.
           IF DRFEE-PAYMENT-CNT < 9999999
               ADD 1 TO DRFEE-PAYMENT-CNT
           END-IF
           IF DRFEE-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO DRFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES YTD FEE ON THE CASH PAY YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2740-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRFEE-SHARES-AMT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE DRFEE-SHARES-AMT-YTD ROUNDED =
                       DRFEE-CASH-PAY-YTD * DRFEE-DISCOUNT / 200
               WHEN DRFEE-CYCLE-CD = 'DE'
                   MOVE 5.00 TO DRFEE-SHARES-AMT-YTD
               WHEN DRFEE-CYCLE-CD = 'FB'
                   MOVE 3.00 TO DRFEE-SHARES-AMT-YTD
               WHEN OTHER
                   COMPUTE DRFEE-SHARES-AMT-YTD ROUNDED =
                       DRFEE-CASH-PAY-YTD * DRFEE-DISCOUNT / 100
           END-EVALUATE
           IF DRFEE-SHARES-AMT-YTD > 0 AND DRFEE-SHARES-AMT-YTD < 10.00
               MOVE 10.00 TO DRFEE-SHARES-AMT-YTD
           END-IF
           IF DRFEE-SHARES-AMT-YTD > 25.00
               MOVE 25.00 TO DRFEE-SHARES-AMT-YTD
           END-IF
           ADD DRFEE-SHARES-AMT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SHARES AMT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2750-FLAG-SHARES-AMT.
           IF DRFEE-SHARES-AMT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRFEE-SHARES-AMT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2760-ACCRUE-SHARES-AMT.
           IF DRFEE-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRFEE-SHARES-AMT-AMT * DRFEE-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRFEE-CASH-PAY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2770-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2780-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT YTD BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2790-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRFEE-PLAN-OPTION-CD
                   ADD DRFEE-SHARES-AMT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRFEE-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRFEE-SHARES-AMT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHARES AMT AMT.
      *----------------------------------------------------------------*
       2800-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRFEE-SHARES-AMT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRFEE-SHARES-AMT-AMT
           END-IF
           ADD DRFEE-SHARES-AMT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2810-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRFEE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRFEE-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE DRFEE-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2820-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = DRFEE-WITHHOLD-YTD / 3
           COMPUTE WS-WORK-AMT-2 = DRFEE-WITHHOLD-YTD - (WS-WORK-AMT *
               3)
           COMPUTE DRFEE-SHARES-AMT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2830-COUNT-NOTICE.
           IF DRFEE-NOTICE-CNT < 9999999
               ADD 1 TO DRFEE-NOTICE-CNT
           END-IF
           IF DRFEE-NOTICE-CNT NOT < 3
               MOVE 'Y' TO DRFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2840-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2850-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE DRFEE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2860-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRFEE-LAST-PAY-DATE > 0 AND DRFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRFEE-SHARES-AMT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD DRFEE-SHARES-AMT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRFEE-SHARES-AMT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRFEE-SHARES-AMT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRFEE-SHARES-AMT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SHARES AMT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2870-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRFEE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRFEE-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = DRFEE-SHARES-AMT-YTD
                       * DRFEE-DISCOUNT / 200
               WHEN DRFEE-CYCLE-CD = 'DE'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN DRFEE-CYCLE-CD = 'FB'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = DRFEE-SHARES-AMT-YTD
                       * DRFEE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2880-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN OPEN DATE IS PAST
      * H -> K WHEN OPEN DATE IS PAST
      * K -> L WHEN OPEN DATE IS PAST
      * L -> M WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2890-SET-HOLDER-TYPE.
           EVALUATE DRFEE-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRFEE-OPEN-DATE > 0 AND DRFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO DRFEE-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRFEE-OPEN-DATE > 0 AND DRFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRFEE-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRFEE-OPEN-DATE > 0 AND DRFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRFEE-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRFEE-OPEN-DATE > 0 AND DRFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO DRFEE-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E869 ADDR REQUIRED
      * E798 CASH PAY YTD NEGATIVE
      * E432 CASH PAY YTD OVER 50000
      * E558 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2900-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRFEE-ADDR = SPACES
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN DRFEE-CASH-PAY-YTD < 0
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN DRFEE-CASH-PAY-YTD > 50000.00
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN DRFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E558' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2910-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRFEE-CASH-PAY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE DRFEE-CASH-PAY-YTD ROUNDED = WS-WORK-AMT-2 *
                       DRFEE-RATE / 200
               WHEN DRFEE-HOLDER-TYPE-CD = 'C'
                   MOVE 20.00 TO DRFEE-CASH-PAY-YTD
               WHEN DRFEE-HOLDER-TYPE-CD = 'H'
                   MOVE 7.00 TO DRFEE-CASH-PAY-YTD
               WHEN OTHER
                   COMPUTE DRFEE-CASH-PAY-YTD ROUNDED = WS-WORK-AMT-2 *
                       DRFEE-RATE / 100
           END-EVALUATE
           IF DRFEE-CASH-PAY-YTD > 0 AND DRFEE-CASH-PAY-YTD < 1.00
               MOVE 1.00 TO DRFEE-CASH-PAY-YTD
           END-IF
           IF DRFEE-CASH-PAY-YTD > 35.00
               MOVE 35.00 TO DRFEE-CASH-PAY-YTD
           END-IF
           ADD DRFEE-CASH-PAY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SHARES AMT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2920-FLAG-SHARES-AMT.
           IF DRFEE-SHARES-AMT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRFEE-SHARES-AMT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2930-ACCRUE-FEE.
           IF DRFEE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRFEE-FEE-AMT * DRFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHARES AMT AMT.
      *----------------------------------------------------------------*
       2940-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRFEE-SHARES-AMT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRFEE-SHARES-AMT-AMT
           END-IF
           ADD DRFEE-SHARES-AMT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES YTD FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2950-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRFEE-SHARES-AMT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE DRFEE-SHARES-AMT-YTD ROUNDED = WS-WORK-AMT *
                       DRFEE-RATE / 200
               WHEN DRFEE-REASON-CD = 'B'
                   MOVE 20.00 TO DRFEE-SHARES-AMT-YTD
               WHEN DRFEE-REASON-CD = 'G'
                   MOVE 15.00 TO DRFEE-SHARES-AMT-YTD
               WHEN OTHER
                   COMPUTE DRFEE-SHARES-AMT-YTD ROUNDED = WS-WORK-AMT *
                       DRFEE-RATE / 100
           END-EVALUATE
           IF DRFEE-SHARES-AMT-YTD > 0 AND DRFEE-SHARES-AMT-YTD < 2.00
               MOVE 2.00 TO DRFEE-SHARES-AMT-YTD
           END-IF
           IF DRFEE-SHARES-AMT-YTD > 100.00
               MOVE 100.00 TO DRFEE-SHARES-AMT-YTD
           END-IF
           ADD DRFEE-SHARES-AMT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE REINVEST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2960-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   DRFEE-REINVEST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2970-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRFEE-REINVEST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST YTD FEE ON THE SHARES AMT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2980-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRFEE-REINVEST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE DRFEE-REINVEST-YTD ROUNDED =
                       DRFEE-SHARES-AMT-YTD * DRFEE-RATE / 200
               WHEN DRFEE-HOLDER-TYPE-CD = 'C'
                   MOVE 20.00 TO DRFEE-REINVEST-YTD
               WHEN DRFEE-HOLDER-TYPE-CD = 'H'
                   MOVE 20.00 TO DRFEE-REINVEST-YTD
               WHEN OTHER
                   COMPUTE DRFEE-REINVEST-YTD ROUNDED =
                       DRFEE-SHARES-AMT-YTD * DRFEE-RATE / 100
           END-EVALUATE
           IF DRFEE-REINVEST-YTD > 0 AND DRFEE-REINVEST-YTD < 2.00
               MOVE 2.00 TO DRFEE-REINVEST-YTD
           END-IF
           IF DRFEE-REINVEST-YTD > 25.00
               MOVE 25.00 TO DRFEE-REINVEST-YTD
           END-IF
           ADD DRFEE-REINVEST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SHARES AMT YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2990-LIMIT-SHARES-AMT.
           IF DRFEE-SHARES-AMT-YTD < 50.00
               MOVE 50.00 TO DRFEE-SHARES-AMT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRFEE-SHARES-AMT-YTD > 500.00
               MOVE 500.00 TO DRFEE-SHARES-AMT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-SCHED-RATE(1) TO DRFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-CASH-PAY-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-PAY-SCHED-RATE(WS-TX) TO DRFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-FEE.
           IF DRFEE-FEE-AMT < 50.00
               MOVE 50.00 TO DRFEE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRFEE-FEE-AMT > 10000.00
               MOVE 10000.00 TO DRFEE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO DRFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO DRFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO DRFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'DRFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
