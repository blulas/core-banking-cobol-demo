      *================================================================*
      * PROGRAM  : DRELG                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DIVIDEND REINVESTMENT RULE - DATE RULES. CALLED BY  *
      *            THE DIVIDEND REINVESTMENT BATCH AND ONLINE PROGRAMS.*
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-03-07 MKL  ORIGINAL                                       *
      * 1990-03-18 BHS  AUDIT FINDING - CASH-PAY CONTROL TOTALS PRB2955*
      * 1990-08-09 DPS  FEE TABLE EXPANDED INC9369                     *
      * 1991-04-09 DPS  DIVIDEND EXCEPTIONS TO OPS QUEUE REQ3924       *
      * 1993-07-02 RJT  REINVEST CALCULATION CORRECTED REQ9690         *
      * 1995-01-17 LMB  REINVEST STATUS CODES ADDED PRB4709            *
      * 1996-02-04 RJT  WITHHOLD NOTICE TEXT PER LEGAL PRB6453         *
      * 2001-09-13 RJT  FEE STATUS CODES ADDED CHG7652                 *
      * 2006-10-20 RJT  REINVEST TABLE EXPANDED CHG6759                *
      * 2007-06-28 SKN  REINVEST TABLE EXPANDED PRB9104                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRELG.
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
                                           VALUE 19.000000.
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
      * WS-REINVEST-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REINVEST-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000339500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000882000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000186000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000115000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014500000000288000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000809500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000230000'.
       01  WS-REINVEST-GRID-TBL REDEFINES WS-REINVEST-GRID-VALUES.
           05  WS-REINVEST-GRID-ROW OCCURS 7 TIMES.
               10  WS-REINVEST-GRID-LOW    PIC 9(09)V99.
               10  WS-REINVEST-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHARES-A-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHARES-A-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000067000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000326000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000878500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000420000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017600000000624500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027600000000113500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00037600000000495500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00037650000000740500'.
       01  WS-SHARES-A-SCHED-TBL REDEFINES WS-SHARES-A-SCHED-VALUES.
           05  WS-SHARES-A-SCHED-ROW OCCURS 8 TIMES.
               10  WS-SHARES-A-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHARES-A-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REINVEST-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REINVEST-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000424500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000109000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000308000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000561000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000896500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015200000000482000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015300000000437000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015800000000594000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020800000000751000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030800000000099500'.
       01  WS-REINVEST-BAND-TBL REDEFINES WS-REINVEST-BAND-VALUES.
           05  WS-REINVEST-BAND-ROW OCCURS 10 TIMES.
               10  WS-REINVEST-BAND-LOW    PIC 9(09)V99.
               10  WS-REINVEST-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY DRELGP.

       PROCEDURE DIVISION USING DRELG-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ASSESS-SUSPEND
           PERFORM 2010-ACCUM-SOURCE
           PERFORM 2020-SET-SOURCE
           PERFORM 2030-AGE-LAST-MAINT
           PERFORM 2040-SPLIT-REINVEST
           PERFORM 2050-SET-CHANNEL
           PERFORM 2060-COUNT-EXCEPT
           PERFORM 2070-AGE-MATURITY
           PERFORM 2080-ACCRUE-CASH-PAY
           PERFORM 2090-PRORATE-WITHHOLD
           PERFORM 2100-ACCRUE-WITHHOLD
           PERFORM 2110-COUNT-ITEM
           PERFORM 2120-SELECT-DISCOUNT
           PERFORM 2130-DESCRIBE-CYCLE
           PERFORM 2140-EDIT-REF-NO
           PERFORM 2150-EDIT-REF-NO
           PERFORM 2160-DESCRIBE-PLAN-OPTION
           PERFORM 2170-EDIT-REF-NO
           PERFORM 2180-EDIT-CUST-ID
           PERFORM 2190-EDIT-CUST-ID
           PERFORM 2200-FLAG-REINVEST
           PERFORM 2210-EDIT-REF-NO
           PERFORM 2220-EDIT-CUST-ID
           PERFORM 2230-DESCRIBE-CYCLE
           PERFORM 2240-COUNT-ITEM
           PERFORM 2250-PRORATE-REINVEST
           PERFORM 2260-SELECT-DISCOUNT
           PERFORM 2270-SET-CYCLE
           PERFORM 2280-PRORATE-CASH-PAY
           PERFORM 2290-DESCRIBE-SOURCE
           PERFORM 2300-AGE-MATURITY
           PERFORM 2310-PRORATE-WITHHOLD
           PERFORM 2320-ACCRUE-SHARES-AMT
           PERFORM 2330-DESCRIBE-PLAN-OPTION
           PERFORM 2340-EDIT-CUST-ID
           PERFORM 2350-LIMIT-WITHHOLD
           PERFORM 2360-PRORATE-ITEM
           PERFORM 2370-SPLIT-REINVEST
           PERFORM 2380-PRORATE-CASH-PAY
           PERFORM 2390-PRORATE-CASH-PAY
           PERFORM 2400-EDIT-CUST-ID
           PERFORM 2410-LIMIT-CASH-PAY
           PERFORM 2420-COUNT-EXCEPT
           PERFORM 2430-AGE-MATURITY
           PERFORM 2440-PRORATE-REINVEST
           PERFORM 2450-SPLIT-CASH-PAY
           PERFORM 2460-DESCRIBE-CYCLE
           PERFORM 2470-PRORATE-SHARES-AMT
           PERFORM 2480-SET-CHANNEL
           PERFORM 2490-LIMIT-REINVEST
           PERFORM 2500-FLAG-WITHHOLD
           PERFORM 2510-SPLIT-CASH-PAY
           PERFORM 2520-COUNT-ITEM
           PERFORM 2530-CHECK-EFFECTIVE
           PERFORM 2540-EDIT-CUST-ID
           PERFORM 2550-ASSESS-SUSPEND
           PERFORM 2560-ASSESS-PREFERRED
           PERFORM 2570-ASSESS-PREFERRED
           PERFORM 2580-ACCRUE-SHARES-AMT
           PERFORM 2590-PRORATE-CASH-PAY
           PERFORM 2600-SELECT-RATE
           PERFORM 2610-EDIT-REF-NO
           PERFORM 2620-LIMIT-WITHHOLD
           PERFORM 2630-FLAG-ITEM
           PERFORM 2640-SET-CYCLE
           PERFORM 2650-ASSESS-SUSPEND
           PERFORM 2660-AGE-LAST-MAINT
           PERFORM 2670-SELECT-DISCOUNT
           PERFORM 2680-SELECT-DISCOUNT
           PERFORM 2690-FLAG-WITHHOLD
           PERFORM 2700-LIMIT-SHARES-AMT
           PERFORM 2710-AGE-EFFECTIVE
           PERFORM 2720-DESCRIBE-CHANNEL
           PERFORM 2730-ACCUM-SOURCE
           PERFORM 2740-EDIT-REF-NO
           PERFORM 2750-LIMIT-WITHHOLD
           PERFORM 2760-FLAG-WITHHOLD
           PERFORM 2770-ACCRUE-RATE
           PERFORM 2780-FLAG-ITEM
           PERFORM 2790-COUNT-ITEM
           PERFORM 2800-SPLIT-SHARES-AMT
           PERFORM 2810-SET-SOURCE
           PERFORM 2820-AGE-OPEN
           PERFORM 2830-LIMIT-REINVEST
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO DRELG-RETURN-CODE.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD FEE ON THE CASH PAY AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2000-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRELG-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRELG-WITHHOLD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRELG-STOP-FLAG = 'Y'
                   COMPUTE DRELG-WITHHOLD-AMT ROUNDED =
                       DRELG-CASH-PAY-AMT * DRELG-RATE / 200
               WHEN DRELG-CHANNEL-CD = 'RM'
                   MOVE 5.00 TO DRELG-WITHHOLD-AMT
               WHEN DRELG-CHANNEL-CD = 'VP'
                   MOVE 12.00 TO DRELG-WITHHOLD-AMT
               WHEN OTHER
                   COMPUTE DRELG-WITHHOLD-AMT ROUNDED =
                       DRELG-CASH-PAY-AMT * DRELG-RATE / 100
           END-EVALUATE
           IF DRELG-WITHHOLD-AMT > 0 AND DRELG-WITHHOLD-AMT < 2.00
               MOVE 2.00 TO DRELG-WITHHOLD-AMT
           END-IF
           IF DRELG-WITHHOLD-AMT > 150.00
               MOVE 150.00 TO DRELG-WITHHOLD-AMT
           END-IF
           ADD DRELG-WITHHOLD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRELG-SOURCE-CD
                   ADD DRELG-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRELG-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRELG-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN MATURITY DATE IS PAST
      * FL -> NK WHEN MATURITY DATE IS PAST
      * NK -> YC WHEN MATURITY DATE IS PAST
      * YC -> BT WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2020-SET-SOURCE.
           EVALUATE DRELG-SOURCE-CD
               WHEN 'BT'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRELG-LAST-MAINT-DATE > 0 AND DRELG-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRELG-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRELG-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRELG-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRELG-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRELG-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRELG-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2040-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRELG-REINVEST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRELG-REINVEST-AMT - (WS-WORK-AMT *
               3)
           COMPUTE DRELG-CASH-PAY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * RM -> VP WHEN LAST MAINT DATE IS PAST
      * VP -> YS WHEN LAST MAINT DATE IS PAST
      * YS -> RM WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2050-SET-CHANNEL.
           EVALUATE DRELG-CHANNEL-CD
               WHEN 'RM'
                   IF DRELG-LAST-MAINT-DATE > 0 AND
                       DRELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VP' TO DRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VP'
                   IF DRELG-LAST-MAINT-DATE > 0 AND
                       DRELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'YS' TO DRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YS'
                   IF DRELG-LAST-MAINT-DATE > 0 AND
                       DRELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'RM' TO DRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2060-COUNT-EXCEPT.
           IF DRELG-EXCEPT-CNT < 9999999
               ADD 1 TO DRELG-EXCEPT-CNT
           END-IF
           IF DRELG-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO DRELG-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRELG-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRELG-CASH-PAY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRELG-CASH-PAY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRELG-CASH-PAY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD DRELG-CASH-PAY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRELG-CASH-PAY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2080-ACCRUE-CASH-PAY.
           IF DRELG-CASH-PAY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRELG-CASH-PAY-AMT * DRELG-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WITHHOLD AMT.
      *----------------------------------------------------------------*
       2090-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRELG-WITHHOLD-AMT ROUNDED =
                   DRELG-REINVEST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRELG-WITHHOLD-AMT
           END-IF
           ADD DRELG-WITHHOLD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WITHHOLD AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2100-ACCRUE-WITHHOLD.
           IF DRELG-WITHHOLD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRELG-WITHHOLD-AMT * DRELG-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2110-COUNT-ITEM.
           IF DRELG-ITEM-CNT < 9999999
               ADD 1 TO DRELG-ITEM-CNT
           END-IF
           IF DRELG-ITEM-CNT NOT < 5
               MOVE 'Y' TO DRELG-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REINVEST GRID TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2120-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-REINVEST-GRID-RATE(1) TO DRELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF DRELG-WITHHOLD-AMT NOT < WS-REINVEST-GRID-LOW(WS-TX)
                   MOVE WS-REINVEST-GRID-RATE(WS-TX) TO DRELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2130-DESCRIBE-CYCLE.
           EVALUATE DRELG-CYCLE-CD
               WHEN 'DE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E606 REF NO REQUIRED
      * E337 CASH PAY AMT NEGATIVE
      * E492 CASH PAY AMT OVER 5000
      * E071 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2140-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-REF-NO = SPACES
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN DRELG-CASH-PAY-AMT < 0
                   MOVE 'E337' TO WS-REJECT-CODE
               WHEN DRELG-CASH-PAY-AMT > 5000.00
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN DRELG-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E071' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E104 REF NO REQUIRED
      * E413 WORK AMT 2 NEGATIVE
      * E765 WORK AMT 2 OVER 1000000
      * E689 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2150-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-REF-NO = SPACES
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN DRELG-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E689' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2160-DESCRIBE-PLAN-OPTION.
           EVALUATE DRELG-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E630 REF NO REQUIRED
      * E652 WORK AMT NEGATIVE
      * E221 WORK AMT OVER 5000
      * E654 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2170-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-REF-NO = SPACES
                   MOVE 'E630' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E221' TO WS-REJECT-CODE
               WHEN DRELG-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E734 CUST ID REQUIRED
      * E998 WORK AMT NEGATIVE
      * E447 WORK AMT OVER 5000
      * E461 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2180-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-CUST-ID = SPACES
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN DRELG-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E008 CUST ID REQUIRED
      * E585 FEE YTD NEGATIVE
      * E719 FEE YTD OVER 10000
      * E211 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2190-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-CUST-ID = SPACES
                   MOVE 'E008' TO WS-REJECT-CODE
               WHEN DRELG-FEE-YTD < 0
                   MOVE 'E585' TO WS-REJECT-CODE
               WHEN DRELG-FEE-YTD > 10000.00
                   MOVE 'E719' TO WS-REJECT-CODE
               WHEN DRELG-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E211' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2200-FLAG-REINVEST.
           IF DRELG-REINVEST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRELG-REINVEST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E544 REF NO REQUIRED
      * E644 FEE YTD NEGATIVE
      * E143 FEE YTD OVER 99999
      * E831 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2210-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-REF-NO = SPACES
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN DRELG-FEE-YTD < 0
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN DRELG-FEE-YTD > 99999.00
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN DRELG-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E831' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E525 CUST ID REQUIRED
      * E799 REINVEST AMT NEGATIVE
      * E994 REINVEST AMT OVER 99999
      * E140 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2220-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-CUST-ID = SPACES
                   MOVE 'E525' TO WS-REJECT-CODE
               WHEN DRELG-REINVEST-AMT < 0
                   MOVE 'E799' TO WS-REJECT-CODE
               WHEN DRELG-REINVEST-AMT > 99999.00
                   MOVE 'E994' TO WS-REJECT-CODE
               WHEN DRELG-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2230-DESCRIBE-CYCLE.
           EVALUATE DRELG-CYCLE-CD
               WHEN 'DE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-ITEM.
           IF DRELG-ITEM-CNT < 9999999
               ADD 1 TO DRELG-ITEM-CNT
           END-IF
           IF DRELG-ITEM-CNT NOT < 12
               MOVE 'Y' TO DRELG-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST AMT.
      *----------------------------------------------------------------*
       2250-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRELG-REINVEST-AMT ROUNDED =
                   DRELG-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRELG-REINVEST-AMT
           END-IF
           ADD DRELG-REINVEST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A SCHED TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2260-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-SCHED-RATE(1) TO DRELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRELG-FEE-YTD NOT < WS-SHARES-A-SCHED-LOW(WS-TX)
                   MOVE WS-SHARES-A-SCHED-RATE(WS-TX) TO DRELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN MATURITY DATE IS PAST
      * FB -> PC WHEN MATURITY DATE IS PAST
      * PC -> WY WHEN MATURITY DATE IS PAST
      * WY -> XD WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2270-SET-CYCLE.
           EVALUATE DRELG-CYCLE-CD
               WHEN 'DE'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REINVEST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH PAY YTD.
      *----------------------------------------------------------------*
       2280-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRELG-CASH-PAY-YTD ROUNDED =
                   DRELG-REINVEST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRELG-CASH-PAY-YTD
           END-IF
           ADD DRELG-CASH-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2290-DESCRIBE-SOURCE.
           EVALUATE DRELG-SOURCE-CD
               WHEN 'BT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2300-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRELG-MATURITY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASH PAY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WITHHOLD AMT.
      *----------------------------------------------------------------*
       2310-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRELG-WITHHOLD-AMT ROUNDED =
                   DRELG-CASH-PAY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRELG-WITHHOLD-AMT
           END-IF
           ADD DRELG-WITHHOLD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2320-ACCRUE-SHARES-AMT.
           IF DRELG-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRELG-SHARES-AMT-AMT * DRELG-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRELG-CASH-PAY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2330-DESCRIBE-PLAN-OPTION.
           EVALUATE DRELG-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Z'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E499 CUST ID REQUIRED
      * E302 CASH PAY AMT NEGATIVE
      * E138 CASH PAY AMT OVER 1000000
      * E928 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-CUST-ID = SPACES
                   MOVE 'E499' TO WS-REJECT-CODE
               WHEN DRELG-CASH-PAY-AMT < 0
                   MOVE 'E302' TO WS-REJECT-CODE
               WHEN DRELG-CASH-PAY-AMT > 1000000.00
                   MOVE 'E138' TO WS-REJECT-CODE
               WHEN DRELG-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E928' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2350-LIMIT-WITHHOLD.
           IF DRELG-WITHHOLD-AMT < 25.00
               MOVE 25.00 TO DRELG-WITHHOLD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRELG-WITHHOLD-AMT > 1000.00
               MOVE 1000.00 TO DRELG-WITHHOLD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2360-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2370-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRELG-REINVEST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRELG-REINVEST-AMT - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH PAY AMT.
      *----------------------------------------------------------------*
       2380-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRELG-CASH-PAY-AMT ROUNDED =
                   DRELG-WITHHOLD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRELG-CASH-PAY-AMT
           END-IF
           ADD DRELG-CASH-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH PAY YTD.
      *----------------------------------------------------------------*
       2390-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRELG-CASH-PAY-YTD ROUNDED =
                   DRELG-SHARES-AMT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRELG-CASH-PAY-YTD
           END-IF
           ADD DRELG-CASH-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E077 CUST ID REQUIRED
      * E221 SHARES AMT AMT NEGATIVE
      * E634 SHARES AMT AMT OVER 250000
      * E918 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2400-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-CUST-ID = SPACES
                   MOVE 'E077' TO WS-REJECT-CODE
               WHEN DRELG-SHARES-AMT-AMT < 0
                   MOVE 'E221' TO WS-REJECT-CODE
               WHEN DRELG-SHARES-AMT-AMT > 250000.00
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN DRELG-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2410-LIMIT-CASH-PAY.
           IF DRELG-CASH-PAY-YTD < 25.00
               MOVE 25.00 TO DRELG-CASH-PAY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRELG-CASH-PAY-YTD > 10000.00
               MOVE 10000.00 TO DRELG-CASH-PAY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2420-COUNT-EXCEPT.
           IF DRELG-EXCEPT-CNT < 9999999
               ADD 1 TO DRELG-EXCEPT-CNT
           END-IF
           IF DRELG-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO DRELG-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2430-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRELG-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRELG-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRELG-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRELG-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRELG-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRELG-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST AMT.
      *----------------------------------------------------------------*
       2440-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRELG-REINVEST-AMT ROUNDED =
                   DRELG-SHARES-AMT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRELG-REINVEST-AMT
           END-IF
           ADD DRELG-REINVEST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2450-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRELG-CASH-PAY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = DRELG-CASH-PAY-YTD - (WS-WORK-AMT *
               3)
           COMPUTE DRELG-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2460-DESCRIBE-CYCLE.
           EVALUATE DRELG-CYCLE-CD
               WHEN 'DE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASH PAY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHARES AMT AMT.
      *----------------------------------------------------------------*
       2470-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRELG-SHARES-AMT-AMT ROUNDED =
                   DRELG-CASH-PAY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRELG-SHARES-AMT-AMT
           END-IF
           ADD DRELG-SHARES-AMT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * RM -> VP WHEN LAST MAINT DATE IS PAST
      * VP -> YS WHEN LAST MAINT DATE IS PAST
      * YS -> RM WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2480-SET-CHANNEL.
           EVALUATE DRELG-CHANNEL-CD
               WHEN 'RM'
                   IF DRELG-LAST-MAINT-DATE > 0 AND
                       DRELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VP' TO DRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VP'
                   IF DRELG-LAST-MAINT-DATE > 0 AND
                       DRELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'YS' TO DRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YS'
                   IF DRELG-LAST-MAINT-DATE > 0 AND
                       DRELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'RM' TO DRELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2490-LIMIT-REINVEST.
           IF DRELG-REINVEST-AMT < 25.00
               MOVE 25.00 TO DRELG-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRELG-REINVEST-AMT > 5000.00
               MOVE 5000.00 TO DRELG-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2500-FLAG-WITHHOLD.
           IF DRELG-WITHHOLD-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRELG-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE DRELG-CASH-PAY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2520-COUNT-ITEM.
           IF DRELG-ITEM-CNT < 9999999
               ADD 1 TO DRELG-ITEM-CNT
           END-IF
           IF DRELG-ITEM-CNT NOT < 3
               MOVE 'Y' TO DRELG-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2530-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRELG-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRELG-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E022 CUST ID REQUIRED
      * E950 FEE YTD NEGATIVE
      * E255 FEE YTD OVER 50000
      * E342 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2540-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-CUST-ID = SPACES
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN DRELG-FEE-YTD < 0
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN DRELG-FEE-YTD > 50000.00
                   MOVE 'E255' TO WS-REJECT-CODE
               WHEN DRELG-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E342' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE CASH PAY YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2550-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRELG-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRELG-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRELG-STOP-FLAG = 'Y'
                   COMPUTE DRELG-FEE-YTD ROUNDED = DRELG-CASH-PAY-YTD *
                       DRELG-RATE / 200
               WHEN DRELG-PLAN-OPTION-CD = 'E'
                   MOVE 15.00 TO DRELG-FEE-YTD
               WHEN DRELG-PLAN-OPTION-CD = 'R'
                   MOVE 12.00 TO DRELG-FEE-YTD
               WHEN OTHER
                   COMPUTE DRELG-FEE-YTD ROUNDED = DRELG-CASH-PAY-YTD *
                       DRELG-RATE / 100
           END-EVALUATE
           IF DRELG-FEE-YTD > 0 AND DRELG-FEE-YTD < 5.00
               MOVE 5.00 TO DRELG-FEE-YTD
           END-IF
           IF DRELG-FEE-YTD > 35.00
               MOVE 35.00 TO DRELG-FEE-YTD
           END-IF
           ADD DRELG-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2560-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO DRELG-REINVEST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRELG-SUSPEND-FLAG = 'Y'
                   COMPUTE DRELG-REINVEST-AMT ROUNDED = WS-WORK-AMT-2 *
                       DRELG-DISCOUNT / 200
               WHEN DRELG-PLAN-OPTION-CD = 'E'
                   MOVE 7.00 TO DRELG-REINVEST-AMT
               WHEN DRELG-PLAN-OPTION-CD = 'R'
                   MOVE 20.00 TO DRELG-REINVEST-AMT
               WHEN OTHER
                   COMPUTE DRELG-REINVEST-AMT ROUNDED = WS-WORK-AMT-2 *
                       DRELG-DISCOUNT / 100
           END-EVALUATE
           IF DRELG-REINVEST-AMT > 0 AND DRELG-REINVEST-AMT < 10.00
               MOVE 10.00 TO DRELG-REINVEST-AMT
           END-IF
           IF DRELG-REINVEST-AMT > 50.00
               MOVE 50.00 TO DRELG-REINVEST-AMT
           END-IF
           ADD DRELG-REINVEST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CASH PAY YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2570-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRELG-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = DRELG-CASH-PAY-YTD *
                       DRELG-RATE / 200
               WHEN DRELG-PLAN-OPTION-CD = 'E'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN DRELG-PLAN-OPTION-CD = 'R'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = DRELG-CASH-PAY-YTD *
                       DRELG-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-SHARES-AMT.
           IF DRELG-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRELG-SHARES-AMT-AMT * DRELG-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH PAY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH PAY YTD.
      *----------------------------------------------------------------*
       2590-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRELG-CASH-PAY-YTD ROUNDED =
                   DRELG-CASH-PAY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRELG-CASH-PAY-YTD
           END-IF
           ADD DRELG-CASH-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A SCHED TIER FOR THE REINVEST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2600-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-SCHED-RATE(1) TO DRELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRELG-REINVEST-AMT NOT < WS-SHARES-A-SCHED-LOW(WS-TX)
                   MOVE WS-SHARES-A-SCHED-RATE(WS-TX) TO DRELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E236 REF NO REQUIRED
      * E622 FEE YTD NEGATIVE
      * E299 FEE YTD OVER 50000
      * E965 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-REF-NO = SPACES
                   MOVE 'E236' TO WS-REJECT-CODE
               WHEN DRELG-FEE-YTD < 0
                   MOVE 'E622' TO WS-REJECT-CODE
               WHEN DRELG-FEE-YTD > 50000.00
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN DRELG-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E965' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2620-LIMIT-WITHHOLD.
           IF DRELG-WITHHOLD-AMT < 25.00
               MOVE 25.00 TO DRELG-WITHHOLD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRELG-WITHHOLD-AMT > 10000.00
               MOVE 10000.00 TO DRELG-WITHHOLD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2630-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN MATURITY DATE IS PAST
      * FB -> PC WHEN MATURITY DATE IS PAST
      * PC -> WY WHEN MATURITY DATE IS PAST
      * WY -> XD WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2640-SET-CYCLE.
           EVALUATE DRELG-CYCLE-CD
               WHEN 'DE'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRELG-MATURITY-DATE > 0 AND DRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY YTD FEE ON THE SHARES AMT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2650-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRELG-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRELG-CASH-PAY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRELG-STOP-FLAG = 'Y'
                   COMPUTE DRELG-CASH-PAY-YTD ROUNDED =
                       DRELG-SHARES-AMT-AMT * DRELG-RATE / 200
               WHEN DRELG-CHANNEL-CD = 'RM'
                   MOVE 12.00 TO DRELG-CASH-PAY-YTD
               WHEN DRELG-CHANNEL-CD = 'VP'
                   MOVE 3.00 TO DRELG-CASH-PAY-YTD
               WHEN OTHER
                   COMPUTE DRELG-CASH-PAY-YTD ROUNDED =
                       DRELG-SHARES-AMT-AMT * DRELG-RATE / 100
           END-EVALUATE
           IF DRELG-CASH-PAY-YTD > 0 AND DRELG-CASH-PAY-YTD < 5.00
               MOVE 5.00 TO DRELG-CASH-PAY-YTD
           END-IF
           IF DRELG-CASH-PAY-YTD > 50.00
               MOVE 50.00 TO DRELG-CASH-PAY-YTD
           END-IF
           ADD DRELG-CASH-PAY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2660-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRELG-LAST-MAINT-DATE > 0 AND DRELG-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRELG-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRELG-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRELG-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD DRELG-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD DRELG-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRELG-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE REINVEST BAND TIER FOR THE CASH PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2670-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-REINVEST-BAND-RATE(1) TO DRELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF DRELG-CASH-PAY-YTD NOT < WS-REINVEST-BAND-LOW(WS-TX)
                   MOVE WS-REINVEST-BAND-RATE(WS-TX) TO DRELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REINVEST BAND TIER FOR THE CASH PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2680-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-REINVEST-BAND-RATE(1) TO DRELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF DRELG-CASH-PAY-AMT NOT < WS-REINVEST-BAND-LOW(WS-TX)
                   MOVE WS-REINVEST-BAND-RATE(WS-TX) TO DRELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2690-FLAG-WITHHOLD.
           IF DRELG-WITHHOLD-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRELG-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2700-LIMIT-SHARES-AMT.
           IF DRELG-SHARES-AMT-AMT < 50.00
               MOVE 50.00 TO DRELG-SHARES-AMT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRELG-SHARES-AMT-AMT > 500.00
               MOVE 500.00 TO DRELG-SHARES-AMT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2710-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRELG-EFFECTIVE-DATE > 0 AND DRELG-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRELG-SHARES-AMT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD DRELG-SHARES-AMT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRELG-SHARES-AMT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRELG-SHARES-AMT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRELG-SHARES-AMT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2720-DESCRIBE-CHANNEL.
           EVALUATE DRELG-CHANNEL-CD
               WHEN 'RM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2730-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRELG-SOURCE-CD
                   ADD DRELG-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRELG-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRELG-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E981 REF NO REQUIRED
      * E197 SHARES AMT AMT NEGATIVE
      * E425 SHARES AMT AMT OVER 99999
      * E862 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2740-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRELG-REF-NO = SPACES
                   MOVE 'E981' TO WS-REJECT-CODE
               WHEN DRELG-SHARES-AMT-AMT < 0
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN DRELG-SHARES-AMT-AMT > 99999.00
                   MOVE 'E425' TO WS-REJECT-CODE
               WHEN DRELG-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E862' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2750-LIMIT-WITHHOLD.
           IF DRELG-WITHHOLD-AMT < 0.00
               MOVE 0.00 TO DRELG-WITHHOLD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRELG-WITHHOLD-AMT > 2500.00
               MOVE 2500.00 TO DRELG-WITHHOLD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2760-FLAG-WITHHOLD.
           IF DRELG-WITHHOLD-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRELG-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2770-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * DRELG-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2780-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2790-COUNT-ITEM.
           IF DRELG-ITEM-CNT < 9999999
               ADD 1 TO DRELG-ITEM-CNT
           END-IF
           IF DRELG-ITEM-CNT NOT < 12
               MOVE 'Y' TO DRELG-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2800-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRELG-SHARES-AMT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRELG-SHARES-AMT-AMT - (WS-WORK-AMT *
               4)
           COMPUTE DRELG-CASH-PAY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN EFFECTIVE DATE IS PAST
      * FL -> NK WHEN EFFECTIVE DATE IS PAST
      * NK -> YC WHEN EFFECTIVE DATE IS PAST
      * YC -> BT WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2810-SET-SOURCE.
           EVALUATE DRELG-SOURCE-CD
               WHEN 'BT'
                   IF DRELG-EFFECTIVE-DATE > 0 AND DRELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRELG-EFFECTIVE-DATE > 0 AND DRELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRELG-EFFECTIVE-DATE > 0 AND DRELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRELG-EFFECTIVE-DATE > 0 AND DRELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2820-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRELG-OPEN-DATE > 0 AND DRELG-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRELG-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRELG-CASH-PAY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRELG-CASH-PAY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRELG-CASH-PAY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRELG-CASH-PAY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRELG-CASH-PAY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2830-LIMIT-REINVEST.
           IF DRELG-REINVEST-AMT < 25.00
               MOVE 25.00 TO DRELG-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRELG-REINVEST-AMT > 10000.00
               MOVE 10000.00 TO DRELG-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO DRELG-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO DRELG-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO DRELG-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'DRELG EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
