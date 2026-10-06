      *================================================================*
      * PROGRAM  : ATVAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : ATM NETWORK SETTLEMENT RULE - ELIGIBILITY. CALLED BY*
      *            THE ATM NETWORK SETTLEMENT BATCH AND ONLINE         *
      *            PROGRAMS.                                           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-25 SKN  ORIGINAL                                       *
      * 1993-06-15 RJT  PERFORMANCE - SURCHG READ AHEAD CHG1229        *
      * 1996-08-26 CRP  PERFORMANCE - SETTLE READ AHEAD INC7090        *
      * 1997-05-17 GAF  PERFORMANCE - INTERCHG READ AHEAD REQ7650      *
      * 2006-10-01 JLM  NEW SETTLE REPORT COLUMNS CHG0295              *
      * 2006-12-23 RJT  SETTLE NOTICE TEXT PER LEGAL REQ0375           *
      * 2008-10-05 PQR  ADJUST TABLE EXPANDED INC6201                  *
      * 2011-05-17 PQR  ADJUST ROUNDING - HALF UP INC0401              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATVAL.
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
                                           VALUE 23.000000.
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
      * WS-SETTLE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SETTLE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000897000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000541500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000349000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000265000'.
       01  WS-SETTLE-BAND-TBL REDEFINES WS-SETTLE-BAND-VALUES.
           05  WS-SETTLE-BAND-ROW OCCURS 4 TIMES.
               10  WS-SETTLE-BAND-LOW      PIC 9(09)V99.
               10  WS-SETTLE-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SETTLE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SETTLE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000697000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000560500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000097000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000206000'.
       01  WS-SETTLE-SCHED-TBL REDEFINES WS-SETTLE-SCHED-VALUES.
           05  WS-SETTLE-SCHED-ROW OCCURS 4 TIMES.
               10  WS-SETTLE-SCHED-LOW     PIC 9(09)V99.
               10  WS-SETTLE-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISPENSE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISPENSE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000540500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000072000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000402000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020100000000196500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021100000000086500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031100000000560000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033600000000178500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038600000000606500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038850000000377500'.
       01  WS-DISPENSE-SCHED-TBL REDEFINES WS-DISPENSE-SCHED-VALUES.
           05  WS-DISPENSE-SCHED-ROW OCCURS 9 TIMES.
               10  WS-DISPENSE-SCHED-LOW   PIC 9(09)V99.
               10  WS-DISPENSE-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ATVALP.

       PROCEDURE DIVISION USING ATVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-FLAG-SETTLE
           PERFORM 2010-COUNT-PAYMENT
           PERFORM 2020-ACCRUE-ADJUST
           PERFORM 2030-SET-STATUS
           PERFORM 2040-SELECT-DISCOUNT
           PERFORM 2050-ACCUM-TERMINAL
           PERFORM 2060-PRORATE-ADJUST
           PERFORM 2070-LIMIT-INTERCHG
           PERFORM 2080-SET-REASON
           PERFORM 2090-AGE-MATURITY
           PERFORM 2100-ASSESS-SUSPEND
           PERFORM 2110-LIMIT-ITEM
           PERFORM 2120-COUNT-NOTICE
           PERFORM 2130-EDIT-CONTACT
           PERFORM 2140-DESCRIBE-TERMINAL
           PERFORM 2150-SELECT-RATE
           PERFORM 2160-ACCUM-BRANCH
           PERFORM 2170-FLAG-ADJUST
           PERFORM 2180-ACCRUE-DISCOUNT
           PERFORM 2190-EDIT-CUST-ID
           PERFORM 2200-COUNT-PAYMENT
           PERFORM 2210-SPLIT-ADJUST
           PERFORM 2220-ASSESS-EXEMPT
           PERFORM 2230-ASSESS-SUSPEND
           PERFORM 2240-EDIT-CUST-ID
           PERFORM 2250-ACCRUE-RATE
           PERFORM 2260-AGE-MATURITY
           PERFORM 2270-PRORATE-SETTLE
           PERFORM 2280-FLAG-ITEM
           PERFORM 2290-SET-STATUS
           PERFORM 2300-EDIT-CONTACT
           PERFORM 2310-ACCUM-STATUS
           PERFORM 2320-CHECK-MATURITY
           PERFORM 2330-SET-REASON
           PERFORM 2340-EDIT-CONTACT
           PERFORM 2350-DESCRIBE-STATUS
           PERFORM 2360-EDIT-CONTACT
           PERFORM 2370-LIMIT-INTERCHG
           PERFORM 2380-PRORATE-ADJUST
           PERFORM 2390-FLAG-SETTLE
           PERFORM 2400-ACCRUE-ADJUST
           PERFORM 2410-PRORATE-SURCHG
           PERFORM 2420-AGE-NOTICE
           PERFORM 2430-ACCRUE-SURCHG
           PERFORM 2440-SET-TERMINAL
           PERFORM 2450-PRORATE-ADJUST
           PERFORM 2460-ASSESS-EXEMPT
           PERFORM 2470-SET-BRANCH
           PERFORM 2480-LIMIT-INTERCHG
           PERFORM 2490-COUNT-PAYMENT
           PERFORM 2500-DESCRIBE-TERMINAL
           PERFORM 2510-ASSESS-REVIEW
           PERFORM 2520-ASSESS-REVIEW
           PERFORM 2530-ASSESS-SUSPEND
           PERFORM 2540-SPLIT-CASSETTE
           PERFORM 2550-DESCRIBE-REASON
           PERFORM 2560-ASSESS-REVIEW
           PERFORM 2570-SET-TERMINAL
           PERFORM 2580-PRORATE-INTERCHG
           PERFORM 2590-EDIT-CONTACT
           PERFORM 2600-AGE-MATURITY
           PERFORM 2610-FLAG-CASSETTE
           PERFORM 2620-DESCRIBE-STATUS
           PERFORM 2630-ASSESS-EXEMPT
           PERFORM 2640-ACCUM-STATUS
           PERFORM 2650-DESCRIBE-STATUS
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ATVAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * SETTLE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2000-FLAG-SETTLE.
           IF ATVAL-SETTLE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATVAL-SETTLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-PAYMENT.
           IF ATVAL-PAYMENT-CNT < 9999999
               ADD 1 TO ATVAL-PAYMENT-CNT
           END-IF
           IF ATVAL-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO ATVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-ADJUST.
           IF ATVAL-ADJUST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATVAL-ADJUST-AMT * ATVAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO ATVAL-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN NEXT REVIEW DATE IS PAST
      * DP -> FJ WHEN NEXT REVIEW DATE IS PAST
      * FJ -> NA WHEN NEXT REVIEW DATE IS PAST
      * NA -> DF WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2030-SET-STATUS.
           EVALUATE ATVAL-STATUS-CD
               WHEN 'DF'
                   IF ATVAL-NEXT-REVIEW-DATE > 0 AND
                       ATVAL-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DP' TO ATVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF ATVAL-NEXT-REVIEW-DATE > 0 AND
                       ATVAL-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FJ' TO ATVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF ATVAL-NEXT-REVIEW-DATE > 0 AND
                       ATVAL-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NA' TO ATVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NA'
                   IF ATVAL-NEXT-REVIEW-DATE > 0 AND
                       ATVAL-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DF' TO ATVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE SCHED TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-SCHED-RATE(1) TO ATVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ATVAL-INTERCHG-YTD NOT < WS-DISPENSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISPENSE-SCHED-RATE(WS-TX) TO ATVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2050-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATVAL-TERMINAL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATVAL-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADJUST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2060-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATVAL-ADJUST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * INTERCHG YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2070-LIMIT-INTERCHG.
           IF ATVAL-INTERCHG-YTD < 50.00
               MOVE 50.00 TO ATVAL-INTERCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATVAL-INTERCHG-YTD > 10000.00
               MOVE 10000.00 TO ATVAL-INTERCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN MATURITY DATE IS PAST
      * HX -> PR WHEN MATURITY DATE IS PAST
      * PR -> XD WHEN MATURITY DATE IS PAST
      * XD -> ZJ WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2080-SET-REASON.
           EVALUATE ATVAL-REASON-CD
               WHEN 'AK'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2090-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATVAL-SURCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATVAL-SURCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATVAL-SURCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATVAL-SURCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATVAL-SURCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SETTLE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2100-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATVAL-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATVAL-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATVAL-SETTLE-YTD *
                       ATVAL-RATE / 200
               WHEN ATVAL-STATUS-CD = 'DF'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN ATVAL-STATUS-CD = 'DP'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATVAL-SETTLE-YTD *
                       ATVAL-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2110-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-NOTICE.
           IF ATVAL-NOTICE-CNT < 9999999
               ADD 1 TO ATVAL-NOTICE-CNT
           END-IF
           IF ATVAL-NOTICE-CNT NOT < 12
               MOVE 'Y' TO ATVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E478 CONTACT REQUIRED
      * E072 WORK AMT 2 NEGATIVE
      * E311 WORK AMT 2 OVER 25000
      * E791 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATVAL-CONTACT = SPACES
                   MOVE 'E478' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E072' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E311' TO WS-REJECT-CODE
               WHEN ATVAL-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E791' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TERMINAL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2140-DESCRIBE-TERMINAL.
           EVALUATE ATVAL-TERMINAL-CD
               WHEN 'BJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-SCHED-RATE(1) TO ATVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-DISPENSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISPENSE-SCHED-RATE(WS-TX) TO ATVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2160-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATVAL-BRANCH-CD
                   ADD ATVAL-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATVAL-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATVAL-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ADJUST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2170-FLAG-ADJUST.
           IF ATVAL-ADJUST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATVAL-ADJUST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2180-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ATVAL-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E289 CUST ID REQUIRED
      * E870 WORK AMT NEGATIVE
      * E336 WORK AMT OVER 5000
      * E397 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2190-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATVAL-CUST-ID = SPACES
                   MOVE 'E289' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E336' TO WS-REJECT-CODE
               WHEN ATVAL-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E397' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-PAYMENT.
           IF ATVAL-PAYMENT-CNT < 9999999
               ADD 1 TO ATVAL-PAYMENT-CNT
           END-IF
           IF ATVAL-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO ATVAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2210-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATVAL-ADJUST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATVAL-ADJUST-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATVAL-SETTLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2220-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATVAL-INTERCHG-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE ATVAL-INTERCHG-YTD ROUNDED = WS-WORK-AMT-2 *
                       ATVAL-RATE / 200
               WHEN ATVAL-BRANCH-CD = 'DU'
                   MOVE 15.00 TO ATVAL-INTERCHG-YTD
               WHEN ATVAL-BRANCH-CD = 'HX'
                   MOVE 5.00 TO ATVAL-INTERCHG-YTD
               WHEN OTHER
                   COMPUTE ATVAL-INTERCHG-YTD ROUNDED = WS-WORK-AMT-2 *
                       ATVAL-RATE / 100
           END-EVALUATE
           IF ATVAL-INTERCHG-YTD > 0 AND ATVAL-INTERCHG-YTD < 10.00
               MOVE 10.00 TO ATVAL-INTERCHG-YTD
           END-IF
           IF ATVAL-INTERCHG-YTD > 100.00
               MOVE 100.00 TO ATVAL-INTERCHG-YTD
           END-IF
           ADD ATVAL-INTERCHG-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG YTD FEE ON THE CASSETTE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2230-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATVAL-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATVAL-SURCHG-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE ATVAL-SURCHG-YTD ROUNDED = ATVAL-CASSETTE-AMT
                       * ATVAL-RATE / 200
               WHEN ATVAL-REASON-CD = 'AK'
                   MOVE 7.00 TO ATVAL-SURCHG-YTD
               WHEN ATVAL-REASON-CD = 'HX'
                   MOVE 20.00 TO ATVAL-SURCHG-YTD
               WHEN OTHER
                   COMPUTE ATVAL-SURCHG-YTD ROUNDED = ATVAL-CASSETTE-AMT
                       * ATVAL-RATE / 100
           END-EVALUATE
           IF ATVAL-SURCHG-YTD > 0 AND ATVAL-SURCHG-YTD < 2.00
               MOVE 2.00 TO ATVAL-SURCHG-YTD
           END-IF
           IF ATVAL-SURCHG-YTD > 25.00
               MOVE 25.00 TO ATVAL-SURCHG-YTD
           END-IF
           ADD ATVAL-SURCHG-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E489 CUST ID REQUIRED
      * E748 SETTLE YTD NEGATIVE
      * E366 SETTLE YTD OVER 250000
      * E188 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2240-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATVAL-CUST-ID = SPACES
                   MOVE 'E489' TO WS-REJECT-CODE
               WHEN ATVAL-SETTLE-YTD < 0
                   MOVE 'E748' TO WS-REJECT-CODE
               WHEN ATVAL-SETTLE-YTD > 250000.00
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN ATVAL-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2250-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ATVAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATVAL-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATVAL-ADJUST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATVAL-ADJUST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATVAL-ADJUST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATVAL-ADJUST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATVAL-ADJUST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASSETTE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE YTD.
      *----------------------------------------------------------------*
       2270-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATVAL-SETTLE-YTD ROUNDED =
                   ATVAL-CASSETTE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATVAL-SETTLE-YTD
           END-IF
           ADD ATVAL-SETTLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2280-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN OPEN DATE IS PAST
      * DP -> FJ WHEN OPEN DATE IS PAST
      * FJ -> NA WHEN OPEN DATE IS PAST
      * NA -> DF WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2290-SET-STATUS.
           EVALUATE ATVAL-STATUS-CD
               WHEN 'DF'
                   IF ATVAL-OPEN-DATE > 0 AND ATVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO ATVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF ATVAL-OPEN-DATE > 0 AND ATVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO ATVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF ATVAL-OPEN-DATE > 0 AND ATVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NA' TO ATVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NA'
                   IF ATVAL-OPEN-DATE > 0 AND ATVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO ATVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E905 CONTACT REQUIRED
      * E305 WORK AMT NEGATIVE
      * E661 WORK AMT OVER 25000
      * E098 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2300-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATVAL-CONTACT = SPACES
                   MOVE 'E905' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E305' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN ATVAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E098' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADJUST AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2310-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATVAL-STATUS-CD
                   ADD ATVAL-ADJUST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATVAL-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATVAL-ADJUST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2320-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATVAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ATVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN NOTICE DATE IS PAST
      * HX -> PR WHEN NOTICE DATE IS PAST
      * PR -> XD WHEN NOTICE DATE IS PAST
      * XD -> ZJ WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2330-SET-REASON.
           EVALUATE ATVAL-REASON-CD
               WHEN 'AK'
                   IF ATVAL-NOTICE-DATE > 0 AND ATVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATVAL-NOTICE-DATE > 0 AND ATVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATVAL-NOTICE-DATE > 0 AND ATVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATVAL-NOTICE-DATE > 0 AND ATVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E444 CONTACT REQUIRED
      * E179 ADJUST AMT NEGATIVE
      * E621 ADJUST AMT OVER 99999
      * E892 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATVAL-CONTACT = SPACES
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN ATVAL-ADJUST-AMT < 0
                   MOVE 'E179' TO WS-REJECT-CODE
               WHEN ATVAL-ADJUST-AMT > 99999.00
                   MOVE 'E621' TO WS-REJECT-CODE
               WHEN ATVAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E892' TO WS-REJECT-CODE
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
           EVALUATE ATVAL-STATUS-CD
               WHEN 'DF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E427 CONTACT REQUIRED
      * E613 ADJUST AMT NEGATIVE
      * E742 ADJUST AMT OVER 1000000
      * E061 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2360-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATVAL-CONTACT = SPACES
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN ATVAL-ADJUST-AMT < 0
                   MOVE 'E613' TO WS-REJECT-CODE
               WHEN ATVAL-ADJUST-AMT > 1000000.00
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN ATVAL-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E061' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTERCHG YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2370-LIMIT-INTERCHG.
           IF ATVAL-INTERCHG-YTD < 0.00
               MOVE 0.00 TO ATVAL-INTERCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATVAL-INTERCHG-YTD > 500.00
               MOVE 500.00 TO ATVAL-INTERCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADJUST AMT.
      *----------------------------------------------------------------*
       2380-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATVAL-ADJUST-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATVAL-ADJUST-AMT
           END-IF
           ADD ATVAL-ADJUST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SETTLE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2390-FLAG-SETTLE.
           IF ATVAL-SETTLE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATVAL-SETTLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2400-ACCRUE-ADJUST.
           IF ATVAL-ADJUST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATVAL-ADJUST-AMT * ATVAL-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATVAL-INTERCHG-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURCHG YTD.
      *----------------------------------------------------------------*
       2410-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATVAL-SURCHG-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATVAL-SURCHG-YTD
           END-IF
           ADD ATVAL-SURCHG-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2420-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATVAL-NOTICE-DATE > 0 AND ATVAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2430-ACCRUE-SURCHG.
           IF ATVAL-SURCHG-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATVAL-SURCHG-YTD * ATVAL-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATVAL-ADJUST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN MATURITY DATE IS PAST
      * JJ -> JZ WHEN MATURITY DATE IS PAST
      * JZ -> KK WHEN MATURITY DATE IS PAST
      * KK -> BJ WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2440-SET-TERMINAL.
           EVALUATE ATVAL-TERMINAL-CD
               WHEN 'BJ'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JJ' TO ATVAL-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JJ'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO ATVAL-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KK' TO ATVAL-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KK'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO ATVAL-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADJUST AMT.
      *----------------------------------------------------------------*
       2450-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATVAL-ADJUST-AMT ROUNDED =
                   ATVAL-SURCHG-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATVAL-ADJUST-AMT
           END-IF
           ADD ATVAL-ADJUST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2460-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATVAL-CASSETTE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATVAL-REVIEW-FLAG = 'Y'
                   COMPUTE ATVAL-CASSETTE-AMT ROUNDED = WS-WORK-AMT *
                       ATVAL-RATE / 200
               WHEN ATVAL-REASON-CD = 'AK'
                   MOVE 12.00 TO ATVAL-CASSETTE-AMT
               WHEN ATVAL-REASON-CD = 'HX'
                   MOVE 5.00 TO ATVAL-CASSETTE-AMT
               WHEN OTHER
                   COMPUTE ATVAL-CASSETTE-AMT ROUNDED = WS-WORK-AMT *
                       ATVAL-RATE / 100
           END-EVALUATE
           IF ATVAL-CASSETTE-AMT > 0 AND ATVAL-CASSETTE-AMT < 2.00
               MOVE 2.00 TO ATVAL-CASSETTE-AMT
           END-IF
           IF ATVAL-CASSETTE-AMT > 75.00
               MOVE 75.00 TO ATVAL-CASSETTE-AMT
           END-IF
           ADD ATVAL-CASSETTE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN MATURITY DATE IS PAST
      * HX -> YC WHEN MATURITY DATE IS PAST
      * YC -> ZB WHEN MATURITY DATE IS PAST
      * ZB -> DU WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-BRANCH.
           EVALUATE ATVAL-BRANCH-CD
               WHEN 'DU'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTERCHG YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2480-LIMIT-INTERCHG.
           IF ATVAL-INTERCHG-YTD < 50.00
               MOVE 50.00 TO ATVAL-INTERCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATVAL-INTERCHG-YTD > 1000.00
               MOVE 1000.00 TO ATVAL-INTERCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2490-COUNT-PAYMENT.
           IF ATVAL-PAYMENT-CNT < 9999999
               ADD 1 TO ATVAL-PAYMENT-CNT
           END-IF
           IF ATVAL-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO ATVAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TERMINAL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2500-DESCRIBE-TERMINAL.
           EVALUATE ATVAL-TERMINAL-CD
               WHEN 'BJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE FEE ON THE INTERCHG YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2510-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ATVAL-CASSETTE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE ATVAL-CASSETTE-AMT ROUNDED =
                       ATVAL-INTERCHG-YTD * ATVAL-RATE / 200
               WHEN ATVAL-TERMINAL-CD = 'BJ'
                   MOVE 5.00 TO ATVAL-CASSETTE-AMT
               WHEN ATVAL-TERMINAL-CD = 'JJ'
                   MOVE 7.00 TO ATVAL-CASSETTE-AMT
               WHEN OTHER
                   COMPUTE ATVAL-CASSETTE-AMT ROUNDED =
                       ATVAL-INTERCHG-YTD * ATVAL-RATE / 100
           END-EVALUATE
           IF ATVAL-CASSETTE-AMT > 0 AND ATVAL-CASSETTE-AMT < 2.00
               MOVE 2.00 TO ATVAL-CASSETTE-AMT
           END-IF
           IF ATVAL-CASSETTE-AMT > 100.00
               MOVE 100.00 TO ATVAL-CASSETTE-AMT
           END-IF
           ADD ATVAL-CASSETTE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE CASSETTE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2520-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ATVAL-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE ATVAL-SURCHG-AMT ROUNDED = ATVAL-CASSETTE-AMT
                       * ATVAL-DISCOUNT / 200
               WHEN ATVAL-STATUS-CD = 'DF'
                   MOVE 10.00 TO ATVAL-SURCHG-AMT
               WHEN ATVAL-STATUS-CD = 'DP'
                   MOVE 5.00 TO ATVAL-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATVAL-SURCHG-AMT ROUNDED = ATVAL-CASSETTE-AMT
                       * ATVAL-DISCOUNT / 100
           END-EVALUATE
           IF ATVAL-SURCHG-AMT > 0 AND ATVAL-SURCHG-AMT < 10.00
               MOVE 10.00 TO ATVAL-SURCHG-AMT
           END-IF
           IF ATVAL-SURCHG-AMT > 25.00
               MOVE 25.00 TO ATVAL-SURCHG-AMT
           END-IF
           ADD ATVAL-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SETTLE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2530-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATVAL-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATVAL-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATVAL-SETTLE-YTD *
                       ATVAL-RATE / 200
               WHEN ATVAL-TERMINAL-CD = 'BJ'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN ATVAL-TERMINAL-CD = 'JJ'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATVAL-SETTLE-YTD *
                       ATVAL-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CASSETTE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2540-SPLIT-CASSETTE.
           COMPUTE WS-WORK-AMT = ATVAL-CASSETTE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ATVAL-CASSETTE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2550-DESCRIBE-REASON.
           EVALUATE ATVAL-REASON-CD
               WHEN 'AK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE YTD FEE ON THE ADJUST AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2560-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ATVAL-SETTLE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE ATVAL-SETTLE-YTD ROUNDED = ATVAL-ADJUST-AMT *
                       ATVAL-RATE / 200
               WHEN ATVAL-REASON-CD = 'AK'
                   MOVE 5.00 TO ATVAL-SETTLE-YTD
               WHEN ATVAL-REASON-CD = 'HX'
                   MOVE 20.00 TO ATVAL-SETTLE-YTD
               WHEN OTHER
                   COMPUTE ATVAL-SETTLE-YTD ROUNDED = ATVAL-ADJUST-AMT *
                       ATVAL-RATE / 100
           END-EVALUATE
           IF ATVAL-SETTLE-YTD > 0 AND ATVAL-SETTLE-YTD < 1.00
               MOVE 1.00 TO ATVAL-SETTLE-YTD
           END-IF
           IF ATVAL-SETTLE-YTD > 100.00
               MOVE 100.00 TO ATVAL-SETTLE-YTD
           END-IF
           ADD ATVAL-SETTLE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN OPEN DATE IS PAST
      * JJ -> JZ WHEN OPEN DATE IS PAST
      * JZ -> KK WHEN OPEN DATE IS PAST
      * KK -> BJ WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2570-SET-TERMINAL.
           EVALUATE ATVAL-TERMINAL-CD
               WHEN 'BJ'
                   IF ATVAL-OPEN-DATE > 0 AND ATVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JJ' TO ATVAL-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JJ'
                   IF ATVAL-OPEN-DATE > 0 AND ATVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO ATVAL-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF ATVAL-OPEN-DATE > 0 AND ATVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KK' TO ATVAL-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KK'
                   IF ATVAL-OPEN-DATE > 0 AND ATVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO ATVAL-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTERCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2580-PRORATE-INTERCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATVAL-INTERCHG-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E431 CONTACT REQUIRED
      * E499 INTERCHG YTD NEGATIVE
      * E125 INTERCHG YTD OVER 99999
      * E425 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2590-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATVAL-CONTACT = SPACES
                   MOVE 'E431' TO WS-REJECT-CODE
               WHEN ATVAL-INTERCHG-YTD < 0
                   MOVE 'E499' TO WS-REJECT-CODE
               WHEN ATVAL-INTERCHG-YTD > 99999.00
                   MOVE 'E125' TO WS-REJECT-CODE
               WHEN ATVAL-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E425' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2600-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATVAL-MATURITY-DATE > 0 AND ATVAL-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATVAL-SURCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATVAL-SURCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATVAL-SURCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATVAL-SURCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATVAL-SURCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2610-FLAG-CASSETTE.
           IF ATVAL-CASSETTE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATVAL-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2620-DESCRIBE-STATUS.
           EVALUATE ATVAL-STATUS-CD
               WHEN 'DF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE INTERCHG YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2630-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATVAL-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ATVAL-INTERCHG-YTD *
                       ATVAL-RATE / 200
               WHEN ATVAL-STATUS-CD = 'DF'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN ATVAL-STATUS-CD = 'DP'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ATVAL-INTERCHG-YTD *
                       ATVAL-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATVAL-STATUS-CD
                   ADD ATVAL-SURCHG-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATVAL-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATVAL-SURCHG-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2650-DESCRIBE-STATUS.
           EVALUATE ATVAL-STATUS-CD
               WHEN 'DF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ATVAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ATVAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ATVAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ATVAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
