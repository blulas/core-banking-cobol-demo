      *================================================================*
      * PROGRAM  : DRLIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DIVIDEND REINVESTMENT RULE - FEES. CALLED BY THE    *
      *            DIVIDEND REINVESTMENT BATCH AND ONLINE PROGRAMS.    *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-01-16 BHS  ORIGINAL                                       *
      * 1986-12-27 CRP  NEW CASH-PAY REPORT COLUMNS INC5876            *
      * 1992-09-15 MKL  CASH-PAY EXCEPTIONS TO OPS QUEUE CHG3803       *
      * 1997-02-05 GAF  CASH-PAY RERUN PROTECTION REQ6271              *
      * 2001-08-05 DPS  FEE CUTOFF MOVED PRB3008                       *
      * 2004-08-01 GAF  PERFORMANCE - REINVEST READ AHEAD REQ8641      *
      * 2006-03-11 GAF  REINVEST CUTOFF MOVED INC6118                  *
      * 2006-05-04 TWH  NEW FEE REPORT COLUMNS REQ3156                 *
      * 2009-07-14 EWS  SHARES-AMT ROUNDING - HALF UP REQ4375          *
      * 2010-05-20 RJT  SHARES-AMT ROUNDING - HALF UP CHG2391          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRLIM.
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
                                           VALUE 15.000000.
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
      * WS-DIVIDEND-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DIVIDEND-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000266500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000817000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000879000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000783500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004100000000243500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009100000000036500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000270500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014100000000029500'.
       01  WS-DIVIDEND-BAND-TBL REDEFINES WS-DIVIDEND-BAND-VALUES.
           05  WS-DIVIDEND-BAND-ROW OCCURS 8 TIMES.
               10  WS-DIVIDEND-BAND-LOW    PIC 9(09)V99.
               10  WS-DIVIDEND-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY DRLIMP.

       PROCEDURE DIVISION USING DRLIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-FLAG-WITHHOLD
           PERFORM 2010-SPLIT-WITHHOLD
           PERFORM 2020-ACCRUE-REINVEST
           PERFORM 2030-SELECT-RATE
           PERFORM 2040-CHECK-LAST-PAY
           PERFORM 2050-SPLIT-CASH-PAY
           PERFORM 2060-SET-PLAN-OPTION
           PERFORM 2070-PRORATE-CASH-PAY
           PERFORM 2080-ACCUM-PLAN-OPTION
           PERFORM 2090-LIMIT-REINVEST
           PERFORM 2100-LIMIT-CASH-PAY
           PERFORM 2110-SPLIT-DIVIDEND
           PERFORM 2120-EDIT-CUST-ID
           PERFORM 2130-FLAG-DIVIDEND
           PERFORM 2140-ASSESS-SUSPEND
           PERFORM 2150-DESCRIBE-CHANNEL
           PERFORM 2160-SELECT-RATE
           PERFORM 2170-CHECK-LAST-PAY
           PERFORM 2180-PRORATE-CASH-PAY
           PERFORM 2190-SET-CHANNEL
           PERFORM 2200-ACCUM-SOURCE
           PERFORM 2210-SET-HOLDER-TYPE
           PERFORM 2220-SELECT-RATE
           PERFORM 2230-SPLIT-WITHHOLD
           PERFORM 2240-LIMIT-ITEM
           PERFORM 2250-ASSESS-PREFERRED
           PERFORM 2260-SELECT-DISCOUNT
           PERFORM 2270-LIMIT-DIVIDEND
           PERFORM 2280-SELECT-RATE
           PERFORM 2290-DESCRIBE-SOURCE
           PERFORM 2300-EDIT-DESC
           PERFORM 2310-CHECK-MATURITY
           PERFORM 2320-COUNT-PAYMENT
           PERFORM 2330-SPLIT-DIVIDEND
           PERFORM 2340-EDIT-CUST-ID
           PERFORM 2350-EDIT-CUST-ID
           PERFORM 2360-ACCRUE-RATE
           PERFORM 2370-DESCRIBE-PLAN-OPTION
           PERFORM 2380-PRORATE-WITHHOLD
           PERFORM 2390-SPLIT-REINVEST
           PERFORM 2400-COUNT-NOTICE
           PERFORM 2410-EDIT-DESC
           PERFORM 2420-LIMIT-ITEM
           PERFORM 2430-LIMIT-CASH-PAY
           PERFORM 2440-DESCRIBE-SOURCE
           PERFORM 2450-SELECT-RATE
           PERFORM 2460-PRORATE-CASH-PAY
           PERFORM 2470-FLAG-CASH-PAY
           PERFORM 2480-CHECK-LAST-MAINT
           PERFORM 2490-CHECK-LAST-MAINT
           PERFORM 2500-DESCRIBE-SOURCE
           PERFORM 2510-AGE-LAST-MAINT
           PERFORM 2520-DESCRIBE-SOURCE
           PERFORM 2530-FLAG-WITHHOLD
           PERFORM 2540-ASSESS-SUSPEND
           PERFORM 2550-COUNT-PAYMENT
           PERFORM 2560-ASSESS-PREFERRED
           PERFORM 2570-COUNT-PAYMENT
           PERFORM 2580-PRORATE-WITHHOLD
           PERFORM 2590-LIMIT-CASH-PAY
           PERFORM 2600-COUNT-PAYMENT
           PERFORM 2610-SELECT-DISCOUNT
           PERFORM 2620-FLAG-REINVEST
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO DRLIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * WITHHOLD YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2000-FLAG-WITHHOLD.
           IF DRLIM-WITHHOLD-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRLIM-WITHHOLD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2010-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = DRLIM-WITHHOLD-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRLIM-WITHHOLD-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-REINVEST.
           IF DRLIM-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRLIM-REINVEST-AMT * DRLIM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRLIM-CASH-PAY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND BAND TIER FOR THE REINVEST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-BAND-RATE(1) TO DRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRLIM-REINVEST-AMT NOT < WS-DIVIDEND-BAND-LOW(WS-TX)
                   MOVE WS-DIVIDEND-BAND-RATE(WS-TX) TO DRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2040-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRLIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO DRLIM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2050-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRLIM-CASH-PAY-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRLIM-CASH-PAY-AMT - (WS-WORK-AMT *
               3)
           COMPUTE DRLIM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN OPTION CD:
      * E -> R WHEN LAST MAINT DATE IS PAST
      * R -> T WHEN LAST MAINT DATE IS PAST
      * T -> Z WHEN LAST MAINT DATE IS PAST
      * Z -> E WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2060-SET-PLAN-OPTION.
           EVALUATE DRLIM-PLAN-OPTION-CD
               WHEN 'E'
                   IF DRLIM-LAST-MAINT-DATE > 0 AND
                       DRLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO DRLIM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF DRLIM-LAST-MAINT-DATE > 0 AND
                       DRLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'T' TO DRLIM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF DRLIM-LAST-MAINT-DATE > 0 AND
                       DRLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'Z' TO DRLIM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF DRLIM-LAST-MAINT-DATE > 0 AND
                       DRLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'E' TO DRLIM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH PAY YTD.
      *----------------------------------------------------------------*
       2070-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRLIM-CASH-PAY-YTD ROUNDED =
                   DRLIM-WITHHOLD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRLIM-CASH-PAY-YTD
           END-IF
           ADD DRLIM-CASH-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2080-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRLIM-PLAN-OPTION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRLIM-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2090-LIMIT-REINVEST.
           IF DRLIM-REINVEST-AMT < 100.00
               MOVE 100.00 TO DRLIM-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRLIM-REINVEST-AMT > 500.00
               MOVE 500.00 TO DRLIM-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2100-LIMIT-CASH-PAY.
           IF DRLIM-CASH-PAY-YTD < 10.00
               MOVE 10.00 TO DRLIM-CASH-PAY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRLIM-CASH-PAY-YTD > 2500.00
               MOVE 2500.00 TO DRLIM-CASH-PAY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DIVIDEND AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2110-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = DRLIM-DIVIDEND-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRLIM-DIVIDEND-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E080 CUST ID REQUIRED
      * E940 WITHHOLD AMT NEGATIVE
      * E138 WITHHOLD AMT OVER 99999
      * E190 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2120-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRLIM-CUST-ID = SPACES
                   MOVE 'E080' TO WS-REJECT-CODE
               WHEN DRLIM-WITHHOLD-AMT < 0
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN DRLIM-WITHHOLD-AMT > 99999.00
                   MOVE 'E138' TO WS-REJECT-CODE
               WHEN DRLIM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E190' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2130-FLAG-DIVIDEND.
           IF DRLIM-DIVIDEND-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRLIM-DIVIDEND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY FEE ON THE REINVEST AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2140-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRLIM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRLIM-CASH-PAY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRLIM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRLIM-CASH-PAY-AMT ROUNDED =
                       DRLIM-REINVEST-AMT * DRLIM-DISCOUNT / 200
               WHEN DRLIM-HOLDER-TYPE-CD = 'C'
                   MOVE 20.00 TO DRLIM-CASH-PAY-AMT
               WHEN DRLIM-HOLDER-TYPE-CD = 'H'
                   MOVE 20.00 TO DRLIM-CASH-PAY-AMT
               WHEN OTHER
                   COMPUTE DRLIM-CASH-PAY-AMT ROUNDED =
                       DRLIM-REINVEST-AMT * DRLIM-DISCOUNT / 100
           END-EVALUATE
           IF DRLIM-CASH-PAY-AMT > 0 AND DRLIM-CASH-PAY-AMT < 1.00
               MOVE 1.00 TO DRLIM-CASH-PAY-AMT
           END-IF
           IF DRLIM-CASH-PAY-AMT > 75.00
               MOVE 75.00 TO DRLIM-CASH-PAY-AMT
           END-IF
           ADD DRLIM-CASH-PAY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2150-DESCRIBE-CHANNEL.
           EVALUATE DRLIM-CHANNEL-CD
               WHEN 'RM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2160-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-BAND-RATE(1) TO DRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-DIVIDEND-BAND-LOW(WS-TX)
                   MOVE WS-DIVIDEND-BAND-RATE(WS-TX) TO DRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2170-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRLIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO DRLIM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH PAY YTD.
      *----------------------------------------------------------------*
       2180-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRLIM-CASH-PAY-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRLIM-CASH-PAY-YTD
           END-IF
           ADD DRLIM-CASH-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * RM -> VP WHEN MATURITY DATE IS PAST
      * VP -> YS WHEN MATURITY DATE IS PAST
      * YS -> RM WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2190-SET-CHANNEL.
           EVALUATE DRLIM-CHANNEL-CD
               WHEN 'RM'
                   IF DRLIM-MATURITY-DATE > 0 AND DRLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VP' TO DRLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VP'
                   IF DRLIM-MATURITY-DATE > 0 AND DRLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YS' TO DRLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YS'
                   IF DRLIM-MATURITY-DATE > 0 AND DRLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO DRLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2200-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRLIM-SOURCE-CD
                   ADD DRLIM-DIVIDEND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRLIM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRLIM-DIVIDEND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN LAST PAY DATE IS PAST
      * H -> K WHEN LAST PAY DATE IS PAST
      * K -> L WHEN LAST PAY DATE IS PAST
      * L -> M WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-HOLDER-TYPE.
           EVALUATE DRLIM-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRLIM-LAST-PAY-DATE > 0 AND DRLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO DRLIM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRLIM-LAST-PAY-DATE > 0 AND DRLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRLIM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRLIM-LAST-PAY-DATE > 0 AND DRLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRLIM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRLIM-LAST-PAY-DATE > 0 AND DRLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO DRLIM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND BAND TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-BAND-RATE(1) TO DRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRLIM-WITHHOLD-AMT NOT < WS-DIVIDEND-BAND-LOW(WS-TX)
                   MOVE WS-DIVIDEND-BAND-RATE(WS-TX) TO DRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2230-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = DRLIM-WITHHOLD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = DRLIM-WITHHOLD-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2240-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CASH PAY YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2250-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRLIM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRLIM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = DRLIM-CASH-PAY-YTD *
                       DRLIM-DISCOUNT / 200
               WHEN DRLIM-PLAN-OPTION-CD = 'E'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN DRLIM-PLAN-OPTION-CD = 'R'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = DRLIM-CASH-PAY-YTD *
                       DRLIM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND BAND TIER FOR THE CASH PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2260-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-BAND-RATE(1) TO DRLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRLIM-CASH-PAY-AMT NOT < WS-DIVIDEND-BAND-LOW(WS-TX)
                   MOVE WS-DIVIDEND-BAND-RATE(WS-TX) TO DRLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2270-LIMIT-DIVIDEND.
           IF DRLIM-DIVIDEND-AMT < 50.00
               MOVE 50.00 TO DRLIM-DIVIDEND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRLIM-DIVIDEND-AMT > 5000.00
               MOVE 5000.00 TO DRLIM-DIVIDEND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2280-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-BAND-RATE(1) TO DRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-DIVIDEND-BAND-LOW(WS-TX)
                   MOVE WS-DIVIDEND-BAND-RATE(WS-TX) TO DRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2290-DESCRIBE-SOURCE.
           EVALUATE DRLIM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E895 DESC REQUIRED
      * E858 CASH PAY YTD NEGATIVE
      * E462 CASH PAY YTD OVER 10000
      * E310 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2300-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRLIM-DESC = SPACES
                   MOVE 'E895' TO WS-REJECT-CODE
               WHEN DRLIM-CASH-PAY-YTD < 0
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN DRLIM-CASH-PAY-YTD > 10000.00
                   MOVE 'E462' TO WS-REJECT-CODE
               WHEN DRLIM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E310' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRLIM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRLIM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRLIM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2320-COUNT-PAYMENT.
           IF DRLIM-PAYMENT-CNT < 9999999
               ADD 1 TO DRLIM-PAYMENT-CNT
           END-IF
           IF DRLIM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO DRLIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2330-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRLIM-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E435 CUST ID REQUIRED
      * E959 REINVEST AMT NEGATIVE
      * E015 REINVEST AMT OVER 10000
      * E691 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRLIM-CUST-ID = SPACES
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN DRLIM-REINVEST-AMT < 0
                   MOVE 'E959' TO WS-REJECT-CODE
               WHEN DRLIM-REINVEST-AMT > 10000.00
                   MOVE 'E015' TO WS-REJECT-CODE
               WHEN DRLIM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E290 CUST ID REQUIRED
      * E440 REINVEST AMT NEGATIVE
      * E182 REINVEST AMT OVER 250000
      * E349 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2350-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRLIM-CUST-ID = SPACES
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN DRLIM-REINVEST-AMT < 0
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN DRLIM-REINVEST-AMT > 250000.00
                   MOVE 'E182' TO WS-REJECT-CODE
               WHEN DRLIM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2360-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * DRLIM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRLIM-DIVIDEND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-PLAN-OPTION.
           EVALUATE DRLIM-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASH PAY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WITHHOLD YTD.
      *----------------------------------------------------------------*
       2380-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRLIM-WITHHOLD-YTD ROUNDED =
                   DRLIM-CASH-PAY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRLIM-WITHHOLD-YTD
           END-IF
           ADD DRLIM-WITHHOLD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2390-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRLIM-REINVEST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRLIM-REINVEST-AMT - (WS-WORK-AMT *
               2)
           COMPUTE DRLIM-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2400-COUNT-NOTICE.
           IF DRLIM-NOTICE-CNT < 9999999
               ADD 1 TO DRLIM-NOTICE-CNT
           END-IF
           IF DRLIM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO DRLIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E681 DESC REQUIRED
      * E380 CASH PAY AMT NEGATIVE
      * E942 CASH PAY AMT OVER 1000000
      * E288 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRLIM-DESC = SPACES
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN DRLIM-CASH-PAY-AMT < 0
                   MOVE 'E380' TO WS-REJECT-CODE
               WHEN DRLIM-CASH-PAY-AMT > 1000000.00
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN DRLIM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2420-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2430-LIMIT-CASH-PAY.
           IF DRLIM-CASH-PAY-AMT < 25.00
               MOVE 25.00 TO DRLIM-CASH-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRLIM-CASH-PAY-AMT > 2500.00
               MOVE 2500.00 TO DRLIM-CASH-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2440-DESCRIBE-SOURCE.
           EVALUATE DRLIM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND BAND TIER FOR THE CASH PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2450-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-BAND-RATE(1) TO DRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRLIM-CASH-PAY-AMT NOT < WS-DIVIDEND-BAND-LOW(WS-TX)
                   MOVE WS-DIVIDEND-BAND-RATE(WS-TX) TO DRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH PAY AMT.
      *----------------------------------------------------------------*
       2460-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRLIM-CASH-PAY-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRLIM-CASH-PAY-AMT
           END-IF
           ADD DRLIM-CASH-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH PAY AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2470-FLAG-CASH-PAY.
           IF DRLIM-CASH-PAY-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRLIM-CASH-PAY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2480-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRLIM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRLIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRLIM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRLIM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRLIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO DRLIM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2500-DESCRIBE-SOURCE.
           EVALUATE DRLIM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2510-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRLIM-LAST-MAINT-DATE > 0 AND DRLIM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRLIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2520-DESCRIBE-SOURCE.
           EVALUATE DRLIM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WITHHOLD YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2530-FLAG-WITHHOLD.
           IF DRLIM-WITHHOLD-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRLIM-WITHHOLD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE REINVEST AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2540-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRLIM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRLIM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = DRLIM-REINVEST-AMT *
                       DRLIM-RATE / 200
               WHEN DRLIM-SOURCE-CD = 'BT'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN DRLIM-SOURCE-CD = 'FL'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = DRLIM-REINVEST-AMT *
                       DRLIM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2550-COUNT-PAYMENT.
           IF DRLIM-PAYMENT-CNT < 9999999
               ADD 1 TO DRLIM-PAYMENT-CNT
           END-IF
           IF DRLIM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO DRLIM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY YTD FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2560-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRLIM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO DRLIM-CASH-PAY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRLIM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRLIM-CASH-PAY-YTD ROUNDED = WS-WORK-AMT *
                       DRLIM-RATE / 200
               WHEN DRLIM-HOLDER-TYPE-CD = 'C'
                   MOVE 12.00 TO DRLIM-CASH-PAY-YTD
               WHEN DRLIM-HOLDER-TYPE-CD = 'H'
                   MOVE 3.00 TO DRLIM-CASH-PAY-YTD
               WHEN OTHER
                   COMPUTE DRLIM-CASH-PAY-YTD ROUNDED = WS-WORK-AMT *
                       DRLIM-RATE / 100
           END-EVALUATE
           IF DRLIM-CASH-PAY-YTD > 0 AND DRLIM-CASH-PAY-YTD < 5.00
               MOVE 5.00 TO DRLIM-CASH-PAY-YTD
           END-IF
           IF DRLIM-CASH-PAY-YTD > 250.00
               MOVE 250.00 TO DRLIM-CASH-PAY-YTD
           END-IF
           ADD DRLIM-CASH-PAY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2570-COUNT-PAYMENT.
           IF DRLIM-PAYMENT-CNT < 9999999
               ADD 1 TO DRLIM-PAYMENT-CNT
           END-IF
           IF DRLIM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO DRLIM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WITHHOLD AMT.
      *----------------------------------------------------------------*
       2580-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRLIM-WITHHOLD-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRLIM-WITHHOLD-AMT
           END-IF
           ADD DRLIM-WITHHOLD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH PAY YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2590-LIMIT-CASH-PAY.
           IF DRLIM-CASH-PAY-YTD < 0.00
               MOVE 0.00 TO DRLIM-CASH-PAY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRLIM-CASH-PAY-YTD > 50000.00
               MOVE 50000.00 TO DRLIM-CASH-PAY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2600-COUNT-PAYMENT.
           IF DRLIM-PAYMENT-CNT < 9999999
               ADD 1 TO DRLIM-PAYMENT-CNT
           END-IF
           IF DRLIM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO DRLIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2610-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-BAND-RATE(1) TO DRLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-DIVIDEND-BAND-LOW(WS-TX)
                   MOVE WS-DIVIDEND-BAND-RATE(WS-TX) TO DRLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2620-FLAG-REINVEST.
           IF DRLIM-REINVEST-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRLIM-REINVEST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO DRLIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO DRLIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO DRLIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'DRLIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
