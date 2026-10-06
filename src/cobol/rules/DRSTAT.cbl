      *================================================================*
      * PROGRAM  : DRSTAT                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DIVIDEND REINVESTMENT RULE - VALIDATION. CALLED BY  *
      *            THE DIVIDEND REINVESTMENT BATCH AND ONLINE PROGRAMS.*
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-03-28 GAF  ORIGINAL                                       *
      * 1987-02-06 EWS  CASH-PAY EXCEPTIONS TO OPS QUEUE CHG2623       *
      * 1989-09-21 GAF  REINVEST EXCEPTIONS TO OPS QUEUE PRB8854       *
      * 2003-07-24 CRP  REINVEST EXCEPTIONS TO OPS QUEUE PRB3619       *
      * 2007-03-05 BHS  AUDIT FINDING - SHARES-AMT CONTROL TOTALS REQ13*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRSTAT.
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
                                           VALUE 22.000000.
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
      * WS-REINVEST-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REINVEST-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000837500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000327000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000195000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000196000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007750000000288000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000472000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000876500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000363500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012900000000876000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013900000000734500'.
       01  WS-REINVEST-SCHED-TBL REDEFINES WS-REINVEST-SCHED-VALUES.
           05  WS-REINVEST-SCHED-ROW OCCURS 10 TIMES.
               10  WS-REINVEST-SCHED-LOW   PIC 9(09)V99.
               10  WS-REINVEST-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHARES-A-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHARES-A-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000825500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000300000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000530000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000450000000341500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001450000000120000'.
       01  WS-SHARES-A-SCHED-TBL REDEFINES WS-SHARES-A-SCHED-VALUES.
           05  WS-SHARES-A-SCHED-ROW OCCURS 5 TIMES.
               10  WS-SHARES-A-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHARES-A-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000072500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000384500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000841500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000165000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000727000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000525500'.
       01  WS-FEE-GRID-TBL REDEFINES WS-FEE-GRID-VALUES.
           05  WS-FEE-GRID-ROW OCCURS 6 TIMES.
               10  WS-FEE-GRID-LOW         PIC 9(09)V99.
               10  WS-FEE-GRID-RATE        PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY DRSTATP.

       PROCEDURE DIVISION USING DRSTAT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-RATE
           PERFORM 2010-DESCRIBE-PLAN-OPTION
           PERFORM 2020-ACCUM-CHANNEL
           PERFORM 2030-AGE-NEXT-REVIEW
           PERFORM 2040-SPLIT-FEE
           PERFORM 2050-SET-HOLDER-TYPE
           PERFORM 2060-CHECK-MATURITY
           PERFORM 2070-ASSESS-STOP
           PERFORM 2080-PRORATE-DIVIDEND
           PERFORM 2090-LIMIT-ITEM
           PERFORM 2100-LIMIT-SHARES-AMT
           PERFORM 2110-LIMIT-CASH-PAY
           PERFORM 2120-CHECK-NEXT-DUE
           PERFORM 2130-SELECT-DISCOUNT
           PERFORM 2140-ACCUM-HOLDER-TYPE
           PERFORM 2150-SET-REASON
           PERFORM 2160-SPLIT-DIVIDEND
           PERFORM 2170-EDIT-REF-NO
           PERFORM 2180-SPLIT-SHARES-AMT
           PERFORM 2190-COUNT-EXCEPT
           PERFORM 2200-SPLIT-SHARES-AMT
           PERFORM 2210-ACCRUE-SHARES-AMT
           PERFORM 2220-SELECT-DISCOUNT
           PERFORM 2230-DESCRIBE-HOLDER-TYPE
           PERFORM 2240-AGE-LAST-PAY
           PERFORM 2250-SET-REASON
           PERFORM 2260-DESCRIBE-CHANNEL
           PERFORM 2270-EDIT-REF-NO
           PERFORM 2280-CHECK-NEXT-DUE
           PERFORM 2290-SPLIT-DIVIDEND
           PERFORM 2300-COUNT-NOTICE
           PERFORM 2310-CHECK-NEXT-DUE
           PERFORM 2320-SET-REASON
           PERFORM 2330-DESCRIBE-REASON
           PERFORM 2340-ASSESS-AUTO-RENEW
           PERFORM 2350-SPLIT-CASH-PAY
           PERFORM 2360-LIMIT-CASH-PAY
           PERFORM 2370-SELECT-DISCOUNT
           PERFORM 2380-DESCRIBE-REASON
           PERFORM 2390-DESCRIBE-PLAN-OPTION
           PERFORM 2400-PRORATE-SHARES-AMT
           PERFORM 2410-ACCUM-REASON
           PERFORM 2420-EDIT-CUST-ID
           PERFORM 2430-CHECK-NEXT-DUE
           PERFORM 2440-ACCRUE-CASH-PAY
           PERFORM 2450-ASSESS-STOP
           PERFORM 2460-ACCRUE-DISCOUNT
           PERFORM 2470-SPLIT-CASH-PAY
           PERFORM 2480-EDIT-CUST-ID
           PERFORM 2490-ACCRUE-SHARES-AMT
           PERFORM 2500-ASSESS-STOP
           PERFORM 2510-ACCUM-PLAN-OPTION
           PERFORM 2520-COUNT-EXCEPT
           PERFORM 2530-SELECT-DISCOUNT
           PERFORM 2540-SPLIT-DIVIDEND
           PERFORM 2550-ACCRUE-SHARES-AMT
           PERFORM 2560-ACCRUE-WITHHOLD
           PERFORM 2570-SET-HOLDER-TYPE
           PERFORM 2580-ACCRUE-DIVIDEND
           PERFORM 2590-CHECK-NEXT-REVIEW
           PERFORM 2600-SET-HOLDER-TYPE
           PERFORM 2610-ACCRUE-DIVIDEND
           PERFORM 2620-COUNT-EXCEPT
           PERFORM 2630-CHECK-NEXT-REVIEW
           PERFORM 2640-COUNT-EXCEPT
           PERFORM 2650-EDIT-REF-NO
           PERFORM 2660-EDIT-CUST-ID
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO DRSTAT-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * DRSTAT-RATE / 100 / 360
               ADD WS-WORK-AMT TO DRSTAT-DIVIDEND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2010-DESCRIBE-PLAN-OPTION.
           EVALUATE DRSTAT-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2020-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRSTAT-CHANNEL-CD
                   ADD DRSTAT-DIVIDEND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRSTAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRSTAT-DIVIDEND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRSTAT-NEXT-REVIEW-DATE > 0 AND DRSTAT-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRSTAT-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2040-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE DRSTAT-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN MATURITY DATE IS PAST
      * H -> K WHEN MATURITY DATE IS PAST
      * K -> L WHEN MATURITY DATE IS PAST
      * L -> M WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2050-SET-HOLDER-TYPE.
           EVALUATE DRSTAT-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2060-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRSTAT-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRSTAT-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO DRSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE SHARES AMT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2070-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRSTAT-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRSTAT-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRSTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRSTAT-FEE-YTD ROUNDED =
                       DRSTAT-SHARES-AMT-AMT * DRSTAT-RATE / 200
               WHEN DRSTAT-HOLDER-TYPE-CD = 'C'
                   MOVE 12.00 TO DRSTAT-FEE-YTD
               WHEN DRSTAT-HOLDER-TYPE-CD = 'H'
                   MOVE 15.00 TO DRSTAT-FEE-YTD
               WHEN OTHER
                   COMPUTE DRSTAT-FEE-YTD ROUNDED =
                       DRSTAT-SHARES-AMT-AMT * DRSTAT-RATE / 100
           END-EVALUATE
           IF DRSTAT-FEE-YTD > 0 AND DRSTAT-FEE-YTD < 10.00
               MOVE 10.00 TO DRSTAT-FEE-YTD
           END-IF
           IF DRSTAT-FEE-YTD > 25.00
               MOVE 25.00 TO DRSTAT-FEE-YTD
           END-IF
           ADD DRSTAT-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       2080-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRSTAT-DIVIDEND-AMT ROUNDED =
                   DRSTAT-SHARES-AMT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRSTAT-DIVIDEND-AMT
           END-IF
           ADD DRSTAT-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2090-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2100-LIMIT-SHARES-AMT.
           IF DRSTAT-SHARES-AMT-AMT < 10.00
               MOVE 10.00 TO DRSTAT-SHARES-AMT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRSTAT-SHARES-AMT-AMT > 5000.00
               MOVE 5000.00 TO DRSTAT-SHARES-AMT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2110-LIMIT-CASH-PAY.
           IF DRSTAT-CASH-PAY-AMT < 0.00
               MOVE 0.00 TO DRSTAT-CASH-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRSTAT-CASH-PAY-AMT > 50000.00
               MOVE 50000.00 TO DRSTAT-CASH-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2120-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRSTAT-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2130-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO DRSTAT-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF DRSTAT-DIVIDEND-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO DRSTAT-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRSTAT-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRSTAT-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND AMT BY HOLDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2140-ACCUM-HOLDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRSTAT-HOLDER-TYPE-CD
                   ADD DRSTAT-DIVIDEND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRSTAT-HOLDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRSTAT-DIVIDEND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> G WHEN MATURITY DATE IS PAST
      * G -> J WHEN MATURITY DATE IS PAST
      * J -> K WHEN MATURITY DATE IS PAST
      * K -> L WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2150-SET-REASON.
           EVALUATE DRSTAT-REASON-CD
               WHEN 'B'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'G' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'J' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE DRSTAT-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E412 REF NO REQUIRED
      * E521 CASH PAY AMT NEGATIVE
      * E180 CASH PAY AMT OVER 50000
      * E919 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2170-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRSTAT-REF-NO = SPACES
                   MOVE 'E412' TO WS-REJECT-CODE
               WHEN DRSTAT-CASH-PAY-AMT < 0
                   MOVE 'E521' TO WS-REJECT-CODE
               WHEN DRSTAT-CASH-PAY-AMT > 50000.00
                   MOVE 'E180' TO WS-REJECT-CODE
               WHEN DRSTAT-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2180-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE DRSTAT-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2190-COUNT-EXCEPT.
           IF DRSTAT-EXCEPT-CNT < 9999999
               ADD 1 TO DRSTAT-EXCEPT-CNT
           END-IF
           IF DRSTAT-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO DRSTAT-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2200-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRSTAT-SHARES-AMT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRSTAT-SHARES-AMT-AMT - (WS-WORK-AMT
               * 12)
           COMPUTE DRSTAT-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2210-ACCRUE-SHARES-AMT.
           IF DRSTAT-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRSTAT-SHARES-AMT-AMT * DRSTAT-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRSTAT-CASH-PAY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REINVEST SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-REINVEST-SCHED-RATE(1) TO DRSTAT-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-REINVEST-SCHED-LOW(WS-TX)
                   MOVE WS-REINVEST-SCHED-RATE(WS-TX) TO DRSTAT-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRSTAT-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRSTAT-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2230-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRSTAT-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2240-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRSTAT-LAST-PAY-DATE > 0 AND DRSTAT-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRSTAT-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRSTAT-DIVIDEND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD DRSTAT-DIVIDEND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRSTAT-DIVIDEND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRSTAT-DIVIDEND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRSTAT-DIVIDEND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> G WHEN LAST PAY DATE IS PAST
      * G -> J WHEN LAST PAY DATE IS PAST
      * J -> K WHEN LAST PAY DATE IS PAST
      * K -> L WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2250-SET-REASON.
           EVALUATE DRSTAT-REASON-CD
               WHEN 'B'
                   IF DRSTAT-LAST-PAY-DATE > 0 AND DRSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'G' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF DRSTAT-LAST-PAY-DATE > 0 AND DRSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'J' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF DRSTAT-LAST-PAY-DATE > 0 AND DRSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRSTAT-LAST-PAY-DATE > 0 AND DRSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2260-DESCRIBE-CHANNEL.
           EVALUATE DRSTAT-CHANNEL-CD
               WHEN 'RM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E571 REF NO REQUIRED
      * E430 SHARES AMT AMT NEGATIVE
      * E884 SHARES AMT AMT OVER 1000000
      * E496 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2270-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRSTAT-REF-NO = SPACES
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN DRSTAT-SHARES-AMT-AMT < 0
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN DRSTAT-SHARES-AMT-AMT > 1000000.00
                   MOVE 'E884' TO WS-REJECT-CODE
               WHEN DRSTAT-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2280-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRSTAT-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRSTAT-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DIVIDEND AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2290-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = DRSTAT-DIVIDEND-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRSTAT-DIVIDEND-AMT - (WS-WORK-AMT *
               2)
           COMPUTE DRSTAT-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2300-COUNT-NOTICE.
           IF DRSTAT-NOTICE-CNT < 9999999
               ADD 1 TO DRSTAT-NOTICE-CNT
           END-IF
           IF DRSTAT-NOTICE-CNT NOT < 3
               MOVE 'Y' TO DRSTAT-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRSTAT-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO DRSTAT-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> G WHEN NEXT REVIEW DATE IS PAST
      * G -> J WHEN NEXT REVIEW DATE IS PAST
      * J -> K WHEN NEXT REVIEW DATE IS PAST
      * K -> L WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2320-SET-REASON.
           EVALUATE DRSTAT-REASON-CD
               WHEN 'B'
                   IF DRSTAT-NEXT-REVIEW-DATE > 0 AND
                       DRSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'G' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF DRSTAT-NEXT-REVIEW-DATE > 0 AND
                       DRSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'J' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF DRSTAT-NEXT-REVIEW-DATE > 0 AND
                       DRSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'K' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRSTAT-NEXT-REVIEW-DATE > 0 AND
                       DRSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO DRSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2330-DESCRIBE-REASON.
           EVALUATE DRSTAT-REASON-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'G'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'J'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2340-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN DRSTAT-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO DRSTAT-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRSTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRSTAT-DIVIDEND-AMT ROUNDED = DRSTAT-FEE-YTD
                       * DRSTAT-RATE / 200
               WHEN DRSTAT-PLAN-OPTION-CD = 'E'
                   MOVE 3.00 TO DRSTAT-DIVIDEND-AMT
               WHEN DRSTAT-PLAN-OPTION-CD = 'R'
                   MOVE 5.00 TO DRSTAT-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRSTAT-DIVIDEND-AMT ROUNDED = DRSTAT-FEE-YTD
                       * DRSTAT-RATE / 100
           END-EVALUATE
           IF DRSTAT-DIVIDEND-AMT > 0 AND DRSTAT-DIVIDEND-AMT < 2.00
               MOVE 2.00 TO DRSTAT-DIVIDEND-AMT
           END-IF
           IF DRSTAT-DIVIDEND-AMT > 25.00
               MOVE 25.00 TO DRSTAT-DIVIDEND-AMT
           END-IF
           ADD DRSTAT-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2350-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRSTAT-CASH-PAY-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRSTAT-CASH-PAY-AMT - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2360-LIMIT-CASH-PAY.
           IF DRSTAT-CASH-PAY-AMT < 100.00
               MOVE 100.00 TO DRSTAT-CASH-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRSTAT-CASH-PAY-AMT > 10000.00
               MOVE 10000.00 TO DRSTAT-CASH-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A SCHED TIER FOR THE CASH PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2370-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-SCHED-RATE(1) TO DRSTAT-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF DRSTAT-CASH-PAY-AMT NOT < WS-SHARES-A-SCHED-LOW(WS-TX)
                   MOVE WS-SHARES-A-SCHED-RATE(WS-TX) TO DRSTAT-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRSTAT-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRSTAT-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-REASON.
           EVALUATE DRSTAT-REASON-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2390-DESCRIBE-PLAN-OPTION.
           EVALUATE DRSTAT-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHARES AMT AMT.
      *----------------------------------------------------------------*
       2400-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRSTAT-SHARES-AMT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRSTAT-SHARES-AMT-AMT
           END-IF
           ADD DRSTAT-SHARES-AMT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2410-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRSTAT-REASON-CD
                   ADD DRSTAT-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRSTAT-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRSTAT-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E153 CUST ID REQUIRED
      * E780 WITHHOLD YTD NEGATIVE
      * E542 WITHHOLD YTD OVER 50000
      * E765 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2420-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRSTAT-CUST-ID = SPACES
                   MOVE 'E153' TO WS-REJECT-CODE
               WHEN DRSTAT-WITHHOLD-YTD < 0
                   MOVE 'E780' TO WS-REJECT-CODE
               WHEN DRSTAT-WITHHOLD-YTD > 50000.00
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN DRSTAT-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2430-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRSTAT-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRSTAT-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2440-ACCRUE-CASH-PAY.
           IF DRSTAT-CASH-PAY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRSTAT-CASH-PAY-AMT * DRSTAT-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRSTAT-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE REINVEST AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2450-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRSTAT-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRSTAT-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRSTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRSTAT-FEE-YTD ROUNDED = DRSTAT-REINVEST-AMT
                       * DRSTAT-RATE / 200
               WHEN DRSTAT-REASON-CD = 'B'
                   MOVE 10.00 TO DRSTAT-FEE-YTD
               WHEN DRSTAT-REASON-CD = 'G'
                   MOVE 15.00 TO DRSTAT-FEE-YTD
               WHEN OTHER
                   COMPUTE DRSTAT-FEE-YTD ROUNDED = DRSTAT-REINVEST-AMT
                       * DRSTAT-RATE / 100
           END-EVALUATE
           IF DRSTAT-FEE-YTD > 0 AND DRSTAT-FEE-YTD < 2.00
               MOVE 2.00 TO DRSTAT-FEE-YTD
           END-IF
           IF DRSTAT-FEE-YTD > 35.00
               MOVE 35.00 TO DRSTAT-FEE-YTD
           END-IF
           ADD DRSTAT-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * DRSTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRSTAT-CASH-PAY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2470-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRSTAT-CASH-PAY-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRSTAT-CASH-PAY-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E002 CUST ID REQUIRED
      * E458 SHARES AMT AMT NEGATIVE
      * E057 SHARES AMT AMT OVER 250000
      * E326 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2480-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRSTAT-CUST-ID = SPACES
                   MOVE 'E002' TO WS-REJECT-CODE
               WHEN DRSTAT-SHARES-AMT-AMT < 0
                   MOVE 'E458' TO WS-REJECT-CODE
               WHEN DRSTAT-SHARES-AMT-AMT > 250000.00
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN DRSTAT-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E326' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2490-ACCRUE-SHARES-AMT.
           IF DRSTAT-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRSTAT-SHARES-AMT-AMT * DRSTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRSTAT-DIVIDEND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2500-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRSTAT-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRSTAT-REINVEST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRSTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRSTAT-REINVEST-AMT ROUNDED = WS-WORK-AMT-2 *
                       DRSTAT-RATE / 200
               WHEN DRSTAT-REASON-CD = 'B'
                   MOVE 10.00 TO DRSTAT-REINVEST-AMT
               WHEN DRSTAT-REASON-CD = 'G'
                   MOVE 3.00 TO DRSTAT-REINVEST-AMT
               WHEN OTHER
                   COMPUTE DRSTAT-REINVEST-AMT ROUNDED = WS-WORK-AMT-2 *
                       DRSTAT-RATE / 100
           END-EVALUATE
           IF DRSTAT-REINVEST-AMT > 0 AND DRSTAT-REINVEST-AMT < 5.00
               MOVE 5.00 TO DRSTAT-REINVEST-AMT
           END-IF
           IF DRSTAT-REINVEST-AMT > 25.00
               MOVE 25.00 TO DRSTAT-REINVEST-AMT
           END-IF
           ADD DRSTAT-REINVEST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY PLAN OPTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRSTAT-PLAN-OPTION-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRSTAT-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2520-COUNT-EXCEPT.
           IF DRSTAT-EXCEPT-CNT < 9999999
               ADD 1 TO DRSTAT-EXCEPT-CNT
           END-IF
           IF DRSTAT-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO DRSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A SCHED TIER FOR THE CASH PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-SCHED-RATE(1) TO DRSTAT-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF DRSTAT-CASH-PAY-AMT NOT < WS-SHARES-A-SCHED-LOW(WS-TX)
                   MOVE WS-SHARES-A-SCHED-RATE(WS-TX) TO DRSTAT-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRSTAT-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRSTAT-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2540-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE DRSTAT-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2550-ACCRUE-SHARES-AMT.
           IF DRSTAT-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRSTAT-SHARES-AMT-AMT * DRSTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRSTAT-REINVEST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WITHHOLD YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2560-ACCRUE-WITHHOLD.
           IF DRSTAT-WITHHOLD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRSTAT-WITHHOLD-YTD * DRSTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRSTAT-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN LAST PAY DATE IS PAST
      * H -> K WHEN LAST PAY DATE IS PAST
      * K -> L WHEN LAST PAY DATE IS PAST
      * L -> M WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2570-SET-HOLDER-TYPE.
           EVALUATE DRSTAT-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRSTAT-LAST-PAY-DATE > 0 AND DRSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRSTAT-LAST-PAY-DATE > 0 AND DRSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRSTAT-LAST-PAY-DATE > 0 AND DRSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRSTAT-LAST-PAY-DATE > 0 AND DRSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-DIVIDEND.
           IF DRSTAT-DIVIDEND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRSTAT-DIVIDEND-AMT * DRSTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRSTAT-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2590-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRSTAT-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRSTAT-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRSTAT-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN MATURITY DATE IS PAST
      * H -> K WHEN MATURITY DATE IS PAST
      * K -> L WHEN MATURITY DATE IS PAST
      * L -> M WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2600-SET-HOLDER-TYPE.
           EVALUATE DRSTAT-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRSTAT-MATURITY-DATE > 0 AND DRSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO DRSTAT-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2610-ACCRUE-DIVIDEND.
           IF DRSTAT-DIVIDEND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRSTAT-DIVIDEND-AMT * DRSTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2620-COUNT-EXCEPT.
           IF DRSTAT-EXCEPT-CNT < 9999999
               ADD 1 TO DRSTAT-EXCEPT-CNT
           END-IF
           IF DRSTAT-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO DRSTAT-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRSTAT-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRSTAT-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO DRSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2640-COUNT-EXCEPT.
           IF DRSTAT-EXCEPT-CNT < 9999999
               ADD 1 TO DRSTAT-EXCEPT-CNT
           END-IF
           IF DRSTAT-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO DRSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E323 REF NO REQUIRED
      * E767 REINVEST AMT NEGATIVE
      * E866 REINVEST AMT OVER 1000000
      * E739 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2650-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRSTAT-REF-NO = SPACES
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN DRSTAT-REINVEST-AMT < 0
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN DRSTAT-REINVEST-AMT > 1000000.00
                   MOVE 'E866' TO WS-REJECT-CODE
               WHEN DRSTAT-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E927 CUST ID REQUIRED
      * E340 WITHHOLD YTD NEGATIVE
      * E666 WITHHOLD YTD OVER 5000
      * E351 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2660-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRSTAT-CUST-ID = SPACES
                   MOVE 'E927' TO WS-REJECT-CODE
               WHEN DRSTAT-WITHHOLD-YTD < 0
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN DRSTAT-WITHHOLD-YTD > 5000.00
                   MOVE 'E666' TO WS-REJECT-CODE
               WHEN DRSTAT-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO DRSTAT-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO DRSTAT-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO DRSTAT-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'DRSTAT EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
