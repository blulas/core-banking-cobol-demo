      *================================================================*
      * PROGRAM  : CRELG                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PROPRIETARY CREDIT CARD RULE - FEES. CALLED BY THE  *
      *            PROPRIETARY CREDIT CARD BATCH AND ONLINE PROGRAMS.  *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-19 PQR  ORIGINAL                                       *
      * 1997-07-11 BHS  FINANCE CUTOFF MOVED REQ4724                   *
      * 1998-12-01 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-08-16 JLM  AUDIT FINDING - LIMIT CONTROL TOTALS PRB5700   *
      * 2001-03-14 BHS  PAYMENT NOTICE TEXT PER LEGAL CHG7600          *
      * 2007-02-15 TWH  LIMIT FEE SCHEDULE UPDATE PRB3423              *
      * 2007-04-02 JLM  CASH-ADV CUTOFF MOVED REQ7077                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRELG.
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
                                           VALUE 10.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 31.
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
      * WS-CASH-ADV-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-ADV-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000251000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000331500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000244000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000425500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005650000000847000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006650000000257000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006900000000730500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007150000000620000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008150000000059000'.
       01  WS-CASH-ADV-SCHED-TBL REDEFINES WS-CASH-ADV-SCHED-VALUES.
           05  WS-CASH-ADV-SCHED-ROW OCCURS 9 TIMES.
               10  WS-CASH-ADV-SCHED-LOW   PIC 9(09)V99.
               10  WS-CASH-ADV-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000223500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000113000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000021500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000812000'.
       01  WS-PAYMENT-GRID-TBL REDEFINES WS-PAYMENT-GRID-VALUES.
           05  WS-PAYMENT-GRID-ROW OCCURS 4 TIMES.
               10  WS-PAYMENT-GRID-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-GRID-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CRELGP.

       PROCEDURE DIVISION USING CRELG-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-LATE-FEE
           PERFORM 2010-LIMIT-ITEM
           PERFORM 2020-AGE-LAST-ACT
           PERFORM 2030-DESCRIBE-BILL-CYCLE
           PERFORM 2040-SELECT-RATE
           PERFORM 2050-ACCUM-BILL-CYCLE
           PERFORM 2060-SET-TYPE
           PERFORM 2070-SET-BILL-CYCLE
           PERFORM 2080-DESCRIBE-CARD-TYPE
           PERFORM 2090-SELECT-DISCOUNT
           PERFORM 2100-PRORATE-LIMIT
           PERFORM 2110-SELECT-RATE
           PERFORM 2120-PRORATE-LIMIT
           PERFORM 2130-SET-BILL-CYCLE
           PERFORM 2140-AGE-MATURITY
           PERFORM 2150-ACCUM-TYPE
           PERFORM 2160-PRORATE-CASH-ADV
           PERFORM 2170-CHECK-MATURITY
           PERFORM 2180-SPLIT-LIMIT
           PERFORM 2190-DESCRIBE-TYPE
           PERFORM 2200-ACCUM-SOURCE
           PERFORM 2210-SELECT-RATE
           PERFORM 2220-ASSESS-EXEMPT
           PERFORM 2230-PRORATE-CASH-ADV
           PERFORM 2240-EDIT-DESC
           PERFORM 2250-ACCRUE-RATE
           PERFORM 2260-CHECK-LAST-PAY
           PERFORM 2270-ACCUM-CARD-TYPE
           PERFORM 2280-AGE-LAST-PAY
           PERFORM 2290-ACCUM-SOURCE
           PERFORM 2300-EDIT-DESC
           PERFORM 2310-FLAG-ITEM
           PERFORM 2320-SPLIT-CASH-ADV
           PERFORM 2330-SELECT-RATE
           PERFORM 2340-LIMIT-LATE-FEE
           PERFORM 2350-ASSESS-AUTO-RENEW
           PERFORM 2360-LIMIT-LIMIT
           PERFORM 2370-SET-BILL-CYCLE
           PERFORM 2380-DESCRIBE-BILL-CYCLE
           PERFORM 2390-EDIT-DESC
           PERFORM 2400-COUNT-CYCLE
           PERFORM 2410-COUNT-CYCLE
           PERFORM 2420-EDIT-DESC
           PERFORM 2430-COUNT-CYCLE
           PERFORM 2440-LIMIT-LIMIT
           PERFORM 2450-ASSESS-EXEMPT
           PERFORM 2460-DESCRIBE-BILL-CYCLE
           PERFORM 2470-AGE-LAST-PAY
           PERFORM 2480-DESCRIBE-CARD-TYPE
           PERFORM 2490-FLAG-LIMIT
           PERFORM 2500-ACCUM-CARD-TYPE
           PERFORM 2510-ASSESS-EXEMPT
           PERFORM 2520-ASSESS-STOP
           PERFORM 2530-EDIT-ADDR
           PERFORM 2540-PRORATE-LIMIT
           PERFORM 2550-SELECT-DISCOUNT
           PERFORM 2560-ASSESS-STOP
           PERFORM 2570-SELECT-DISCOUNT
           PERFORM 2580-SET-CARD-TYPE
           PERFORM 2590-COUNT-CYCLE
           PERFORM 2600-EDIT-ADDR
           PERFORM 2610-SET-SOURCE
           PERFORM 2620-SELECT-RATE
           PERFORM 2630-ACCUM-SOURCE
           PERFORM 2640-CHECK-MATURITY
           PERFORM 2650-CHECK-LAST-ACT
           PERFORM 2660-SPLIT-ITEM
           PERFORM 2670-CHECK-OPEN
           PERFORM 2680-PRORATE-MIN-DUE
           PERFORM 2690-ASSESS-STOP
           PERFORM 2700-SPLIT-PURCHASE
           PERFORM 2710-SPLIT-LIMIT
           PERFORM 2720-FLAG-MIN-DUE
           PERFORM 2730-SELECT-DISCOUNT
           PERFORM 2740-SPLIT-LIMIT
           PERFORM 2750-ASSESS-AUTO-RENEW
           PERFORM 2760-COUNT-CYCLE
           PERFORM 2770-SPLIT-LATE-FEE
           PERFORM 2780-FLAG-ITEM
           PERFORM 2790-SELECT-DISCOUNT
           PERFORM 2800-CHECK-MATURITY
           PERFORM 2810-PRORATE-LIMIT
           PERFORM 2820-SELECT-RATE
           PERFORM 2830-SPLIT-PURCHASE
           PERFORM 2840-SELECT-DISCOUNT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CRELG-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-LATE-FEE.
           IF CRELG-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRELG-LATE-FEE-AMT * CRELG-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRELG-PURCHASE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2010-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2020-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CRELG-LAST-ACT-DATE > 0 AND CRELG-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRELG-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRELG-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRELG-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRELG-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRELG-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRELG-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2030-DESCRIBE-BILL-CYCLE.
           EVALUATE CRELG-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE LIMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CRELG-LIMIT-YTD NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE AMT BY BILL CYCLE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2050-ACCUM-BILL-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRELG-BILL-CYCLE-CD
                   ADD CRELG-PURCHASE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRELG-BILL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRELG-PURCHASE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN LAST PAY DATE IS PAST
      * UV -> YK WHEN LAST PAY DATE IS PAST
      * YK -> GP WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2060-SET-TYPE.
           EVALUATE CRELG-TYPE-CD
               WHEN 'GP'
                   IF CRELG-LAST-PAY-DATE > 0 AND CRELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRELG-LAST-PAY-DATE > 0 AND CRELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRELG-LAST-PAY-DATE > 0 AND CRELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN LAST ACT DATE IS PAST
      * D -> E WHEN LAST ACT DATE IS PAST
      * E -> F WHEN LAST ACT DATE IS PAST
      * F -> H WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2070-SET-BILL-CYCLE.
           EVALUATE CRELG-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRELG-LAST-ACT-DATE > 0 AND CRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRELG-LAST-ACT-DATE > 0 AND CRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRELG-LAST-ACT-DATE > 0 AND CRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRELG-LAST-ACT-DATE > 0 AND CRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2080-DESCRIBE-CARD-TYPE.
           EVALUATE CRELG-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO CRELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRELG-LATE-FEE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO CRELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       2100-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRELG-LIMIT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRELG-LIMIT-AMT
           END-IF
           ADD CRELG-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2110-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CRELG-LATE-FEE-AMT NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       2120-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRELG-LIMIT-AMT ROUNDED =
                   CRELG-LIMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRELG-LIMIT-AMT
           END-IF
           ADD CRELG-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN MATURITY DATE IS PAST
      * D -> E WHEN MATURITY DATE IS PAST
      * E -> F WHEN MATURITY DATE IS PAST
      * F -> H WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2130-SET-BILL-CYCLE.
           EVALUATE CRELG-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRELG-MATURITY-DATE > 0 AND CRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRELG-MATURITY-DATE > 0 AND CRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRELG-MATURITY-DATE > 0 AND CRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRELG-MATURITY-DATE > 0 AND CRELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRELG-MATURITY-DATE > 0 AND CRELG-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRELG-MATURITY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2150-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRELG-TYPE-CD
                   ADD CRELG-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRELG-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRELG-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ADV AMT.
      *----------------------------------------------------------------*
       2160-PRORATE-CASH-ADV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRELG-CASH-ADV-AMT ROUNDED =
                   CRELG-LIMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRELG-CASH-ADV-AMT
           END-IF
           ADD CRELG-CASH-ADV-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2170-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRELG-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRELG-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CRELG-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2180-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRELG-LIMIT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CRELG-LIMIT-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2190-DESCRIBE-TYPE.
           EVALUATE CRELG-TYPE-CD
               WHEN 'GP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2200-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRELG-SOURCE-CD
                   ADD CRELG-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRELG-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRELG-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE LIMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2210-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CRELG-LIMIT-YTD NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE YTD FEE ON THE LIMIT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2220-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRELG-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRELG-MIN-DUE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRELG-STOP-FLAG = 'Y'
                   COMPUTE CRELG-MIN-DUE-YTD ROUNDED = CRELG-LIMIT-AMT *
                       CRELG-DISCOUNT / 200
               WHEN CRELG-CARD-TYPE-CD = 'PGU'
                   MOVE 15.00 TO CRELG-MIN-DUE-YTD
               WHEN CRELG-CARD-TYPE-CD = 'TRN'
                   MOVE 7.00 TO CRELG-MIN-DUE-YTD
               WHEN OTHER
                   COMPUTE CRELG-MIN-DUE-YTD ROUNDED = CRELG-LIMIT-AMT *
                       CRELG-DISCOUNT / 100
           END-EVALUATE
           IF CRELG-MIN-DUE-YTD > 0 AND CRELG-MIN-DUE-YTD < 10.00
               MOVE 10.00 TO CRELG-MIN-DUE-YTD
           END-IF
           IF CRELG-MIN-DUE-YTD > 50.00
               MOVE 50.00 TO CRELG-MIN-DUE-YTD
           END-IF
           ADD CRELG-MIN-DUE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CASH ADV AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2230-PRORATE-CASH-ADV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRELG-CASH-ADV-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E142 DESC REQUIRED
      * E745 WORK AMT 2 NEGATIVE
      * E251 WORK AMT 2 OVER 25000
      * E004 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2240-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRELG-DESC = SPACES
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN CRELG-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E004' TO WS-REJECT-CODE
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
                   WS-WORK-AMT-2 * CRELG-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRELG-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2260-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRELG-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CRELG-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT YTD BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2270-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRELG-CARD-TYPE-CD
                   ADD CRELG-LIMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRELG-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRELG-LIMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2280-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRELG-LAST-PAY-DATE > 0 AND CRELG-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRELG-PURCHASE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRELG-PURCHASE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRELG-PURCHASE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRELG-PURCHASE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRELG-PURCHASE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRELG-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRELG-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E208 DESC REQUIRED
      * E387 LIMIT YTD NEGATIVE
      * E343 LIMIT YTD OVER 10000
      * E328 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2300-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRELG-DESC = SPACES
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN CRELG-LIMIT-YTD < 0
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN CRELG-LIMIT-YTD > 10000.00
                   MOVE 'E343' TO WS-REJECT-CODE
               WHEN CRELG-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2310-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRELG-CASH-ADV-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CRELG-CASH-ADV-AMT - (WS-WORK-AMT *
               3)
           COMPUTE CRELG-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE CASH ADV AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2330-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CRELG-CASH-ADV-AMT NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2340-LIMIT-LATE-FEE.
           IF CRELG-LATE-FEE-AMT < 10.00
               MOVE 10.00 TO CRELG-LATE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRELG-LATE-FEE-AMT > 500.00
               MOVE 500.00 TO CRELG-LATE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV FEE ON THE LATE FEE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2350-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRELG-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRELG-CASH-ADV-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRELG-EXEMPT-FLAG = 'Y'
                   COMPUTE CRELG-CASH-ADV-AMT ROUNDED =
                       CRELG-LATE-FEE-AMT * CRELG-RATE / 200
               WHEN CRELG-TYPE-CD = 'GP'
                   MOVE 3.00 TO CRELG-CASH-ADV-AMT
               WHEN CRELG-TYPE-CD = 'UV'
                   MOVE 5.00 TO CRELG-CASH-ADV-AMT
               WHEN OTHER
                   COMPUTE CRELG-CASH-ADV-AMT ROUNDED =
                       CRELG-LATE-FEE-AMT * CRELG-RATE / 100
           END-EVALUATE
           IF CRELG-CASH-ADV-AMT > 0 AND CRELG-CASH-ADV-AMT < 2.00
               MOVE 2.00 TO CRELG-CASH-ADV-AMT
           END-IF
           IF CRELG-CASH-ADV-AMT > 75.00
               MOVE 75.00 TO CRELG-CASH-ADV-AMT
           END-IF
           ADD CRELG-CASH-ADV-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2360-LIMIT-LIMIT.
           IF CRELG-LIMIT-AMT < 10.00
               MOVE 10.00 TO CRELG-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRELG-LIMIT-AMT > 10000.00
               MOVE 10000.00 TO CRELG-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN OPEN DATE IS PAST
      * D -> E WHEN OPEN DATE IS PAST
      * E -> F WHEN OPEN DATE IS PAST
      * F -> H WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2370-SET-BILL-CYCLE.
           EVALUATE CRELG-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRELG-OPEN-DATE > 0 AND CRELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRELG-OPEN-DATE > 0 AND CRELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRELG-OPEN-DATE > 0 AND CRELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRELG-OPEN-DATE > 0 AND CRELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRELG-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-BILL-CYCLE.
           EVALUATE CRELG-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'E'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'F'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E483 DESC REQUIRED
      * E843 PURCHASE AMT NEGATIVE
      * E560 PURCHASE AMT OVER 5000
      * E096 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2390-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRELG-DESC = SPACES
                   MOVE 'E483' TO WS-REJECT-CODE
               WHEN CRELG-PURCHASE-AMT < 0
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN CRELG-PURCHASE-AMT > 5000.00
                   MOVE 'E560' TO WS-REJECT-CODE
               WHEN CRELG-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2400-COUNT-CYCLE.
           IF CRELG-CYCLE-CNT < 9999999
               ADD 1 TO CRELG-CYCLE-CNT
           END-IF
           IF CRELG-CYCLE-CNT NOT < 5
               MOVE 'Y' TO CRELG-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-CYCLE.
           IF CRELG-CYCLE-CNT < 9999999
               ADD 1 TO CRELG-CYCLE-CNT
           END-IF
           IF CRELG-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CRELG-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E847 DESC REQUIRED
      * E666 MIN DUE YTD NEGATIVE
      * E392 MIN DUE YTD OVER 25000
      * E612 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2420-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRELG-DESC = SPACES
                   MOVE 'E847' TO WS-REJECT-CODE
               WHEN CRELG-MIN-DUE-YTD < 0
                   MOVE 'E666' TO WS-REJECT-CODE
               WHEN CRELG-MIN-DUE-YTD > 25000.00
                   MOVE 'E392' TO WS-REJECT-CODE
               WHEN CRELG-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-CYCLE.
           IF CRELG-CYCLE-CNT < 9999999
               ADD 1 TO CRELG-CYCLE-CNT
           END-IF
           IF CRELG-CYCLE-CNT NOT < 5
               MOVE 'Y' TO CRELG-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2440-LIMIT-LIMIT.
           IF CRELG-LIMIT-AMT < 25.00
               MOVE 25.00 TO CRELG-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRELG-LIMIT-AMT > 5000.00
               MOVE 5000.00 TO CRELG-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE LATE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2450-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRELG-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRELG-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRELG-STOP-FLAG = 'Y'
                   COMPUTE CRELG-PURCHASE-AMT ROUNDED =
                       CRELG-LATE-FEE-AMT * CRELG-RATE / 200
               WHEN CRELG-SOURCE-CD = 'C'
                   MOVE 10.00 TO CRELG-PURCHASE-AMT
               WHEN CRELG-SOURCE-CD = 'H'
                   MOVE 10.00 TO CRELG-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE CRELG-PURCHASE-AMT ROUNDED =
                       CRELG-LATE-FEE-AMT * CRELG-RATE / 100
           END-EVALUATE
           IF CRELG-PURCHASE-AMT > 0 AND CRELG-PURCHASE-AMT < 1.00
               MOVE 1.00 TO CRELG-PURCHASE-AMT
           END-IF
           IF CRELG-PURCHASE-AMT > 100.00
               MOVE 100.00 TO CRELG-PURCHASE-AMT
           END-IF
           ADD CRELG-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2460-DESCRIBE-BILL-CYCLE.
           EVALUATE CRELG-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'E'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2470-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRELG-LAST-PAY-DATE > 0 AND CRELG-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRELG-CASH-ADV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRELG-CASH-ADV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CRELG-CASH-ADV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD CRELG-CASH-ADV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRELG-CASH-ADV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-CARD-TYPE.
           EVALUATE CRELG-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LIMIT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2490-FLAG-LIMIT.
           IF CRELG-LIMIT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRELG-LIMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRELG-CARD-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRELG-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE FEE ON THE CASH ADV AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2510-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRELG-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRELG-LATE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRELG-STOP-FLAG = 'Y'
                   COMPUTE CRELG-LATE-FEE-AMT ROUNDED =
                       CRELG-CASH-ADV-AMT * CRELG-DISCOUNT / 200
               WHEN CRELG-SOURCE-CD = 'C'
                   MOVE 12.00 TO CRELG-LATE-FEE-AMT
               WHEN CRELG-SOURCE-CD = 'H'
                   MOVE 20.00 TO CRELG-LATE-FEE-AMT
               WHEN OTHER
                   COMPUTE CRELG-LATE-FEE-AMT ROUNDED =
                       CRELG-CASH-ADV-AMT * CRELG-DISCOUNT / 100
           END-EVALUATE
           IF CRELG-LATE-FEE-AMT > 0 AND CRELG-LATE-FEE-AMT < 10.00
               MOVE 10.00 TO CRELG-LATE-FEE-AMT
           END-IF
           IF CRELG-LATE-FEE-AMT > 150.00
               MOVE 150.00 TO CRELG-LATE-FEE-AMT
           END-IF
           ADD CRELG-LATE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CASH ADV AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2520-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CRELG-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRELG-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRELG-CASH-ADV-AMT *
                       CRELG-DISCOUNT / 200
               WHEN CRELG-BILL-CYCLE-CD = 'B'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN CRELG-BILL-CYCLE-CD = 'D'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRELG-CASH-ADV-AMT *
                       CRELG-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E879 ADDR REQUIRED
      * E708 MIN DUE YTD NEGATIVE
      * E591 MIN DUE YTD OVER 1000000
      * E969 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2530-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRELG-ADDR = SPACES
                   MOVE 'E879' TO WS-REJECT-CODE
               WHEN CRELG-MIN-DUE-YTD < 0
                   MOVE 'E708' TO WS-REJECT-CODE
               WHEN CRELG-MIN-DUE-YTD > 1000000.00
                   MOVE 'E591' TO WS-REJECT-CODE
               WHEN CRELG-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E969' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT YTD.
      *----------------------------------------------------------------*
       2540-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRELG-LIMIT-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRELG-LIMIT-YTD
           END-IF
           ADD CRELG-LIMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2550-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE LATE FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2560-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CRELG-STOP-FLAG = 'Y'
                   MOVE ZERO TO CRELG-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRELG-EXEMPT-FLAG = 'Y'
                   COMPUTE CRELG-PURCHASE-AMT ROUNDED =
                       CRELG-LATE-FEE-AMT * CRELG-RATE / 200
               WHEN CRELG-CARD-TYPE-CD = 'PGU'
                   MOVE 12.00 TO CRELG-PURCHASE-AMT
               WHEN CRELG-CARD-TYPE-CD = 'TRN'
                   MOVE 15.00 TO CRELG-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE CRELG-PURCHASE-AMT ROUNDED =
                       CRELG-LATE-FEE-AMT * CRELG-RATE / 100
           END-EVALUATE
           IF CRELG-PURCHASE-AMT > 0 AND CRELG-PURCHASE-AMT < 1.00
               MOVE 1.00 TO CRELG-PURCHASE-AMT
           END-IF
           IF CRELG-PURCHASE-AMT > 35.00
               MOVE 35.00 TO CRELG-PURCHASE-AMT
           END-IF
           ADD CRELG-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2570-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CRELG-LIMIT-AMT NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARD TYPE CD:
      * PGU -> TRN WHEN LAST PAY DATE IS PAST
      * TRN -> WHR WHEN LAST PAY DATE IS PAST
      * WHR -> ZWB WHEN LAST PAY DATE IS PAST
      * ZWB -> PGU WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2580-SET-CARD-TYPE.
           EVALUATE CRELG-CARD-TYPE-CD
               WHEN 'PGU'
                   IF CRELG-LAST-PAY-DATE > 0 AND CRELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TRN' TO CRELG-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TRN'
                   IF CRELG-LAST-PAY-DATE > 0 AND CRELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WHR' TO CRELG-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WHR'
                   IF CRELG-LAST-PAY-DATE > 0 AND CRELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWB' TO CRELG-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZWB'
                   IF CRELG-LAST-PAY-DATE > 0 AND CRELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PGU' TO CRELG-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-CYCLE.
           IF CRELG-CYCLE-CNT < 9999999
               ADD 1 TO CRELG-CYCLE-CNT
           END-IF
           IF CRELG-CYCLE-CNT NOT < 3
               MOVE 'Y' TO CRELG-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E129 ADDR REQUIRED
      * E239 PURCHASE AMT NEGATIVE
      * E487 PURCHASE AMT OVER 1000000
      * E635 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2600-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRELG-ADDR = SPACES
                   MOVE 'E129' TO WS-REJECT-CODE
               WHEN CRELG-PURCHASE-AMT < 0
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN CRELG-PURCHASE-AMT > 1000000.00
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN CRELG-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E635' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN LAST ACT DATE IS PAST
      * H -> M WHEN LAST ACT DATE IS PAST
      * M -> R WHEN LAST ACT DATE IS PAST
      * R -> S WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2610-SET-SOURCE.
           EVALUATE CRELG-SOURCE-CD
               WHEN 'C'
                   IF CRELG-LAST-ACT-DATE > 0 AND CRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF CRELG-LAST-ACT-DATE > 0 AND CRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CRELG-LAST-ACT-DATE > 0 AND CRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO CRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF CRELG-LAST-ACT-DATE > 0 AND CRELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CRELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2620-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO CRELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO CRELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2630-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRELG-SOURCE-CD
                   ADD CRELG-PURCHASE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRELG-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRELG-PURCHASE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2640-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRELG-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRELG-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CRELG-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2650-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CRELG-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRELG-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CRELG-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2660-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2670-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRELG-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRELG-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CRELG-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       2680-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRELG-MIN-DUE-YTD ROUNDED =
                   CRELG-LIMIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRELG-MIN-DUE-YTD
           END-IF
           ADD CRELG-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CASH ADV AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2690-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CRELG-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRELG-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CRELG-CASH-ADV-AMT *
                       CRELG-DISCOUNT / 200
               WHEN CRELG-SOURCE-CD = 'C'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN CRELG-SOURCE-CD = 'H'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CRELG-CASH-ADV-AMT *
                       CRELG-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2700-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = CRELG-PURCHASE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CRELG-PURCHASE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE CRELG-LATE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2710-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE CRELG-LIMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MIN DUE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2720-FLAG-MIN-DUE.
           IF CRELG-MIN-DUE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRELG-MIN-DUE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2730-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO CRELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO CRELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2740-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRELG-LIMIT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CRELG-LIMIT-YTD - (WS-WORK-AMT * 2)
           COMPUTE CRELG-MIN-DUE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2750-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRELG-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRELG-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       CRELG-RATE / 200
               WHEN CRELG-BILL-CYCLE-CD = 'B'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN CRELG-BILL-CYCLE-CD = 'D'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       CRELG-RATE / 100
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
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2760-COUNT-CYCLE.
           IF CRELG-CYCLE-CNT < 9999999
               ADD 1 TO CRELG-CYCLE-CNT
           END-IF
           IF CRELG-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CRELG-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2770-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRELG-LATE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CRELG-LATE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE CRELG-MIN-DUE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2780-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2790-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CRELG-LATE-FEE-AMT NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2800-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRELG-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRELG-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CRELG-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT YTD.
      *----------------------------------------------------------------*
       2810-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRELG-LIMIT-YTD ROUNDED =
                   CRELG-LIMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRELG-LIMIT-YTD
           END-IF
           ADD CRELG-LIMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE MIN DUE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2820-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CRELG-MIN-DUE-YTD NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2830-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = CRELG-PURCHASE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CRELG-PURCHASE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE CASH ADV AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2840-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CRELG-CASH-ADV-AMT NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CRELG-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CRELG-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CRELG-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CRELG EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
