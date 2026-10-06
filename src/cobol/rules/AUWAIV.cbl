      *================================================================*
      * PROGRAM  : AUWAIV                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DEALER AUTO FINANCE RULE - DATE RULES. CALLED BY THE*
      *            DEALER AUTO FINANCE BATCH AND ONLINE PROGRAMS.      *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-05-06 DPS  ORIGINAL                                       *
      * 1994-12-23 EWS  REBATE STATUS CODES ADDED INC0160              *
      * 1995-05-18 TWH  REBATE TABLE EXPANDED REQ8236                  *
      * 1996-12-22 SKN  AUDIT FINDING - RESERVE CONTROL TOTALS CHG2118 *
      * 1997-06-19 CRP  GAP STATUS CODES ADDED PRB9086                 *
      * 1999-06-24 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2006-12-27 DPS  PERFORMANCE - GAP READ AHEAD CHG7944           *
      * 2006-03-12 TWH  RESERVE TABLE EXPANDED INC7803                 *
      * 2008-06-03 PQR  CHARGEBK STATUS CODES ADDED REQ8521            *
      * 2008-08-18 LMB  PAYMENT ROUNDING - HALF UP CHG5473             *
      * 2008-08-18 CRP  AMOUNT-FIN ROUNDING - HALF UP PRB3981          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUWAIV.
       AUTHOR. D P SHAW.

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
                                           VALUE 13.000000.
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
      * WS-REBATE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REBATE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000758500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000362000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000140000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000264000'.
       01  WS-REBATE-BAND-TBL REDEFINES WS-REBATE-BAND-VALUES.
           05  WS-REBATE-BAND-ROW OCCURS 4 TIMES.
               10  WS-REBATE-BAND-LOW      PIC 9(09)V99.
               10  WS-REBATE-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CHARGEBK-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CHARGEBK-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000613000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000139500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000572000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000597500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000276000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000838000'.
       01  WS-CHARGEBK-SCHED-TBL REDEFINES WS-CHARGEBK-SCHED-VALUES.
           05  WS-CHARGEBK-SCHED-ROW OCCURS 6 TIMES.
               10  WS-CHARGEBK-SCHED-LOW   PIC 9(09)V99.
               10  WS-CHARGEBK-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REBATE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REBATE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000432000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000070000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000747000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000848500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000576000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000260000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000200500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000844000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000185500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000501500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000417500'.
       01  WS-REBATE-SCHED-TBL REDEFINES WS-REBATE-SCHED-VALUES.
           05  WS-REBATE-SCHED-ROW OCCURS 11 TIMES.
               10  WS-REBATE-SCHED-LOW     PIC 9(09)V99.
               10  WS-REBATE-SCHED-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY AUWAIVP.

       PROCEDURE DIVISION USING AUWAIV-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-LIMIT-CHARGEBK
           PERFORM 2010-COUNT-PAYMENT
           PERFORM 2020-ACCUM-TYPE
           PERFORM 2030-ASSESS-NOTICE-SENT
           PERFORM 2040-COUNT-PAYMENT
           PERFORM 2050-ACCRUE-RATE
           PERFORM 2060-ACCUM-STATUS
           PERFORM 2070-EDIT-NAME
           PERFORM 2080-CHECK-LAST-MAINT
           PERFORM 2090-ACCUM-STATUS
           PERFORM 2100-PRORATE-AMOUNT-FIN
           PERFORM 2110-AGE-LAST-MAINT
           PERFORM 2120-SET-STATUS
           PERFORM 2130-EDIT-NAME
           PERFORM 2140-SELECT-FEE-PCT
           PERFORM 2150-ACCUM-TIER
           PERFORM 2160-ACCRUE-PAYMENT
           PERFORM 2170-ASSESS-EXEMPT
           PERFORM 2180-EDIT-NAME
           PERFORM 2190-AGE-LAST-MAINT
           PERFORM 2200-COUNT-PAYMENT
           PERFORM 2210-SPLIT-AMOUNT-FIN
           PERFORM 2220-CHECK-NEXT-DUE
           PERFORM 2230-SET-TYPE
           PERFORM 2240-PRORATE-AMOUNT-FIN
           PERFORM 2250-DESCRIBE-CYCLE
           PERFORM 2260-LIMIT-AMOUNT-FIN
           PERFORM 2270-SELECT-RATE
           PERFORM 2280-CHECK-LAST-MAINT
           PERFORM 2290-AGE-LAST-PAY
           PERFORM 2300-SELECT-FEE-PCT
           PERFORM 2310-COUNT-RETRY
           PERFORM 2320-LIMIT-PAYMENT
           PERFORM 2330-SELECT-FEE-PCT
           PERFORM 2340-FLAG-PAYMENT
           PERFORM 2350-COUNT-PAYMENT
           PERFORM 2360-ASSESS-EXEMPT
           PERFORM 2370-SPLIT-GAP
           PERFORM 2380-SELECT-RATE
           PERFORM 2390-EDIT-CONTACT
           PERFORM 2400-SELECT-RATE
           PERFORM 2410-CHECK-LAST-MAINT
           PERFORM 2420-LIMIT-CHARGEBK
           PERFORM 2430-AGE-NEXT-REVIEW
           PERFORM 2440-SET-TYPE
           PERFORM 2450-PRORATE-GAP
           PERFORM 2460-SPLIT-GAP
           PERFORM 2470-ACCRUE-GAP
           PERFORM 2480-AGE-NEXT-DUE
           PERFORM 2490-SPLIT-CHARGEBK
           PERFORM 2500-CHECK-LAST-MAINT
           PERFORM 2510-PRORATE-PAYMENT
           PERFORM 2520-SPLIT-AMOUNT-FIN
           PERFORM 2530-LIMIT-ITEM
           PERFORM 2540-AGE-LAST-PAY
           PERFORM 2550-ACCRUE-FEE-PCT
           PERFORM 2560-ACCUM-STATUS
           PERFORM 2570-COUNT-PAYMENT
           PERFORM 2580-SELECT-FEE-PCT
           PERFORM 2590-SELECT-RATE
           PERFORM 2600-ASSESS-EXEMPT
           PERFORM 2610-EDIT-NAME
           PERFORM 2620-EDIT-CONTACT
           PERFORM 2630-SPLIT-AMOUNT-FIN
           PERFORM 2640-FLAG-CHARGEBK
           PERFORM 2650-FLAG-ITEM
           PERFORM 2660-SPLIT-PAYMENT
           PERFORM 2670-PRORATE-GAP
           PERFORM 2680-CHECK-LAST-PAY
           PERFORM 2690-CHECK-NEXT-DUE
           PERFORM 2700-PRORATE-GAP
           PERFORM 2710-SELECT-RATE
           PERFORM 2720-AGE-LAST-PAY
           PERFORM 2730-ASSESS-PREFERRED
           PERFORM 2740-ASSESS-NOTICE-SENT
           PERFORM 2750-LIMIT-CHARGEBK
           PERFORM 2760-SPLIT-GAP
           PERFORM 2770-PRORATE-AMOUNT-FIN
           PERFORM 2780-SET-STATUS
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO AUWAIV-RETURN-CODE.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2000-LIMIT-CHARGEBK.
           IF AUWAIV-CHARGEBK-AMT < 10.00
               MOVE 10.00 TO AUWAIV-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AUWAIV-CHARGEBK-AMT > 50000.00
               MOVE 50000.00 TO AUWAIV-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-PAYMENT.
           IF AUWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO AUWAIV-PAYMENT-CNT
           END-IF
           IF AUWAIV-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO AUWAIV-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2020-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUWAIV-TYPE-CD
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUWAIV-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AUWAIV-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE CHARGEBK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2030-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN AUWAIV-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO AUWAIV-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUWAIV-PREFERRED-FLAG = 'Y'
                   COMPUTE AUWAIV-PAYMENT-YTD ROUNDED =
                       AUWAIV-CHARGEBK-AMT * AUWAIV-FEE-PCT / 200
               WHEN AUWAIV-TIER-CD = 'EW'
                   MOVE 7.00 TO AUWAIV-PAYMENT-YTD
               WHEN AUWAIV-TIER-CD = 'JF'
                   MOVE 12.00 TO AUWAIV-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE AUWAIV-PAYMENT-YTD ROUNDED =
                       AUWAIV-CHARGEBK-AMT * AUWAIV-FEE-PCT / 100
           END-EVALUATE
           IF AUWAIV-PAYMENT-YTD > 0 AND AUWAIV-PAYMENT-YTD < 10.00
               MOVE 10.00 TO AUWAIV-PAYMENT-YTD
           END-IF
           IF AUWAIV-PAYMENT-YTD > 35.00
               MOVE 35.00 TO AUWAIV-PAYMENT-YTD
           END-IF
           ADD AUWAIV-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-PAYMENT.
           IF AUWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO AUWAIV-PAYMENT-CNT
           END-IF
           IF AUWAIV-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO AUWAIV-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2050-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * AUWAIV-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGEBK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2060-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUWAIV-STATUS-CD
                   ADD AUWAIV-CHARGEBK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUWAIV-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AUWAIV-CHARGEBK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E490 NAME REQUIRED
      * E606 WORK AMT 2 NEGATIVE
      * E638 WORK AMT 2 OVER 25000
      * E383 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2070-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AUWAIV-NAME = SPACES
                   MOVE 'E490' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E638' TO WS-REJECT-CODE
               WHEN AUWAIV-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E383' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2080-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO AUWAIV-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2090-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUWAIV-STATUS-CD
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUWAIV-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AUWAIV-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2100-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   AUWAIV-AMOUNT-FIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-LAST-MAINT-DATE > 0 AND AUWAIV-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN LAST MAINT DATE IS PAST
      * HMW -> RCB WHEN LAST MAINT DATE IS PAST
      * RCB -> EUA WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2120-SET-STATUS.
           EVALUATE AUWAIV-STATUS-CD
               WHEN 'EUA'
                   IF AUWAIV-LAST-MAINT-DATE > 0 AND
                       AUWAIV-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HMW' TO AUWAIV-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF AUWAIV-LAST-MAINT-DATE > 0 AND
                       AUWAIV-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'RCB' TO AUWAIV-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF AUWAIV-LAST-MAINT-DATE > 0 AND
                       AUWAIV-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'EUA' TO AUWAIV-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E994 NAME REQUIRED
      * E135 WORK AMT 2 NEGATIVE
      * E827 WORK AMT 2 OVER 10000
      * E090 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AUWAIV-NAME = SPACES
                   MOVE 'E994' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E135' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN AUWAIV-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHARGEBK SCHED TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2140-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CHARGEBK-SCHED-RATE(1) TO AUWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF AUWAIV-PAYMENT-AMT NOT < WS-CHARGEBK-SCHED-LOW(WS-TX)
                   MOVE WS-CHARGEBK-SCHED-RATE(WS-TX) TO AUWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN AMT BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2150-ACCUM-TIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUWAIV-TIER-CD
                   ADD AUWAIV-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUWAIV-TIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AUWAIV-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2160-ACCRUE-PAYMENT.
           IF AUWAIV-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AUWAIV-PAYMENT-YTD * AUWAIV-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO AUWAIV-CHARGEBK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GAP FEE ON THE AMOUNT FIN YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2170-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN AUWAIV-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO AUWAIV-GAP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUWAIV-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE AUWAIV-GAP-AMT ROUNDED =
                       AUWAIV-AMOUNT-FIN-YTD * AUWAIV-RATE / 200
               WHEN AUWAIV-TIER-CD = 'EW'
                   MOVE 15.00 TO AUWAIV-GAP-AMT
               WHEN AUWAIV-TIER-CD = 'JF'
                   MOVE 7.00 TO AUWAIV-GAP-AMT
               WHEN OTHER
                   COMPUTE AUWAIV-GAP-AMT ROUNDED =
                       AUWAIV-AMOUNT-FIN-YTD * AUWAIV-RATE / 100
           END-EVALUATE
           IF AUWAIV-GAP-AMT > 0 AND AUWAIV-GAP-AMT < 5.00
               MOVE 5.00 TO AUWAIV-GAP-AMT
           END-IF
           IF AUWAIV-GAP-AMT > 50.00
               MOVE 50.00 TO AUWAIV-GAP-AMT
           END-IF
           ADD AUWAIV-GAP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E390 NAME REQUIRED
      * E182 PAYMENT AMT NEGATIVE
      * E592 PAYMENT AMT OVER 250000
      * E721 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2180-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AUWAIV-NAME = SPACES
                   MOVE 'E390' TO WS-REJECT-CODE
               WHEN AUWAIV-PAYMENT-AMT < 0
                   MOVE 'E182' TO WS-REJECT-CODE
               WHEN AUWAIV-PAYMENT-AMT > 250000.00
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN AUWAIV-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E721' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2190-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-LAST-MAINT-DATE > 0 AND AUWAIV-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUWAIV-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD AUWAIV-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD AUWAIV-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD AUWAIV-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUWAIV-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-PAYMENT.
           IF AUWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO AUWAIV-PAYMENT-CNT
           END-IF
           IF AUWAIV-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO AUWAIV-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2210-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = AUWAIV-AMOUNT-FIN-AMT / 12
           COMPUTE WS-WORK-AMT-2 = AUWAIV-AMOUNT-FIN-AMT - (WS-WORK-AMT
               * 12)
           COMPUTE AUWAIV-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2220-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO AUWAIV-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN LAST MAINT DATE IS PAST
      * H -> S WHEN LAST MAINT DATE IS PAST
      * S -> V WHEN LAST MAINT DATE IS PAST
      * V -> W WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2230-SET-TYPE.
           EVALUATE AUWAIV-TYPE-CD
               WHEN 'B'
                   IF AUWAIV-LAST-MAINT-DATE > 0 AND
                       AUWAIV-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO AUWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF AUWAIV-LAST-MAINT-DATE > 0 AND
                       AUWAIV-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO AUWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF AUWAIV-LAST-MAINT-DATE > 0 AND
                       AUWAIV-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO AUWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF AUWAIV-LAST-MAINT-DATE > 0 AND
                       AUWAIV-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'W' TO AUWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT FIN YTD.
      *----------------------------------------------------------------*
       2240-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AUWAIV-AMOUNT-FIN-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO AUWAIV-AMOUNT-FIN-YTD
           END-IF
           ADD AUWAIV-AMOUNT-FIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2250-DESCRIBE-CYCLE.
           EVALUATE AUWAIV-CYCLE-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'J'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2260-LIMIT-AMOUNT-FIN.
           IF AUWAIV-AMOUNT-FIN-AMT < 0.00
               MOVE 0.00 TO AUWAIV-AMOUNT-FIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AUWAIV-AMOUNT-FIN-AMT > 1000.00
               MOVE 1000.00 TO AUWAIV-AMOUNT-FIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHARGEBK SCHED TIER FOR THE AMOUNT FIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2270-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CHARGEBK-SCHED-RATE(1) TO AUWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF AUWAIV-AMOUNT-FIN-AMT NOT <
                   WS-CHARGEBK-SCHED-LOW(WS-TX)
                   MOVE WS-CHARGEBK-SCHED-RATE(WS-TX) TO AUWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2280-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO AUWAIV-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-LAST-PAY-DATE > 0 AND AUWAIV-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUWAIV-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD AUWAIV-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD AUWAIV-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AUWAIV-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUWAIV-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CHARGEBK SCHED TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2300-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CHARGEBK-SCHED-RATE(1) TO AUWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF AUWAIV-PAYMENT-YTD NOT < WS-CHARGEBK-SCHED-LOW(WS-TX)
                   MOVE WS-CHARGEBK-SCHED-RATE(WS-TX) TO AUWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2310-COUNT-RETRY.
           IF AUWAIV-RETRY-CNT < 9999999
               ADD 1 TO AUWAIV-RETRY-CNT
           END-IF
           IF AUWAIV-RETRY-CNT NOT < 6
               MOVE 'Y' TO AUWAIV-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2320-LIMIT-PAYMENT.
           IF AUWAIV-PAYMENT-AMT < 100.00
               MOVE 100.00 TO AUWAIV-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AUWAIV-PAYMENT-AMT > 1000.00
               MOVE 1000.00 TO AUWAIV-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REBATE BAND TIER FOR THE CHARGEBK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2330-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-BAND-RATE(1) TO AUWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF AUWAIV-CHARGEBK-AMT NOT < WS-REBATE-BAND-LOW(WS-TX)
                   MOVE WS-REBATE-BAND-RATE(WS-TX) TO AUWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2340-FLAG-PAYMENT.
           IF AUWAIV-PAYMENT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE AUWAIV-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2350-COUNT-PAYMENT.
           IF AUWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO AUWAIV-PAYMENT-CNT
           END-IF
           IF AUWAIV-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO AUWAIV-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GAP FEE ON THE AMOUNT FIN YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2360-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN AUWAIV-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO AUWAIV-GAP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUWAIV-PREFERRED-FLAG = 'Y'
                   COMPUTE AUWAIV-GAP-AMT ROUNDED =
                       AUWAIV-AMOUNT-FIN-YTD * AUWAIV-RATE / 200
               WHEN AUWAIV-TYPE-CD = 'B'
                   MOVE 3.00 TO AUWAIV-GAP-AMT
               WHEN AUWAIV-TYPE-CD = 'H'
                   MOVE 5.00 TO AUWAIV-GAP-AMT
               WHEN OTHER
                   COMPUTE AUWAIV-GAP-AMT ROUNDED =
                       AUWAIV-AMOUNT-FIN-YTD * AUWAIV-RATE / 100
           END-EVALUATE
           IF AUWAIV-GAP-AMT > 0 AND AUWAIV-GAP-AMT < 5.00
               MOVE 5.00 TO AUWAIV-GAP-AMT
           END-IF
           IF AUWAIV-GAP-AMT > 50.00
               MOVE 50.00 TO AUWAIV-GAP-AMT
           END-IF
           ADD AUWAIV-GAP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2370-SPLIT-GAP.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE AUWAIV-GAP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHARGEBK SCHED TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2380-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CHARGEBK-SCHED-RATE(1) TO AUWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF AUWAIV-PAYMENT-AMT NOT < WS-CHARGEBK-SCHED-LOW(WS-TX)
                   MOVE WS-CHARGEBK-SCHED-RATE(WS-TX) TO AUWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E830 CONTACT REQUIRED
      * E195 WORK AMT NEGATIVE
      * E860 WORK AMT OVER 5000
      * E818 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2390-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AUWAIV-CONTACT = SPACES
                   MOVE 'E830' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E195' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E860' TO WS-REJECT-CODE
               WHEN AUWAIV-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E818' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REBATE BAND TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2400-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-BAND-RATE(1) TO AUWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF AUWAIV-GAP-AMT NOT < WS-REBATE-BAND-LOW(WS-TX)
                   MOVE WS-REBATE-BAND-RATE(WS-TX) TO AUWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2410-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO AUWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2420-LIMIT-CHARGEBK.
           IF AUWAIV-CHARGEBK-AMT < 0.00
               MOVE 0.00 TO AUWAIV-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AUWAIV-CHARGEBK-AMT > 10000.00
               MOVE 10000.00 TO AUWAIV-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2430-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-NEXT-REVIEW-DATE > 0 AND AUWAIV-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUWAIV-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD AUWAIV-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD AUWAIV-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AUWAIV-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUWAIV-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> S WHEN NEXT REVIEW DATE IS PAST
      * S -> V WHEN NEXT REVIEW DATE IS PAST
      * V -> W WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2440-SET-TYPE.
           EVALUATE AUWAIV-TYPE-CD
               WHEN 'B'
                   IF AUWAIV-NEXT-REVIEW-DATE > 0 AND
                       AUWAIV-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO AUWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF AUWAIV-NEXT-REVIEW-DATE > 0 AND
                       AUWAIV-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO AUWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF AUWAIV-NEXT-REVIEW-DATE > 0 AND
                       AUWAIV-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO AUWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF AUWAIV-NEXT-REVIEW-DATE > 0 AND
                       AUWAIV-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'W' TO AUWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GAP AMT.
      *----------------------------------------------------------------*
       2450-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AUWAIV-GAP-AMT ROUNDED =
                   AUWAIV-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO AUWAIV-GAP-AMT
           END-IF
           ADD AUWAIV-GAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT GAP AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2460-SPLIT-GAP.
           COMPUTE WS-WORK-AMT = AUWAIV-GAP-AMT / 4
           COMPUTE WS-WORK-AMT-2 = AUWAIV-GAP-AMT - (WS-WORK-AMT * 4)
           COMPUTE AUWAIV-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GAP AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2470-ACCRUE-GAP.
           IF AUWAIV-GAP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AUWAIV-GAP-AMT * AUWAIV-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO AUWAIV-CHARGEBK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2480-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-NEXT-DUE-DATE > 0 AND AUWAIV-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUWAIV-AMOUNT-FIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2490-SPLIT-CHARGEBK.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE AUWAIV-CHARGEBK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2500-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO AUWAIV-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       2510-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AUWAIV-PAYMENT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO AUWAIV-PAYMENT-YTD
           END-IF
           ADD AUWAIV-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2520-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = AUWAIV-AMOUNT-FIN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = AUWAIV-AMOUNT-FIN-AMT - (WS-WORK-AMT
               * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2530-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-LAST-PAY-DATE > 0 AND AUWAIV-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUWAIV-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD AUWAIV-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD AUWAIV-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AUWAIV-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUWAIV-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2550-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * AUWAIV-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO AUWAIV-AMOUNT-FIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUWAIV-STATUS-CD
                   ADD AUWAIV-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUWAIV-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AUWAIV-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2570-COUNT-PAYMENT.
           IF AUWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO AUWAIV-PAYMENT-CNT
           END-IF
           IF AUWAIV-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO AUWAIV-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REBATE SCHED TIER FOR THE AMOUNT FIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2580-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-SCHED-RATE(1) TO AUWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AUWAIV-AMOUNT-FIN-YTD NOT < WS-REBATE-SCHED-LOW(WS-TX)
                   MOVE WS-REBATE-SCHED-RATE(WS-TX) TO AUWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHARGEBK SCHED TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2590-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CHARGEBK-SCHED-RATE(1) TO AUWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF AUWAIV-PAYMENT-YTD NOT < WS-CHARGEBK-SCHED-LOW(WS-TX)
                   MOVE WS-CHARGEBK-SCHED-RATE(WS-TX) TO AUWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PAYMENT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2600-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN AUWAIV-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUWAIV-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AUWAIV-PAYMENT-AMT *
                       AUWAIV-RATE / 200
               WHEN AUWAIV-TIER-CD = 'EW'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN AUWAIV-TIER-CD = 'JF'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AUWAIV-PAYMENT-AMT *
                       AUWAIV-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E833 NAME REQUIRED
      * E823 PAYMENT YTD NEGATIVE
      * E106 PAYMENT YTD OVER 1000000
      * E204 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AUWAIV-NAME = SPACES
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN AUWAIV-PAYMENT-YTD < 0
                   MOVE 'E823' TO WS-REJECT-CODE
               WHEN AUWAIV-PAYMENT-YTD > 1000000.00
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN AUWAIV-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E204' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E864 CONTACT REQUIRED
      * E926 WORK AMT 2 NEGATIVE
      * E559 WORK AMT 2 OVER 1000000
      * E863 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2620-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AUWAIV-CONTACT = SPACES
                   MOVE 'E864' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E559' TO WS-REJECT-CODE
               WHEN AUWAIV-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E863' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2630-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = AUWAIV-AMOUNT-FIN-YTD / 12
           COMPUTE WS-WORK-AMT-2 = AUWAIV-AMOUNT-FIN-YTD - (WS-WORK-AMT
               * 12)
           COMPUTE AUWAIV-AMOUNT-FIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2640-FLAG-CHARGEBK.
           IF AUWAIV-CHARGEBK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE AUWAIV-CHARGEBK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2650-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2660-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = AUWAIV-PAYMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = AUWAIV-PAYMENT-AMT - (WS-WORK-AMT *
               3)
           COMPUTE AUWAIV-CHARGEBK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GAP AMT.
      *----------------------------------------------------------------*
       2670-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AUWAIV-GAP-AMT ROUNDED =
                   AUWAIV-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO AUWAIV-GAP-AMT
           END-IF
           ADD AUWAIV-GAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2680-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO AUWAIV-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2690-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO AUWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2700-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AUWAIV-GAP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CHARGEBK SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2710-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CHARGEBK-SCHED-RATE(1) TO AUWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-CHARGEBK-SCHED-LOW(WS-TX)
                   MOVE WS-CHARGEBK-SCHED-RATE(WS-TX) TO AUWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2720-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF AUWAIV-LAST-PAY-DATE > 0 AND AUWAIV-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUWAIV-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD AUWAIV-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD AUWAIV-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD AUWAIV-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUWAIV-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE GAP AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2730-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN AUWAIV-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUWAIV-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AUWAIV-GAP-AMT *
                       AUWAIV-FEE-PCT / 200
               WHEN AUWAIV-STATUS-CD = 'EUA'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN AUWAIV-STATUS-CD = 'HMW'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AUWAIV-GAP-AMT *
                       AUWAIV-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT FIN FEE ON THE PAYMENT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2740-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN AUWAIV-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO AUWAIV-AMOUNT-FIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUWAIV-EXEMPT-FLAG = 'Y'
                   COMPUTE AUWAIV-AMOUNT-FIN-AMT ROUNDED =
                       AUWAIV-PAYMENT-AMT * AUWAIV-RATE / 200
               WHEN AUWAIV-TIER-CD = 'EW'
                   MOVE 10.00 TO AUWAIV-AMOUNT-FIN-AMT
               WHEN AUWAIV-TIER-CD = 'JF'
                   MOVE 3.00 TO AUWAIV-AMOUNT-FIN-AMT
               WHEN OTHER
                   COMPUTE AUWAIV-AMOUNT-FIN-AMT ROUNDED =
                       AUWAIV-PAYMENT-AMT * AUWAIV-RATE / 100
           END-EVALUATE
           IF AUWAIV-AMOUNT-FIN-AMT > 0 AND AUWAIV-AMOUNT-FIN-AMT < 5.00
               MOVE 5.00 TO AUWAIV-AMOUNT-FIN-AMT
           END-IF
           IF AUWAIV-AMOUNT-FIN-AMT > 35.00
               MOVE 35.00 TO AUWAIV-AMOUNT-FIN-AMT
           END-IF
           ADD AUWAIV-AMOUNT-FIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2750-LIMIT-CHARGEBK.
           IF AUWAIV-CHARGEBK-AMT < 50.00
               MOVE 50.00 TO AUWAIV-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AUWAIV-CHARGEBK-AMT > 50000.00
               MOVE 50000.00 TO AUWAIV-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GAP AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2760-SPLIT-GAP.
           COMPUTE WS-WORK-AMT = AUWAIV-GAP-AMT / 2
           COMPUTE WS-WORK-AMT-2 = AUWAIV-GAP-AMT - (WS-WORK-AMT * 2)
           COMPUTE AUWAIV-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT FIN AMT.
      *----------------------------------------------------------------*
       2770-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AUWAIV-AMOUNT-FIN-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO AUWAIV-AMOUNT-FIN-AMT
           END-IF
           ADD AUWAIV-AMOUNT-FIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN NEXT DUE DATE IS PAST
      * HMW -> RCB WHEN NEXT DUE DATE IS PAST
      * RCB -> EUA WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2780-SET-STATUS.
           EVALUATE AUWAIV-STATUS-CD
               WHEN 'EUA'
                   IF AUWAIV-NEXT-DUE-DATE > 0 AND AUWAIV-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HMW' TO AUWAIV-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF AUWAIV-NEXT-DUE-DATE > 0 AND AUWAIV-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RCB' TO AUWAIV-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF AUWAIV-NEXT-DUE-DATE > 0 AND AUWAIV-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EUA' TO AUWAIV-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO AUWAIV-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO AUWAIV-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO AUWAIV-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'AUWAIV EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
