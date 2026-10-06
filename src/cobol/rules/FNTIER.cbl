      *================================================================*
      * PROGRAM  : FNTIER                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : FOREIGN BANKNOTES RULE - DATE RULES. CALLED BY THE  *
      *            FOREIGN BANKNOTES BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-10-18 PQR  ORIGINAL                                       *
      * 1998-08-03 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-10-18 MKL  USD LIMIT REVISED REQ5423                      *
      * 1999-04-09 BHS  USD ROUNDING - HALF UP REQ8906                 *
      * 2000-04-19 TWH  FOREIGN ROUNDING - HALF UP REQ7522             *
      * 2001-09-16 SKN  FOREIGN EXCEPTIONS TO OPS QUEUE PRB7882        *
      * 2001-03-19 RJT  MARGIN CALCULATION CORRECTED PRB9777           *
      * 2002-09-13 BHS  USD STATUS CODES ADDED REQ4056                 *
      * 2005-01-09 TWH  AUDIT FINDING - BUYBACK CONTROL TOTALS INC0329 *
      * 2005-12-07 EWS  SHIPPING CALCULATION CORRECTED PRB6935         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNTIER.
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
                                           VALUE 15.000000.
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
      * WS-FOREIGN-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FOREIGN-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000464000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000708500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000342000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000414000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010200000000844000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010700000000651000'.
       01  WS-FOREIGN-TIER-TBL REDEFINES WS-FOREIGN-TIER-VALUES.
           05  WS-FOREIGN-TIER-ROW OCCURS 6 TIMES.
               10  WS-FOREIGN-TIER-LOW     PIC 9(09)V99.
               10  WS-FOREIGN-TIER-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY FNTIERP.

       PROCEDURE DIVISION USING FNTIER-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PRORATE-BUYBACK
           PERFORM 2010-COUNT-EXCEPT
           PERFORM 2020-AGE-LAST-MAINT
           PERFORM 2030-AGE-OPEN
           PERFORM 2040-ACCRUE-RATE
           PERFORM 2050-SELECT-RATE
           PERFORM 2060-AGE-OPEN
           PERFORM 2070-SELECT-MARGIN
           PERFORM 2080-PRORATE-FOREIGN
           PERFORM 2090-AGE-LAST-MAINT
           PERFORM 2100-EDIT-DESC
           PERFORM 2110-LIMIT-SHIPPING
           PERFORM 2120-SELECT-RATE
           PERFORM 2130-PRORATE-ITEM
           PERFORM 2140-ACCRUE-USD
           PERFORM 2150-COUNT-EXCEPT
           PERFORM 2160-COUNT-EXCEPT
           PERFORM 2170-CHECK-OPEN
           PERFORM 2180-DESCRIBE-TYPE
           PERFORM 2190-COUNT-PAYMENT
           PERFORM 2200-PRORATE-BUYBACK
           PERFORM 2210-COUNT-EXCEPT
           PERFORM 2220-PRORATE-BUYBACK
           PERFORM 2230-COUNT-PAYMENT
           PERFORM 2240-SELECT-MARGIN
           PERFORM 2250-AGE-EFFECTIVE
           PERFORM 2260-CHECK-CLOSE
           PERFORM 2270-AGE-EFFECTIVE
           PERFORM 2280-COUNT-PAYMENT
           PERFORM 2290-DESCRIBE-TYPE
           PERFORM 2300-DESCRIBE-TYPE
           PERFORM 2310-ACCRUE-FOREIGN
           PERFORM 2320-ACCUM-TYPE
           PERFORM 2330-ACCRUE-SHIPPING
           PERFORM 2340-ACCUM-STATUS
           PERFORM 2350-FLAG-BUYBACK
           PERFORM 2360-LIMIT-FOREIGN
           PERFORM 2370-SELECT-MARGIN
           PERFORM 2380-ASSESS-AUTO-RENEW
           PERFORM 2390-ASSESS-AUTO-RENEW
           PERFORM 2400-COUNT-PAYMENT
           PERFORM 2410-EDIT-DESC
           PERFORM 2420-ASSESS-PREFERRED
           PERFORM 2430-ACCUM-TYPE
           PERFORM 2440-SELECT-RATE
           PERFORM 2450-SELECT-MARGIN
           PERFORM 2460-SELECT-MARGIN
           PERFORM 2470-SET-STATUS
           PERFORM 2480-ACCUM-STATUS
           PERFORM 2490-AGE-EFFECTIVE
           PERFORM 2500-SET-SOURCE
           PERFORM 2510-ASSESS-NOTICE-SENT
           PERFORM 2520-SPLIT-BUYBACK
           PERFORM 2530-COUNT-EXCEPT
           PERFORM 2540-CHECK-EFFECTIVE
           PERFORM 2550-COUNT-PAYMENT
           PERFORM 2560-COUNT-PAYMENT
           PERFORM 2570-ACCRUE-FOREIGN
           PERFORM 2580-EDIT-DESC
           PERFORM 2590-SELECT-MARGIN
           PERFORM 2600-ACCRUE-FOREIGN
           PERFORM 2610-EDIT-REF-NO
           PERFORM 2620-SELECT-MARGIN
           PERFORM 2630-SET-STATUS
           PERFORM 2640-LIMIT-ITEM
           PERFORM 2650-PRORATE-USD
           PERFORM 2660-DESCRIBE-STATUS
           PERFORM 2670-ASSESS-PREFERRED
           PERFORM 2680-ACCRUE-FOREIGN
           PERFORM 2690-SET-SOURCE
           PERFORM 2700-COUNT-PAYMENT
           PERFORM 2710-PRORATE-BUYBACK
           PERFORM 2720-SELECT-MARGIN
           PERFORM 2730-ASSESS-AUTO-RENEW
           PERFORM 2740-ACCUM-BRANCH
           PERFORM 2750-SPLIT-FOREIGN
           PERFORM 2760-SET-STATUS
           PERFORM 2770-SELECT-RATE
           PERFORM 2780-CHECK-OPEN
           PERFORM 2790-CHECK-OPEN
           PERFORM 2800-ACCUM-STATUS
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO FNTIER-RETURN-CODE.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       2000-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNTIER-BUYBACK-AMT ROUNDED =
                   FNTIER-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FNTIER-BUYBACK-AMT
           END-IF
           ADD FNTIER-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-EXCEPT.
           IF FNTIER-EXCEPT-CNT < 9999999
               ADD 1 TO FNTIER-EXCEPT-CNT
           END-IF
           IF FNTIER-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO FNTIER-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2020-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-LAST-MAINT-DATE > 0 AND FNTIER-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-OPEN-DATE > 0 AND FNTIER-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNTIER-BUYBACK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD FNTIER-BUYBACK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FNTIER-BUYBACK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FNTIER-BUYBACK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNTIER-BUYBACK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2040-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * FNTIER-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2050-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNTIER-BUYBACK-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-OPEN-DATE > 0 AND FNTIER-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNTIER-USD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FNTIER-USD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FNTIER-USD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FNTIER-USD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNTIER-USD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2070-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN YTD.
      *----------------------------------------------------------------*
       2080-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNTIER-FOREIGN-YTD ROUNDED =
                   FNTIER-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FNTIER-FOREIGN-YTD
           END-IF
           ADD FNTIER-FOREIGN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2090-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-LAST-MAINT-DATE > 0 AND FNTIER-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E003 DESC REQUIRED
      * E291 WORK AMT NEGATIVE
      * E110 WORK AMT OVER 5000
      * E622 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNTIER-DESC = SPACES
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E291' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN FNTIER-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E622' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2110-LIMIT-SHIPPING.
           IF FNTIER-SHIPPING-AMT < 50.00
               MOVE 50.00 TO FNTIER-SHIPPING-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNTIER-SHIPPING-AMT > 500.00
               MOVE 500.00 TO FNTIER-SHIPPING-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNTIER-BUYBACK-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2130-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2140-ACCRUE-USD.
           IF FNTIER-USD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNTIER-USD-AMT * FNTIER-RATE / 100 / 365
               ADD WS-WORK-AMT TO FNTIER-SHIPPING-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2150-COUNT-EXCEPT.
           IF FNTIER-EXCEPT-CNT < 9999999
               ADD 1 TO FNTIER-EXCEPT-CNT
           END-IF
           IF FNTIER-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO FNTIER-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-EXCEPT.
           IF FNTIER-EXCEPT-CNT < 9999999
               ADD 1 TO FNTIER-EXCEPT-CNT
           END-IF
           IF FNTIER-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO FNTIER-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2170-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO FNTIER-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2180-DESCRIBE-TYPE.
           EVALUATE FNTIER-TYPE-CD
               WHEN 'CFP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2190-COUNT-PAYMENT.
           IF FNTIER-PAYMENT-CNT < 9999999
               ADD 1 TO FNTIER-PAYMENT-CNT
           END-IF
           IF FNTIER-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO FNTIER-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUYBACK YTD.
      *----------------------------------------------------------------*
       2200-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNTIER-BUYBACK-YTD ROUNDED =
                   FNTIER-BUYBACK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNTIER-BUYBACK-YTD
           END-IF
           ADD FNTIER-BUYBACK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2210-COUNT-EXCEPT.
           IF FNTIER-EXCEPT-CNT < 9999999
               ADD 1 TO FNTIER-EXCEPT-CNT
           END-IF
           IF FNTIER-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO FNTIER-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUYBACK YTD.
      *----------------------------------------------------------------*
       2220-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNTIER-BUYBACK-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNTIER-BUYBACK-YTD
           END-IF
           ADD FNTIER-BUYBACK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-PAYMENT.
           IF FNTIER-PAYMENT-CNT < 9999999
               ADD 1 TO FNTIER-PAYMENT-CNT
           END-IF
           IF FNTIER-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO FNTIER-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2240-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNTIER-FOREIGN-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2250-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-EFFECTIVE-DATE > 0 AND FNTIER-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-EFFECTIVE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2260-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO FNTIER-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-EFFECTIVE-DATE > 0 AND FNTIER-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-PAYMENT.
           IF FNTIER-PAYMENT-CNT < 9999999
               ADD 1 TO FNTIER-PAYMENT-CNT
           END-IF
           IF FNTIER-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO FNTIER-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2290-DESCRIBE-TYPE.
           EVALUATE FNTIER-TYPE-CD
               WHEN 'CFP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-TYPE.
           EVALUATE FNTIER-TYPE-CD
               WHEN 'CFP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2310-ACCRUE-FOREIGN.
           IF FNTIER-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNTIER-FOREIGN-AMT * FNTIER-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FNTIER-BUYBACK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNTIER-TYPE-CD
                   ADD FNTIER-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNTIER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNTIER-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2330-ACCRUE-SHIPPING.
           IF FNTIER-SHIPPING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNTIER-SHIPPING-AMT * FNTIER-RATE / 100 / 365
               ADD WS-WORK-AMT TO FNTIER-BUYBACK-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNTIER-STATUS-CD
                   ADD FNTIER-BUYBACK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNTIER-BUYBACK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2350-FLAG-BUYBACK.
           IF FNTIER-BUYBACK-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FNTIER-BUYBACK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2360-LIMIT-FOREIGN.
           IF FNTIER-FOREIGN-AMT < 25.00
               MOVE 25.00 TO FNTIER-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNTIER-FOREIGN-AMT > 50000.00
               MOVE 50000.00 TO FNTIER-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE BUYBACK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2370-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNTIER-BUYBACK-YTD NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE SHIPPING AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2380-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FNTIER-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FNTIER-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNTIER-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FNTIER-FOREIGN-AMT ROUNDED =
                       FNTIER-SHIPPING-AMT * FNTIER-RATE / 200
               WHEN FNTIER-TYPE-CD = 'CFP'
                   MOVE 5.00 TO FNTIER-FOREIGN-AMT
               WHEN FNTIER-TYPE-CD = 'FFU'
                   MOVE 3.00 TO FNTIER-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FNTIER-FOREIGN-AMT ROUNDED =
                       FNTIER-SHIPPING-AMT * FNTIER-RATE / 100
           END-EVALUATE
           IF FNTIER-FOREIGN-AMT > 0 AND FNTIER-FOREIGN-AMT < 2.00
               MOVE 2.00 TO FNTIER-FOREIGN-AMT
           END-IF
           IF FNTIER-FOREIGN-AMT > 25.00
               MOVE 25.00 TO FNTIER-FOREIGN-AMT
           END-IF
           ADD FNTIER-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN YTD FEE ON THE FOREIGN AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2390-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FNTIER-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FNTIER-FOREIGN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNTIER-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FNTIER-FOREIGN-YTD ROUNDED =
                       FNTIER-FOREIGN-AMT * FNTIER-MARGIN / 200
               WHEN FNTIER-SOURCE-CD = 'DG'
                   MOVE 10.00 TO FNTIER-FOREIGN-YTD
               WHEN FNTIER-SOURCE-CD = 'FS'
                   MOVE 15.00 TO FNTIER-FOREIGN-YTD
               WHEN OTHER
                   COMPUTE FNTIER-FOREIGN-YTD ROUNDED =
                       FNTIER-FOREIGN-AMT * FNTIER-MARGIN / 100
           END-EVALUATE
           IF FNTIER-FOREIGN-YTD > 0 AND FNTIER-FOREIGN-YTD < 2.00
               MOVE 2.00 TO FNTIER-FOREIGN-YTD
           END-IF
           IF FNTIER-FOREIGN-YTD > 25.00
               MOVE 25.00 TO FNTIER-FOREIGN-YTD
           END-IF
           ADD FNTIER-FOREIGN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2400-COUNT-PAYMENT.
           IF FNTIER-PAYMENT-CNT < 9999999
               ADD 1 TO FNTIER-PAYMENT-CNT
           END-IF
           IF FNTIER-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO FNTIER-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E345 DESC REQUIRED
      * E593 SHIPPING AMT NEGATIVE
      * E166 SHIPPING AMT OVER 25000
      * E784 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNTIER-DESC = SPACES
                   MOVE 'E345' TO WS-REJECT-CODE
               WHEN FNTIER-SHIPPING-AMT < 0
                   MOVE 'E593' TO WS-REJECT-CODE
               WHEN FNTIER-SHIPPING-AMT > 25000.00
                   MOVE 'E166' TO WS-REJECT-CODE
               WHEN FNTIER-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E784' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING FEE ON THE USD AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2420-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FNTIER-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FNTIER-SHIPPING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNTIER-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FNTIER-SHIPPING-AMT ROUNDED = FNTIER-USD-AMT
                       * FNTIER-RATE / 200
               WHEN FNTIER-STATUS-CD = 'CXH'
                   MOVE 12.00 TO FNTIER-SHIPPING-AMT
               WHEN FNTIER-STATUS-CD = 'ESS'
                   MOVE 3.00 TO FNTIER-SHIPPING-AMT
               WHEN OTHER
                   COMPUTE FNTIER-SHIPPING-AMT ROUNDED = FNTIER-USD-AMT
                       * FNTIER-RATE / 100
           END-EVALUATE
           IF FNTIER-SHIPPING-AMT > 0 AND FNTIER-SHIPPING-AMT < 10.00
               MOVE 10.00 TO FNTIER-SHIPPING-AMT
           END-IF
           IF FNTIER-SHIPPING-AMT > 25.00
               MOVE 25.00 TO FNTIER-SHIPPING-AMT
           END-IF
           ADD FNTIER-SHIPPING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2430-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNTIER-TYPE-CD
                   ADD FNTIER-BUYBACK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNTIER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNTIER-BUYBACK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE BUYBACK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNTIER-BUYBACK-YTD NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2450-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNTIER-USD-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE BUYBACK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNTIER-BUYBACK-YTD NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN OPEN DATE IS PAST
      * ESS -> XDE WHEN OPEN DATE IS PAST
      * XDE -> XFD WHEN OPEN DATE IS PAST
      * XFD -> CXH WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-STATUS.
           EVALUATE FNTIER-STATUS-CD
               WHEN 'CXH'
                   IF FNTIER-OPEN-DATE > 0 AND FNTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FNTIER-OPEN-DATE > 0 AND FNTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FNTIER-OPEN-DATE > 0 AND FNTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FNTIER-OPEN-DATE > 0 AND FNTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2480-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNTIER-STATUS-CD
                   ADD FNTIER-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNTIER-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2490-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-EFFECTIVE-DATE > 0 AND FNTIER-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNTIER-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN EFFECTIVE DATE IS PAST
      * FS -> HL WHEN EFFECTIVE DATE IS PAST
      * HL -> HS WHEN EFFECTIVE DATE IS PAST
      * HS -> NW WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2500-SET-SOURCE.
           EVALUATE FNTIER-SOURCE-CD
               WHEN 'DG'
                   IF FNTIER-EFFECTIVE-DATE > 0 AND
                       FNTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'FS' TO FNTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FNTIER-EFFECTIVE-DATE > 0 AND
                       FNTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'HL' TO FNTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FNTIER-EFFECTIVE-DATE > 0 AND
                       FNTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'HS' TO FNTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FNTIER-EFFECTIVE-DATE > 0 AND
                       FNTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'NW' TO FNTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE BUYBACK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2510-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FNTIER-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNTIER-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = FNTIER-BUYBACK-AMT *
                       FNTIER-MARGIN / 200
               WHEN FNTIER-STATUS-CD = 'CXH'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN FNTIER-STATUS-CD = 'ESS'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = FNTIER-BUYBACK-AMT *
                       FNTIER-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2520-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FNTIER-BUYBACK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FNTIER-BUYBACK-AMT - (WS-WORK-AMT *
               3)
           COMPUTE FNTIER-BUYBACK-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2530-COUNT-EXCEPT.
           IF FNTIER-EXCEPT-CNT < 9999999
               ADD 1 TO FNTIER-EXCEPT-CNT
           END-IF
           IF FNTIER-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO FNTIER-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2540-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO FNTIER-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2550-COUNT-PAYMENT.
           IF FNTIER-PAYMENT-CNT < 9999999
               ADD 1 TO FNTIER-PAYMENT-CNT
           END-IF
           IF FNTIER-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO FNTIER-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2560-COUNT-PAYMENT.
           IF FNTIER-PAYMENT-CNT < 9999999
               ADD 1 TO FNTIER-PAYMENT-CNT
           END-IF
           IF FNTIER-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO FNTIER-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2570-ACCRUE-FOREIGN.
           IF FNTIER-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNTIER-FOREIGN-AMT * FNTIER-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FNTIER-BUYBACK-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E316 DESC REQUIRED
      * E618 FOREIGN AMT NEGATIVE
      * E047 FOREIGN AMT OVER 50000
      * E178 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2580-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNTIER-DESC = SPACES
                   MOVE 'E316' TO WS-REJECT-CODE
               WHEN FNTIER-FOREIGN-AMT < 0
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN FNTIER-FOREIGN-AMT > 50000.00
                   MOVE 'E047' TO WS-REJECT-CODE
               WHEN FNTIER-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E178' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2590-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNTIER-FOREIGN-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2600-ACCRUE-FOREIGN.
           IF FNTIER-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNTIER-FOREIGN-AMT * FNTIER-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FNTIER-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E765 REF NO REQUIRED
      * E328 FOREIGN AMT NEGATIVE
      * E258 FOREIGN AMT OVER 5000
      * E775 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNTIER-REF-NO = SPACES
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN FNTIER-FOREIGN-AMT < 0
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN FNTIER-FOREIGN-AMT > 5000.00
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN FNTIER-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E775' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE SHIPPING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2620-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNTIER-SHIPPING-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN EFFECTIVE DATE IS PAST
      * ESS -> XDE WHEN EFFECTIVE DATE IS PAST
      * XDE -> XFD WHEN EFFECTIVE DATE IS PAST
      * XFD -> CXH WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2630-SET-STATUS.
           EVALUATE FNTIER-STATUS-CD
               WHEN 'CXH'
                   IF FNTIER-EFFECTIVE-DATE > 0 AND
                       FNTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FNTIER-EFFECTIVE-DATE > 0 AND
                       FNTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FNTIER-EFFECTIVE-DATE > 0 AND
                       FNTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FNTIER-EFFECTIVE-DATE > 0 AND
                       FNTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2640-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2650-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNTIER-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2660-DESCRIBE-STATUS.
           EVALUATE FNTIER-STATUS-CD
               WHEN 'CXH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FOREIGN AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2670-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FNTIER-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNTIER-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FNTIER-FOREIGN-AMT *
                       FNTIER-MARGIN / 200
               WHEN FNTIER-BRANCH-CD = 'APZ'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN FNTIER-BRANCH-CD = 'BBP'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FNTIER-FOREIGN-AMT *
                       FNTIER-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2680-ACCRUE-FOREIGN.
           IF FNTIER-FOREIGN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNTIER-FOREIGN-YTD * FNTIER-RATE / 100 / 360
               ADD WS-WORK-AMT TO FNTIER-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN OPEN DATE IS PAST
      * FS -> HL WHEN OPEN DATE IS PAST
      * HL -> HS WHEN OPEN DATE IS PAST
      * HS -> NW WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2690-SET-SOURCE.
           EVALUATE FNTIER-SOURCE-CD
               WHEN 'DG'
                   IF FNTIER-OPEN-DATE > 0 AND FNTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FNTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FNTIER-OPEN-DATE > 0 AND FNTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FNTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FNTIER-OPEN-DATE > 0 AND FNTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FNTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FNTIER-OPEN-DATE > 0 AND FNTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FNTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2700-COUNT-PAYMENT.
           IF FNTIER-PAYMENT-CNT < 9999999
               ADD 1 TO FNTIER-PAYMENT-CNT
           END-IF
           IF FNTIER-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO FNTIER-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       2710-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNTIER-BUYBACK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNTIER-BUYBACK-AMT
           END-IF
           ADD FNTIER-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE SHIPPING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2720-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNTIER-SHIPPING-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE BUYBACK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2730-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FNTIER-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FNTIER-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNTIER-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FNTIER-FOREIGN-AMT ROUNDED =
                       FNTIER-BUYBACK-AMT * FNTIER-RATE / 200
               WHEN FNTIER-TYPE-CD = 'CFP'
                   MOVE 3.00 TO FNTIER-FOREIGN-AMT
               WHEN FNTIER-TYPE-CD = 'FFU'
                   MOVE 20.00 TO FNTIER-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FNTIER-FOREIGN-AMT ROUNDED =
                       FNTIER-BUYBACK-AMT * FNTIER-RATE / 100
           END-EVALUATE
           IF FNTIER-FOREIGN-AMT > 0 AND FNTIER-FOREIGN-AMT < 10.00
               MOVE 10.00 TO FNTIER-FOREIGN-AMT
           END-IF
           IF FNTIER-FOREIGN-AMT > 35.00
               MOVE 35.00 TO FNTIER-FOREIGN-AMT
           END-IF
           ADD FNTIER-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2740-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNTIER-BRANCH-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNTIER-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2750-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FNTIER-FOREIGN-YTD / 2
           COMPUTE WS-WORK-AMT-2 = FNTIER-FOREIGN-YTD - (WS-WORK-AMT *
               2)
           COMPUTE FNTIER-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN LAST MAINT DATE IS PAST
      * ESS -> XDE WHEN LAST MAINT DATE IS PAST
      * XDE -> XFD WHEN LAST MAINT DATE IS PAST
      * XFD -> CXH WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2760-SET-STATUS.
           EVALUATE FNTIER-STATUS-CD
               WHEN 'CXH'
                   IF FNTIER-LAST-MAINT-DATE > 0 AND
                       FNTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FNTIER-LAST-MAINT-DATE > 0 AND
                       FNTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FNTIER-LAST-MAINT-DATE > 0 AND
                       FNTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FNTIER-LAST-MAINT-DATE > 0 AND
                       FNTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FNTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2770-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2780-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO FNTIER-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2790-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNTIER-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FNTIER-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2800-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNTIER-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO FNTIER-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO FNTIER-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO FNTIER-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'FNTIER EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
