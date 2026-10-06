      *================================================================*
      * PROGRAM  : CNDTE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : COIN AND CURRENCY RULE - DATE RULES. CALLED BY THE  *
      *            COIN AND CURRENCY BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-11-24 JLM  ORIGINAL                                       *
      * 1992-11-21 RJT  CURRENCY STATUS CODES ADDED REQ1189            *
      * 1993-04-15 BHS  SHIPMENT CALCULATION CORRECTED INC5474         *
      * 1994-04-24 PQR  FEE NOTICE TEXT PER LEGAL PRB4934              *
      * 1997-11-15 SKN  PERFORMANCE - COIN READ AHEAD REQ5674          *
      * 1999-04-22 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-03-10 LMB  PERFORMANCE - SHIPMENT READ AHEAD CHG6208      *
      * 2004-08-19 JLM  STRAP STATUS CODES ADDED INC8642               *
      * 2004-02-15 DPS  STRAP RERUN PROTECTION PRB1791                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNDTE.
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
                                           VALUE 15.000000.
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
      * WS-SHIPMENT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHIPMENT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000563500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000839500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000375000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000295500'.
       01  WS-SHIPMENT-SCHED-TBL REDEFINES WS-SHIPMENT-SCHED-VALUES.
           05  WS-SHIPMENT-SCHED-ROW OCCURS 4 TIMES.
               10  WS-SHIPMENT-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHIPMENT-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CNDTEP.

       PROCEDURE DIVISION USING CNDTE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-COUNT-ITEM
           PERFORM 2010-AGE-EFFECTIVE
           PERFORM 2020-ASSESS-AUTO-RENEW
           PERFORM 2030-SET-CARRIER
           PERFORM 2040-COUNT-ITEM
           PERFORM 2050-DESCRIBE-DENOM
           PERFORM 2060-LIMIT-FEE
           PERFORM 2070-DESCRIBE-DENOM
           PERFORM 2080-SELECT-FEE-PCT
           PERFORM 2090-SPLIT-COIN
           PERFORM 2100-COUNT-NOTICE
           PERFORM 2110-SPLIT-SHIPMENT
           PERFORM 2120-LIMIT-COIN
           PERFORM 2130-FLAG-COIN
           PERFORM 2140-SELECT-PENALTY-PCT
           PERFORM 2150-LIMIT-FEE
           PERFORM 2160-SPLIT-FEE
           PERFORM 2170-ACCUM-STATUS
           PERFORM 2180-SELECT-PENALTY-PCT
           PERFORM 2190-SELECT-PENALTY-PCT
           PERFORM 2200-PRORATE-COIN
           PERFORM 2210-CHECK-NEXT-REVIEW
           PERFORM 2220-SELECT-PENALTY-PCT
           PERFORM 2230-AGE-LAST-ACT
           PERFORM 2240-FLAG-SHIPMENT
           PERFORM 2250-DESCRIBE-DENOM
           PERFORM 2260-ACCRUE-CURRENCY
           PERFORM 2270-ACCRUE-PENALTY-PCT
           PERFORM 2280-FLAG-COIN
           PERFORM 2290-AGE-EFFECTIVE
           PERFORM 2300-CHECK-LAST-ACT
           PERFORM 2310-ASSESS-AUTO-RENEW
           PERFORM 2320-COUNT-NOTICE
           PERFORM 2330-COUNT-NOTICE
           PERFORM 2340-PRORATE-CURRENCY
           PERFORM 2350-SPLIT-SHIPMENT
           PERFORM 2360-CHECK-EFFECTIVE
           PERFORM 2370-DESCRIBE-CHANNEL
           PERFORM 2380-FLAG-SHIPMENT
           PERFORM 2390-FLAG-ITEM
           PERFORM 2400-COUNT-ITEM
           PERFORM 2410-DESCRIBE-CHANNEL
           PERFORM 2420-ACCUM-DENOM
           PERFORM 2430-ASSESS-HOLD
           PERFORM 2440-CHECK-MATURITY
           PERFORM 2450-COUNT-NOTICE
           PERFORM 2460-AGE-LAST-ACT
           PERFORM 2470-SELECT-FEE-PCT
           PERFORM 2480-ACCRUE-SHIPMENT
           PERFORM 2490-SET-CARRIER
           PERFORM 2500-ACCUM-CARRIER
           PERFORM 2510-DESCRIBE-CHANNEL
           PERFORM 2520-FLAG-FEE
           PERFORM 2530-SPLIT-CURRENCY
           PERFORM 2540-ACCUM-STATUS
           PERFORM 2550-FLAG-SHIPMENT
           PERFORM 2560-COUNT-ITEM
           PERFORM 2570-SET-STATUS
           PERFORM 2580-ACCRUE-FEE
           PERFORM 2590-PRORATE-CURRENCY
           PERFORM 2600-EDIT-DESC
           PERFORM 2610-DESCRIBE-CARRIER
           PERFORM 2620-LIMIT-ITEM
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CNDTE-RETURN-CODE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2000-COUNT-ITEM.
           IF CNDTE-ITEM-CNT < 9999999
               ADD 1 TO CNDTE-ITEM-CNT
           END-IF
           IF CNDTE-ITEM-CNT NOT < 5
               MOVE 'Y' TO CNDTE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CNDTE-EFFECTIVE-DATE > 0 AND CNDTE-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNDTE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNDTE-CURRENCY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CNDTE-CURRENCY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CNDTE-CURRENCY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CNDTE-CURRENCY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNDTE-CURRENCY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FEE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2020-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CNDTE-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNDTE-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CNDTE-FEE-AMT *
                       CNDTE-FEE-PCT / 200
               WHEN CNDTE-CARRIER-CD = 'J'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN CNDTE-CARRIER-CD = 'L'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CNDTE-FEE-AMT *
                       CNDTE-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN MATURITY DATE IS PAST
      * L -> M WHEN MATURITY DATE IS PAST
      * M -> X WHEN MATURITY DATE IS PAST
      * X -> J WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2030-SET-CARRIER.
           EVALUATE CNDTE-CARRIER-CD
               WHEN 'J'
                   IF CNDTE-MATURITY-DATE > 0 AND CNDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CNDTE-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CNDTE-MATURITY-DATE > 0 AND CNDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CNDTE-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CNDTE-MATURITY-DATE > 0 AND CNDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CNDTE-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CNDTE-MATURITY-DATE > 0 AND CNDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CNDTE-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-ITEM.
           IF CNDTE-ITEM-CNT < 9999999
               ADD 1 TO CNDTE-ITEM-CNT
           END-IF
           IF CNDTE-ITEM-CNT NOT < 6
               MOVE 'Y' TO CNDTE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2050-DESCRIBE-DENOM.
           EVALUATE CNDTE-DENOM-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'D'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2060-LIMIT-FEE.
           IF CNDTE-FEE-AMT < 0.00
               MOVE 0.00 TO CNDTE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNDTE-FEE-AMT > 50000.00
               MOVE 50000.00 TO CNDTE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2070-DESCRIBE-DENOM.
           EVALUATE CNDTE-DENOM-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'P'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHIPMENT SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2080-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPMENT-SCHED-RATE(1) TO CNDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-SHIPMENT-SCHED-LOW(WS-TX)
                   MOVE WS-SHIPMENT-SCHED-RATE(WS-TX) TO CNDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COIN YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2090-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CNDTE-COIN-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CNDTE-COIN-YTD - (WS-WORK-AMT * 3)
           COMPUTE CNDTE-CURRENCY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2100-COUNT-NOTICE.
           IF CNDTE-NOTICE-CNT < 9999999
               ADD 1 TO CNDTE-NOTICE-CNT
           END-IF
           IF CNDTE-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CNDTE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2110-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CNDTE-SHIPMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CNDTE-SHIPMENT-AMT - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2120-LIMIT-COIN.
           IF CNDTE-COIN-AMT < 100.00
               MOVE 100.00 TO CNDTE-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNDTE-COIN-AMT > 500.00
               MOVE 500.00 TO CNDTE-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2130-FLAG-COIN.
           IF CNDTE-COIN-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CNDTE-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPMENT SCHED TIER FOR THE COIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2140-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPMENT-SCHED-RATE(1) TO CNDTE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CNDTE-COIN-YTD NOT < WS-SHIPMENT-SCHED-LOW(WS-TX)
                   MOVE WS-SHIPMENT-SCHED-RATE(WS-TX) TO
                       CNDTE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNDTE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNDTE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2150-LIMIT-FEE.
           IF CNDTE-FEE-AMT < 25.00
               MOVE 25.00 TO CNDTE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNDTE-FEE-AMT > 2500.00
               MOVE 2500.00 TO CNDTE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = CNDTE-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CNDTE-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE CNDTE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CURRENCY YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2170-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNDTE-STATUS-CD
                   ADD CNDTE-CURRENCY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNDTE-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNDTE-CURRENCY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPMENT SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2180-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPMENT-SCHED-RATE(1) TO CNDTE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-SHIPMENT-SCHED-LOW(WS-TX)
                   MOVE WS-SHIPMENT-SCHED-RATE(WS-TX) TO
                       CNDTE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNDTE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNDTE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPMENT SCHED TIER FOR THE CURRENCY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2190-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPMENT-SCHED-RATE(1) TO CNDTE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CNDTE-CURRENCY-YTD NOT < WS-SHIPMENT-SCHED-LOW(WS-TX)
                   MOVE WS-SHIPMENT-SCHED-RATE(WS-TX) TO
                       CNDTE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNDTE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNDTE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COIN YTD.
      *----------------------------------------------------------------*
       2200-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNDTE-COIN-YTD ROUNDED =
                   CNDTE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CNDTE-COIN-YTD
           END-IF
           ADD CNDTE-COIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2210-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNDTE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNDTE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CNDTE-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPMENT SCHED TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPMENT-SCHED-RATE(1) TO CNDTE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CNDTE-FEE-YTD NOT < WS-SHIPMENT-SCHED-LOW(WS-TX)
                   MOVE WS-SHIPMENT-SCHED-RATE(WS-TX) TO
                       CNDTE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNDTE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNDTE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2230-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CNDTE-LAST-ACT-DATE > 0 AND CNDTE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNDTE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNDTE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CNDTE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CNDTE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CNDTE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNDTE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2240-FLAG-SHIPMENT.
           IF CNDTE-SHIPMENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CNDTE-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2250-DESCRIBE-DENOM.
           EVALUATE CNDTE-DENOM-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'S'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CURRENCY YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2260-ACCRUE-CURRENCY.
           IF CNDTE-CURRENCY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNDTE-CURRENCY-YTD * CNDTE-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CNDTE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CNDTE-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CNDTE-COIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2280-FLAG-COIN.
           IF CNDTE-COIN-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CNDTE-COIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CNDTE-EFFECTIVE-DATE > 0 AND CNDTE-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNDTE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNDTE-CURRENCY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CNDTE-CURRENCY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CNDTE-CURRENCY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CNDTE-CURRENCY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNDTE-CURRENCY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CNDTE-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNDTE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CNDTE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE CURRENCY YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2310-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CNDTE-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CNDTE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNDTE-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CNDTE-FEE-YTD ROUNDED = CNDTE-CURRENCY-YTD *
                       CNDTE-PENALTY-PCT / 200
               WHEN CNDTE-STATUS-CD = 'ADH'
                   MOVE 12.00 TO CNDTE-FEE-YTD
               WHEN CNDTE-STATUS-CD = 'FVV'
                   MOVE 5.00 TO CNDTE-FEE-YTD
               WHEN OTHER
                   COMPUTE CNDTE-FEE-YTD ROUNDED = CNDTE-CURRENCY-YTD *
                       CNDTE-PENALTY-PCT / 100
           END-EVALUATE
           IF CNDTE-FEE-YTD > 0 AND CNDTE-FEE-YTD < 1.00
               MOVE 1.00 TO CNDTE-FEE-YTD
           END-IF
           IF CNDTE-FEE-YTD > 25.00
               MOVE 25.00 TO CNDTE-FEE-YTD
           END-IF
           ADD CNDTE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2320-COUNT-NOTICE.
           IF CNDTE-NOTICE-CNT < 9999999
               ADD 1 TO CNDTE-NOTICE-CNT
           END-IF
           IF CNDTE-NOTICE-CNT NOT < 3
               MOVE 'Y' TO CNDTE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2330-COUNT-NOTICE.
           IF CNDTE-NOTICE-CNT < 9999999
               ADD 1 TO CNDTE-NOTICE-CNT
           END-IF
           IF CNDTE-NOTICE-CNT NOT < 5
               MOVE 'Y' TO CNDTE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CURRENCY YTD.
      *----------------------------------------------------------------*
       2340-PRORATE-CURRENCY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNDTE-CURRENCY-YTD ROUNDED =
                   CNDTE-COIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CNDTE-CURRENCY-YTD
           END-IF
           ADD CNDTE-CURRENCY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2350-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CNDTE-SHIPMENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CNDTE-SHIPMENT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE CNDTE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2360-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CNDTE-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNDTE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CNDTE-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-CHANNEL.
           EVALUATE CNDTE-CHANNEL-CD
               WHEN 'AW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2380-FLAG-SHIPMENT.
           IF CNDTE-SHIPMENT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CNDTE-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2390-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2400-COUNT-ITEM.
           IF CNDTE-ITEM-CNT < 9999999
               ADD 1 TO CNDTE-ITEM-CNT
           END-IF
           IF CNDTE-ITEM-CNT NOT < 6
               MOVE 'Y' TO CNDTE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2410-DESCRIBE-CHANNEL.
           EVALUATE CNDTE-CHANNEL-CD
               WHEN 'AW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2420-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNDTE-DENOM-CD
                   ADD CNDTE-COIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNDTE-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNDTE-COIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE CURRENCY YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2430-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CNDTE-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CNDTE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNDTE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CNDTE-FEE-YTD ROUNDED = CNDTE-CURRENCY-YTD *
                       CNDTE-FEE-PCT / 200
               WHEN CNDTE-DENOM-CD = 'B'
                   MOVE 3.00 TO CNDTE-FEE-YTD
               WHEN CNDTE-DENOM-CD = 'D'
                   MOVE 5.00 TO CNDTE-FEE-YTD
               WHEN OTHER
                   COMPUTE CNDTE-FEE-YTD ROUNDED = CNDTE-CURRENCY-YTD *
                       CNDTE-FEE-PCT / 100
           END-EVALUATE
           IF CNDTE-FEE-YTD > 0 AND CNDTE-FEE-YTD < 2.00
               MOVE 2.00 TO CNDTE-FEE-YTD
           END-IF
           IF CNDTE-FEE-YTD > 35.00
               MOVE 35.00 TO CNDTE-FEE-YTD
           END-IF
           ADD CNDTE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CNDTE-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNDTE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CNDTE-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2450-COUNT-NOTICE.
           IF CNDTE-NOTICE-CNT < 9999999
               ADD 1 TO CNDTE-NOTICE-CNT
           END-IF
           IF CNDTE-NOTICE-CNT NOT < 3
               MOVE 'Y' TO CNDTE-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2460-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CNDTE-LAST-ACT-DATE > 0 AND CNDTE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNDTE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNDTE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CNDTE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CNDTE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CNDTE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNDTE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHIPMENT SCHED TIER FOR THE SHIPMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPMENT-SCHED-RATE(1) TO CNDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CNDTE-SHIPMENT-AMT NOT < WS-SHIPMENT-SCHED-LOW(WS-TX)
                   MOVE WS-SHIPMENT-SCHED-RATE(WS-TX) TO CNDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2480-ACCRUE-SHIPMENT.
           IF CNDTE-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNDTE-SHIPMENT-AMT * CNDTE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CNDTE-CURRENCY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN LAST ACT DATE IS PAST
      * L -> M WHEN LAST ACT DATE IS PAST
      * M -> X WHEN LAST ACT DATE IS PAST
      * X -> J WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-CARRIER.
           EVALUATE CNDTE-CARRIER-CD
               WHEN 'J'
                   IF CNDTE-LAST-ACT-DATE > 0 AND CNDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CNDTE-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CNDTE-LAST-ACT-DATE > 0 AND CNDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CNDTE-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CNDTE-LAST-ACT-DATE > 0 AND CNDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CNDTE-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CNDTE-LAST-ACT-DATE > 0 AND CNDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CNDTE-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNDTE-CARRIER-CD
                   ADD CNDTE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNDTE-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNDTE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2510-DESCRIBE-CHANNEL.
           EVALUATE CNDTE-CHANNEL-CD
               WHEN 'AW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2520-FLAG-FEE.
           IF CNDTE-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CNDTE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CURRENCY YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = CNDTE-CURRENCY-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CNDTE-CURRENCY-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CNDTE-COIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNDTE-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNDTE-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2550-FLAG-SHIPMENT.
           IF CNDTE-SHIPMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CNDTE-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2560-COUNT-ITEM.
           IF CNDTE-ITEM-CNT < 9999999
               ADD 1 TO CNDTE-ITEM-CNT
           END-IF
           IF CNDTE-ITEM-CNT NOT < 12
               MOVE 'Y' TO CNDTE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * ADH -> FVV WHEN MATURITY DATE IS PAST
      * FVV -> GCB WHEN MATURITY DATE IS PAST
      * GCB -> UZP WHEN MATURITY DATE IS PAST
      * UZP -> VXB WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2570-SET-STATUS.
           EVALUATE CNDTE-STATUS-CD
               WHEN 'ADH'
                   IF CNDTE-MATURITY-DATE > 0 AND CNDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FVV' TO CNDTE-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FVV'
                   IF CNDTE-MATURITY-DATE > 0 AND CNDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GCB' TO CNDTE-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GCB'
                   IF CNDTE-MATURITY-DATE > 0 AND CNDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZP' TO CNDTE-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UZP'
                   IF CNDTE-MATURITY-DATE > 0 AND CNDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VXB' TO CNDTE-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-FEE.
           IF CNDTE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNDTE-FEE-YTD * CNDTE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CURRENCY YTD.
      *----------------------------------------------------------------*
       2590-PRORATE-CURRENCY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNDTE-CURRENCY-YTD ROUNDED =
                   CNDTE-COIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CNDTE-CURRENCY-YTD
           END-IF
           ADD CNDTE-CURRENCY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E765 DESC REQUIRED
      * E522 SHIPMENT AMT NEGATIVE
      * E776 SHIPMENT AMT OVER 50000
      * E034 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2600-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNDTE-DESC = SPACES
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN CNDTE-SHIPMENT-AMT < 0
                   MOVE 'E522' TO WS-REJECT-CODE
               WHEN CNDTE-SHIPMENT-AMT > 50000.00
                   MOVE 'E776' TO WS-REJECT-CODE
               WHEN CNDTE-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E034' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2610-DESCRIBE-CARRIER.
           EVALUATE CNDTE-CARRIER-CD
               WHEN 'J'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2620-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CNDTE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CNDTE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CNDTE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CNDTE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
