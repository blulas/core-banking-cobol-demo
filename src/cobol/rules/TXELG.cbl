      *================================================================*
      * PROGRAM  : TXELG                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TELEX WIRE MESSAGING RULE - PRICING. CALLED BY THE  *
      *            TELEX WIRE MESSAGING BATCH AND ONLINE PROGRAMS.     *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-07-12 MKL  ORIGINAL                                       *
      * 1995-09-13 SKN  CORRESP NOTICE TEXT PER LEGAL PRB2202          *
      * 1996-04-16 CRP  PERFORMANCE - QUEUED READ AHEAD CHG2482        *
      * 1997-11-13 JLM  WIRE TABLE EXPANDED PRB2510                    *
      * 1999-09-24 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-05-04 SKN  NEW QUEUED REPORT COLUMNS CHG5951              *
      * 2004-11-11 EWS  AUDIT FINDING - COVER CONTROL TOTALS REQ6564   *
      * 2004-06-27 SKN  WIRE FEE SCHEDULE UPDATE PRB2769               *
      * 2006-12-22 MKL  SETTLE RERUN PROTECTION CHG3246                *
      * 2008-11-02 MKL  QUEUED STATUS CODES ADDED PRB3184              *
      * 2010-06-28 DPS  SETTLE CUTOFF MOVED CHG2015                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXELG.
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
                                           VALUE 21.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 91.
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
      * WS-CORRESP-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CORRESP-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000226000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000668500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000532500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000289500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000152000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027550000000331500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028050000000142000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028550000000253500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038550000000200000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039050000000778000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00044050000000099000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00049050000000561000'.
       01  WS-CORRESP-TIER-TBL REDEFINES WS-CORRESP-TIER-VALUES.
           05  WS-CORRESP-TIER-ROW OCCURS 12 TIMES.
               10  WS-CORRESP-TIER-LOW     PIC 9(09)V99.
               10  WS-CORRESP-TIER-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TXELGP.

       PROCEDURE DIVISION USING TXELG-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CHECK-LAST-PAY
           PERFORM 2010-AGE-EFFECTIVE
           PERFORM 2020-ACCRUE-CORRESP
           PERFORM 2030-ASSESS-SUSPEND
           PERFORM 2040-AGE-NEXT-DUE
           PERFORM 2050-CHECK-EFFECTIVE
           PERFORM 2060-SELECT-RATE
           PERFORM 2070-AGE-EFFECTIVE
           PERFORM 2080-ACCRUE-RATE
           PERFORM 2090-SELECT-FEE-PCT
           PERFORM 2100-COUNT-PAYMENT
           PERFORM 2110-ACCRUE-CORRESP
           PERFORM 2120-SET-MSG-TYPE
           PERFORM 2130-FLAG-QUEUED
           PERFORM 2140-CHECK-EFFECTIVE
           PERFORM 2150-AGE-CLOSE
           PERFORM 2160-ACCUM-REASON
           PERFORM 2170-SET-REASON
           PERFORM 2180-ACCRUE-RATE
           PERFORM 2190-SET-TYPE
           PERFORM 2200-SELECT-RATE
           PERFORM 2210-ACCRUE-WIRE
           PERFORM 2220-COUNT-NOTICE
           PERFORM 2230-ASSESS-SUSPEND
           PERFORM 2240-CHECK-EFFECTIVE
           PERFORM 2250-SET-MSG-TYPE
           PERFORM 2260-PRORATE-CORRESP
           PERFORM 2270-SET-TYPE
           PERFORM 2280-SPLIT-WIRE
           PERFORM 2290-AGE-LAST-PAY
           PERFORM 2300-LIMIT-CORRESP
           PERFORM 2310-AGE-CLOSE
           PERFORM 2320-FLAG-CORRESP
           PERFORM 2330-DESCRIBE-TYPE
           PERFORM 2340-DESCRIBE-REGION
           PERFORM 2350-ASSESS-STOP
           PERFORM 2360-DESCRIBE-REASON
           PERFORM 2370-SET-TYPE
           PERFORM 2380-ACCRUE-RATE
           PERFORM 2390-AGE-EFFECTIVE
           PERFORM 2400-PRORATE-CHARGE
           PERFORM 2410-ACCRUE-CORRESP
           PERFORM 2420-SPLIT-QUEUED
           PERFORM 2430-SELECT-FEE-PCT
           PERFORM 2440-SELECT-RATE
           PERFORM 2450-CHECK-NEXT-DUE
           PERFORM 2460-FLAG-CORRESP
           PERFORM 2470-ACCUM-REGION
           PERFORM 2480-DESCRIBE-REGION
           PERFORM 2490-DESCRIBE-MSG-TYPE
           PERFORM 2500-EDIT-ADDR
           PERFORM 2510-FLAG-QUEUED
           PERFORM 2520-SELECT-FEE-PCT
           PERFORM 2530-EDIT-ADDR
           PERFORM 2540-CHECK-LAST-PAY
           PERFORM 2550-ASSESS-OVERRIDE
           PERFORM 2560-ACCUM-REGION
           PERFORM 2570-LIMIT-QUEUED
           PERFORM 2580-SELECT-FEE-PCT
           PERFORM 2590-LIMIT-ITEM
           PERFORM 2600-CHECK-LAST-PAY
           PERFORM 2610-CHECK-CLOSE
           PERFORM 2620-LIMIT-QUEUED
           PERFORM 2630-CHECK-LAST-PAY
           PERFORM 2640-ACCUM-MSG-TYPE
           PERFORM 2650-ACCUM-TYPE
           PERFORM 2660-SELECT-FEE-PCT
           PERFORM 2670-FLAG-ITEM
           PERFORM 2680-ACCRUE-CORRESP
           PERFORM 2690-ACCUM-REASON
           PERFORM 2700-FLAG-CORRESP
           PERFORM 2710-COUNT-NOTICE
           PERFORM 2720-EDIT-DESC
           PERFORM 2730-ASSESS-STOP
           PERFORM 2740-FLAG-ITEM
           PERFORM 2750-FLAG-CHARGE
           PERFORM 2760-DESCRIBE-REGION
           PERFORM 2770-AGE-NEXT-DUE
           PERFORM 2780-SELECT-FEE-PCT
           PERFORM 2790-ASSESS-SUSPEND
           PERFORM 2800-FLAG-ITEM
           PERFORM 2810-ASSESS-SUSPEND
           PERFORM 2820-ACCUM-REGION
           PERFORM 2830-AGE-NEXT-DUE
           PERFORM 2840-LIMIT-CORRESP
           PERFORM 2850-ASSESS-OVERRIDE
           PERFORM 2860-EDIT-ADDR
           PERFORM 2870-EDIT-ADDR
           PERFORM 2880-AGE-LAST-PAY
           PERFORM 2890-LIMIT-CHARGE
           PERFORM 2900-SPLIT-CORRESP
           PERFORM 2910-PRORATE-QUEUED
           PERFORM 2920-PRORATE-WIRE
           PERFORM 2930-LIMIT-WIRE
           PERFORM 2940-SPLIT-CORRESP
           PERFORM 2950-LIMIT-CORRESP
           PERFORM 2960-ASSESS-STOP
           PERFORM 2970-EDIT-DESC
           PERFORM 2980-CHECK-EFFECTIVE
           PERFORM 2990-DESCRIBE-REASON
           PERFORM 3000-DESCRIBE-TYPE
           PERFORM 3010-COUNT-NOTICE
           PERFORM 3020-ACCRUE-CORRESP
           PERFORM 3030-ACCRUE-CHARGE
           PERFORM 3040-SPLIT-CORRESP
           PERFORM 3050-SET-REASON
           PERFORM 3060-CHECK-CLOSE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TXELG-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2000-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TXELG-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TXELG-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-EFFECTIVE-DATE TO DTU-DATE-1
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
      * DAILY INTEREST ON CORRESP YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-CORRESP.
           IF TXELG-CORRESP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXELG-CORRESP-YTD * TXELG-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TXELG-CORRESP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WIRE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2030-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TXELG-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TXELG-WIRE-YTD *
                       TXELG-RATE / 200
               WHEN TXELG-MSG-TYPE-CD = 'LG'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN TXELG-MSG-TYPE-CD = 'LX'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TXELG-WIRE-YTD *
                       TXELG-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2040-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-NEXT-DUE-DATE > 0 AND TXELG-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2050-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TXELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE CHARGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2060-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TXELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TXELG-CHARGE-AMT NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TXELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-EFFECTIVE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2080-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TXELG-RATE / 100 / 365
               ADD WS-WORK-AMT TO TXELG-CORRESP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TXELG-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TXELG-QUEUED-AMT NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TXELG-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXELG-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXELG-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2100-COUNT-PAYMENT.
           IF TXELG-PAYMENT-CNT < 9999999
               ADD 1 TO TXELG-PAYMENT-CNT
           END-IF
           IF TXELG-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO TXELG-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CORRESP YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2110-ACCRUE-CORRESP.
           IF TXELG-CORRESP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXELG-CORRESP-YTD * TXELG-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TXELG-CHARGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN CLOSE DATE IS PAST
      * LX -> ND WHEN CLOSE DATE IS PAST
      * ND -> PT WHEN CLOSE DATE IS PAST
      * PT -> LG WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2120-SET-MSG-TYPE.
           EVALUATE TXELG-MSG-TYPE-CD
               WHEN 'LG'
                   IF TXELG-CLOSE-DATE > 0 AND TXELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LX' TO TXELG-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TXELG-CLOSE-DATE > 0 AND TXELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO TXELG-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TXELG-CLOSE-DATE > 0 AND TXELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TXELG-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TXELG-CLOSE-DATE > 0 AND TXELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LG' TO TXELG-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * QUEUED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2130-FLAG-QUEUED.
           IF TXELG-QUEUED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TXELG-QUEUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TXELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-CLOSE-DATE > 0 AND TXELG-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXELG-WIRE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TXELG-WIRE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TXELG-WIRE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TXELG-WIRE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXELG-WIRE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2160-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXELG-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXELG-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DGN -> JBT WHEN EFFECTIVE DATE IS PAST
      * JBT -> LSS WHEN EFFECTIVE DATE IS PAST
      * LSS -> RDP WHEN EFFECTIVE DATE IS PAST
      * RDP -> RPL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2170-SET-REASON.
           EVALUATE TXELG-REASON-CD
               WHEN 'DGN'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JBT' TO TXELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBT'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LSS' TO TXELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LSS'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RDP' TO TXELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RDP'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RPL' TO TXELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2180-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TXELG-RATE / 100 / 365
               ADD WS-WORK-AMT TO TXELG-WIRE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN EFFECTIVE DATE IS PAST
      * L -> M WHEN EFFECTIVE DATE IS PAST
      * M -> P WHEN EFFECTIVE DATE IS PAST
      * P -> W WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2190-SET-TYPE.
           EVALUATE TXELG-TYPE-CD
               WHEN 'K'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'P' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'W' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE CHARGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TXELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TXELG-CHARGE-AMT NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TXELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2210-ACCRUE-WIRE.
           IF TXELG-WIRE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXELG-WIRE-AMT * TXELG-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TXELG-CORRESP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2220-COUNT-NOTICE.
           IF TXELG-NOTICE-CNT < 9999999
               ADD 1 TO TXELG-NOTICE-CNT
           END-IF
           IF TXELG-NOTICE-CNT NOT < 12
               MOVE 'Y' TO TXELG-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CORRESP YTD FEE ON THE WIRE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2230-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TXELG-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TXELG-CORRESP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE TXELG-CORRESP-YTD ROUNDED = TXELG-WIRE-YTD *
                       TXELG-FEE-PCT / 200
               WHEN TXELG-REGION-CD = 'G'
                   MOVE 7.00 TO TXELG-CORRESP-YTD
               WHEN TXELG-REGION-CD = 'M'
                   MOVE 12.00 TO TXELG-CORRESP-YTD
               WHEN OTHER
                   COMPUTE TXELG-CORRESP-YTD ROUNDED = TXELG-WIRE-YTD *
                       TXELG-FEE-PCT / 100
           END-EVALUATE
           IF TXELG-CORRESP-YTD > 0 AND TXELG-CORRESP-YTD < 1.00
               MOVE 1.00 TO TXELG-CORRESP-YTD
           END-IF
           IF TXELG-CORRESP-YTD > 50.00
               MOVE 50.00 TO TXELG-CORRESP-YTD
           END-IF
           ADD TXELG-CORRESP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2240-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TXELG-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN EFFECTIVE DATE IS PAST
      * LX -> ND WHEN EFFECTIVE DATE IS PAST
      * ND -> PT WHEN EFFECTIVE DATE IS PAST
      * PT -> LG WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2250-SET-MSG-TYPE.
           EVALUATE TXELG-MSG-TYPE-CD
               WHEN 'LG'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LX' TO TXELG-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ND' TO TXELG-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PT' TO TXELG-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LG' TO TXELG-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CORRESP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CORRESP YTD.
      *----------------------------------------------------------------*
       2260-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXELG-CORRESP-YTD ROUNDED =
                   TXELG-CORRESP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TXELG-CORRESP-YTD
           END-IF
           ADD TXELG-CORRESP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN EFFECTIVE DATE IS PAST
      * L -> M WHEN EFFECTIVE DATE IS PAST
      * M -> P WHEN EFFECTIVE DATE IS PAST
      * P -> W WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2270-SET-TYPE.
           EVALUATE TXELG-TYPE-CD
               WHEN 'K'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'P' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'W' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WIRE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TXELG-WIRE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TXELG-WIRE-YTD - (WS-WORK-AMT * 4)
           COMPUTE TXELG-CHARGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TXELG-LAST-PAY-DATE > 0 AND TXELG-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CORRESP AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2300-LIMIT-CORRESP.
           IF TXELG-CORRESP-AMT < 25.00
               MOVE 25.00 TO TXELG-CORRESP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXELG-CORRESP-AMT > 5000.00
               MOVE 5000.00 TO TXELG-CORRESP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2310-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-CLOSE-DATE > 0 AND TXELG-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CORRESP YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2320-FLAG-CORRESP.
           IF TXELG-CORRESP-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TXELG-CORRESP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2330-DESCRIBE-TYPE.
           EVALUATE TXELG-TYPE-CD
               WHEN 'K'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2340-DESCRIBE-REGION.
           EVALUATE TXELG-REGION-CD
               WHEN 'G'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE FEE ON THE CORRESP YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2350-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TXELG-STOP-FLAG = 'Y'
                   MOVE ZERO TO TXELG-WIRE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE TXELG-WIRE-AMT ROUNDED = TXELG-CORRESP-YTD *
                       TXELG-RATE / 200
               WHEN TXELG-REGION-CD = 'G'
                   MOVE 5.00 TO TXELG-WIRE-AMT
               WHEN TXELG-REGION-CD = 'M'
                   MOVE 10.00 TO TXELG-WIRE-AMT
               WHEN OTHER
                   COMPUTE TXELG-WIRE-AMT ROUNDED = TXELG-CORRESP-YTD *
                       TXELG-RATE / 100
           END-EVALUATE
           IF TXELG-WIRE-AMT > 0 AND TXELG-WIRE-AMT < 1.00
               MOVE 1.00 TO TXELG-WIRE-AMT
           END-IF
           IF TXELG-WIRE-AMT > 35.00
               MOVE 35.00 TO TXELG-WIRE-AMT
           END-IF
           ADD TXELG-WIRE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2360-DESCRIBE-REASON.
           EVALUATE TXELG-REASON-CD
               WHEN 'DGN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JBT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LSS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RDP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RPL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SNE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UJS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN LAST PAY DATE IS PAST
      * L -> M WHEN LAST PAY DATE IS PAST
      * M -> P WHEN LAST PAY DATE IS PAST
      * P -> W WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2370-SET-TYPE.
           EVALUATE TXELG-TYPE-CD
               WHEN 'K'
                   IF TXELG-LAST-PAY-DATE > 0 AND TXELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TXELG-LAST-PAY-DATE > 0 AND TXELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TXELG-LAST-PAY-DATE > 0 AND TXELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TXELG-LAST-PAY-DATE > 0 AND TXELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TXELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TXELG-RATE / 100 / 365
               ADD WS-WORK-AMT TO TXELG-QUEUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-EFFECTIVE-DATE > 0 AND TXELG-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WIRE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHARGE AMT.
      *----------------------------------------------------------------*
       2400-PRORATE-CHARGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXELG-CHARGE-AMT ROUNDED =
                   TXELG-WIRE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TXELG-CHARGE-AMT
           END-IF
           ADD TXELG-CHARGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CORRESP AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2410-ACCRUE-CORRESP.
           IF TXELG-CORRESP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXELG-CORRESP-AMT * TXELG-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TXELG-QUEUED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TXELG-QUEUED-AMT - (WS-WORK-AMT * 3)
           COMPUTE TXELG-CORRESP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2430-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TXELG-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TXELG-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXELG-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXELG-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE WIRE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TXELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TXELG-WIRE-YTD NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TXELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2450-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TXELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2460-FLAG-CORRESP.
           IF TXELG-CORRESP-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TXELG-CORRESP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2470-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXELG-REGION-CD
                   ADD TXELG-CHARGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXELG-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXELG-CHARGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-REGION.
           EVALUATE TXELG-REGION-CD
               WHEN 'G'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE MSG TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2490-DESCRIBE-MSG-TYPE.
           EVALUATE TXELG-MSG-TYPE-CD
               WHEN 'LG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E862 ADDR REQUIRED
      * E409 WORK AMT NEGATIVE
      * E090 WORK AMT OVER 99999
      * E378 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2500-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXELG-ADDR = SPACES
                   MOVE 'E862' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN TXELG-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E378' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2510-FLAG-QUEUED.
           IF TXELG-QUEUED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TXELG-QUEUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE WIRE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2520-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TXELG-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TXELG-WIRE-YTD NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TXELG-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXELG-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXELG-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E408 ADDR REQUIRED
      * E592 QUEUED AMT NEGATIVE
      * E005 QUEUED AMT OVER 250000
      * E366 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2530-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXELG-ADDR = SPACES
                   MOVE 'E408' TO WS-REJECT-CODE
               WHEN TXELG-QUEUED-AMT < 0
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN TXELG-QUEUED-AMT > 250000.00
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN TXELG-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2540-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TXELG-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TXELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2550-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TXELG-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXELG-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       TXELG-FEE-PCT / 200
               WHEN TXELG-REGION-CD = 'G'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN TXELG-REGION-CD = 'M'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       TXELG-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXELG-REGION-CD
                   ADD TXELG-CORRESP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXELG-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXELG-CORRESP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2570-LIMIT-QUEUED.
           IF TXELG-QUEUED-AMT < 25.00
               MOVE 25.00 TO TXELG-QUEUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXELG-QUEUED-AMT > 50000.00
               MOVE 50000.00 TO TXELG-QUEUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE WIRE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2580-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TXELG-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TXELG-WIRE-AMT NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TXELG-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXELG-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXELG-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2590-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2600-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TXELG-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TXELG-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2610-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TXELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2620-LIMIT-QUEUED.
           IF TXELG-QUEUED-AMT < 25.00
               MOVE 25.00 TO TXELG-QUEUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXELG-QUEUED-AMT > 1000.00
               MOVE 1000.00 TO TXELG-QUEUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TXELG-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TXELG-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP AMT BY MSG TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-MSG-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXELG-MSG-TYPE-CD
                   ADD TXELG-CORRESP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXELG-MSG-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXELG-CORRESP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2650-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXELG-TYPE-CD
                   ADD TXELG-CHARGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXELG-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXELG-CHARGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2660-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TXELG-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TXELG-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXELG-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXELG-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2670-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CORRESP AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2680-ACCRUE-CORRESP.
           IF TXELG-CORRESP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXELG-CORRESP-AMT * TXELG-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TXELG-CORRESP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2690-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXELG-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXELG-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2700-FLAG-CORRESP.
           IF TXELG-CORRESP-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TXELG-CORRESP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2710-COUNT-NOTICE.
           IF TXELG-NOTICE-CNT < 9999999
               ADD 1 TO TXELG-NOTICE-CNT
           END-IF
           IF TXELG-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TXELG-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E874 DESC REQUIRED
      * E308 WORK AMT NEGATIVE
      * E900 WORK AMT OVER 250000
      * E464 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2720-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXELG-DESC = SPACES
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E308' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E900' TO WS-REJECT-CODE
               WHEN TXELG-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E464' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2730-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TXELG-STOP-FLAG = 'Y'
                   MOVE ZERO TO TXELG-WIRE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE TXELG-WIRE-AMT ROUNDED = WS-WORK-AMT *
                       TXELG-FEE-PCT / 200
               WHEN TXELG-REASON-CD = 'DGN'
                   MOVE 15.00 TO TXELG-WIRE-AMT
               WHEN TXELG-REASON-CD = 'JBT'
                   MOVE 7.00 TO TXELG-WIRE-AMT
               WHEN OTHER
                   COMPUTE TXELG-WIRE-AMT ROUNDED = WS-WORK-AMT *
                       TXELG-FEE-PCT / 100
           END-EVALUATE
           IF TXELG-WIRE-AMT > 0 AND TXELG-WIRE-AMT < 1.00
               MOVE 1.00 TO TXELG-WIRE-AMT
           END-IF
           IF TXELG-WIRE-AMT > 25.00
               MOVE 25.00 TO TXELG-WIRE-AMT
           END-IF
           ADD TXELG-WIRE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2740-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2750-FLAG-CHARGE.
           IF TXELG-CHARGE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TXELG-CHARGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2760-DESCRIBE-REGION.
           EVALUATE TXELG-REGION-CD
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2770-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-NEXT-DUE-DATE > 0 AND TXELG-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXELG-CORRESP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TXELG-CORRESP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TXELG-CORRESP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TXELG-CORRESP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXELG-CORRESP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE WIRE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2780-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TXELG-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TXELG-WIRE-AMT NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TXELG-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXELG-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXELG-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE FEE ON THE CORRESP AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2790-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TXELG-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TXELG-WIRE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXELG-STOP-FLAG = 'Y'
                   COMPUTE TXELG-WIRE-AMT ROUNDED = TXELG-CORRESP-AMT *
                       TXELG-FEE-PCT / 200
               WHEN TXELG-REGION-CD = 'G'
                   MOVE 7.00 TO TXELG-WIRE-AMT
               WHEN TXELG-REGION-CD = 'M'
                   MOVE 7.00 TO TXELG-WIRE-AMT
               WHEN OTHER
                   COMPUTE TXELG-WIRE-AMT ROUNDED = TXELG-CORRESP-AMT *
                       TXELG-FEE-PCT / 100
           END-EVALUATE
           IF TXELG-WIRE-AMT > 0 AND TXELG-WIRE-AMT < 2.00
               MOVE 2.00 TO TXELG-WIRE-AMT
           END-IF
           IF TXELG-WIRE-AMT > 50.00
               MOVE 50.00 TO TXELG-WIRE-AMT
           END-IF
           ADD TXELG-WIRE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2800-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE QUEUED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2810-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TXELG-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXELG-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TXELG-QUEUED-AMT *
                       TXELG-RATE / 200
               WHEN TXELG-MSG-TYPE-CD = 'LG'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN TXELG-MSG-TYPE-CD = 'LX'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TXELG-QUEUED-AMT *
                       TXELG-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2820-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXELG-REGION-CD
                   ADD TXELG-CORRESP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXELG-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXELG-CORRESP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2830-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-NEXT-DUE-DATE > 0 AND TXELG-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXELG-WIRE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CORRESP AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2840-LIMIT-CORRESP.
           IF TXELG-CORRESP-AMT < 10.00
               MOVE 10.00 TO TXELG-CORRESP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXELG-CORRESP-AMT > 50000.00
               MOVE 50000.00 TO TXELG-CORRESP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CORRESP YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2850-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TXELG-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXELG-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TXELG-CORRESP-YTD *
                       TXELG-FEE-PCT / 200
               WHEN TXELG-TYPE-CD = 'K'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN TXELG-TYPE-CD = 'L'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TXELG-CORRESP-YTD *
                       TXELG-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E730 ADDR REQUIRED
      * E225 QUEUED AMT NEGATIVE
      * E405 QUEUED AMT OVER 50000
      * E716 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2860-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXELG-ADDR = SPACES
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN TXELG-QUEUED-AMT < 0
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN TXELG-QUEUED-AMT > 50000.00
                   MOVE 'E405' TO WS-REJECT-CODE
               WHEN TXELG-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E578 ADDR REQUIRED
      * E947 CORRESP YTD NEGATIVE
      * E544 CORRESP YTD OVER 1000000
      * E358 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2870-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXELG-ADDR = SPACES
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN TXELG-CORRESP-YTD < 0
                   MOVE 'E947' TO WS-REJECT-CODE
               WHEN TXELG-CORRESP-YTD > 1000000.00
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN TXELG-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E358' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2880-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TXELG-LAST-PAY-DATE > 0 AND TXELG-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXELG-QUEUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TXELG-QUEUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TXELG-QUEUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TXELG-QUEUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXELG-QUEUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHARGE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2890-LIMIT-CHARGE.
           IF TXELG-CHARGE-AMT < 0.00
               MOVE 0.00 TO TXELG-CHARGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXELG-CHARGE-AMT > 10000.00
               MOVE 10000.00 TO TXELG-CHARGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CORRESP AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2900-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TXELG-CORRESP-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TXELG-CORRESP-AMT - (WS-WORK-AMT * 4)
           COMPUTE TXELG-CHARGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WIRE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO QUEUED AMT.
      *----------------------------------------------------------------*
       2910-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXELG-QUEUED-AMT ROUNDED =
                   TXELG-WIRE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TXELG-QUEUED-AMT
           END-IF
           ADD TXELG-QUEUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CHARGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WIRE YTD.
      *----------------------------------------------------------------*
       2920-PRORATE-WIRE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXELG-WIRE-YTD ROUNDED =
                   TXELG-CHARGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TXELG-WIRE-YTD
           END-IF
           ADD TXELG-WIRE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2930-LIMIT-WIRE.
           IF TXELG-WIRE-AMT < 0.00
               MOVE 0.00 TO TXELG-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXELG-WIRE-AMT > 1000.00
               MOVE 1000.00 TO TXELG-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CORRESP YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2940-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TXELG-CORRESP-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TXELG-CORRESP-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2950-LIMIT-CORRESP.
           IF TXELG-CORRESP-AMT < 0.00
               MOVE 0.00 TO TXELG-CORRESP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXELG-CORRESP-AMT > 10000.00
               MOVE 10000.00 TO TXELG-CORRESP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE YTD FEE ON THE CHARGE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2960-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TXELG-STOP-FLAG = 'Y'
                   MOVE ZERO TO TXELG-WIRE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE TXELG-WIRE-YTD ROUNDED = TXELG-CHARGE-AMT *
                       TXELG-FEE-PCT / 200
               WHEN TXELG-TYPE-CD = 'K'
                   MOVE 10.00 TO TXELG-WIRE-YTD
               WHEN TXELG-TYPE-CD = 'L'
                   MOVE 15.00 TO TXELG-WIRE-YTD
               WHEN OTHER
                   COMPUTE TXELG-WIRE-YTD ROUNDED = TXELG-CHARGE-AMT *
                       TXELG-FEE-PCT / 100
           END-EVALUATE
           IF TXELG-WIRE-YTD > 0 AND TXELG-WIRE-YTD < 2.00
               MOVE 2.00 TO TXELG-WIRE-YTD
           END-IF
           IF TXELG-WIRE-YTD > 150.00
               MOVE 150.00 TO TXELG-WIRE-YTD
           END-IF
           ADD TXELG-WIRE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E895 DESC REQUIRED
      * E058 WIRE YTD NEGATIVE
      * E381 WIRE YTD OVER 1000000
      * E684 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2970-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXELG-DESC = SPACES
                   MOVE 'E895' TO WS-REJECT-CODE
               WHEN TXELG-WIRE-YTD < 0
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN TXELG-WIRE-YTD > 1000000.00
                   MOVE 'E381' TO WS-REJECT-CODE
               WHEN TXELG-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E684' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2980-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TXELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2990-DESCRIBE-REASON.
           EVALUATE TXELG-REASON-CD
               WHEN 'DGN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JBT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LSS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RDP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RPL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SNE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UJS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-TYPE.
           EVALUATE TXELG-TYPE-CD
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'P'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-NOTICE.
           IF TXELG-NOTICE-CNT < 9999999
               ADD 1 TO TXELG-NOTICE-CNT
           END-IF
           IF TXELG-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TXELG-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CORRESP AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-CORRESP.
           IF TXELG-CORRESP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXELG-CORRESP-AMT * TXELG-RATE / 100 / 360
               ADD WS-WORK-AMT TO TXELG-QUEUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-CHARGE.
           IF TXELG-CHARGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXELG-CHARGE-AMT * TXELG-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TXELG-CORRESP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CORRESP AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TXELG-CORRESP-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TXELG-CORRESP-AMT - (WS-WORK-AMT * 2)
           COMPUTE TXELG-WIRE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DGN -> JBT WHEN NEXT DUE DATE IS PAST
      * JBT -> LSS WHEN NEXT DUE DATE IS PAST
      * LSS -> RDP WHEN NEXT DUE DATE IS PAST
      * RDP -> RPL WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-REASON.
           EVALUATE TXELG-REASON-CD
               WHEN 'DGN'
                   IF TXELG-NEXT-DUE-DATE > 0 AND TXELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBT' TO TXELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBT'
                   IF TXELG-NEXT-DUE-DATE > 0 AND TXELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LSS' TO TXELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LSS'
                   IF TXELG-NEXT-DUE-DATE > 0 AND TXELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RDP' TO TXELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RDP'
                   IF TXELG-NEXT-DUE-DATE > 0 AND TXELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RPL' TO TXELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TXELG-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TXELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TXELG-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TXELG-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TXELG-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TXELG EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
