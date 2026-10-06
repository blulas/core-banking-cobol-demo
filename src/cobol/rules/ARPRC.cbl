      *================================================================*
      * PROGRAM  : ARPRC                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : ACCOUNT RECONCILEMENT RULE - FEES. CALLED BY THE    *
      *            ACCOUNT RECONCILEMENT BATCH AND ONLINE PROGRAMS.    *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-05-06 GAF  ORIGINAL                                       *
      * 2000-08-05 GAF  AUDIT FINDING - FEE CONTROL TOTALS REQ5523     *
      * 2006-10-14 DPS  PAID FEE SCHEDULE UPDATE INC4147               *
      * 2009-10-12 PQR  FEE EXCEPTIONS TO OPS QUEUE REQ3258            *
      * 2009-11-01 BHS  ISSUED TABLE EXPANDED REQ0289                  *
      * 2011-11-13 MKL  ISSUED LIMIT REVISED INC4370                   *
      * 2012-10-21 GAF  STOP FEE SCHEDULE UPDATE REQ7871               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARPRC.
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
                                           VALUE 16.000000.
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
      * WS-OUTSTAND-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OUTSTAND-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000115000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000407500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000296500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000134500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000325500'.
       01  WS-OUTSTAND-BAND-TBL REDEFINES WS-OUTSTAND-BAND-VALUES.
           05  WS-OUTSTAND-BAND-ROW OCCURS 5 TIMES.
               10  WS-OUTSTAND-BAND-LOW    PIC 9(09)V99.
               10  WS-OUTSTAND-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ARPRCP.

       PROCEDURE DIVISION USING ARPRC-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ASSESS-HOLD
           PERFORM 2010-ACCRUE-PENALTY-PCT
           PERFORM 2020-COUNT-PAYMENT
           PERFORM 2030-PRORATE-ISSUED
           PERFORM 2040-LIMIT-OUTSTAND
           PERFORM 2050-ACCRUE-ISSUED
           PERFORM 2060-LIMIT-OUTSTAND
           PERFORM 2070-ASSESS-HOLD
           PERFORM 2080-FLAG-FEE
           PERFORM 2090-ACCUM-RECON-TYPE
           PERFORM 2100-SELECT-DISCOUNT
           PERFORM 2110-AGE-OPEN
           PERFORM 2120-ASSESS-HOLD
           PERFORM 2130-COUNT-PAYMENT
           PERFORM 2140-CHECK-OPEN
           PERFORM 2150-LIMIT-STOP
           PERFORM 2160-AGE-NEXT-REVIEW
           PERFORM 2170-SELECT-DISCOUNT
           PERFORM 2180-ACCRUE-OUTSTAND
           PERFORM 2190-CHECK-LAST-ACT
           PERFORM 2200-AGE-OPEN
           PERFORM 2210-FLAG-ITEM
           PERFORM 2220-FLAG-ITEM
           PERFORM 2230-SET-CHANNEL
           PERFORM 2240-FLAG-OUTSTAND
           PERFORM 2250-DESCRIBE-SOURCE
           PERFORM 2260-COUNT-PAYMENT
           PERFORM 2270-AGE-NEXT-DUE
           PERFORM 2280-ACCRUE-DISCOUNT
           PERFORM 2290-PRORATE-ISSUED
           PERFORM 2300-CHECK-NEXT-DUE
           PERFORM 2310-ASSESS-PREFERRED
           PERFORM 2320-ASSESS-HOLD
           PERFORM 2330-PRORATE-OUTSTAND
           PERFORM 2340-SPLIT-OUTSTAND
           PERFORM 2350-ACCUM-SOURCE
           PERFORM 2360-SPLIT-ISSUED
           PERFORM 2370-ACCUM-SOURCE
           PERFORM 2380-ACCRUE-DISCOUNT
           PERFORM 2390-SET-SOURCE
           PERFORM 2400-CHECK-NEXT-REVIEW
           PERFORM 2410-COUNT-EXCEPT
           PERFORM 2420-SPLIT-STOP
           PERFORM 2430-SET-CHANNEL
           PERFORM 2440-FLAG-FEE
           PERFORM 2450-SPLIT-STOP
           PERFORM 2460-ACCRUE-ISSUED
           PERFORM 2470-ASSESS-HOLD
           PERFORM 2480-PRORATE-ISSUED
           PERFORM 2490-DESCRIBE-SOURCE
           PERFORM 2500-PRORATE-ITEM
           PERFORM 2510-ASSESS-PREFERRED
           PERFORM 2520-CHECK-OPEN
           PERFORM 2530-LIMIT-OUTSTAND
           PERFORM 2540-ACCUM-CHANNEL
           PERFORM 2550-AGE-OPEN
           PERFORM 2560-ACCUM-BRANCH
           PERFORM 2570-SELECT-PENALTY-PCT
           PERFORM 2580-COUNT-EXCEPT
           PERFORM 2590-ACCRUE-DISCOUNT
           PERFORM 2600-DESCRIBE-CHANNEL
           PERFORM 2610-ASSESS-PREFERRED
           PERFORM 2620-AGE-OPEN
           PERFORM 2630-AGE-OPEN
           PERFORM 2640-DESCRIBE-SOURCE
           PERFORM 2650-FLAG-OUTSTAND
           PERFORM 2660-PRORATE-STOP
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ARPRC-RETURN-CODE.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2000-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPRC-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPRC-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPRC-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPRC-ISSUED-AMT ROUNDED = WS-WORK-AMT-2 *
                       ARPRC-PENALTY-PCT / 200
               WHEN ARPRC-CHANNEL-CD = 'ME'
                   MOVE 12.00 TO ARPRC-ISSUED-AMT
               WHEN ARPRC-CHANNEL-CD = 'SW'
                   MOVE 20.00 TO ARPRC-ISSUED-AMT
               WHEN OTHER
                   COMPUTE ARPRC-ISSUED-AMT ROUNDED = WS-WORK-AMT-2 *
                       ARPRC-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPRC-ISSUED-AMT > 0 AND ARPRC-ISSUED-AMT < 2.00
               MOVE 2.00 TO ARPRC-ISSUED-AMT
           END-IF
           IF ARPRC-ISSUED-AMT > 25.00
               MOVE 25.00 TO ARPRC-ISSUED-AMT
           END-IF
           ADD ARPRC-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2010-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ARPRC-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPRC-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2020-COUNT-PAYMENT.
           IF ARPRC-PAYMENT-CNT < 9999999
               ADD 1 TO ARPRC-PAYMENT-CNT
           END-IF
           IF ARPRC-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO ARPRC-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2030-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ARPRC-ISSUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * OUTSTAND YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2040-LIMIT-OUTSTAND.
           IF ARPRC-OUTSTAND-YTD < 10.00
               MOVE 10.00 TO ARPRC-OUTSTAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPRC-OUTSTAND-YTD > 5000.00
               MOVE 5000.00 TO ARPRC-OUTSTAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2050-ACCRUE-ISSUED.
           IF ARPRC-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPRC-ISSUED-AMT * ARPRC-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPRC-OUTSTAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2060-LIMIT-OUTSTAND.
           IF ARPRC-OUTSTAND-YTD < 0.00
               MOVE 0.00 TO ARPRC-OUTSTAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPRC-OUTSTAND-YTD > 2500.00
               MOVE 2500.00 TO ARPRC-OUTSTAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE STOP YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2070-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPRC-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPRC-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPRC-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPRC-ISSUED-YTD ROUNDED = ARPRC-STOP-YTD *
                       ARPRC-DISCOUNT / 200
               WHEN ARPRC-SOURCE-CD = 'GW'
                   MOVE 10.00 TO ARPRC-ISSUED-YTD
               WHEN ARPRC-SOURCE-CD = 'MB'
                   MOVE 15.00 TO ARPRC-ISSUED-YTD
               WHEN OTHER
                   COMPUTE ARPRC-ISSUED-YTD ROUNDED = ARPRC-STOP-YTD *
                       ARPRC-DISCOUNT / 100
           END-EVALUATE
           IF ARPRC-ISSUED-YTD > 0 AND ARPRC-ISSUED-YTD < 5.00
               MOVE 5.00 TO ARPRC-ISSUED-YTD
           END-IF
           IF ARPRC-ISSUED-YTD > 250.00
               MOVE 250.00 TO ARPRC-ISSUED-YTD
           END-IF
           ADD ARPRC-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2080-FLAG-FEE.
           IF ARPRC-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPRC-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY RECON TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2090-ACCUM-RECON-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPRC-RECON-TYPE-CD
                   ADD ARPRC-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPRC-RECON-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPRC-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2100-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARPRC-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO ARPRC-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPRC-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPRC-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE STOP YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2120-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPRC-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPRC-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPRC-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPRC-FEE-YTD ROUNDED = ARPRC-STOP-YTD *
                       ARPRC-DISCOUNT / 200
               WHEN ARPRC-BRANCH-CD = 'FN'
                   MOVE 7.00 TO ARPRC-FEE-YTD
               WHEN ARPRC-BRANCH-CD = 'GM'
                   MOVE 15.00 TO ARPRC-FEE-YTD
               WHEN OTHER
                   COMPUTE ARPRC-FEE-YTD ROUNDED = ARPRC-STOP-YTD *
                       ARPRC-DISCOUNT / 100
           END-EVALUATE
           IF ARPRC-FEE-YTD > 0 AND ARPRC-FEE-YTD < 1.00
               MOVE 1.00 TO ARPRC-FEE-YTD
           END-IF
           IF ARPRC-FEE-YTD > 150.00
               MOVE 150.00 TO ARPRC-FEE-YTD
           END-IF
           ADD ARPRC-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2130-COUNT-PAYMENT.
           IF ARPRC-PAYMENT-CNT < 9999999
               ADD 1 TO ARPRC-PAYMENT-CNT
           END-IF
           IF ARPRC-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO ARPRC-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARPRC-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STOP YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2150-LIMIT-STOP.
           IF ARPRC-STOP-YTD < 25.00
               MOVE 25.00 TO ARPRC-STOP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPRC-STOP-YTD > 50000.00
               MOVE 50000.00 TO ARPRC-STOP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2160-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-NEXT-REVIEW-DATE > 0 AND ARPRC-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPRC-OUTSTAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARPRC-OUTSTAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARPRC-OUTSTAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPRC-OUTSTAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPRC-OUTSTAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2170-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARPRC-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO ARPRC-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPRC-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPRC-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2180-ACCRUE-OUTSTAND.
           IF ARPRC-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPRC-OUTSTAND-AMT * ARPRC-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPRC-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2190-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ARPRC-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2200-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPRC-STOP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ARPRC-STOP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARPRC-STOP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPRC-STOP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPRC-STOP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2210-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2220-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN OPEN DATE IS PAST
      * SW -> UA WHEN OPEN DATE IS PAST
      * UA -> WB WHEN OPEN DATE IS PAST
      * WB -> ME WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2230-SET-CHANNEL.
           EVALUATE ARPRC-CHANNEL-CD
               WHEN 'ME'
                   IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2240-FLAG-OUTSTAND.
           IF ARPRC-OUTSTAND-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPRC-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2250-DESCRIBE-SOURCE.
           EVALUATE ARPRC-SOURCE-CD
               WHEN 'GW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2260-COUNT-PAYMENT.
           IF ARPRC-PAYMENT-CNT < 9999999
               ADD 1 TO ARPRC-PAYMENT-CNT
           END-IF
           IF ARPRC-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO ARPRC-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-NEXT-DUE-DATE > 0 AND ARPRC-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-NEXT-DUE-DATE TO DTU-DATE-1
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
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ARPRC-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPRC-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2290-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ARPRC-ISSUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARPRC-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2310-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPRC-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPRC-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPRC-STOP-FLAG = 'Y'
                   COMPUTE ARPRC-ISSUED-YTD ROUNDED = ARPRC-OUTSTAND-YTD
                       * ARPRC-PENALTY-PCT / 200
               WHEN ARPRC-CHANNEL-CD = 'ME'
                   MOVE 15.00 TO ARPRC-ISSUED-YTD
               WHEN ARPRC-CHANNEL-CD = 'SW'
                   MOVE 20.00 TO ARPRC-ISSUED-YTD
               WHEN OTHER
                   COMPUTE ARPRC-ISSUED-YTD ROUNDED = ARPRC-OUTSTAND-YTD
                       * ARPRC-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPRC-ISSUED-YTD > 0 AND ARPRC-ISSUED-YTD < 5.00
               MOVE 5.00 TO ARPRC-ISSUED-YTD
           END-IF
           IF ARPRC-ISSUED-YTD > 75.00
               MOVE 75.00 TO ARPRC-ISSUED-YTD
           END-IF
           ADD ARPRC-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FEE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2320-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPRC-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPRC-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ARPRC-FEE-YTD *
                       ARPRC-DISCOUNT / 200
               WHEN ARPRC-SOURCE-CD = 'GW'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN ARPRC-SOURCE-CD = 'MB'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ARPRC-FEE-YTD *
                       ARPRC-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       2330-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPRC-OUTSTAND-YTD ROUNDED =
                   ARPRC-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPRC-OUTSTAND-YTD
           END-IF
           ADD ARPRC-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2340-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPRC-OUTSTAND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARPRC-OUTSTAND-YTD - (WS-WORK-AMT *
               4)
           COMPUTE ARPRC-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2350-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPRC-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPRC-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2360-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPRC-ISSUED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ARPRC-ISSUED-YTD - (WS-WORK-AMT * 3)
           COMPUTE ARPRC-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2370-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPRC-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPRC-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ARPRC-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPRC-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN NEXT DUE DATE IS PAST
      * MB -> MH WHEN NEXT DUE DATE IS PAST
      * MH -> UG WHEN NEXT DUE DATE IS PAST
      * UG -> UR WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2390-SET-SOURCE.
           EVALUATE ARPRC-SOURCE-CD
               WHEN 'GW'
                   IF ARPRC-NEXT-DUE-DATE > 0 AND ARPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPRC-NEXT-DUE-DATE > 0 AND ARPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPRC-NEXT-DUE-DATE > 0 AND ARPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPRC-NEXT-DUE-DATE > 0 AND ARPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2400-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ARPRC-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-EXCEPT.
           IF ARPRC-EXCEPT-CNT < 9999999
               ADD 1 TO ARPRC-EXCEPT-CNT
           END-IF
           IF ARPRC-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO ARPRC-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-STOP.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE ARPRC-STOP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN OPEN DATE IS PAST
      * SW -> UA WHEN OPEN DATE IS PAST
      * UA -> WB WHEN OPEN DATE IS PAST
      * WB -> ME WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2430-SET-CHANNEL.
           EVALUATE ARPRC-CHANNEL-CD
               WHEN 'ME'
                   IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2440-FLAG-FEE.
           IF ARPRC-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPRC-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STOP YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2450-SPLIT-STOP.
           COMPUTE WS-WORK-AMT = ARPRC-STOP-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ARPRC-STOP-YTD - (WS-WORK-AMT * 2)
           COMPUTE ARPRC-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-ISSUED.
           IF ARPRC-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPRC-ISSUED-AMT * ARPRC-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE ISSUED YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2470-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPRC-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPRC-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPRC-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPRC-FEE-YTD ROUNDED = ARPRC-ISSUED-YTD *
                       ARPRC-PENALTY-PCT / 200
               WHEN ARPRC-RECON-TYPE-CD = 'EEH'
                   MOVE 3.00 TO ARPRC-FEE-YTD
               WHEN ARPRC-RECON-TYPE-CD = 'KBS'
                   MOVE 12.00 TO ARPRC-FEE-YTD
               WHEN OTHER
                   COMPUTE ARPRC-FEE-YTD ROUNDED = ARPRC-ISSUED-YTD *
                       ARPRC-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPRC-FEE-YTD > 0 AND ARPRC-FEE-YTD < 10.00
               MOVE 10.00 TO ARPRC-FEE-YTD
           END-IF
           IF ARPRC-FEE-YTD > 25.00
               MOVE 25.00 TO ARPRC-FEE-YTD
           END-IF
           ADD ARPRC-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2480-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ARPRC-ISSUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2490-DESCRIBE-SOURCE.
           EVALUATE ARPRC-SOURCE-CD
               WHEN 'GW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2500-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND YTD FEE ON THE ISSUED AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2510-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPRC-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPRC-OUTSTAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPRC-HOLD-FLAG = 'Y'
                   COMPUTE ARPRC-OUTSTAND-YTD ROUNDED = ARPRC-ISSUED-AMT
                       * ARPRC-PENALTY-PCT / 200
               WHEN ARPRC-CHANNEL-CD = 'ME'
                   MOVE 15.00 TO ARPRC-OUTSTAND-YTD
               WHEN ARPRC-CHANNEL-CD = 'SW'
                   MOVE 7.00 TO ARPRC-OUTSTAND-YTD
               WHEN OTHER
                   COMPUTE ARPRC-OUTSTAND-YTD ROUNDED = ARPRC-ISSUED-AMT
                       * ARPRC-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPRC-OUTSTAND-YTD > 0 AND ARPRC-OUTSTAND-YTD < 1.00
               MOVE 1.00 TO ARPRC-OUTSTAND-YTD
           END-IF
           IF ARPRC-OUTSTAND-YTD > 50.00
               MOVE 50.00 TO ARPRC-OUTSTAND-YTD
           END-IF
           ADD ARPRC-OUTSTAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2520-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ARPRC-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2530-LIMIT-OUTSTAND.
           IF ARPRC-OUTSTAND-YTD < 10.00
               MOVE 10.00 TO ARPRC-OUTSTAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPRC-OUTSTAND-YTD > 2500.00
               MOVE 2500.00 TO ARPRC-OUTSTAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPRC-CHANNEL-CD
                   ADD ARPRC-OUTSTAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPRC-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPRC-OUTSTAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2550-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPRC-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD ARPRC-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ARPRC-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPRC-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPRC-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STOP YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPRC-BRANCH-CD
                   ADD ARPRC-STOP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPRC-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPRC-STOP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2570-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARPRC-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPRC-FEE-YTD NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO
                       ARPRC-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPRC-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPRC-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2580-COUNT-EXCEPT.
           IF ARPRC-EXCEPT-CNT < 9999999
               ADD 1 TO ARPRC-EXCEPT-CNT
           END-IF
           IF ARPRC-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO ARPRC-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ARPRC-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPRC-STOP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2600-DESCRIBE-CHANNEL.
           EVALUATE ARPRC-CHANNEL-CD
               WHEN 'ME'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STOP YTD FEE ON THE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2610-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPRC-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPRC-STOP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPRC-HOLD-FLAG = 'Y'
                   COMPUTE ARPRC-STOP-YTD ROUNDED = ARPRC-FEE-YTD *
                       ARPRC-DISCOUNT / 200
               WHEN ARPRC-BRANCH-CD = 'FN'
                   MOVE 5.00 TO ARPRC-STOP-YTD
               WHEN ARPRC-BRANCH-CD = 'GM'
                   MOVE 20.00 TO ARPRC-STOP-YTD
               WHEN OTHER
                   COMPUTE ARPRC-STOP-YTD ROUNDED = ARPRC-FEE-YTD *
                       ARPRC-DISCOUNT / 100
           END-EVALUATE
           IF ARPRC-STOP-YTD > 0 AND ARPRC-STOP-YTD < 1.00
               MOVE 1.00 TO ARPRC-STOP-YTD
           END-IF
           IF ARPRC-STOP-YTD > 250.00
               MOVE 250.00 TO ARPRC-STOP-YTD
           END-IF
           ADD ARPRC-STOP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2620-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPRC-ISSUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARPRC-ISSUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ARPRC-ISSUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ARPRC-ISSUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPRC-ISSUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2630-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPRC-OPEN-DATE > 0 AND ARPRC-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPRC-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPRC-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARPRC-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ARPRC-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPRC-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPRC-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2640-DESCRIBE-SOURCE.
           EVALUATE ARPRC-SOURCE-CD
               WHEN 'GW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2650-FLAG-OUTSTAND.
           IF ARPRC-OUTSTAND-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPRC-OUTSTAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STOP YTD.
      *----------------------------------------------------------------*
       2660-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPRC-STOP-YTD ROUNDED =
                   ARPRC-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPRC-STOP-YTD
           END-IF
           ADD ARPRC-STOP-YTD TO WS-TOTAL-PRORATED.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ARPRC-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ARPRC-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ARPRC-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ARPRC EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
