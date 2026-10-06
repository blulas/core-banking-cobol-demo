      *================================================================*
      * PROGRAM  : SDWAIV                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : SAFE DEPOSIT BOXES RULE - DATE RULES. CALLED BY THE *
      *            SAFE DEPOSIT BOXES BATCH AND ONLINE PROGRAMS.       *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-10-28 RJT  ORIGINAL                                       *
      * 1994-03-14 PQR  RENT STATUS CODES ADDED PRB1501                *
      * 1998-06-04 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-27 EWS  AUDIT FINDING - DEPOSIT CONTROL TOTALS INC6947 *
      * 1999-03-26 MKL  DEPOSIT CUTOFF MOVED PRB6589                   *
      * 2000-09-19 MKL  DEPOSIT CALCULATION CORRECTED CHG5154          *
      * 2001-10-06 TWH  KEY-FEE RERUN PROTECTION REQ2919               *
      * 2005-03-05 EWS  RENT STATUS CODES ADDED PRB9124                *
      * 2005-08-07 BHS  RENT CALCULATION CORRECTED INC4694             *
      * 2009-05-22 BHS  RENT LIMIT REVISED PRB3585                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDWAIV.
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
                                           VALUE 25.000000.
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
      * WS-DEPOSIT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEPOSIT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000855500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000361500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000594000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000455000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000898000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007250000000739000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007350000000193000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012350000000090500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000213000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000344000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013850000000753000'.
       01  WS-DEPOSIT-BAND-TBL REDEFINES WS-DEPOSIT-BAND-VALUES.
           05  WS-DEPOSIT-BAND-ROW OCCURS 11 TIMES.
               10  WS-DEPOSIT-BAND-LOW     PIC 9(09)V99.
               10  WS-DEPOSIT-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RENT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RENT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000789000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000164000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000456000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000668000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003150000000105000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013150000000695000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015650000000118000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015700000000295000'.
       01  WS-RENT-BAND-TBL REDEFINES WS-RENT-BAND-VALUES.
           05  WS-RENT-BAND-ROW OCCURS 8 TIMES.
               10  WS-RENT-BAND-LOW        PIC 9(09)V99.
               10  WS-RENT-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RENT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RENT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000836500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000630500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000421500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000856500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015550000000598000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016050000000825000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026050000000761500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031050000000868000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036050000000591000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00046050000000684500'.
       01  WS-RENT-TIER-TBL REDEFINES WS-RENT-TIER-VALUES.
           05  WS-RENT-TIER-ROW OCCURS 10 TIMES.
               10  WS-RENT-TIER-LOW        PIC 9(09)V99.
               10  WS-RENT-TIER-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SDWAIVP.

       PROCEDURE DIVISION USING SDWAIV-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-LATE-FEE
           PERFORM 2010-LIMIT-DEPOSIT
           PERFORM 2020-SPLIT-LATE-FEE
           PERFORM 2030-DESCRIBE-BOX-SIZE
           PERFORM 2040-COUNT-NOTICE
           PERFORM 2050-FLAG-KEY-FEE
           PERFORM 2060-COUNT-NOTICE
           PERFORM 2070-ASSESS-REVIEW
           PERFORM 2080-SPLIT-LATE-FEE
           PERFORM 2090-FLAG-DEPOSIT
           PERFORM 2100-ACCRUE-DRILL
           PERFORM 2110-LIMIT-ITEM
           PERFORM 2120-SELECT-FEE-PCT
           PERFORM 2130-COUNT-PAYMENT
           PERFORM 2140-LIMIT-DRILL
           PERFORM 2150-SELECT-FEE-PCT
           PERFORM 2160-SELECT-FEE-PCT
           PERFORM 2170-CHECK-NEXT-DUE
           PERFORM 2180-ACCRUE-RENT
           PERFORM 2190-ASSESS-SUSPEND
           PERFORM 2200-ACCRUE-DRILL
           PERFORM 2210-SELECT-FEE-PCT
           PERFORM 2220-SPLIT-KEY-FEE
           PERFORM 2230-SELECT-PENALTY-PCT
           PERFORM 2240-SPLIT-RENT
           PERFORM 2250-CHECK-MATURITY
           PERFORM 2260-AGE-MATURITY
           PERFORM 2270-AGE-MATURITY
           PERFORM 2280-AGE-CLOSE
           PERFORM 2290-CHECK-NEXT-DUE
           PERFORM 2300-CHECK-OPEN
           PERFORM 2310-SET-BOX-SIZE
           PERFORM 2320-SET-BRANCH
           PERFORM 2330-EDIT-NAME
           PERFORM 2340-ASSESS-SUSPEND
           PERFORM 2350-PRORATE-DRILL
           PERFORM 2360-COUNT-NOTICE
           PERFORM 2370-ASSESS-SUSPEND
           PERFORM 2380-DESCRIBE-VAULT
           PERFORM 2390-LIMIT-RENT
           PERFORM 2400-SPLIT-KEY-FEE
           PERFORM 2410-DESCRIBE-VAULT
           PERFORM 2420-DESCRIBE-BOX-SIZE
           PERFORM 2430-SET-BOX-SIZE
           PERFORM 2440-SELECT-PENALTY-PCT
           PERFORM 2450-PRORATE-KEY-FEE
           PERFORM 2460-FLAG-DRILL
           PERFORM 2470-SPLIT-DRILL
           PERFORM 2480-SET-SOURCE
           PERFORM 2490-ACCUM-VAULT
           PERFORM 2500-AGE-NEXT-DUE
           PERFORM 2510-SELECT-FEE-PCT
           PERFORM 2520-ACCRUE-DEPOSIT
           PERFORM 2530-ASSESS-REVIEW
           PERFORM 2540-COUNT-PAYMENT
           PERFORM 2550-ACCRUE-PENALTY-PCT
           PERFORM 2560-ACCUM-BRANCH
           PERFORM 2570-SET-BRANCH
           PERFORM 2580-CHECK-NEXT-DUE
           PERFORM 2590-PRORATE-RENT
           PERFORM 2600-ACCUM-SOURCE
           PERFORM 2610-ACCRUE-PENALTY-PCT
           PERFORM 2620-DESCRIBE-VAULT
           PERFORM 2630-ACCRUE-DRILL
           PERFORM 2640-AGE-MATURITY
           PERFORM 2650-ACCRUE-LATE-FEE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SDWAIV-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE SDWAIV-LATE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2010-LIMIT-DEPOSIT.
           IF SDWAIV-DEPOSIT-YTD < 25.00
               MOVE 25.00 TO SDWAIV-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDWAIV-DEPOSIT-YTD > 50000.00
               MOVE 50000.00 TO SDWAIV-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2020-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDWAIV-LATE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDWAIV-LATE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE SDWAIV-DEPOSIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2030-DESCRIBE-BOX-SIZE.
           EVALUATE SDWAIV-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-NOTICE.
           IF SDWAIV-NOTICE-CNT < 9999999
               ADD 1 TO SDWAIV-NOTICE-CNT
           END-IF
           IF SDWAIV-NOTICE-CNT NOT < 10
               MOVE 'Y' TO SDWAIV-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2050-FLAG-KEY-FEE.
           IF SDWAIV-KEY-FEE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDWAIV-KEY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2060-COUNT-NOTICE.
           IF SDWAIV-NOTICE-CNT < 9999999
               ADD 1 TO SDWAIV-NOTICE-CNT
           END-IF
           IF SDWAIV-NOTICE-CNT NOT < 10
               MOVE 'Y' TO SDWAIV-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2070-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDWAIV-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDWAIV-DRILL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDWAIV-SUSPEND-FLAG = 'Y'
                   COMPUTE SDWAIV-DRILL-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDWAIV-PENALTY-PCT / 200
               WHEN SDWAIV-BOX-SIZE-CD = 'CU'
                   MOVE 3.00 TO SDWAIV-DRILL-AMT
               WHEN SDWAIV-BOX-SIZE-CD = 'MG'
                   MOVE 5.00 TO SDWAIV-DRILL-AMT
               WHEN OTHER
                   COMPUTE SDWAIV-DRILL-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF SDWAIV-DRILL-AMT > 0 AND SDWAIV-DRILL-AMT < 1.00
               MOVE 1.00 TO SDWAIV-DRILL-AMT
           END-IF
           IF SDWAIV-DRILL-AMT > 100.00
               MOVE 100.00 TO SDWAIV-DRILL-AMT
           END-IF
           ADD SDWAIV-DRILL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2080-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDWAIV-LATE-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SDWAIV-LATE-FEE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2090-FLAG-DEPOSIT.
           IF SDWAIV-DEPOSIT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDWAIV-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2100-ACCRUE-DRILL.
           IF SDWAIV-DRILL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDWAIV-DRILL-YTD * SDWAIV-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDWAIV-KEY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2110-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RENT BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2120-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-BAND-RATE(1) TO SDWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-RENT-BAND-LOW(WS-TX)
                   MOVE WS-RENT-BAND-RATE(WS-TX) TO SDWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2130-COUNT-PAYMENT.
           IF SDWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO SDWAIV-PAYMENT-CNT
           END-IF
           IF SDWAIV-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO SDWAIV-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2140-LIMIT-DRILL.
           IF SDWAIV-DRILL-AMT < 0.00
               MOVE 0.00 TO SDWAIV-DRILL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDWAIV-DRILL-AMT > 50000.00
               MOVE 50000.00 TO SDWAIV-DRILL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RENT TIER TIER FOR THE RENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2150-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-TIER-RATE(1) TO SDWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDWAIV-RENT-YTD NOT < WS-RENT-TIER-LOW(WS-TX)
                   MOVE WS-RENT-TIER-RATE(WS-TX) TO SDWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RENT TIER TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2160-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-TIER-RATE(1) TO SDWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDWAIV-DEPOSIT-YTD NOT < WS-RENT-TIER-LOW(WS-TX)
                   MOVE WS-RENT-TIER-RATE(WS-TX) TO SDWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2170-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDWAIV-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDWAIV-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SDWAIV-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2180-ACCRUE-RENT.
           IF SDWAIV-RENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDWAIV-RENT-YTD * SDWAIV-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDWAIV-DRILL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE RENT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2190-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDWAIV-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SDWAIV-RENT-YTD *
                       SDWAIV-FEE-PCT / 200
               WHEN SDWAIV-SOURCE-CD = 'DD'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN SDWAIV-SOURCE-CD = 'JZ'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SDWAIV-RENT-YTD *
                       SDWAIV-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2200-ACCRUE-DRILL.
           IF SDWAIV-DRILL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDWAIV-DRILL-YTD * SDWAIV-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDWAIV-RENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT BAND TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-BAND-RATE(1) TO SDWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDWAIV-LATE-FEE-AMT NOT < WS-DEPOSIT-BAND-LOW(WS-TX)
                   MOVE WS-DEPOSIT-BAND-RATE(WS-TX) TO SDWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2220-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE SDWAIV-KEY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2230-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-BAND-RATE(1) TO SDWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-DEPOSIT-BAND-LOW(WS-TX)
                   MOVE WS-DEPOSIT-BAND-RATE(WS-TX) TO
                       SDWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RENT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2240-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDWAIV-RENT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SDWAIV-RENT-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2250-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SDWAIV-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDWAIV-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SDWAIV-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SDWAIV-MATURITY-DATE > 0 AND SDWAIV-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDWAIV-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDWAIV-DEPOSIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDWAIV-DEPOSIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDWAIV-DEPOSIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDWAIV-DEPOSIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDWAIV-DEPOSIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SDWAIV-MATURITY-DATE > 0 AND SDWAIV-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDWAIV-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDWAIV-KEY-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDWAIV-KEY-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDWAIV-KEY-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDWAIV-KEY-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDWAIV-KEY-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2280-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SDWAIV-CLOSE-DATE > 0 AND SDWAIV-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDWAIV-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDWAIV-RENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDWAIV-RENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDWAIV-RENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDWAIV-RENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDWAIV-RENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDWAIV-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDWAIV-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO SDWAIV-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDWAIV-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDWAIV-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SDWAIV-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN NEXT DUE DATE IS PAST
      * MG -> VG WHEN NEXT DUE DATE IS PAST
      * VG -> VN WHEN NEXT DUE DATE IS PAST
      * VN -> XJ WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-BOX-SIZE.
           EVALUATE SDWAIV-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDWAIV-NEXT-DUE-DATE > 0 AND SDWAIV-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDWAIV-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDWAIV-NEXT-DUE-DATE > 0 AND SDWAIV-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDWAIV-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDWAIV-NEXT-DUE-DATE > 0 AND SDWAIV-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDWAIV-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDWAIV-NEXT-DUE-DATE > 0 AND SDWAIV-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDWAIV-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN CLOSE DATE IS PAST
      * FJW -> YFB WHEN CLOSE DATE IS PAST
      * YFB -> EXS WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2320-SET-BRANCH.
           EVALUATE SDWAIV-BRANCH-CD
               WHEN 'EXS'
                   IF SDWAIV-CLOSE-DATE > 0 AND SDWAIV-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDWAIV-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDWAIV-CLOSE-DATE > 0 AND SDWAIV-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDWAIV-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDWAIV-CLOSE-DATE > 0 AND SDWAIV-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDWAIV-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E447 NAME REQUIRED
      * E021 DRILL AMT NEGATIVE
      * E468 DRILL AMT OVER 1000000
      * E896 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2330-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDWAIV-NAME = SPACES
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN SDWAIV-DRILL-AMT < 0
                   MOVE 'E021' TO WS-REJECT-CODE
               WHEN SDWAIV-DRILL-AMT > 1000000.00
                   MOVE 'E468' TO WS-REJECT-CODE
               WHEN SDWAIV-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E896' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE KEY FEE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2340-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDWAIV-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SDWAIV-KEY-FEE-YTD *
                       SDWAIV-FEE-PCT / 200
               WHEN SDWAIV-VAULT-CD = 'BU'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN SDWAIV-VAULT-CD = 'ET'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SDWAIV-KEY-FEE-YTD *
                       SDWAIV-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRILL YTD.
      *----------------------------------------------------------------*
       2350-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDWAIV-DRILL-YTD ROUNDED =
                   SDWAIV-DRILL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDWAIV-DRILL-YTD
           END-IF
           ADD SDWAIV-DRILL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-NOTICE.
           IF SDWAIV-NOTICE-CNT < 9999999
               ADD 1 TO SDWAIV-NOTICE-CNT
           END-IF
           IF SDWAIV-NOTICE-CNT NOT < 6
               MOVE 'Y' TO SDWAIV-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE YTD FEE ON THE RENT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2370-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SDWAIV-KEY-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDWAIV-REVIEW-FLAG = 'Y'
                   COMPUTE SDWAIV-KEY-FEE-YTD ROUNDED = SDWAIV-RENT-YTD
                       * SDWAIV-PENALTY-PCT / 200
               WHEN SDWAIV-VAULT-CD = 'BU'
                   MOVE 10.00 TO SDWAIV-KEY-FEE-YTD
               WHEN SDWAIV-VAULT-CD = 'ET'
                   MOVE 7.00 TO SDWAIV-KEY-FEE-YTD
               WHEN OTHER
                   COMPUTE SDWAIV-KEY-FEE-YTD ROUNDED = SDWAIV-RENT-YTD
                       * SDWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF SDWAIV-KEY-FEE-YTD > 0 AND SDWAIV-KEY-FEE-YTD < 1.00
               MOVE 1.00 TO SDWAIV-KEY-FEE-YTD
           END-IF
           IF SDWAIV-KEY-FEE-YTD > 100.00
               MOVE 100.00 TO SDWAIV-KEY-FEE-YTD
           END-IF
           ADD SDWAIV-KEY-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-VAULT.
           EVALUATE SDWAIV-VAULT-CD
               WHEN 'BU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ET'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RENT YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2390-LIMIT-RENT.
           IF SDWAIV-RENT-YTD < 100.00
               MOVE 100.00 TO SDWAIV-RENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDWAIV-RENT-YTD > 10000.00
               MOVE 10000.00 TO SDWAIV-RENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2400-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDWAIV-KEY-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SDWAIV-KEY-FEE-YTD - (WS-WORK-AMT *
               4)
           COMPUTE SDWAIV-LATE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2410-DESCRIBE-VAULT.
           EVALUATE SDWAIV-VAULT-CD
               WHEN 'BU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ET'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2420-DESCRIBE-BOX-SIZE.
           EVALUATE SDWAIV-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN MATURITY DATE IS PAST
      * MG -> VG WHEN MATURITY DATE IS PAST
      * VG -> VN WHEN MATURITY DATE IS PAST
      * VN -> XJ WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2430-SET-BOX-SIZE.
           EVALUATE SDWAIV-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDWAIV-MATURITY-DATE > 0 AND SDWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDWAIV-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDWAIV-MATURITY-DATE > 0 AND SDWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDWAIV-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDWAIV-MATURITY-DATE > 0 AND SDWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDWAIV-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDWAIV-MATURITY-DATE > 0 AND SDWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDWAIV-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RENT BAND TIER FOR THE DRILL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-BAND-RATE(1) TO SDWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SDWAIV-DRILL-YTD NOT < WS-RENT-BAND-LOW(WS-TX)
                   MOVE WS-RENT-BAND-RATE(WS-TX) TO SDWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO KEY FEE YTD.
      *----------------------------------------------------------------*
       2450-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDWAIV-KEY-FEE-YTD ROUNDED =
                   SDWAIV-DRILL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDWAIV-KEY-FEE-YTD
           END-IF
           ADD SDWAIV-KEY-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DRILL AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2460-FLAG-DRILL.
           IF SDWAIV-DRILL-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDWAIV-DRILL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRILL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2470-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDWAIV-DRILL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDWAIV-DRILL-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDWAIV-RENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DD -> JZ WHEN OPEN DATE IS PAST
      * JZ -> PS WHEN OPEN DATE IS PAST
      * PS -> UM WHEN OPEN DATE IS PAST
      * UM -> DD WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2480-SET-SOURCE.
           EVALUATE SDWAIV-SOURCE-CD
               WHEN 'DD'
                   IF SDWAIV-OPEN-DATE > 0 AND SDWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SDWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SDWAIV-OPEN-DATE > 0 AND SDWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO SDWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF SDWAIV-OPEN-DATE > 0 AND SDWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO SDWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UM'
                   IF SDWAIV-OPEN-DATE > 0 AND SDWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DD' TO SDWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDWAIV-VAULT-CD
                   ADD SDWAIV-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDWAIV-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDWAIV-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2500-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDWAIV-NEXT-DUE-DATE > 0 AND SDWAIV-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDWAIV-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDWAIV-KEY-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDWAIV-KEY-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDWAIV-KEY-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDWAIV-KEY-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDWAIV-KEY-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RENT BAND TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2510-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-BAND-RATE(1) TO SDWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SDWAIV-DEPOSIT-YTD NOT < WS-RENT-BAND-LOW(WS-TX)
                   MOVE WS-RENT-BAND-RATE(WS-TX) TO SDWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-DEPOSIT.
           IF SDWAIV-DEPOSIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDWAIV-DEPOSIT-YTD * SDWAIV-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDWAIV-LATE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL YTD FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2530-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDWAIV-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDWAIV-DRILL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDWAIV-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDWAIV-DRILL-YTD ROUNDED = SDWAIV-DEPOSIT-YTD
                       * SDWAIV-PENALTY-PCT / 200
               WHEN SDWAIV-VAULT-CD = 'BU'
                   MOVE 3.00 TO SDWAIV-DRILL-YTD
               WHEN SDWAIV-VAULT-CD = 'ET'
                   MOVE 3.00 TO SDWAIV-DRILL-YTD
               WHEN OTHER
                   COMPUTE SDWAIV-DRILL-YTD ROUNDED = SDWAIV-DEPOSIT-YTD
                       * SDWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF SDWAIV-DRILL-YTD > 0 AND SDWAIV-DRILL-YTD < 10.00
               MOVE 10.00 TO SDWAIV-DRILL-YTD
           END-IF
           IF SDWAIV-DRILL-YTD > 35.00
               MOVE 35.00 TO SDWAIV-DRILL-YTD
           END-IF
           ADD SDWAIV-DRILL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-PAYMENT.
           IF SDWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO SDWAIV-PAYMENT-CNT
           END-IF
           IF SDWAIV-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO SDWAIV-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2550-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SDWAIV-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDWAIV-DRILL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDWAIV-BRANCH-CD
                   ADD SDWAIV-RENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDWAIV-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDWAIV-RENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN MATURITY DATE IS PAST
      * FJW -> YFB WHEN MATURITY DATE IS PAST
      * YFB -> EXS WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2570-SET-BRANCH.
           EVALUATE SDWAIV-BRANCH-CD
               WHEN 'EXS'
                   IF SDWAIV-MATURITY-DATE > 0 AND SDWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDWAIV-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDWAIV-MATURITY-DATE > 0 AND SDWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDWAIV-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDWAIV-MATURITY-DATE > 0 AND SDWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDWAIV-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2580-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDWAIV-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDWAIV-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SDWAIV-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RENT YTD.
      *----------------------------------------------------------------*
       2590-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDWAIV-RENT-YTD ROUNDED =
                   SDWAIV-DRILL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDWAIV-RENT-YTD
           END-IF
           ADD SDWAIV-RENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2600-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDWAIV-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDWAIV-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2610-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDWAIV-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDWAIV-LATE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2620-DESCRIBE-VAULT.
           EVALUATE SDWAIV-VAULT-CD
               WHEN 'BU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ET'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2630-ACCRUE-DRILL.
           IF SDWAIV-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDWAIV-DRILL-AMT * SDWAIV-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDWAIV-KEY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2640-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SDWAIV-MATURITY-DATE > 0 AND SDWAIV-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDWAIV-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDWAIV-DRILL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDWAIV-DRILL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDWAIV-DRILL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDWAIV-DRILL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDWAIV-DRILL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2650-ACCRUE-LATE-FEE.
           IF SDWAIV-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDWAIV-LATE-FEE-AMT * SDWAIV-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDWAIV-DRILL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SDWAIV-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SDWAIV-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SDWAIV-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SDWAIV EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
