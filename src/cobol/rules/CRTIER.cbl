      *================================================================*
      * PROGRAM  : CRTIER                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PROPRIETARY CREDIT CARD RULE - STATUS. CALLED BY THE*
      *            PROPRIETARY CREDIT CARD BATCH AND ONLINE PROGRAMS.  *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-09-19 EWS  ORIGINAL                                       *
      * 1997-08-26 CRP  NEW LIMIT REPORT COLUMNS INC5898               *
      * 1998-06-27 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-05-24 JLM  LIMIT LIMIT REVISED REQ6410                    *
      * 2000-11-14 PQR  LATE-FEE TABLE EXPANDED CHG7008                *
      * 2000-09-12 JLM  LIMIT TABLE EXPANDED PRB8495                   *
      * 2001-08-17 JLM  NEW PAYMENT REPORT COLUMNS REQ5707             *
      * 2002-05-03 GAF  LATE-FEE EXCEPTIONS TO OPS QUEUE PRB6964       *
      * 2003-09-24 RJT  LATE-FEE ROUNDING - HALF UP REQ2976            *
      * 2005-01-05 CRP  CASH-ADV RERUN PROTECTION INC2705              *
      * 2006-08-18 DPS  FINANCE CALCULATION CORRECTED INC1320          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRTIER.
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
      * WS-PAYMENT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000742500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000196000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000650000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000492500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000358000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010650000000126000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020650000000126500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021150000000680000'.
       01  WS-PAYMENT-GRID-TBL REDEFINES WS-PAYMENT-GRID-VALUES.
           05  WS-PAYMENT-GRID-ROW OCCURS 8 TIMES.
               10  WS-PAYMENT-GRID-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FINANCE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FINANCE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000296500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000546500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000468000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000559000'.
       01  WS-FINANCE-SCHED-TBL REDEFINES WS-FINANCE-SCHED-VALUES.
           05  WS-FINANCE-SCHED-ROW OCCURS 4 TIMES.
               10  WS-FINANCE-SCHED-LOW    PIC 9(09)V99.
               10  WS-FINANCE-SCHED-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CRTIERP.

       PROCEDURE DIVISION USING CRTIER-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-FINANCE
           PERFORM 2010-PRORATE-PAYMENT
           PERFORM 2020-LIMIT-LATE-FEE
           PERFORM 2030-FLAG-ITEM
           PERFORM 2040-LIMIT-LATE-FEE
           PERFORM 2050-LIMIT-CASH-ADV
           PERFORM 2060-AGE-MATURITY
           PERFORM 2070-LIMIT-CASH-ADV
           PERFORM 2080-ASSESS-PREFERRED
           PERFORM 2090-ASSESS-AUTO-RENEW
           PERFORM 2100-SELECT-DISCOUNT
           PERFORM 2110-ACCUM-CARD-TYPE
           PERFORM 2120-DESCRIBE-CARD-TYPE
           PERFORM 2130-PRORATE-LATE-FEE
           PERFORM 2140-SPLIT-PAYMENT
           PERFORM 2150-ASSESS-AUTO-RENEW
           PERFORM 2160-FLAG-LATE-FEE
           PERFORM 2170-ACCRUE-FINANCE
           PERFORM 2180-COUNT-CYCLE
           PERFORM 2190-PRORATE-LIMIT
           PERFORM 2200-EDIT-NAME
           PERFORM 2210-DESCRIBE-STATUS
           PERFORM 2220-ACCUM-STATUS
           PERFORM 2230-EDIT-NAME
           PERFORM 2240-LIMIT-LATE-FEE
           PERFORM 2250-COUNT-RETRY
           PERFORM 2260-ACCUM-BRANCH
           PERFORM 2270-PRORATE-LATE-FEE
           PERFORM 2280-ACCUM-CARD-TYPE
           PERFORM 2290-SELECT-DISCOUNT
           PERFORM 2300-SET-STATUS
           PERFORM 2310-LIMIT-LATE-FEE
           PERFORM 2320-SPLIT-LATE-FEE
           PERFORM 2330-ACCUM-CARD-TYPE
           PERFORM 2340-EDIT-DESC
           PERFORM 2350-CHECK-LAST-PAY
           PERFORM 2360-DESCRIBE-CARD-TYPE
           PERFORM 2370-ACCUM-STATUS
           PERFORM 2380-FLAG-CASH-ADV
           PERFORM 2390-AGE-OPEN
           PERFORM 2400-AGE-LAST-PAY
           PERFORM 2410-DESCRIBE-STATUS
           PERFORM 2420-SET-STATUS
           PERFORM 2430-PRORATE-LATE-FEE
           PERFORM 2440-PRORATE-LATE-FEE
           PERFORM 2450-EDIT-DESC
           PERFORM 2460-ASSESS-PREFERRED
           PERFORM 2470-SPLIT-FINANCE
           PERFORM 2480-SET-STATUS
           PERFORM 2490-DESCRIBE-SOURCE
           PERFORM 2500-ACCUM-BRANCH
           PERFORM 2510-ASSESS-AUTO-RENEW
           PERFORM 2520-CHECK-LAST-PAY
           PERFORM 2530-PRORATE-PAYMENT
           PERFORM 2540-COUNT-CYCLE
           PERFORM 2550-COUNT-CYCLE
           PERFORM 2560-COUNT-RETRY
           PERFORM 2570-SELECT-DISCOUNT
           PERFORM 2580-COUNT-CYCLE
           PERFORM 2590-ACCUM-SOURCE
           PERFORM 2600-ACCRUE-LATE-FEE
           PERFORM 2610-SPLIT-LIMIT
           PERFORM 2620-SET-STATUS
           PERFORM 2630-COUNT-RETRY
           PERFORM 2640-COUNT-CYCLE
           PERFORM 2650-COUNT-CYCLE
           PERFORM 2660-FLAG-CASH-ADV
           PERFORM 2670-ASSESS-PREFERRED
           PERFORM 2680-CHECK-MATURITY
           PERFORM 2690-SET-STATUS
           PERFORM 2700-ASSESS-PREFERRED
           PERFORM 2710-ACCUM-STATUS
           PERFORM 2720-SET-CARD-TYPE
           PERFORM 2730-EDIT-NAME
           PERFORM 2740-CHECK-OPEN
           PERFORM 2750-LIMIT-LATE-FEE
           PERFORM 2760-SPLIT-PAYMENT
           PERFORM 2770-ACCRUE-CASH-ADV
           PERFORM 2780-COUNT-RETRY
           PERFORM 2790-PRORATE-PAYMENT
           PERFORM 2800-SPLIT-LIMIT
           PERFORM 2810-SPLIT-LATE-FEE
           PERFORM 2820-PRORATE-CASH-ADV
           PERFORM 2830-ACCRUE-PENALTY-PCT
           PERFORM 2840-ASSESS-PREFERRED
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CRTIER-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-FINANCE.
           IF CRTIER-FINANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRTIER-FINANCE-YTD * CRTIER-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FINANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       2010-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRTIER-PAYMENT-AMT ROUNDED =
                   CRTIER-FINANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRTIER-PAYMENT-AMT
           END-IF
           ADD CRTIER-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LATE FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2020-LIMIT-LATE-FEE.
           IF CRTIER-LATE-FEE-AMT < 25.00
               MOVE 25.00 TO CRTIER-LATE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRTIER-LATE-FEE-AMT > 1000.00
               MOVE 1000.00 TO CRTIER-LATE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2030-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2040-LIMIT-LATE-FEE.
           IF CRTIER-LATE-FEE-YTD < 25.00
               MOVE 25.00 TO CRTIER-LATE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRTIER-LATE-FEE-YTD > 1000.00
               MOVE 1000.00 TO CRTIER-LATE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2050-LIMIT-CASH-ADV.
           IF CRTIER-CASH-ADV-YTD < 50.00
               MOVE 50.00 TO CRTIER-CASH-ADV-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRTIER-CASH-ADV-YTD > 2500.00
               MOVE 2500.00 TO CRTIER-CASH-ADV-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRTIER-MATURITY-DATE > 0 AND CRTIER-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRTIER-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH ADV YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2070-LIMIT-CASH-ADV.
           IF CRTIER-CASH-ADV-YTD < 10.00
               MOVE 10.00 TO CRTIER-CASH-ADV-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRTIER-CASH-ADV-YTD > 1000.00
               MOVE 1000.00 TO CRTIER-CASH-ADV-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE CASH ADV YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2080-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRTIER-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRTIER-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRTIER-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRTIER-PAYMENT-AMT ROUNDED =
                       CRTIER-CASH-ADV-YTD * CRTIER-DISCOUNT / 200
               WHEN CRTIER-CARD-TYPE-CD = 'PGU'
                   MOVE 5.00 TO CRTIER-PAYMENT-AMT
               WHEN CRTIER-CARD-TYPE-CD = 'TRN'
                   MOVE 12.00 TO CRTIER-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRTIER-PAYMENT-AMT ROUNDED =
                       CRTIER-CASH-ADV-YTD * CRTIER-DISCOUNT / 100
           END-EVALUATE
           IF CRTIER-PAYMENT-AMT > 0 AND CRTIER-PAYMENT-AMT < 10.00
               MOVE 10.00 TO CRTIER-PAYMENT-AMT
           END-IF
           IF CRTIER-PAYMENT-AMT > 250.00
               MOVE 250.00 TO CRTIER-PAYMENT-AMT
           END-IF
           ADD CRTIER-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE LATE FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2090-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRTIER-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRTIER-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRTIER-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRTIER-PAYMENT-AMT ROUNDED =
                       CRTIER-LATE-FEE-YTD * CRTIER-DISCOUNT / 200
               WHEN CRTIER-SOURCE-CD = 'C'
                   MOVE 3.00 TO CRTIER-PAYMENT-AMT
               WHEN CRTIER-SOURCE-CD = 'H'
                   MOVE 3.00 TO CRTIER-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRTIER-PAYMENT-AMT ROUNDED =
                       CRTIER-LATE-FEE-YTD * CRTIER-DISCOUNT / 100
           END-EVALUATE
           IF CRTIER-PAYMENT-AMT > 0 AND CRTIER-PAYMENT-AMT < 2.00
               MOVE 2.00 TO CRTIER-PAYMENT-AMT
           END-IF
           IF CRTIER-PAYMENT-AMT > 75.00
               MOVE 75.00 TO CRTIER-PAYMENT-AMT
           END-IF
           ADD CRTIER-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2100-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO CRTIER-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO CRTIER-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRTIER-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRTIER-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRTIER-CARD-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRTIER-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2120-DESCRIBE-CARD-TYPE.
           EVALUATE CRTIER-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LATE FEE YTD.
      *----------------------------------------------------------------*
       2130-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRTIER-LATE-FEE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRTIER-LATE-FEE-YTD
           END-IF
           ADD CRTIER-LATE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2140-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = CRTIER-PAYMENT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRTIER-PAYMENT-AMT - (WS-WORK-AMT *
               2)
           COMPUTE CRTIER-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE LATE FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2150-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRTIER-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRTIER-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRTIER-PREFERRED-FLAG = 'Y'
                   COMPUTE CRTIER-PAYMENT-AMT ROUNDED =
                       CRTIER-LATE-FEE-YTD * CRTIER-DISCOUNT / 200
               WHEN CRTIER-SOURCE-CD = 'C'
                   MOVE 15.00 TO CRTIER-PAYMENT-AMT
               WHEN CRTIER-SOURCE-CD = 'H'
                   MOVE 15.00 TO CRTIER-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRTIER-PAYMENT-AMT ROUNDED =
                       CRTIER-LATE-FEE-YTD * CRTIER-DISCOUNT / 100
           END-EVALUATE
           IF CRTIER-PAYMENT-AMT > 0 AND CRTIER-PAYMENT-AMT < 10.00
               MOVE 10.00 TO CRTIER-PAYMENT-AMT
           END-IF
           IF CRTIER-PAYMENT-AMT > 25.00
               MOVE 25.00 TO CRTIER-PAYMENT-AMT
           END-IF
           ADD CRTIER-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * LATE FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2160-FLAG-LATE-FEE.
           IF CRTIER-LATE-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRTIER-LATE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2170-ACCRUE-FINANCE.
           IF CRTIER-FINANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRTIER-FINANCE-YTD * CRTIER-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRTIER-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2180-COUNT-CYCLE.
           IF CRTIER-CYCLE-CNT < 9999999
               ADD 1 TO CRTIER-CYCLE-CNT
           END-IF
           IF CRTIER-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CRTIER-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FINANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       2190-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRTIER-LIMIT-AMT ROUNDED =
                   CRTIER-FINANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRTIER-LIMIT-AMT
           END-IF
           ADD CRTIER-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E179 NAME REQUIRED
      * E716 PAYMENT AMT NEGATIVE
      * E324 PAYMENT AMT OVER 10000
      * E708 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2200-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRTIER-NAME = SPACES
                   MOVE 'E179' TO WS-REJECT-CODE
               WHEN CRTIER-PAYMENT-AMT < 0
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN CRTIER-PAYMENT-AMT > 10000.00
                   MOVE 'E324' TO WS-REJECT-CODE
               WHEN CRTIER-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E708' TO WS-REJECT-CODE
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
       2210-DESCRIBE-STATUS.
           EVALUATE CRTIER-STATUS-CD
               WHEN 'BAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FINANCE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2220-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRTIER-STATUS-CD
                   ADD CRTIER-FINANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRTIER-FINANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E380 NAME REQUIRED
      * E919 LATE FEE AMT NEGATIVE
      * E768 LATE FEE AMT OVER 10000
      * E682 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRTIER-NAME = SPACES
                   MOVE 'E380' TO WS-REJECT-CODE
               WHEN CRTIER-LATE-FEE-AMT < 0
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN CRTIER-LATE-FEE-AMT > 10000.00
                   MOVE 'E768' TO WS-REJECT-CODE
               WHEN CRTIER-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E682' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2240-LIMIT-LATE-FEE.
           IF CRTIER-LATE-FEE-AMT < 10.00
               MOVE 10.00 TO CRTIER-LATE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRTIER-LATE-FEE-AMT > 1000.00
               MOVE 1000.00 TO CRTIER-LATE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2250-COUNT-RETRY.
           IF CRTIER-RETRY-CNT < 9999999
               ADD 1 TO CRTIER-RETRY-CNT
           END-IF
           IF CRTIER-RETRY-CNT NOT < 10
               MOVE 'Y' TO CRTIER-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FINANCE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2260-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRTIER-BRANCH-CD
                   ADD CRTIER-FINANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRTIER-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRTIER-FINANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2270-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRTIER-LATE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY CARD TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2280-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRTIER-CARD-TYPE-CD
                   ADD CRTIER-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRTIER-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRTIER-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2290-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO CRTIER-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CRTIER-LATE-FEE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO CRTIER-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRTIER-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRTIER-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN LAST PAY DATE IS PAST
      * DDY -> DPB WHEN LAST PAY DATE IS PAST
      * DPB -> UHF WHEN LAST PAY DATE IS PAST
      * UHF -> WLL WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2300-SET-STATUS.
           EVALUATE CRTIER-STATUS-CD
               WHEN 'BAL'
                   IF CRTIER-LAST-PAY-DATE > 0 AND CRTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRTIER-LAST-PAY-DATE > 0 AND CRTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRTIER-LAST-PAY-DATE > 0 AND CRTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRTIER-LAST-PAY-DATE > 0 AND CRTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LATE FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2310-LIMIT-LATE-FEE.
           IF CRTIER-LATE-FEE-AMT < 10.00
               MOVE 10.00 TO CRTIER-LATE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRTIER-LATE-FEE-AMT > 500.00
               MOVE 500.00 TO CRTIER-LATE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRTIER-LATE-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRTIER-LATE-FEE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE CRTIER-CASH-ADV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2330-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRTIER-CARD-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRTIER-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E996 DESC REQUIRED
      * E749 LIMIT AMT NEGATIVE
      * E569 LIMIT AMT OVER 50000
      * E825 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRTIER-DESC = SPACES
                   MOVE 'E996' TO WS-REJECT-CODE
               WHEN CRTIER-LIMIT-AMT < 0
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN CRTIER-LIMIT-AMT > 50000.00
                   MOVE 'E569' TO WS-REJECT-CODE
               WHEN CRTIER-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E825' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRTIER-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRTIER-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CRTIER-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2360-DESCRIBE-CARD-TYPE.
           EVALUATE CRTIER-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2370-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRTIER-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2380-FLAG-CASH-ADV.
           IF CRTIER-CASH-ADV-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRTIER-CASH-ADV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRTIER-OPEN-DATE > 0 AND CRTIER-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2400-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRTIER-LAST-PAY-DATE > 0 AND CRTIER-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRTIER-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRTIER-CASH-ADV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2410-DESCRIBE-STATUS.
           EVALUATE CRTIER-STATUS-CD
               WHEN 'BAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN MATURITY DATE IS PAST
      * DDY -> DPB WHEN MATURITY DATE IS PAST
      * DPB -> UHF WHEN MATURITY DATE IS PAST
      * UHF -> WLL WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2420-SET-STATUS.
           EVALUATE CRTIER-STATUS-CD
               WHEN 'BAL'
                   IF CRTIER-MATURITY-DATE > 0 AND CRTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRTIER-MATURITY-DATE > 0 AND CRTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRTIER-MATURITY-DATE > 0 AND CRTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRTIER-MATURITY-DATE > 0 AND CRTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2430-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRTIER-LATE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LATE FEE AMT.
      *----------------------------------------------------------------*
       2440-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRTIER-LATE-FEE-AMT ROUNDED =
                   CRTIER-LATE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRTIER-LATE-FEE-AMT
           END-IF
           ADD CRTIER-LATE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E253 DESC REQUIRED
      * E167 PAYMENT AMT NEGATIVE
      * E324 PAYMENT AMT OVER 10000
      * E870 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2450-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRTIER-DESC = SPACES
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN CRTIER-PAYMENT-AMT < 0
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN CRTIER-PAYMENT-AMT > 10000.00
                   MOVE 'E324' TO WS-REJECT-CODE
               WHEN CRTIER-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FINANCE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2460-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRTIER-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRTIER-FINANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRTIER-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRTIER-FINANCE-YTD ROUNDED = WS-WORK-AMT-2 *
                       CRTIER-PENALTY-PCT / 200
               WHEN CRTIER-BRANCH-CD = 'CTB'
                   MOVE 20.00 TO CRTIER-FINANCE-YTD
               WHEN CRTIER-BRANCH-CD = 'LNX'
                   MOVE 15.00 TO CRTIER-FINANCE-YTD
               WHEN OTHER
                   COMPUTE CRTIER-FINANCE-YTD ROUNDED = WS-WORK-AMT-2 *
                       CRTIER-PENALTY-PCT / 100
           END-EVALUATE
           IF CRTIER-FINANCE-YTD > 0 AND CRTIER-FINANCE-YTD < 2.00
               MOVE 2.00 TO CRTIER-FINANCE-YTD
           END-IF
           IF CRTIER-FINANCE-YTD > 35.00
               MOVE 35.00 TO CRTIER-FINANCE-YTD
           END-IF
           ADD CRTIER-FINANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FINANCE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2470-SPLIT-FINANCE.
           COMPUTE WS-WORK-AMT = CRTIER-FINANCE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRTIER-FINANCE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE CRTIER-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN EXPIRY DATE IS PAST
      * DDY -> DPB WHEN EXPIRY DATE IS PAST
      * DPB -> UHF WHEN EXPIRY DATE IS PAST
      * UHF -> WLL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2480-SET-STATUS.
           EVALUATE CRTIER-STATUS-CD
               WHEN 'BAL'
                   IF CRTIER-EXPIRY-DATE > 0 AND CRTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRTIER-EXPIRY-DATE > 0 AND CRTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRTIER-EXPIRY-DATE > 0 AND CRTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRTIER-EXPIRY-DATE > 0 AND CRTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2490-DESCRIBE-SOURCE.
           EVALUATE CRTIER-SOURCE-CD
               WHEN 'C'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRTIER-BRANCH-CD
                   ADD CRTIER-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRTIER-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRTIER-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE LATE FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2510-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRTIER-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRTIER-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRTIER-LATE-FEE-YTD *
                       CRTIER-DISCOUNT / 200
               WHEN CRTIER-STATUS-CD = 'BAL'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN CRTIER-STATUS-CD = 'DDY'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRTIER-LATE-FEE-YTD *
                       CRTIER-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2520-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRTIER-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRTIER-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CRTIER-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2530-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRTIER-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-CYCLE.
           IF CRTIER-CYCLE-CNT < 9999999
               ADD 1 TO CRTIER-CYCLE-CNT
           END-IF
           IF CRTIER-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CRTIER-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2550-COUNT-CYCLE.
           IF CRTIER-CYCLE-CNT < 9999999
               ADD 1 TO CRTIER-CYCLE-CNT
           END-IF
           IF CRTIER-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CRTIER-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2560-COUNT-RETRY.
           IF CRTIER-RETRY-CNT < 9999999
               ADD 1 TO CRTIER-RETRY-CNT
           END-IF
           IF CRTIER-RETRY-CNT NOT < 10
               MOVE 'Y' TO CRTIER-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2570-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO CRTIER-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO CRTIER-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRTIER-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRTIER-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2580-COUNT-CYCLE.
           IF CRTIER-CYCLE-CNT < 9999999
               ADD 1 TO CRTIER-CYCLE-CNT
           END-IF
           IF CRTIER-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CRTIER-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2590-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRTIER-SOURCE-CD
                   ADD CRTIER-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRTIER-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRTIER-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2600-ACCRUE-LATE-FEE.
           IF CRTIER-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRTIER-LATE-FEE-AMT * CRTIER-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2610-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRTIER-LIMIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CRTIER-LIMIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN LAST PAY DATE IS PAST
      * DDY -> DPB WHEN LAST PAY DATE IS PAST
      * DPB -> UHF WHEN LAST PAY DATE IS PAST
      * UHF -> WLL WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2620-SET-STATUS.
           EVALUATE CRTIER-STATUS-CD
               WHEN 'BAL'
                   IF CRTIER-LAST-PAY-DATE > 0 AND CRTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRTIER-LAST-PAY-DATE > 0 AND CRTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRTIER-LAST-PAY-DATE > 0 AND CRTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRTIER-LAST-PAY-DATE > 0 AND CRTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2630-COUNT-RETRY.
           IF CRTIER-RETRY-CNT < 9999999
               ADD 1 TO CRTIER-RETRY-CNT
           END-IF
           IF CRTIER-RETRY-CNT NOT < 6
               MOVE 'Y' TO CRTIER-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2640-COUNT-CYCLE.
           IF CRTIER-CYCLE-CNT < 9999999
               ADD 1 TO CRTIER-CYCLE-CNT
           END-IF
           IF CRTIER-CYCLE-CNT NOT < 3
               MOVE 'Y' TO CRTIER-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2650-COUNT-CYCLE.
           IF CRTIER-CYCLE-CNT < 9999999
               ADD 1 TO CRTIER-CYCLE-CNT
           END-IF
           IF CRTIER-CYCLE-CNT NOT < 5
               MOVE 'Y' TO CRTIER-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2660-FLAG-CASH-ADV.
           IF CRTIER-CASH-ADV-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRTIER-CASH-ADV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE LATE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2670-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRTIER-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRTIER-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRTIER-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRTIER-PAYMENT-AMT ROUNDED =
                       CRTIER-LATE-FEE-YTD * CRTIER-DISCOUNT / 200
               WHEN CRTIER-SOURCE-CD = 'C'
                   MOVE 5.00 TO CRTIER-PAYMENT-AMT
               WHEN CRTIER-SOURCE-CD = 'H'
                   MOVE 10.00 TO CRTIER-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRTIER-PAYMENT-AMT ROUNDED =
                       CRTIER-LATE-FEE-YTD * CRTIER-DISCOUNT / 100
           END-EVALUATE
           IF CRTIER-PAYMENT-AMT > 0 AND CRTIER-PAYMENT-AMT < 1.00
               MOVE 1.00 TO CRTIER-PAYMENT-AMT
           END-IF
           IF CRTIER-PAYMENT-AMT > 25.00
               MOVE 25.00 TO CRTIER-PAYMENT-AMT
           END-IF
           ADD CRTIER-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2680-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRTIER-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRTIER-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CRTIER-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN OPEN DATE IS PAST
      * DDY -> DPB WHEN OPEN DATE IS PAST
      * DPB -> UHF WHEN OPEN DATE IS PAST
      * UHF -> WLL WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2690-SET-STATUS.
           EVALUATE CRTIER-STATUS-CD
               WHEN 'BAL'
                   IF CRTIER-OPEN-DATE > 0 AND CRTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRTIER-OPEN-DATE > 0 AND CRTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRTIER-OPEN-DATE > 0 AND CRTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRTIER-OPEN-DATE > 0 AND CRTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE LATE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2700-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRTIER-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRTIER-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRTIER-LATE-FEE-YTD *
                       CRTIER-PENALTY-PCT / 200
               WHEN CRTIER-CARD-TYPE-CD = 'PGU'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN CRTIER-CARD-TYPE-CD = 'TRN'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRTIER-LATE-FEE-YTD *
                       CRTIER-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2710-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRTIER-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARD TYPE CD:
      * PGU -> TRN WHEN EXPIRY DATE IS PAST
      * TRN -> WHR WHEN EXPIRY DATE IS PAST
      * WHR -> ZWB WHEN EXPIRY DATE IS PAST
      * ZWB -> PGU WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2720-SET-CARD-TYPE.
           EVALUATE CRTIER-CARD-TYPE-CD
               WHEN 'PGU'
                   IF CRTIER-EXPIRY-DATE > 0 AND CRTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TRN' TO CRTIER-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TRN'
                   IF CRTIER-EXPIRY-DATE > 0 AND CRTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WHR' TO CRTIER-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WHR'
                   IF CRTIER-EXPIRY-DATE > 0 AND CRTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWB' TO CRTIER-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZWB'
                   IF CRTIER-EXPIRY-DATE > 0 AND CRTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PGU' TO CRTIER-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E489 NAME REQUIRED
      * E745 LATE FEE YTD NEGATIVE
      * E899 LATE FEE YTD OVER 5000
      * E579 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2730-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRTIER-NAME = SPACES
                   MOVE 'E489' TO WS-REJECT-CODE
               WHEN CRTIER-LATE-FEE-YTD < 0
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN CRTIER-LATE-FEE-YTD > 5000.00
                   MOVE 'E899' TO WS-REJECT-CODE
               WHEN CRTIER-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2740-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRTIER-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CRTIER-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2750-LIMIT-LATE-FEE.
           IF CRTIER-LATE-FEE-YTD < 50.00
               MOVE 50.00 TO CRTIER-LATE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRTIER-LATE-FEE-YTD > 500.00
               MOVE 500.00 TO CRTIER-LATE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2760-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE CRTIER-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ADV YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2770-ACCRUE-CASH-ADV.
           IF CRTIER-CASH-ADV-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRTIER-CASH-ADV-YTD * CRTIER-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRTIER-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2780-COUNT-RETRY.
           IF CRTIER-RETRY-CNT < 9999999
               ADD 1 TO CRTIER-RETRY-CNT
           END-IF
           IF CRTIER-RETRY-CNT NOT < 10
               MOVE 'Y' TO CRTIER-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       2790-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRTIER-PAYMENT-AMT ROUNDED =
                   CRTIER-LATE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRTIER-PAYMENT-AMT
           END-IF
           ADD CRTIER-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2800-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRTIER-LIMIT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRTIER-LIMIT-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRTIER-FINANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2810-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRTIER-LATE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CRTIER-LATE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ADV YTD.
      *----------------------------------------------------------------*
       2820-PRORATE-CASH-ADV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRTIER-CASH-ADV-YTD ROUNDED =
                   CRTIER-LATE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRTIER-CASH-ADV-YTD
           END-IF
           ADD CRTIER-CASH-ADV-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2830-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CRTIER-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRTIER-LATE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE YTD FEE ON THE FINANCE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2840-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRTIER-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRTIER-LATE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRTIER-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRTIER-LATE-FEE-YTD ROUNDED =
                       CRTIER-FINANCE-YTD * CRTIER-PENALTY-PCT / 200
               WHEN CRTIER-CARD-TYPE-CD = 'PGU'
                   MOVE 15.00 TO CRTIER-LATE-FEE-YTD
               WHEN CRTIER-CARD-TYPE-CD = 'TRN'
                   MOVE 15.00 TO CRTIER-LATE-FEE-YTD
               WHEN OTHER
                   COMPUTE CRTIER-LATE-FEE-YTD ROUNDED =
                       CRTIER-FINANCE-YTD * CRTIER-PENALTY-PCT / 100
           END-EVALUATE
           IF CRTIER-LATE-FEE-YTD > 0 AND CRTIER-LATE-FEE-YTD < 2.00
               MOVE 2.00 TO CRTIER-LATE-FEE-YTD
           END-IF
           IF CRTIER-LATE-FEE-YTD > 75.00
               MOVE 75.00 TO CRTIER-LATE-FEE-YTD
           END-IF
           ADD CRTIER-LATE-FEE-YTD TO WS-TOTAL-FEES.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CRTIER-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CRTIER-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CRTIER-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CRTIER EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
