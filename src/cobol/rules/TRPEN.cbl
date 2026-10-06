      *================================================================*
      * PROGRAM  : TRPEN                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PERSONAL TRUST RULE - PRICING. CALLED BY THE        *
      *            PERSONAL TRUST BATCH AND ONLINE PROGRAMS.           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-06-18 RJT  ORIGINAL                                       *
      * 1991-04-10 GAF  TRUSTEE-FEE FEE SCHEDULE UPDATE REQ7750        *
      * 1993-02-18 RJT  PERFORMANCE - MARKET READ AHEAD PRB6445        *
      * 1994-06-28 PQR  PRINCIPAL CALCULATION CORRECTED INC5413        *
      * 1995-05-17 DPS  INCOME EXCEPTIONS TO OPS QUEUE REQ7657         *
      * 1997-09-10 BHS  TRUSTEE-FEE RERUN PROTECTION CHG9976           *
      * 1997-12-27 EWS  MARKET LIMIT REVISED INC0868                   *
      * 1999-06-09 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-11-04 JLM  TRUSTEE-FEE RERUN PROTECTION PRB6571           *
      * 2002-09-15 BHS  INCOME CUTOFF MOVED PRB0737                    *
      * 2003-07-12 EWS  AUDIT FINDING - TRUSTEE-FEE CONTROL TOTALS INC6*
      * 2005-10-13 LMB  AUDIT FINDING - MARKET CONTROL TOTALS CHG5622  *
      * 2007-07-06 MKL  AUDIT FINDING - TRUSTEE-FEE CONTROL TOTALS PRB4*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRPEN.
       AUTHOR. B H SANDERS.

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
      * WS-TRUSTEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TRUSTEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000738500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000334000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000839000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000402000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004250000000619500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004300000000475000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000200000'.
       01  WS-TRUSTEE-BAND-TBL REDEFINES WS-TRUSTEE-BAND-VALUES.
           05  WS-TRUSTEE-BAND-ROW OCCURS 7 TIMES.
               10  WS-TRUSTEE-BAND-LOW     PIC 9(09)V99.
               10  WS-TRUSTEE-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINCIPA-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000402000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000858000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000289000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000516000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022550000000085000'.
       01  WS-PRINCIPA-GRID-TBL REDEFINES WS-PRINCIPA-GRID-VALUES.
           05  WS-PRINCIPA-GRID-ROW OCCURS 5 TIMES.
               10  WS-PRINCIPA-GRID-LOW    PIC 9(09)V99.
               10  WS-PRINCIPA-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TRPENP.

       PROCEDURE DIVISION USING TRPEN-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PRORATE-INCOME
           PERFORM 2010-COUNT-PAYMENT
           PERFORM 2020-SET-CYCLE
           PERFORM 2030-SPLIT-INCOME
           PERFORM 2040-FLAG-TAX
           PERFORM 2050-ACCUM-CYCLE
           PERFORM 2060-ASSESS-EXEMPT
           PERFORM 2070-CHECK-NOTICE
           PERFORM 2080-SELECT-DISCOUNT
           PERFORM 2090-SET-TRUST-TYPE
           PERFORM 2100-COUNT-RETRY
           PERFORM 2110-LIMIT-DISTRIB
           PERFORM 2120-SET-CYCLE
           PERFORM 2130-PRORATE-INCOME
           PERFORM 2140-SPLIT-MARKET
           PERFORM 2150-AGE-CLOSE
           PERFORM 2160-SPLIT-TRUSTEE-FEE
           PERFORM 2170-DESCRIBE-TRUST-TYPE
           PERFORM 2180-SET-CYCLE
           PERFORM 2190-FLAG-ITEM
           PERFORM 2200-PRORATE-MARKET
           PERFORM 2210-ASSESS-PREFERRED
           PERFORM 2220-ACCRUE-MARKET
           PERFORM 2230-ASSESS-EXEMPT
           PERFORM 2240-DESCRIBE-STATUS
           PERFORM 2250-DESCRIBE-CYCLE
           PERFORM 2260-ACCRUE-DISCOUNT
           PERFORM 2270-SET-STATUS
           PERFORM 2280-SELECT-RATE
           PERFORM 2290-SET-REGION
           PERFORM 2300-ACCRUE-TRUSTEE-FEE
           PERFORM 2310-ACCUM-STATUS
           PERFORM 2320-SET-TRUST-TYPE
           PERFORM 2330-FLAG-INCOME
           PERFORM 2340-AGE-OPEN
           PERFORM 2350-ACCUM-CYCLE
           PERFORM 2360-SELECT-DISCOUNT
           PERFORM 2370-SELECT-DISCOUNT
           PERFORM 2380-SET-STATUS
           PERFORM 2390-CHECK-OPEN
           PERFORM 2400-SPLIT-MARKET
           PERFORM 2410-EDIT-REF-NO
           PERFORM 2420-CHECK-NOTICE
           PERFORM 2430-AGE-LAST-STMT
           PERFORM 2440-DESCRIBE-TRUST-TYPE
           PERFORM 2450-EDIT-REF-NO
           PERFORM 2460-FLAG-DISTRIB
           PERFORM 2470-SET-STATUS
           PERFORM 2480-DESCRIBE-STATUS
           PERFORM 2490-SET-TRUST-TYPE
           PERFORM 2500-PRORATE-INCOME
           PERFORM 2510-CHECK-CLOSE
           PERFORM 2520-COUNT-RETRY
           PERFORM 2530-EDIT-ADDR
           PERFORM 2540-EDIT-REF-NO
           PERFORM 2550-CHECK-OPEN
           PERFORM 2560-SELECT-DISCOUNT
           PERFORM 2570-ASSESS-EXEMPT
           PERFORM 2580-SET-REGION
           PERFORM 2590-AGE-OPEN
           PERFORM 2600-SET-STATUS
           PERFORM 2610-ACCRUE-RATE
           PERFORM 2620-LIMIT-DISTRIB
           PERFORM 2630-SELECT-RATE
           PERFORM 2640-ACCUM-TRUST-TYPE
           PERFORM 2650-ASSESS-EXEMPT
           PERFORM 2660-SPLIT-INCOME
           PERFORM 2670-FLAG-DISTRIB
           PERFORM 2680-COUNT-RETRY
           PERFORM 2690-SELECT-DISCOUNT
           PERFORM 2700-AGE-CLOSE
           PERFORM 2710-SPLIT-TRUSTEE-FEE
           PERFORM 2720-DESCRIBE-STATUS
           PERFORM 2730-ASSESS-SUSPEND
           PERFORM 2740-CHECK-CLOSE
           PERFORM 2750-SPLIT-INCOME
           PERFORM 2760-AGE-LAST-STMT
           PERFORM 2770-ACCUM-CYCLE
           PERFORM 2780-DESCRIBE-CYCLE
           PERFORM 2790-AGE-OPEN
           PERFORM 2800-SPLIT-TRUSTEE-FEE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TRPEN-RETURN-CODE.

      *----------------------------------------------------------------*
      * PRORATE MARKET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INCOME YTD.
      *----------------------------------------------------------------*
       2000-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRPEN-INCOME-YTD ROUNDED =
                   TRPEN-MARKET-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRPEN-INCOME-YTD
           END-IF
           ADD TRPEN-INCOME-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-PAYMENT.
           IF TRPEN-PAYMENT-CNT < 9999999
               ADD 1 TO TRPEN-PAYMENT-CNT
           END-IF
           IF TRPEN-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO TRPEN-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EN -> FY WHEN OPEN DATE IS PAST
      * FY -> HP WHEN OPEN DATE IS PAST
      * HP -> JB WHEN OPEN DATE IS PAST
      * JB -> KZ WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2020-SET-CYCLE.
           EVALUATE TRPEN-CYCLE-CD
               WHEN 'EN'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HP' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HP'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JB' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JB'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZ' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INCOME YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2030-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRPEN-INCOME-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRPEN-INCOME-YTD - (WS-WORK-AMT * 12)
           COMPUTE TRPEN-INCOME-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2040-FLAG-TAX.
           IF TRPEN-TAX-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRPEN-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2050-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRPEN-CYCLE-CD
                   ADD TRPEN-MARKET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRPEN-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRPEN-MARKET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE INCOME YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2060-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRPEN-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRPEN-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRPEN-INCOME-YTD *
                       TRPEN-DISCOUNT / 200
               WHEN TRPEN-TRUST-TYPE-CD = 'FLJ'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN TRPEN-TRUST-TYPE-CD = 'MNG'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRPEN-INCOME-YTD *
                       TRPEN-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2070-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TRPEN-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE BAND TIER FOR THE DISTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2080-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-BAND-RATE(1) TO TRPEN-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRPEN-DISTRIB-YTD NOT < WS-TRUSTEE-BAND-LOW(WS-TX)
                   MOVE WS-TRUSTEE-BAND-RATE(WS-TX) TO TRPEN-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRPEN-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRPEN-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN OPEN DATE IS PAST
      * MNG -> VFN WHEN OPEN DATE IS PAST
      * VFN -> XPL WHEN OPEN DATE IS PAST
      * XPL -> FLJ WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2090-SET-TRUST-TYPE.
           EVALUATE TRPEN-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2100-COUNT-RETRY.
           IF TRPEN-RETRY-CNT < 9999999
               ADD 1 TO TRPEN-RETRY-CNT
           END-IF
           IF TRPEN-RETRY-CNT NOT < 5
               MOVE 'Y' TO TRPEN-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2110-LIMIT-DISTRIB.
           IF TRPEN-DISTRIB-YTD < 10.00
               MOVE 10.00 TO TRPEN-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRPEN-DISTRIB-YTD > 500.00
               MOVE 500.00 TO TRPEN-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EN -> FY WHEN CLOSE DATE IS PAST
      * FY -> HP WHEN CLOSE DATE IS PAST
      * HP -> JB WHEN CLOSE DATE IS PAST
      * JB -> KZ WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2120-SET-CYCLE.
           EVALUATE TRPEN-CYCLE-CD
               WHEN 'EN'
                   IF TRPEN-CLOSE-DATE > 0 AND TRPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF TRPEN-CLOSE-DATE > 0 AND TRPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HP' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HP'
                   IF TRPEN-CLOSE-DATE > 0 AND TRPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JB' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JB'
                   IF TRPEN-CLOSE-DATE > 0 AND TRPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZ' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INCOME AMT.
      *----------------------------------------------------------------*
       2130-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRPEN-INCOME-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRPEN-INCOME-AMT
           END-IF
           ADD TRPEN-INCOME-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2140-SPLIT-MARKET.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRPEN-MARKET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-CLOSE-DATE > 0 AND TRPEN-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRPEN-TRUSTEE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TRPEN-TRUSTEE-FEE-AMT - (WS-WORK-AMT
               * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-TRUST-TYPE.
           EVALUATE TRPEN-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EN -> FY WHEN LAST STMT DATE IS PAST
      * FY -> HP WHEN LAST STMT DATE IS PAST
      * HP -> JB WHEN LAST STMT DATE IS PAST
      * JB -> KZ WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2180-SET-CYCLE.
           EVALUATE TRPEN-CYCLE-CD
               WHEN 'EN'
                   IF TRPEN-LAST-STMT-DATE > 0 AND TRPEN-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FY' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF TRPEN-LAST-STMT-DATE > 0 AND TRPEN-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HP' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HP'
                   IF TRPEN-LAST-STMT-DATE > 0 AND TRPEN-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JB' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JB'
                   IF TRPEN-LAST-STMT-DATE > 0 AND TRPEN-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KZ' TO TRPEN-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2190-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARKET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2200-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRPEN-MARKET-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2210-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRPEN-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRPEN-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRPEN-EXEMPT-FLAG = 'Y'
                   COMPUTE TRPEN-TAX-AMT ROUNDED = WS-WORK-AMT *
                       TRPEN-DISCOUNT / 200
               WHEN TRPEN-TRUST-TYPE-CD = 'FLJ'
                   MOVE 5.00 TO TRPEN-TAX-AMT
               WHEN TRPEN-TRUST-TYPE-CD = 'MNG'
                   MOVE 3.00 TO TRPEN-TAX-AMT
               WHEN OTHER
                   COMPUTE TRPEN-TAX-AMT ROUNDED = WS-WORK-AMT *
                       TRPEN-DISCOUNT / 100
           END-EVALUATE
           IF TRPEN-TAX-AMT > 0 AND TRPEN-TAX-AMT < 1.00
               MOVE 1.00 TO TRPEN-TAX-AMT
           END-IF
           IF TRPEN-TAX-AMT > 250.00
               MOVE 250.00 TO TRPEN-TAX-AMT
           END-IF
           ADD TRPEN-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARKET YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2220-ACCRUE-MARKET.
           IF TRPEN-MARKET-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRPEN-MARKET-YTD * TRPEN-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE INCOME YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2230-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRPEN-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRPEN-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE TRPEN-TAX-AMT ROUNDED = TRPEN-INCOME-YTD *
                       TRPEN-DISCOUNT / 200
               WHEN TRPEN-STATUS-CD = 'EH'
                   MOVE 15.00 TO TRPEN-TAX-AMT
               WHEN TRPEN-STATUS-CD = 'GG'
                   MOVE 10.00 TO TRPEN-TAX-AMT
               WHEN OTHER
                   COMPUTE TRPEN-TAX-AMT ROUNDED = TRPEN-INCOME-YTD *
                       TRPEN-DISCOUNT / 100
           END-EVALUATE
           IF TRPEN-TAX-AMT > 0 AND TRPEN-TAX-AMT < 2.00
               MOVE 2.00 TO TRPEN-TAX-AMT
           END-IF
           IF TRPEN-TAX-AMT > 35.00
               MOVE 35.00 TO TRPEN-TAX-AMT
           END-IF
           ADD TRPEN-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2240-DESCRIBE-STATUS.
           EVALUATE TRPEN-STATUS-CD
               WHEN 'EH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2250-DESCRIBE-CYCLE.
           EVALUATE TRPEN-CYCLE-CD
               WHEN 'EN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2260-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRPEN-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN NOTICE DATE IS PAST
      * GG -> JG WHEN NOTICE DATE IS PAST
      * JG -> EH WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2270-SET-STATUS.
           EVALUATE TRPEN-STATUS-CD
               WHEN 'EH'
                   IF TRPEN-NOTICE-DATE > 0 AND TRPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRPEN-NOTICE-DATE > 0 AND TRPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRPEN-NOTICE-DATE > 0 AND TRPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE BAND TIER FOR THE INCOME YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2280-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-BAND-RATE(1) TO TRPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRPEN-INCOME-YTD NOT < WS-TRUSTEE-BAND-LOW(WS-TX)
                   MOVE WS-TRUSTEE-BAND-RATE(WS-TX) TO TRPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN NOTICE DATE IS PAST
      * D -> N WHEN NOTICE DATE IS PAST
      * N -> T WHEN NOTICE DATE IS PAST
      * T -> A WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2290-SET-REGION.
           EVALUATE TRPEN-REGION-CD
               WHEN 'A'
                   IF TRPEN-NOTICE-DATE > 0 AND TRPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRPEN-NOTICE-DATE > 0 AND TRPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRPEN-NOTICE-DATE > 0 AND TRPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRPEN-NOTICE-DATE > 0 AND TRPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2300-ACCRUE-TRUSTEE-FEE.
           IF TRPEN-TRUSTEE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRPEN-TRUSTEE-FEE-AMT * TRPEN-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRPEN-INCOME-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2310-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRPEN-STATUS-CD
                   ADD TRPEN-INCOME-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRPEN-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRPEN-INCOME-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN OPEN DATE IS PAST
      * MNG -> VFN WHEN OPEN DATE IS PAST
      * VFN -> XPL WHEN OPEN DATE IS PAST
      * XPL -> FLJ WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2320-SET-TRUST-TYPE.
           EVALUATE TRPEN-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INCOME YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2330-FLAG-INCOME.
           IF TRPEN-INCOME-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRPEN-INCOME-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2340-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRPEN-MARKET-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRPEN-MARKET-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRPEN-MARKET-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRPEN-MARKET-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRPEN-MARKET-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2350-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRPEN-CYCLE-CD
                   ADD TRPEN-MARKET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRPEN-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRPEN-MARKET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE BAND TIER FOR THE INCOME AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2360-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-BAND-RATE(1) TO TRPEN-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRPEN-INCOME-AMT NOT < WS-TRUSTEE-BAND-LOW(WS-TX)
                   MOVE WS-TRUSTEE-BAND-RATE(WS-TX) TO TRPEN-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRPEN-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRPEN-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE BAND TIER FOR THE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2370-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-BAND-RATE(1) TO TRPEN-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRPEN-TAX-AMT NOT < WS-TRUSTEE-BAND-LOW(WS-TX)
                   MOVE WS-TRUSTEE-BAND-RATE(WS-TX) TO TRPEN-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRPEN-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRPEN-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN NOTICE DATE IS PAST
      * GG -> JG WHEN NOTICE DATE IS PAST
      * JG -> EH WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2380-SET-STATUS.
           EVALUATE TRPEN-STATUS-CD
               WHEN 'EH'
                   IF TRPEN-NOTICE-DATE > 0 AND TRPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRPEN-NOTICE-DATE > 0 AND TRPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRPEN-NOTICE-DATE > 0 AND TRPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2390-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TRPEN-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2400-SPLIT-MARKET.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE TRPEN-MARKET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E086 REF NO REQUIRED
      * E271 INCOME YTD NEGATIVE
      * E815 INCOME YTD OVER 99999
      * E498 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRPEN-REF-NO = SPACES
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN TRPEN-INCOME-YTD < 0
                   MOVE 'E271' TO WS-REJECT-CODE
               WHEN TRPEN-INCOME-YTD > 99999.00
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN TRPEN-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E498' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2420-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TRPEN-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2430-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-LAST-STMT-DATE > 0 AND TRPEN-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2440-DESCRIBE-TRUST-TYPE.
           EVALUATE TRPEN-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E672 REF NO REQUIRED
      * E153 TAX AMT NEGATIVE
      * E516 TAX AMT OVER 250000
      * E930 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2450-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRPEN-REF-NO = SPACES
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN TRPEN-TAX-AMT < 0
                   MOVE 'E153' TO WS-REJECT-CODE
               WHEN TRPEN-TAX-AMT > 250000.00
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN TRPEN-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E930' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2460-FLAG-DISTRIB.
           IF TRPEN-DISTRIB-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRPEN-DISTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN OPEN DATE IS PAST
      * GG -> JG WHEN OPEN DATE IS PAST
      * JG -> EH WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-STATUS.
           EVALUATE TRPEN-STATUS-CD
               WHEN 'EH'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-STATUS.
           EVALUATE TRPEN-STATUS-CD
               WHEN 'EH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN OPEN DATE IS PAST
      * MNG -> VFN WHEN OPEN DATE IS PAST
      * VFN -> XPL WHEN OPEN DATE IS PAST
      * XPL -> FLJ WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-TRUST-TYPE.
           EVALUATE TRPEN-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRPEN-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INCOME YTD.
      *----------------------------------------------------------------*
       2500-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRPEN-INCOME-YTD ROUNDED =
                   TRPEN-DISTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRPEN-INCOME-YTD
           END-IF
           ADD TRPEN-INCOME-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2510-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TRPEN-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2520-COUNT-RETRY.
           IF TRPEN-RETRY-CNT < 9999999
               ADD 1 TO TRPEN-RETRY-CNT
           END-IF
           IF TRPEN-RETRY-CNT NOT < 6
               MOVE 'Y' TO TRPEN-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E262 ADDR REQUIRED
      * E081 DISTRIB YTD NEGATIVE
      * E508 DISTRIB YTD OVER 250000
      * E262 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2530-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRPEN-ADDR = SPACES
                   MOVE 'E262' TO WS-REJECT-CODE
               WHEN TRPEN-DISTRIB-YTD < 0
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN TRPEN-DISTRIB-YTD > 250000.00
                   MOVE 'E508' TO WS-REJECT-CODE
               WHEN TRPEN-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E262' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E406 REF NO REQUIRED
      * E441 WORK AMT 2 NEGATIVE
      * E062 WORK AMT 2 OVER 250000
      * E414 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2540-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRPEN-REF-NO = SPACES
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E441' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E062' TO WS-REJECT-CODE
               WHEN TRPEN-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E414' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2550-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TRPEN-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE TRUSTEE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2560-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO TRPEN-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TRPEN-TRUSTEE-FEE-AMT NOT <
                   WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO TRPEN-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRPEN-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRPEN-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE FEE ON THE MARKET YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2570-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRPEN-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRPEN-TRUSTEE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE TRPEN-TRUSTEE-FEE-AMT ROUNDED =
                       TRPEN-MARKET-YTD * TRPEN-RATE / 200
               WHEN TRPEN-REGION-CD = 'A'
                   MOVE 10.00 TO TRPEN-TRUSTEE-FEE-AMT
               WHEN TRPEN-REGION-CD = 'D'
                   MOVE 10.00 TO TRPEN-TRUSTEE-FEE-AMT
               WHEN OTHER
                   COMPUTE TRPEN-TRUSTEE-FEE-AMT ROUNDED =
                       TRPEN-MARKET-YTD * TRPEN-RATE / 100
           END-EVALUATE
           IF TRPEN-TRUSTEE-FEE-AMT > 0 AND TRPEN-TRUSTEE-FEE-AMT < 1.00
               MOVE 1.00 TO TRPEN-TRUSTEE-FEE-AMT
           END-IF
           IF TRPEN-TRUSTEE-FEE-AMT > 75.00
               MOVE 75.00 TO TRPEN-TRUSTEE-FEE-AMT
           END-IF
           ADD TRPEN-TRUSTEE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN CLOSE DATE IS PAST
      * D -> N WHEN CLOSE DATE IS PAST
      * N -> T WHEN CLOSE DATE IS PAST
      * T -> A WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2580-SET-REGION.
           EVALUATE TRPEN-REGION-CD
               WHEN 'A'
                   IF TRPEN-CLOSE-DATE > 0 AND TRPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRPEN-CLOSE-DATE > 0 AND TRPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRPEN-CLOSE-DATE > 0 AND TRPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRPEN-CLOSE-DATE > 0 AND TRPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRPEN-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2590-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRPEN-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRPEN-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRPEN-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TRPEN-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRPEN-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN OPEN DATE IS PAST
      * GG -> JG WHEN OPEN DATE IS PAST
      * JG -> EH WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2600-SET-STATUS.
           EVALUATE TRPEN-STATUS-CD
               WHEN 'EH'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRPEN-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2610-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRPEN-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRPEN-DISTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2620-LIMIT-DISTRIB.
           IF TRPEN-DISTRIB-YTD < 50.00
               MOVE 50.00 TO TRPEN-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRPEN-DISTRIB-YTD > 50000.00
               MOVE 50000.00 TO TRPEN-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE BAND TIER FOR THE MARKET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2630-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-BAND-RATE(1) TO TRPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRPEN-MARKET-YTD NOT < WS-TRUSTEE-BAND-LOW(WS-TX)
                   MOVE WS-TRUSTEE-BAND-RATE(WS-TX) TO TRPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET YTD BY TRUST TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-TRUST-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRPEN-TRUST-TYPE-CD
                   ADD TRPEN-MARKET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRPEN-TRUST-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRPEN-MARKET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE INCOME AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2650-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRPEN-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TRPEN-INCOME-AMT *
                       TRPEN-RATE / 200
               WHEN TRPEN-TRUST-TYPE-CD = 'FLJ'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN TRPEN-TRUST-TYPE-CD = 'MNG'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TRPEN-INCOME-AMT *
                       TRPEN-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT INCOME AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2660-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRPEN-INCOME-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TRPEN-INCOME-AMT - (WS-WORK-AMT * 3)
           COMPUTE TRPEN-INCOME-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2670-FLAG-DISTRIB.
           IF TRPEN-DISTRIB-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRPEN-DISTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2680-COUNT-RETRY.
           IF TRPEN-RETRY-CNT < 9999999
               ADD 1 TO TRPEN-RETRY-CNT
           END-IF
           IF TRPEN-RETRY-CNT NOT < 10
               MOVE 'Y' TO TRPEN-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2690-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO TRPEN-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO TRPEN-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRPEN-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRPEN-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2700-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-CLOSE-DATE > 0 AND TRPEN-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRPEN-INCOME-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRPEN-INCOME-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRPEN-INCOME-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRPEN-INCOME-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRPEN-INCOME-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2710-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE TRPEN-TRUSTEE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2720-DESCRIBE-STATUS.
           EVALUATE TRPEN-STATUS-CD
               WHEN 'EH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB YTD FEE ON THE INCOME AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2730-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRPEN-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRPEN-DISTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRPEN-EXEMPT-FLAG = 'Y'
                   COMPUTE TRPEN-DISTRIB-YTD ROUNDED = TRPEN-INCOME-AMT
                       * TRPEN-RATE / 200
               WHEN TRPEN-REGION-CD = 'A'
                   MOVE 3.00 TO TRPEN-DISTRIB-YTD
               WHEN TRPEN-REGION-CD = 'D'
                   MOVE 15.00 TO TRPEN-DISTRIB-YTD
               WHEN OTHER
                   COMPUTE TRPEN-DISTRIB-YTD ROUNDED = TRPEN-INCOME-AMT
                       * TRPEN-RATE / 100
           END-EVALUATE
           IF TRPEN-DISTRIB-YTD > 0 AND TRPEN-DISTRIB-YTD < 5.00
               MOVE 5.00 TO TRPEN-DISTRIB-YTD
           END-IF
           IF TRPEN-DISTRIB-YTD > 75.00
               MOVE 75.00 TO TRPEN-DISTRIB-YTD
           END-IF
           ADD TRPEN-DISTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2740-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRPEN-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2750-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE TRPEN-INCOME-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2760-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-LAST-STMT-DATE > 0 AND TRPEN-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRPEN-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRPEN-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRPEN-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRPEN-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRPEN-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2770-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRPEN-CYCLE-CD
                   ADD TRPEN-DISTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRPEN-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRPEN-DISTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2780-DESCRIBE-CYCLE.
           EVALUATE TRPEN-CYCLE-CD
               WHEN 'EN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2790-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRPEN-OPEN-DATE > 0 AND TRPEN-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRPEN-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRPEN-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRPEN-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRPEN-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRPEN-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRPEN-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2800-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRPEN-TRUSTEE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRPEN-TRUSTEE-FEE-AMT - (WS-WORK-AMT
               * 4)
           COMPUTE TRPEN-MARKET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TRPEN-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TRPEN-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TRPEN-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TRPEN EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
