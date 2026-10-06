      *================================================================*
      * PROGRAM  : EUVAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : EURO CONVERSION RULE - ELIGIBILITY. CALLED BY THE   *
      *            EURO CONVERSION BATCH AND ONLINE PROGRAMS.          *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-09-15 EWS  ORIGINAL                                       *
      * 2001-04-17 PQR  TRIANGULAR ROUNDING - HALF UP PRB3961          *
      * 2002-01-10 EWS  FEE FEE SCHEDULE UPDATE INC0206                *
      * 2005-08-09 LMB  NATIONAL LIMIT REVISED REQ1230                 *
      * 2006-03-16 LMB  CONVERTED CALCULATION CORRECTED INC2504        *
      * 2007-01-18 MKL  CONVERTED RERUN PROTECTION PRB7590             *
      * 2007-11-23 CRP  FEE ROUNDING - HALF UP INC1330                 *
      * 2008-11-06 JLM  CONVERTED TABLE EXPANDED CHG7592               *
      * 2010-11-05 EWS  NEW FEE REPORT COLUMNS REQ8539                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUVAL.
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
      * WS-EURO-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EURO-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000587000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000615000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000833500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000697500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014500000000286000'.
       01  WS-EURO-BAND-TBL REDEFINES WS-EURO-BAND-VALUES.
           05  WS-EURO-BAND-ROW OCCURS 6 TIMES.
               10  WS-EURO-BAND-LOW        PIC 9(09)V99.
               10  WS-EURO-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONVERTE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONVERTE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000193500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000723000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000753500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000888000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000647000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005700000000493000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006200000000153000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006250000000527500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006750000000547500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007750000000494000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008250000000711500'.
       01  WS-CONVERTE-BAND-TBL REDEFINES WS-CONVERTE-BAND-VALUES.
           05  WS-CONVERTE-BAND-ROW OCCURS 11 TIMES.
               10  WS-CONVERTE-BAND-LOW    PIC 9(09)V99.
               10  WS-CONVERTE-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY EUVALP.

       PROCEDURE DIVISION USING EUVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-FLAG-TRIANGULAR
           PERFORM 2010-ACCUM-STATUS
           PERFORM 2020-FLAG-NATIONAL
           PERFORM 2030-ACCRUE-FEE-PCT
           PERFORM 2040-EDIT-DESC
           PERFORM 2050-SELECT-PENALTY-PCT
           PERFORM 2060-ACCUM-REGION
           PERFORM 2070-SET-REGION
           PERFORM 2080-SELECT-FEE-PCT
           PERFORM 2090-ACCRUE-PENALTY-PCT
           PERFORM 2100-PRORATE-RESIDUAL
           PERFORM 2110-ACCRUE-CONVERTED
           PERFORM 2120-ASSESS-PREFERRED
           PERFORM 2130-ASSESS-EXEMPT
           PERFORM 2140-FLAG-ITEM
           PERFORM 2150-ASSESS-EXEMPT
           PERFORM 2160-COUNT-ITEM
           PERFORM 2170-ACCUM-STATUS
           PERFORM 2180-AGE-NEXT-REVIEW
           PERFORM 2190-ACCRUE-RESIDUAL
           PERFORM 2200-ACCRUE-TRIANGULAR
           PERFORM 2210-SPLIT-TRIANGULAR
           PERFORM 2220-ACCUM-CONV-METHOD
           PERFORM 2230-EDIT-DESC
           PERFORM 2240-SPLIT-TRIANGULAR
           PERFORM 2250-FLAG-NATIONAL
           PERFORM 2260-SPLIT-RESIDUAL
           PERFORM 2270-SET-REGION
           PERFORM 2280-SELECT-FEE-PCT
           PERFORM 2290-ACCRUE-FEE-PCT
           PERFORM 2300-COUNT-ITEM
           PERFORM 2310-SET-STATUS
           PERFORM 2320-COUNT-PAYMENT
           PERFORM 2330-DESCRIBE-REGION
           PERFORM 2340-FLAG-NATIONAL
           PERFORM 2350-FLAG-TRIANGULAR
           PERFORM 2360-CHECK-MATURITY
           PERFORM 2370-FLAG-CONVERTED
           PERFORM 2380-CHECK-LAST-MAINT
           PERFORM 2390-SELECT-FEE-PCT
           PERFORM 2400-SELECT-PENALTY-PCT
           PERFORM 2410-ACCUM-CONV-METHOD
           PERFORM 2420-PRORATE-CONVERTED
           PERFORM 2430-LIMIT-TRIANGULAR
           PERFORM 2440-PRORATE-CONVERTED
           PERFORM 2450-DESCRIBE-STATUS
           PERFORM 2460-COUNT-PAYMENT
           PERFORM 2470-ASSESS-OVERRIDE
           PERFORM 2480-SELECT-PENALTY-PCT
           PERFORM 2490-FLAG-TRIANGULAR
           PERFORM 2500-LIMIT-RESIDUAL
           PERFORM 2510-CHECK-NOTICE
           PERFORM 2520-CHECK-LAST-MAINT
           PERFORM 2530-SPLIT-CONVERTED
           PERFORM 2540-SELECT-PENALTY-PCT
           PERFORM 2550-SELECT-PENALTY-PCT
           PERFORM 2560-ACCUM-REGION
           PERFORM 2570-CHECK-NEXT-REVIEW
           PERFORM 2580-COUNT-PAYMENT
           PERFORM 2590-ACCRUE-FEE-PCT
           PERFORM 2600-ACCUM-STATUS
           PERFORM 2610-SPLIT-CONVERTED
           PERFORM 2620-ASSESS-PREFERRED
           PERFORM 2630-SELECT-FEE-PCT
           PERFORM 2640-DESCRIBE-STATUS
           PERFORM 2650-EDIT-CONTACT
           PERFORM 2660-ACCRUE-NATIONAL
           PERFORM 2670-ASSESS-OVERRIDE
           PERFORM 2680-EDIT-CONTACT
           PERFORM 2690-EDIT-DESC
           PERFORM 2700-ACCRUE-FEE-PCT
           PERFORM 2710-LIMIT-NATIONAL
           PERFORM 2720-SPLIT-NATIONAL
           PERFORM 2730-DESCRIBE-CONV-METHOD
           PERFORM 2740-FLAG-TRIANGULAR
           PERFORM 2750-ASSESS-PREFERRED
           PERFORM 2760-LIMIT-ITEM
           PERFORM 2770-SET-CONV-METHOD
           PERFORM 2780-FLAG-NATIONAL
           PERFORM 2790-ACCUM-CONV-METHOD
           PERFORM 2800-LIMIT-TRIANGULAR
           PERFORM 2810-SET-CONV-METHOD
           PERFORM 2820-ACCRUE-TRIANGULAR
           PERFORM 2830-SELECT-FEE-PCT
           PERFORM 2840-ACCUM-REGION
           PERFORM 2850-COUNT-PAYMENT
           PERFORM 2860-DESCRIBE-STATUS
           PERFORM 2870-FLAG-TRIANGULAR
           PERFORM 2880-ASSESS-PREFERRED
           PERFORM 2890-LIMIT-RESIDUAL
           PERFORM 2900-PRORATE-RESIDUAL
           PERFORM 2910-AGE-NOTICE
           PERFORM 2920-SET-CURRENCY
           PERFORM 2930-EDIT-CONTACT
           PERFORM 2940-ACCUM-CURRENCY
           PERFORM 2950-PRORATE-RESIDUAL
           PERFORM 2960-EDIT-DESC
           PERFORM 2970-SELECT-FEE-PCT
           PERFORM 2980-ASSESS-PREFERRED
           PERFORM 2990-SPLIT-TRIANGULAR
           PERFORM 3000-LIMIT-RESIDUAL
           PERFORM 3010-ACCUM-REGION
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO EUVAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2000-FLAG-TRIANGULAR.
           IF EUVAL-TRIANGULAR-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EUVAL-TRIANGULAR-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONVERTED AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUVAL-STATUS-CD
                   ADD EUVAL-CONVERTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUVAL-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUVAL-CONVERTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2020-FLAG-NATIONAL.
           IF EUVAL-NATIONAL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EUVAL-NATIONAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2030-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * EUVAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO EUVAL-TRIANGULAR-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E708 DESC REQUIRED
      * E822 NATIONAL AMT NEGATIVE
      * E801 NATIONAL AMT OVER 5000
      * E944 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2040-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUVAL-DESC = SPACES
                   MOVE 'E708' TO WS-REJECT-CODE
               WHEN EUVAL-NATIONAL-AMT < 0
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN EUVAL-NATIONAL-AMT > 5000.00
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN EUVAL-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE BAND TIER FOR THE NATIONAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2050-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-BAND-RATE(1) TO EUVAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EUVAL-NATIONAL-YTD NOT < WS-CONVERTE-BAND-LOW(WS-TX)
                   MOVE WS-CONVERTE-BAND-RATE(WS-TX) TO
                       EUVAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUVAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUVAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRIANGULAR AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2060-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUVAL-REGION-CD
                   ADD EUVAL-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUVAL-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DTL -> HYD WHEN NOTICE DATE IS PAST
      * HYD -> JUL WHEN NOTICE DATE IS PAST
      * JUL -> WSE WHEN NOTICE DATE IS PAST
      * WSE -> YAU WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2070-SET-REGION.
           EVALUATE EUVAL-REGION-CD
               WHEN 'DTL'
                   IF EUVAL-NOTICE-DATE > 0 AND EUVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HYD' TO EUVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HYD'
                   IF EUVAL-NOTICE-DATE > 0 AND EUVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JUL' TO EUVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JUL'
                   IF EUVAL-NOTICE-DATE > 0 AND EUVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WSE' TO EUVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WSE'
                   IF EUVAL-NOTICE-DATE > 0 AND EUVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YAU' TO EUVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE BAND TIER FOR THE TRIANGULAR YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2080-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-BAND-RATE(1) TO EUVAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EUVAL-TRIANGULAR-YTD NOT < WS-CONVERTE-BAND-LOW(WS-TX)
                   MOVE WS-CONVERTE-BAND-RATE(WS-TX) TO EUVAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUVAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUVAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2090-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * EUVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EUVAL-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESIDUAL YTD.
      *----------------------------------------------------------------*
       2100-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUVAL-RESIDUAL-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUVAL-RESIDUAL-YTD
           END-IF
           ADD EUVAL-RESIDUAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2110-ACCRUE-CONVERTED.
           IF EUVAL-CONVERTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUVAL-CONVERTED-AMT * EUVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EUVAL-NATIONAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL FEE ON THE NATIONAL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2120-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUVAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EUVAL-NATIONAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE EUVAL-NATIONAL-AMT ROUNDED =
                       EUVAL-NATIONAL-YTD * EUVAL-PENALTY-PCT / 200
               WHEN EUVAL-CONV-METHOD-CD = 'GL'
                   MOVE 5.00 TO EUVAL-NATIONAL-AMT
               WHEN EUVAL-CONV-METHOD-CD = 'JA'
                   MOVE 7.00 TO EUVAL-NATIONAL-AMT
               WHEN OTHER
                   COMPUTE EUVAL-NATIONAL-AMT ROUNDED =
                       EUVAL-NATIONAL-YTD * EUVAL-PENALTY-PCT / 100
           END-EVALUATE
           IF EUVAL-NATIONAL-AMT > 0 AND EUVAL-NATIONAL-AMT < 5.00
               MOVE 5.00 TO EUVAL-NATIONAL-AMT
           END-IF
           IF EUVAL-NATIONAL-AMT > 100.00
               MOVE 100.00 TO EUVAL-NATIONAL-AMT
           END-IF
           ADD EUVAL-NATIONAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE TRIANGULAR AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2130-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EUVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO EUVAL-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUVAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE EUVAL-TRIANGULAR-YTD ROUNDED =
                       EUVAL-TRIANGULAR-AMT * EUVAL-PENALTY-PCT / 200
               WHEN EUVAL-REGION-CD = 'DTL'
                   MOVE 20.00 TO EUVAL-TRIANGULAR-YTD
               WHEN EUVAL-REGION-CD = 'HYD'
                   MOVE 15.00 TO EUVAL-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EUVAL-TRIANGULAR-YTD ROUNDED =
                       EUVAL-TRIANGULAR-AMT * EUVAL-PENALTY-PCT / 100
           END-EVALUATE
           IF EUVAL-TRIANGULAR-YTD > 0 AND EUVAL-TRIANGULAR-YTD < 1.00
               MOVE 1.00 TO EUVAL-TRIANGULAR-YTD
           END-IF
           IF EUVAL-TRIANGULAR-YTD > 250.00
               MOVE 250.00 TO EUVAL-TRIANGULAR-YTD
           END-IF
           ADD EUVAL-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2140-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONVERTED FEE ON THE NATIONAL YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2150-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EUVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO EUVAL-CONVERTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUVAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE EUVAL-CONVERTED-AMT ROUNDED =
                       EUVAL-NATIONAL-YTD * EUVAL-PENALTY-PCT / 200
               WHEN EUVAL-REGION-CD = 'DTL'
                   MOVE 3.00 TO EUVAL-CONVERTED-AMT
               WHEN EUVAL-REGION-CD = 'HYD'
                   MOVE 7.00 TO EUVAL-CONVERTED-AMT
               WHEN OTHER
                   COMPUTE EUVAL-CONVERTED-AMT ROUNDED =
                       EUVAL-NATIONAL-YTD * EUVAL-PENALTY-PCT / 100
           END-EVALUATE
           IF EUVAL-CONVERTED-AMT > 0 AND EUVAL-CONVERTED-AMT < 2.00
               MOVE 2.00 TO EUVAL-CONVERTED-AMT
           END-IF
           IF EUVAL-CONVERTED-AMT > 50.00
               MOVE 50.00 TO EUVAL-CONVERTED-AMT
           END-IF
           ADD EUVAL-CONVERTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-ITEM.
           IF EUVAL-ITEM-CNT < 9999999
               ADD 1 TO EUVAL-ITEM-CNT
           END-IF
           IF EUVAL-ITEM-CNT NOT < 6
               MOVE 'Y' TO EUVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2170-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUVAL-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUVAL-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2180-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EUVAL-NEXT-REVIEW-DATE > 0 AND EUVAL-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUVAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUVAL-NATIONAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EUVAL-NATIONAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EUVAL-NATIONAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EUVAL-NATIONAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUVAL-NATIONAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESIDUAL YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2190-ACCRUE-RESIDUAL.
           IF EUVAL-RESIDUAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUVAL-RESIDUAL-YTD * EUVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EUVAL-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRIANGULAR YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2200-ACCRUE-TRIANGULAR.
           IF EUVAL-TRIANGULAR-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUVAL-TRIANGULAR-YTD * EUVAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2210-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE EUVAL-TRIANGULAR-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CONV METHOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2220-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUVAL-CONV-METHOD-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUVAL-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E246 DESC REQUIRED
      * E688 WORK AMT NEGATIVE
      * E725 WORK AMT OVER 1000000
      * E369 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUVAL-DESC = SPACES
                   MOVE 'E246' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E688' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E725' TO WS-REJECT-CODE
               WHEN EUVAL-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E369' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2240-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EUVAL-TRIANGULAR-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EUVAL-TRIANGULAR-AMT - (WS-WORK-AMT *
               12)
           COMPUTE EUVAL-RESIDUAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2250-FLAG-NATIONAL.
           IF EUVAL-NATIONAL-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUVAL-NATIONAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESIDUAL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2260-SPLIT-RESIDUAL.
           COMPUTE WS-WORK-AMT = EUVAL-RESIDUAL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EUVAL-RESIDUAL-YTD - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DTL -> HYD WHEN MATURITY DATE IS PAST
      * HYD -> JUL WHEN MATURITY DATE IS PAST
      * JUL -> WSE WHEN MATURITY DATE IS PAST
      * WSE -> YAU WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2270-SET-REGION.
           EVALUATE EUVAL-REGION-CD
               WHEN 'DTL'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HYD' TO EUVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HYD'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JUL' TO EUVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JUL'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WSE' TO EUVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WSE'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YAU' TO EUVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE NATIONAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2280-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EUVAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EUVAL-NATIONAL-YTD NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EUVAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUVAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUVAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2290-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * EUVAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO EUVAL-RESIDUAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2300-COUNT-ITEM.
           IF EUVAL-ITEM-CNT < 9999999
               ADD 1 TO EUVAL-ITEM-CNT
           END-IF
           IF EUVAL-ITEM-CNT NOT < 6
               MOVE 'Y' TO EUVAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * B -> F WHEN MATURITY DATE IS PAST
      * F -> K WHEN MATURITY DATE IS PAST
      * K -> R WHEN MATURITY DATE IS PAST
      * R -> U WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-STATUS.
           EVALUATE EUVAL-STATUS-CD
               WHEN 'B'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO EUVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO EUVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO EUVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO EUVAL-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2320-COUNT-PAYMENT.
           IF EUVAL-PAYMENT-CNT < 9999999
               ADD 1 TO EUVAL-PAYMENT-CNT
           END-IF
           IF EUVAL-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO EUVAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2330-DESCRIBE-REGION.
           EVALUATE EUVAL-REGION-CD
               WHEN 'DTL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NATIONAL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2340-FLAG-NATIONAL.
           IF EUVAL-NATIONAL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUVAL-NATIONAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2350-FLAG-TRIANGULAR.
           IF EUVAL-TRIANGULAR-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUVAL-TRIANGULAR-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2360-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EUVAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EUVAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2370-FLAG-CONVERTED.
           IF EUVAL-CONVERTED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EUVAL-CONVERTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2380-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EUVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO EUVAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2390-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EUVAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EUVAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUVAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUVAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2400-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-BAND-RATE(1) TO EUVAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-CONVERTE-BAND-LOW(WS-TX)
                   MOVE WS-CONVERTE-BAND-RATE(WS-TX) TO
                       EUVAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUVAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUVAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRIANGULAR AMT BY CONV METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2410-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUVAL-CONV-METHOD-CD
                   ADD EUVAL-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUVAL-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUVAL-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONVERTED AMT.
      *----------------------------------------------------------------*
       2420-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUVAL-CONVERTED-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EUVAL-CONVERTED-AMT
           END-IF
           ADD EUVAL-CONVERTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2430-LIMIT-TRIANGULAR.
           IF EUVAL-TRIANGULAR-AMT < 10.00
               MOVE 10.00 TO EUVAL-TRIANGULAR-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUVAL-TRIANGULAR-AMT > 2500.00
               MOVE 2500.00 TO EUVAL-TRIANGULAR-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONVERTED AMT.
      *----------------------------------------------------------------*
       2440-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUVAL-CONVERTED-AMT ROUNDED =
                   EUVAL-TRIANGULAR-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUVAL-CONVERTED-AMT
           END-IF
           ADD EUVAL-CONVERTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2450-DESCRIBE-STATUS.
           EVALUATE EUVAL-STATUS-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2460-COUNT-PAYMENT.
           IF EUVAL-PAYMENT-CNT < 9999999
               ADD 1 TO EUVAL-PAYMENT-CNT
           END-IF
           IF EUVAL-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO EUVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE NATIONAL YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2470-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EUVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EUVAL-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUVAL-PREFERRED-FLAG = 'Y'
                   COMPUTE EUVAL-TRIANGULAR-YTD ROUNDED =
                       EUVAL-NATIONAL-YTD * EUVAL-PENALTY-PCT / 200
               WHEN EUVAL-REGION-CD = 'DTL'
                   MOVE 12.00 TO EUVAL-TRIANGULAR-YTD
               WHEN EUVAL-REGION-CD = 'HYD'
                   MOVE 7.00 TO EUVAL-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EUVAL-TRIANGULAR-YTD ROUNDED =
                       EUVAL-NATIONAL-YTD * EUVAL-PENALTY-PCT / 100
           END-EVALUATE
           IF EUVAL-TRIANGULAR-YTD > 0 AND EUVAL-TRIANGULAR-YTD < 1.00
               MOVE 1.00 TO EUVAL-TRIANGULAR-YTD
           END-IF
           IF EUVAL-TRIANGULAR-YTD > 75.00
               MOVE 75.00 TO EUVAL-TRIANGULAR-YTD
           END-IF
           ADD EUVAL-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE CONVERTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2480-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EUVAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EUVAL-CONVERTED-AMT NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EUVAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUVAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUVAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2490-FLAG-TRIANGULAR.
           IF EUVAL-TRIANGULAR-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUVAL-TRIANGULAR-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RESIDUAL YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2500-LIMIT-RESIDUAL.
           IF EUVAL-RESIDUAL-YTD < 0.00
               MOVE 0.00 TO EUVAL-RESIDUAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUVAL-RESIDUAL-YTD > 500.00
               MOVE 500.00 TO EUVAL-RESIDUAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2510-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUVAL-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO EUVAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2520-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EUVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO EUVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EUVAL-CONVERTED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EUVAL-CONVERTED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE EUVAL-NATIONAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE BAND TIER FOR THE TRIANGULAR YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2540-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-BAND-RATE(1) TO EUVAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EUVAL-TRIANGULAR-YTD NOT < WS-CONVERTE-BAND-LOW(WS-TX)
                   MOVE WS-CONVERTE-BAND-RATE(WS-TX) TO
                       EUVAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUVAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUVAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE BAND TIER FOR THE NATIONAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2550-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-BAND-RATE(1) TO EUVAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EUVAL-NATIONAL-YTD NOT < WS-CONVERTE-BAND-LOW(WS-TX)
                   MOVE WS-CONVERTE-BAND-RATE(WS-TX) TO
                       EUVAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUVAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUVAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NATIONAL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUVAL-REGION-CD
                   ADD EUVAL-NATIONAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUVAL-NATIONAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EUVAL-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUVAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO EUVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2580-COUNT-PAYMENT.
           IF EUVAL-PAYMENT-CNT < 9999999
               ADD 1 TO EUVAL-PAYMENT-CNT
           END-IF
           IF EUVAL-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO EUVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * EUVAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO EUVAL-TRIANGULAR-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NATIONAL YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2600-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUVAL-STATUS-CD
                   ADD EUVAL-NATIONAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUVAL-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUVAL-NATIONAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2610-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EUVAL-CONVERTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EUVAL-CONVERTED-AMT - (WS-WORK-AMT *
               3)
           COMPUTE EUVAL-NATIONAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESIDUAL YTD FEE ON THE NATIONAL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2620-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUVAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EUVAL-RESIDUAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE EUVAL-RESIDUAL-YTD ROUNDED =
                       EUVAL-NATIONAL-YTD * EUVAL-FEE-PCT / 200
               WHEN EUVAL-STATUS-CD = 'B'
                   MOVE 20.00 TO EUVAL-RESIDUAL-YTD
               WHEN EUVAL-STATUS-CD = 'F'
                   MOVE 12.00 TO EUVAL-RESIDUAL-YTD
               WHEN OTHER
                   COMPUTE EUVAL-RESIDUAL-YTD ROUNDED =
                       EUVAL-NATIONAL-YTD * EUVAL-FEE-PCT / 100
           END-EVALUATE
           IF EUVAL-RESIDUAL-YTD > 0 AND EUVAL-RESIDUAL-YTD < 1.00
               MOVE 1.00 TO EUVAL-RESIDUAL-YTD
           END-IF
           IF EUVAL-RESIDUAL-YTD > 150.00
               MOVE 150.00 TO EUVAL-RESIDUAL-YTD
           END-IF
           ADD EUVAL-RESIDUAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2630-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EUVAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EUVAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUVAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUVAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2640-DESCRIBE-STATUS.
           EVALUATE EUVAL-STATUS-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E008 CONTACT REQUIRED
      * E038 NATIONAL YTD NEGATIVE
      * E898 NATIONAL YTD OVER 5000
      * E921 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2650-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUVAL-CONTACT = SPACES
                   MOVE 'E008' TO WS-REJECT-CODE
               WHEN EUVAL-NATIONAL-YTD < 0
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN EUVAL-NATIONAL-YTD > 5000.00
                   MOVE 'E898' TO WS-REJECT-CODE
               WHEN EUVAL-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E921' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2660-ACCRUE-NATIONAL.
           IF EUVAL-NATIONAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUVAL-NATIONAL-YTD * EUVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EUVAL-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESIDUAL YTD FEE ON THE CONVERTED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2670-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EUVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EUVAL-RESIDUAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUVAL-PREFERRED-FLAG = 'Y'
                   COMPUTE EUVAL-RESIDUAL-YTD ROUNDED =
                       EUVAL-CONVERTED-AMT * EUVAL-FEE-PCT / 200
               WHEN EUVAL-CONV-METHOD-CD = 'GL'
                   MOVE 10.00 TO EUVAL-RESIDUAL-YTD
               WHEN EUVAL-CONV-METHOD-CD = 'JA'
                   MOVE 5.00 TO EUVAL-RESIDUAL-YTD
               WHEN OTHER
                   COMPUTE EUVAL-RESIDUAL-YTD ROUNDED =
                       EUVAL-CONVERTED-AMT * EUVAL-FEE-PCT / 100
           END-EVALUATE
           IF EUVAL-RESIDUAL-YTD > 0 AND EUVAL-RESIDUAL-YTD < 1.00
               MOVE 1.00 TO EUVAL-RESIDUAL-YTD
           END-IF
           IF EUVAL-RESIDUAL-YTD > 150.00
               MOVE 150.00 TO EUVAL-RESIDUAL-YTD
           END-IF
           ADD EUVAL-RESIDUAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E171 CONTACT REQUIRED
      * E757 WORK AMT NEGATIVE
      * E460 WORK AMT OVER 10000
      * E677 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2680-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUVAL-CONTACT = SPACES
                   MOVE 'E171' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN EUVAL-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E132 DESC REQUIRED
      * E121 RESIDUAL YTD NEGATIVE
      * E152 RESIDUAL YTD OVER 1000000
      * E295 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2690-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUVAL-DESC = SPACES
                   MOVE 'E132' TO WS-REJECT-CODE
               WHEN EUVAL-RESIDUAL-YTD < 0
                   MOVE 'E121' TO WS-REJECT-CODE
               WHEN EUVAL-RESIDUAL-YTD > 1000000.00
                   MOVE 'E152' TO WS-REJECT-CODE
               WHEN EUVAL-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E295' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2700-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * EUVAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO EUVAL-CONVERTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2710-LIMIT-NATIONAL.
           IF EUVAL-NATIONAL-AMT < 10.00
               MOVE 10.00 TO EUVAL-NATIONAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUVAL-NATIONAL-AMT > 10000.00
               MOVE 10000.00 TO EUVAL-NATIONAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2720-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EUVAL-NATIONAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = EUVAL-NATIONAL-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CONV METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2730-DESCRIBE-CONV-METHOD.
           EVALUATE EUVAL-CONV-METHOD-CD
               WHEN 'GL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2740-FLAG-TRIANGULAR.
           IF EUVAL-TRIANGULAR-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EUVAL-TRIANGULAR-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE RESIDUAL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2750-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUVAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EUVAL-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUVAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE EUVAL-TRIANGULAR-YTD ROUNDED =
                       EUVAL-RESIDUAL-YTD * EUVAL-PENALTY-PCT / 200
               WHEN EUVAL-CURRENCY-CD = 'EW'
                   MOVE 20.00 TO EUVAL-TRIANGULAR-YTD
               WHEN EUVAL-CURRENCY-CD = 'FJ'
                   MOVE 3.00 TO EUVAL-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EUVAL-TRIANGULAR-YTD ROUNDED =
                       EUVAL-RESIDUAL-YTD * EUVAL-PENALTY-PCT / 100
           END-EVALUATE
           IF EUVAL-TRIANGULAR-YTD > 0 AND EUVAL-TRIANGULAR-YTD < 1.00
               MOVE 1.00 TO EUVAL-TRIANGULAR-YTD
           END-IF
           IF EUVAL-TRIANGULAR-YTD > 150.00
               MOVE 150.00 TO EUVAL-TRIANGULAR-YTD
           END-IF
           ADD EUVAL-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2760-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN MATURITY DATE IS PAST
      * JA -> LD WHEN MATURITY DATE IS PAST
      * LD -> MR WHEN MATURITY DATE IS PAST
      * MR -> VC WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2770-SET-CONV-METHOD.
           EVALUATE EUVAL-CONV-METHOD-CD
               WHEN 'GL'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO EUVAL-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO EUVAL-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO EUVAL-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VC' TO EUVAL-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NATIONAL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2780-FLAG-NATIONAL.
           IF EUVAL-NATIONAL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EUVAL-NATIONAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESIDUAL YTD BY CONV METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2790-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUVAL-CONV-METHOD-CD
                   ADD EUVAL-RESIDUAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUVAL-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUVAL-RESIDUAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2800-LIMIT-TRIANGULAR.
           IF EUVAL-TRIANGULAR-AMT < 10.00
               MOVE 10.00 TO EUVAL-TRIANGULAR-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUVAL-TRIANGULAR-AMT > 500.00
               MOVE 500.00 TO EUVAL-TRIANGULAR-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN MATURITY DATE IS PAST
      * JA -> LD WHEN MATURITY DATE IS PAST
      * LD -> MR WHEN MATURITY DATE IS PAST
      * MR -> VC WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2810-SET-CONV-METHOD.
           EVALUATE EUVAL-CONV-METHOD-CD
               WHEN 'GL'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO EUVAL-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO EUVAL-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO EUVAL-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VC' TO EUVAL-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRIANGULAR AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2820-ACCRUE-TRIANGULAR.
           IF EUVAL-TRIANGULAR-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUVAL-TRIANGULAR-AMT * EUVAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO EUVAL-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2830-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EUVAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EUVAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUVAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUVAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRIANGULAR YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2840-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUVAL-REGION-CD
                   ADD EUVAL-TRIANGULAR-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUVAL-TRIANGULAR-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2850-COUNT-PAYMENT.
           IF EUVAL-PAYMENT-CNT < 9999999
               ADD 1 TO EUVAL-PAYMENT-CNT
           END-IF
           IF EUVAL-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO EUVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2860-DESCRIBE-STATUS.
           EVALUATE EUVAL-STATUS-CD
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2870-FLAG-TRIANGULAR.
           IF EUVAL-TRIANGULAR-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUVAL-TRIANGULAR-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE RESIDUAL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2880-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUVAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EUVAL-RESIDUAL-YTD *
                       EUVAL-FEE-PCT / 200
               WHEN EUVAL-CONV-METHOD-CD = 'GL'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN EUVAL-CONV-METHOD-CD = 'JA'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EUVAL-RESIDUAL-YTD *
                       EUVAL-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RESIDUAL YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2890-LIMIT-RESIDUAL.
           IF EUVAL-RESIDUAL-YTD < 0.00
               MOVE 0.00 TO EUVAL-RESIDUAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUVAL-RESIDUAL-YTD > 2500.00
               MOVE 2500.00 TO EUVAL-RESIDUAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESIDUAL YTD.
      *----------------------------------------------------------------*
       2900-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUVAL-RESIDUAL-YTD ROUNDED =
                   EUVAL-CONVERTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EUVAL-RESIDUAL-YTD
           END-IF
           ADD EUVAL-RESIDUAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2910-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUVAL-NOTICE-DATE > 0 AND EUVAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUVAL-TRIANGULAR-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUVAL-TRIANGULAR-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD EUVAL-TRIANGULAR-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EUVAL-TRIANGULAR-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUVAL-TRIANGULAR-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * EW -> FJ WHEN MATURITY DATE IS PAST
      * FJ -> JC WHEN MATURITY DATE IS PAST
      * JC -> EW WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2920-SET-CURRENCY.
           EVALUATE EUVAL-CURRENCY-CD
               WHEN 'EW'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO EUVAL-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO EUVAL-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF EUVAL-MATURITY-DATE > 0 AND EUVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EW' TO EUVAL-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E695 CONTACT REQUIRED
      * E661 TRIANGULAR AMT NEGATIVE
      * E816 TRIANGULAR AMT OVER 50000
      * E735 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2930-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUVAL-CONTACT = SPACES
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN EUVAL-TRIANGULAR-AMT < 0
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN EUVAL-TRIANGULAR-AMT > 50000.00
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN EUVAL-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E735' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESIDUAL YTD BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2940-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUVAL-CURRENCY-CD
                   ADD EUVAL-RESIDUAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUVAL-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUVAL-RESIDUAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESIDUAL YTD.
      *----------------------------------------------------------------*
       2950-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUVAL-RESIDUAL-YTD ROUNDED =
                   EUVAL-TRIANGULAR-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUVAL-RESIDUAL-YTD
           END-IF
           ADD EUVAL-RESIDUAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E225 DESC REQUIRED
      * E937 RESIDUAL YTD NEGATIVE
      * E054 RESIDUAL YTD OVER 250000
      * E031 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2960-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUVAL-DESC = SPACES
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN EUVAL-RESIDUAL-YTD < 0
                   MOVE 'E937' TO WS-REJECT-CODE
               WHEN EUVAL-RESIDUAL-YTD > 250000.00
                   MOVE 'E054' TO WS-REJECT-CODE
               WHEN EUVAL-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE NATIONAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2970-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EUVAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EUVAL-NATIONAL-YTD NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EUVAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUVAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUVAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE NATIONAL AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2980-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUVAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EUVAL-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE EUVAL-TRIANGULAR-YTD ROUNDED =
                       EUVAL-NATIONAL-AMT * EUVAL-FEE-PCT / 200
               WHEN EUVAL-CURRENCY-CD = 'EW'
                   MOVE 7.00 TO EUVAL-TRIANGULAR-YTD
               WHEN EUVAL-CURRENCY-CD = 'FJ'
                   MOVE 3.00 TO EUVAL-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EUVAL-TRIANGULAR-YTD ROUNDED =
                       EUVAL-NATIONAL-AMT * EUVAL-FEE-PCT / 100
           END-EVALUATE
           IF EUVAL-TRIANGULAR-YTD > 0 AND EUVAL-TRIANGULAR-YTD < 2.00
               MOVE 2.00 TO EUVAL-TRIANGULAR-YTD
           END-IF
           IF EUVAL-TRIANGULAR-YTD > 25.00
               MOVE 25.00 TO EUVAL-TRIANGULAR-YTD
           END-IF
           ADD EUVAL-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2990-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EUVAL-TRIANGULAR-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EUVAL-TRIANGULAR-AMT - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESIDUAL YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-RESIDUAL.
           IF EUVAL-RESIDUAL-YTD < 10.00
               MOVE 10.00 TO EUVAL-RESIDUAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUVAL-RESIDUAL-YTD > 5000.00
               MOVE 5000.00 TO EUVAL-RESIDUAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUVAL-REGION-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO EUVAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO EUVAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO EUVAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'EUVAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
