      *================================================================*
      * PROGRAM  : HERISK                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : HOME EQUITY LINES RULE - ELIGIBILITY. CALLED BY THE *
      *            HOME EQUITY LINES BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-27 LMB  ORIGINAL                                       *
      * 1993-04-21 PQR  APPRAISE NOTICE TEXT PER LEGAL PRB3464         *
      * 1995-05-27 RJT  LIMIT CALCULATION CORRECTED CHG5299            *
      * 1998-03-10 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-12-06 JLM  MIN-PAY NOTICE TEXT PER LEGAL INC0975          *
      * 1999-12-27 RJT  DRAW LIMIT REVISED PRB4848                     *
      * 2001-02-23 MKL  AUDIT FINDING - INTEREST CONTROL TOTALS REQ2997*
      * 2002-04-14 MKL  AUDIT FINDING - INTEREST CONTROL TOTALS PRB0408*
      * 2003-08-15 CRP  APPRAISE LIMIT REVISED PRB2412                 *
      * 2005-01-25 BHS  NEW LIMIT REPORT COLUMNS REQ2490               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HERISK.
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
                                           VALUE 23.000000.
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
      * WS-INTEREST-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000700000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000669000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000028500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000571000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000444000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000840500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000647000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014250000000669000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014500000000394500'.
       01  WS-INTEREST-GRID-TBL REDEFINES WS-INTEREST-GRID-VALUES.
           05  WS-INTEREST-GRID-ROW OCCURS 9 TIMES.
               10  WS-INTEREST-GRID-LOW    PIC 9(09)V99.
               10  WS-INTEREST-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DRAW-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DRAW-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000173000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000847500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000357500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000564000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018000000000140500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000682000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025500000000651000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026000000000142000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028500000000073000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031000000000508500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031500000000510000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031750000000035000'.
       01  WS-DRAW-BAND-TBL REDEFINES WS-DRAW-BAND-VALUES.
           05  WS-DRAW-BAND-ROW OCCURS 12 TIMES.
               10  WS-DRAW-BAND-LOW        PIC 9(09)V99.
               10  WS-DRAW-BAND-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY HERISKP.

       PROCEDURE DIVISION USING HERISK-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-EDIT-DESC
           PERFORM 2010-LIMIT-BALANCE
           PERFORM 2020-DESCRIBE-DRAW-PERIOD
           PERFORM 2030-ASSESS-PREFERRED
           PERFORM 2040-SPLIT-MIN-PAY
           PERFORM 2050-CHECK-LAST-MAINT
           PERFORM 2060-SELECT-PENALTY-PCT
           PERFORM 2070-EDIT-DESC
           PERFORM 2080-PRORATE-MIN-PAY
           PERFORM 2090-PRORATE-MIN-PAY
           PERFORM 2100-EDIT-DESC
           PERFORM 2110-ACCUM-TYPE
           PERFORM 2120-AGE-EFFECTIVE
           PERFORM 2130-CHECK-EFFECTIVE
           PERFORM 2140-AGE-OPEN
           PERFORM 2150-FLAG-MIN-PAY
           PERFORM 2160-DESCRIBE-DRAW-PERIOD
           PERFORM 2170-SELECT-MARGIN
           PERFORM 2180-AGE-EFFECTIVE
           PERFORM 2190-ACCRUE-BALANCE
           PERFORM 2200-AGE-EFFECTIVE
           PERFORM 2210-SELECT-PENALTY-PCT
           PERFORM 2220-LIMIT-MIN-PAY
           PERFORM 2230-ACCUM-TYPE
           PERFORM 2240-PRORATE-BALANCE
           PERFORM 2250-FLAG-ANNUAL-FEE
           PERFORM 2260-SELECT-PENALTY-PCT
           PERFORM 2270-LIMIT-ITEM
           PERFORM 2280-LIMIT-ITEM
           PERFORM 2290-PRORATE-APPRAISE
           PERFORM 2300-AGE-EFFECTIVE
           PERFORM 2310-FLAG-BALANCE
           PERFORM 2320-SET-REASON
           PERFORM 2330-PRORATE-MIN-PAY
           PERFORM 2340-EDIT-DESC
           PERFORM 2350-ACCUM-TYPE
           PERFORM 2360-LIMIT-ANNUAL-FEE
           PERFORM 2370-DESCRIBE-TYPE
           PERFORM 2380-LIMIT-MIN-PAY
           PERFORM 2390-COUNT-PAYMENT
           PERFORM 2400-FLAG-MIN-PAY
           PERFORM 2410-SELECT-PENALTY-PCT
           PERFORM 2420-ASSESS-PREFERRED
           PERFORM 2430-CHECK-LAST-MAINT
           PERFORM 2440-EDIT-CONTACT
           PERFORM 2450-ACCRUE-MIN-PAY
           PERFORM 2460-CHECK-OPEN
           PERFORM 2470-SET-TYPE
           PERFORM 2480-ACCRUE-BALANCE
           PERFORM 2490-ASSESS-HOLD
           PERFORM 2500-ACCUM-REASON
           PERFORM 2510-SPLIT-MIN-PAY
           PERFORM 2520-DESCRIBE-DRAW-PERIOD
           PERFORM 2530-COUNT-PAYMENT
           PERFORM 2540-SELECT-MARGIN
           PERFORM 2550-PRORATE-BALANCE
           PERFORM 2560-CHECK-LAST-MAINT
           PERFORM 2570-FLAG-INTEREST
           PERFORM 2580-ACCRUE-INTEREST
           PERFORM 2590-CHECK-LAST-MAINT
           PERFORM 2600-ASSESS-HOLD
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO HERISK-RETURN-CODE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E878 DESC REQUIRED
      * E942 MIN PAY YTD NEGATIVE
      * E534 MIN PAY YTD OVER 25000
      * E208 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2000-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HERISK-DESC = SPACES
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN HERISK-MIN-PAY-YTD < 0
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN HERISK-MIN-PAY-YTD > 25000.00
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN HERISK-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2010-LIMIT-BALANCE.
           IF HERISK-BALANCE-YTD < 0.00
               MOVE 0.00 TO HERISK-BALANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HERISK-BALANCE-YTD > 50000.00
               MOVE 50000.00 TO HERISK-BALANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2020-DESCRIBE-DRAW-PERIOD.
           EVALUATE HERISK-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE YTD FEE ON THE APPRAISE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2030-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HERISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HERISK-BALANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HERISK-SUSPEND-FLAG = 'Y'
                   COMPUTE HERISK-BALANCE-YTD ROUNDED =
                       HERISK-APPRAISE-AMT * HERISK-MARGIN / 200
               WHEN HERISK-STATUS-CD = 'GR'
                   MOVE 3.00 TO HERISK-BALANCE-YTD
               WHEN HERISK-STATUS-CD = 'JC'
                   MOVE 7.00 TO HERISK-BALANCE-YTD
               WHEN OTHER
                   COMPUTE HERISK-BALANCE-YTD ROUNDED =
                       HERISK-APPRAISE-AMT * HERISK-MARGIN / 100
           END-EVALUATE
           IF HERISK-BALANCE-YTD > 0 AND HERISK-BALANCE-YTD < 1.00
               MOVE 1.00 TO HERISK-BALANCE-YTD
           END-IF
           IF HERISK-BALANCE-YTD > 75.00
               MOVE 75.00 TO HERISK-BALANCE-YTD
           END-IF
           ADD HERISK-BALANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT MIN PAY AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2040-SPLIT-MIN-PAY.
           COMPUTE WS-WORK-AMT = HERISK-MIN-PAY-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HERISK-MIN-PAY-AMT - (WS-WORK-AMT *
               4)
           COMPUTE HERISK-ANNUAL-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2050-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HERISK-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HERISK-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO HERISK-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRAW BAND TIER FOR THE MIN PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2060-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-BAND-RATE(1) TO HERISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF HERISK-MIN-PAY-YTD NOT < WS-DRAW-BAND-LOW(WS-TX)
                   MOVE WS-DRAW-BAND-RATE(WS-TX) TO HERISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HERISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HERISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E060 DESC REQUIRED
      * E426 WORK AMT 2 NEGATIVE
      * E874 WORK AMT 2 OVER 50000
      * E938 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2070-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HERISK-DESC = SPACES
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E426' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN HERISK-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN PAY AMT.
      *----------------------------------------------------------------*
       2080-PRORATE-MIN-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HERISK-MIN-PAY-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HERISK-MIN-PAY-AMT
           END-IF
           ADD HERISK-MIN-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE MIN PAY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN PAY AMT.
      *----------------------------------------------------------------*
       2090-PRORATE-MIN-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HERISK-MIN-PAY-AMT ROUNDED =
                   HERISK-MIN-PAY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HERISK-MIN-PAY-AMT
           END-IF
           ADD HERISK-MIN-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E697 DESC REQUIRED
      * E776 BALANCE YTD NEGATIVE
      * E011 BALANCE YTD OVER 5000
      * E432 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HERISK-DESC = SPACES
                   MOVE 'E697' TO WS-REJECT-CODE
               WHEN HERISK-BALANCE-YTD < 0
                   MOVE 'E776' TO WS-REJECT-CODE
               WHEN HERISK-BALANCE-YTD > 5000.00
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN HERISK-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HERISK-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HERISK-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2120-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HERISK-EFFECTIVE-DATE > 0 AND HERISK-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HERISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HERISK-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HERISK-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HERISK-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HERISK-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HERISK-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2130-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HERISK-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HERISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO HERISK-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HERISK-OPEN-DATE > 0 AND HERISK-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HERISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HERISK-MIN-PAY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HERISK-MIN-PAY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD HERISK-MIN-PAY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HERISK-MIN-PAY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HERISK-MIN-PAY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN PAY YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2150-FLAG-MIN-PAY.
           IF HERISK-MIN-PAY-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HERISK-MIN-PAY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2160-DESCRIBE-DRAW-PERIOD.
           EVALUATE HERISK-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DRAW BAND TIER FOR THE BALANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2170-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-BAND-RATE(1) TO HERISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF HERISK-BALANCE-YTD NOT < WS-DRAW-BAND-LOW(WS-TX)
                   MOVE WS-DRAW-BAND-RATE(WS-TX) TO HERISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HERISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HERISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2180-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HERISK-EFFECTIVE-DATE > 0 AND HERISK-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HERISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HERISK-MIN-PAY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HERISK-MIN-PAY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HERISK-MIN-PAY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD HERISK-MIN-PAY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HERISK-MIN-PAY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2190-ACCRUE-BALANCE.
           IF HERISK-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HERISK-BALANCE-YTD * HERISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO HERISK-ANNUAL-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2200-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HERISK-EFFECTIVE-DATE > 0 AND HERISK-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HERISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HERISK-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HERISK-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HERISK-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HERISK-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HERISK-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE APPRAISE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2210-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO HERISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF HERISK-APPRAISE-AMT NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO
                       HERISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HERISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HERISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2220-LIMIT-MIN-PAY.
           IF HERISK-MIN-PAY-AMT < 50.00
               MOVE 50.00 TO HERISK-MIN-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HERISK-MIN-PAY-AMT > 2500.00
               MOVE 2500.00 TO HERISK-MIN-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2230-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HERISK-TYPE-CD
                   ADD HERISK-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HERISK-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HERISK-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BALANCE YTD.
      *----------------------------------------------------------------*
       2240-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HERISK-BALANCE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HERISK-BALANCE-YTD
           END-IF
           ADD HERISK-BALANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2250-FLAG-ANNUAL-FEE.
           IF HERISK-ANNUAL-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HERISK-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRAW BAND TIER FOR THE BALANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2260-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-BAND-RATE(1) TO HERISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF HERISK-BALANCE-YTD NOT < WS-DRAW-BAND-LOW(WS-TX)
                   MOVE WS-DRAW-BAND-RATE(WS-TX) TO HERISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HERISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HERISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2270-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2280-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO APPRAISE AMT.
      *----------------------------------------------------------------*
       2290-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HERISK-APPRAISE-AMT ROUNDED =
                   HERISK-ANNUAL-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HERISK-APPRAISE-AMT
           END-IF
           ADD HERISK-APPRAISE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2300-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HERISK-EFFECTIVE-DATE > 0 AND HERISK-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HERISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2310-FLAG-BALANCE.
           IF HERISK-BALANCE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HERISK-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN OPEN DATE IS PAST
      * NWS -> ZJM WHEN OPEN DATE IS PAST
      * ZJM -> KST WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2320-SET-REASON.
           EVALUATE HERISK-REASON-CD
               WHEN 'KST'
                   IF HERISK-OPEN-DATE > 0 AND HERISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HERISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HERISK-OPEN-DATE > 0 AND HERISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HERISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HERISK-OPEN-DATE > 0 AND HERISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KST' TO HERISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MIN PAY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2330-PRORATE-MIN-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HERISK-MIN-PAY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E155 DESC REQUIRED
      * E523 MIN PAY AMT NEGATIVE
      * E816 MIN PAY AMT OVER 1000000
      * E067 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HERISK-DESC = SPACES
                   MOVE 'E155' TO WS-REJECT-CODE
               WHEN HERISK-MIN-PAY-AMT < 0
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN HERISK-MIN-PAY-AMT > 1000000.00
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN HERISK-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E067' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2350-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HERISK-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HERISK-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2360-LIMIT-ANNUAL-FEE.
           IF HERISK-ANNUAL-FEE-AMT < 25.00
               MOVE 25.00 TO HERISK-ANNUAL-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HERISK-ANNUAL-FEE-AMT > 5000.00
               MOVE 5000.00 TO HERISK-ANNUAL-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-TYPE.
           EVALUATE HERISK-TYPE-CD
               WHEN 'CD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN PAY YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2380-LIMIT-MIN-PAY.
           IF HERISK-MIN-PAY-YTD < 25.00
               MOVE 25.00 TO HERISK-MIN-PAY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HERISK-MIN-PAY-YTD > 10000.00
               MOVE 10000.00 TO HERISK-MIN-PAY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2390-COUNT-PAYMENT.
           IF HERISK-PAYMENT-CNT < 9999999
               ADD 1 TO HERISK-PAYMENT-CNT
           END-IF
           IF HERISK-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO HERISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2400-FLAG-MIN-PAY.
           IF HERISK-MIN-PAY-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HERISK-MIN-PAY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2410-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO HERISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO
                       HERISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HERISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HERISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN PAY FEE ON THE MIN PAY YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2420-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HERISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HERISK-MIN-PAY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HERISK-SUSPEND-FLAG = 'Y'
                   COMPUTE HERISK-MIN-PAY-AMT ROUNDED =
                       HERISK-MIN-PAY-YTD * HERISK-MARGIN / 200
               WHEN HERISK-TYPE-CD = 'CD'
                   MOVE 5.00 TO HERISK-MIN-PAY-AMT
               WHEN HERISK-TYPE-CD = 'JW'
                   MOVE 7.00 TO HERISK-MIN-PAY-AMT
               WHEN OTHER
                   COMPUTE HERISK-MIN-PAY-AMT ROUNDED =
                       HERISK-MIN-PAY-YTD * HERISK-MARGIN / 100
           END-EVALUATE
           IF HERISK-MIN-PAY-AMT > 0 AND HERISK-MIN-PAY-AMT < 5.00
               MOVE 5.00 TO HERISK-MIN-PAY-AMT
           END-IF
           IF HERISK-MIN-PAY-AMT > 75.00
               MOVE 75.00 TO HERISK-MIN-PAY-AMT
           END-IF
           ADD HERISK-MIN-PAY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2430-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HERISK-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HERISK-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO HERISK-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E082 CONTACT REQUIRED
      * E301 APPRAISE AMT NEGATIVE
      * E584 APPRAISE AMT OVER 5000
      * E992 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2440-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HERISK-CONTACT = SPACES
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN HERISK-APPRAISE-AMT < 0
                   MOVE 'E301' TO WS-REJECT-CODE
               WHEN HERISK-APPRAISE-AMT > 5000.00
                   MOVE 'E584' TO WS-REJECT-CODE
               WHEN HERISK-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E992' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN PAY YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2450-ACCRUE-MIN-PAY.
           IF HERISK-MIN-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HERISK-MIN-PAY-YTD * HERISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO HERISK-APPRAISE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2460-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HERISK-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HERISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO HERISK-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN EFFECTIVE DATE IS PAST
      * JW -> KV WHEN EFFECTIVE DATE IS PAST
      * KV -> WS WHEN EFFECTIVE DATE IS PAST
      * WS -> XL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-TYPE.
           EVALUATE HERISK-TYPE-CD
               WHEN 'CD'
                   IF HERISK-EFFECTIVE-DATE > 0 AND
                       HERISK-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'JW' TO HERISK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JW'
                   IF HERISK-EFFECTIVE-DATE > 0 AND
                       HERISK-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'KV' TO HERISK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF HERISK-EFFECTIVE-DATE > 0 AND
                       HERISK-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'WS' TO HERISK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WS'
                   IF HERISK-EFFECTIVE-DATE > 0 AND
                       HERISK-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'XL' TO HERISK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2480-ACCRUE-BALANCE.
           IF HERISK-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HERISK-BALANCE-YTD * HERISK-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HERISK-APPRAISE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE BALANCE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2490-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HERISK-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HERISK-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = HERISK-BALANCE-YTD *
                       HERISK-PENALTY-PCT / 200
               WHEN HERISK-REASON-CD = 'KST'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN HERISK-REASON-CD = 'NWS'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = HERISK-BALANCE-YTD *
                       HERISK-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HERISK-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HERISK-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-MIN-PAY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE HERISK-MIN-PAY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2520-DESCRIBE-DRAW-PERIOD.
           EVALUATE HERISK-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2530-COUNT-PAYMENT.
           IF HERISK-PAYMENT-CNT < 9999999
               ADD 1 TO HERISK-PAYMENT-CNT
           END-IF
           IF HERISK-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO HERISK-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRAW BAND TIER FOR THE MIN PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2540-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-BAND-RATE(1) TO HERISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF HERISK-MIN-PAY-YTD NOT < WS-DRAW-BAND-LOW(WS-TX)
                   MOVE WS-DRAW-BAND-RATE(WS-TX) TO HERISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HERISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HERISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BALANCE YTD.
      *----------------------------------------------------------------*
       2550-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HERISK-BALANCE-YTD ROUNDED =
                   HERISK-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HERISK-BALANCE-YTD
           END-IF
           ADD HERISK-BALANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HERISK-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HERISK-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO HERISK-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2570-FLAG-INTEREST.
           IF HERISK-INTEREST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HERISK-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-INTEREST.
           IF HERISK-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HERISK-INTEREST-AMT * HERISK-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HERISK-APPRAISE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2590-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HERISK-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HERISK-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO HERISK-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE FEE ON THE INTEREST AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2600-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HERISK-HOLD-FLAG = 'Y'
                   MOVE ZERO TO HERISK-APPRAISE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HERISK-SUSPEND-FLAG = 'Y'
                   COMPUTE HERISK-APPRAISE-AMT ROUNDED =
                       HERISK-INTEREST-AMT * HERISK-PENALTY-PCT / 200
               WHEN HERISK-TYPE-CD = 'CD'
                   MOVE 15.00 TO HERISK-APPRAISE-AMT
               WHEN HERISK-TYPE-CD = 'JW'
                   MOVE 5.00 TO HERISK-APPRAISE-AMT
               WHEN OTHER
                   COMPUTE HERISK-APPRAISE-AMT ROUNDED =
                       HERISK-INTEREST-AMT * HERISK-PENALTY-PCT / 100
           END-EVALUATE
           IF HERISK-APPRAISE-AMT > 0 AND HERISK-APPRAISE-AMT < 5.00
               MOVE 5.00 TO HERISK-APPRAISE-AMT
           END-IF
           IF HERISK-APPRAISE-AMT > 250.00
               MOVE 250.00 TO HERISK-APPRAISE-AMT
           END-IF
           ADD HERISK-APPRAISE-AMT TO WS-TOTAL-FEES.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO HERISK-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO HERISK-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO HERISK-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'HERISK EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
