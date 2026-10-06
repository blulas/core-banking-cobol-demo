      *================================================================*
      * PROGRAM  : BUSTAT                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : BUMP-UP CERTIFICATES RULE - VALIDATION. CALLED BY   *
      *            THE BUMP-UP CERTIFICATES BATCH AND ONLINE PROGRAMS. *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-12-28 DPS  ORIGINAL                                       *
      * 1994-11-22 TWH  NEW PENALTY REPORT COLUMNS INC9676             *
      * 1995-05-28 TWH  ADDON FEE SCHEDULE UPDATE REQ1547              *
      * 1997-06-06 MKL  ADDON EXCEPTIONS TO OPS QUEUE PRB5529          *
      * 1998-10-03 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-08 RJT  PERFORMANCE - PRINCIPAL READ AHEAD REQ9410     *
      * 2002-08-03 GAF  PRINCIPAL TABLE EXPANDED PRB0547               *
      * 2004-12-16 LMB  PENALTY CALCULATION CORRECTED INC6273          *
      * 2004-08-27 EWS  NEW PENALTY REPORT COLUMNS REQ4231             *
      * 2004-09-23 BHS  AUDIT FINDING - PENALTY CONTROL TOTALS REQ7189 *
      * 2005-10-24 PQR  PENALTY FEE SCHEDULE UPDATE CHG7750            *
      * 2006-08-28 LMB  PERFORMANCE - PENALTY READ AHEAD CHG8527       *
      * 2007-11-05 MKL  NEW INTEREST REPORT COLUMNS INC1815            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUSTAT.
       AUTHOR. R J TRAVERS.

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
      * WS-PENALTY-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000215000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000813500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000014000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000701000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002350000000634500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003350000000117500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005850000000435500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006850000000084500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006900000000804000'.
       01  WS-PENALTY-SCHED-TBL REDEFINES WS-PENALTY-SCHED-VALUES.
           05  WS-PENALTY-SCHED-ROW OCCURS 9 TIMES.
               10  WS-PENALTY-SCHED-LOW    PIC 9(09)V99.
               10  WS-PENALTY-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ADDON-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ADDON-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000724000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000219000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000812000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000426500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000122000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023500000000866000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024000000000595000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034000000000070500'.
       01  WS-ADDON-BAND-TBL REDEFINES WS-ADDON-BAND-VALUES.
           05  WS-ADDON-BAND-ROW OCCURS 8 TIMES.
               10  WS-ADDON-BAND-LOW       PIC 9(09)V99.
               10  WS-ADDON-BAND-RATE      PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY BUSTATP.

       PROCEDURE DIVISION USING BUSTAT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ASSESS-STOP
           PERFORM 2010-FLAG-ITEM
           PERFORM 2020-PRORATE-PRINCIPAL
           PERFORM 2030-LIMIT-BUMP
           PERFORM 2040-FLAG-PENALTY
           PERFORM 2050-FLAG-ADDON
           PERFORM 2060-AGE-EXPIRY
           PERFORM 2070-ACCRUE-PENALTY
           PERFORM 2080-COUNT-CYCLE
           PERFORM 2090-ACCRUE-BUMP
           PERFORM 2100-FLAG-ADDON
           PERFORM 2110-FLAG-PRINCIPAL
           PERFORM 2120-PRORATE-INTEREST
           PERFORM 2130-SELECT-RATE
           PERFORM 2140-EDIT-NAME
           PERFORM 2150-SET-STEP-PLAN
           PERFORM 2160-DESCRIBE-SOURCE
           PERFORM 2170-ACCUM-ELECTION
           PERFORM 2180-FLAG-ITEM
           PERFORM 2190-PRORATE-BUMP
           PERFORM 2200-CHECK-LAST-PAY
           PERFORM 2210-EDIT-ADDR
           PERFORM 2220-AGE-LAST-STMT
           PERFORM 2230-ACCUM-STEP-PLAN
           PERFORM 2240-LIMIT-PRINCIPAL
           PERFORM 2250-SELECT-PENALTY-PCT
           PERFORM 2260-EDIT-ADDR
           PERFORM 2270-ASSESS-STOP
           PERFORM 2280-ACCRUE-PRINCIPAL
           PERFORM 2290-SPLIT-PRINCIPAL
           PERFORM 2300-DESCRIBE-SOURCE
           PERFORM 2310-EDIT-ADDR
           PERFORM 2320-COUNT-CYCLE
           PERFORM 2330-LIMIT-PENALTY
           PERFORM 2340-CHECK-LAST-STMT
           PERFORM 2350-ASSESS-OVERRIDE
           PERFORM 2360-COUNT-ITEM
           PERFORM 2370-LIMIT-PENALTY
           PERFORM 2380-SPLIT-BUMP
           PERFORM 2390-SET-CHANNEL
           PERFORM 2400-PRORATE-PRINCIPAL
           PERFORM 2410-SET-STEP-PLAN
           PERFORM 2420-SET-SOURCE
           PERFORM 2430-SET-ELECTION
           PERFORM 2440-LIMIT-PENALTY
           PERFORM 2450-ACCRUE-PENALTY
           PERFORM 2460-ACCUM-SOURCE
           PERFORM 2470-FLAG-ITEM
           PERFORM 2480-CHECK-EXPIRY
           PERFORM 2490-ASSESS-STOP
           PERFORM 2500-SELECT-PENALTY-PCT
           PERFORM 2510-DESCRIBE-ELECTION
           PERFORM 2520-SELECT-RATE
           PERFORM 2530-ACCUM-SOURCE
           PERFORM 2540-DESCRIBE-SOURCE
           PERFORM 2550-DESCRIBE-STEP-PLAN
           PERFORM 2560-ASSESS-REVIEW
           PERFORM 2570-SET-ELECTION
           PERFORM 2580-COUNT-ITEM
           PERFORM 2590-ASSESS-OVERRIDE
           PERFORM 2600-COUNT-CYCLE
           PERFORM 2610-SET-SOURCE
           PERFORM 2620-EDIT-ADDR
           PERFORM 2630-CHECK-LAST-STMT
           PERFORM 2640-EDIT-NAME
           PERFORM 2650-ACCUM-STEP-PLAN
           PERFORM 2660-FLAG-PENALTY
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO BUSTAT-RETURN-CODE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2000-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BUSTAT-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUSTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       BUSTAT-PENALTY-PCT / 200
               WHEN BUSTAT-SOURCE-CD = 'AG'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN BUSTAT-SOURCE-CD = 'BF'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       BUSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2010-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       2020-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUSTAT-PRINCIPAL-AMT ROUNDED =
                   BUSTAT-PENALTY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BUSTAT-PRINCIPAL-AMT
           END-IF
           ADD BUSTAT-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BUMP AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2030-LIMIT-BUMP.
           IF BUSTAT-BUMP-AMT < 10.00
               MOVE 10.00 TO BUSTAT-BUMP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUSTAT-BUMP-AMT > 1000.00
               MOVE 1000.00 TO BUSTAT-BUMP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2040-FLAG-PENALTY.
           IF BUSTAT-PENALTY-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BUSTAT-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2050-FLAG-ADDON.
           IF BUSTAT-ADDON-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BUSTAT-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BUSTAT-EXPIRY-DATE > 0 AND BUSTAT-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUSTAT-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BUSTAT-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BUSTAT-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BUSTAT-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BUSTAT-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BUSTAT-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2070-ACCRUE-PENALTY.
           IF BUSTAT-PENALTY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUSTAT-PENALTY-YTD * BUSTAT-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-CYCLE.
           IF BUSTAT-CYCLE-CNT < 9999999
               ADD 1 TO BUSTAT-CYCLE-CNT
           END-IF
           IF BUSTAT-CYCLE-CNT NOT < 6
               MOVE 'Y' TO BUSTAT-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2090-ACCRUE-BUMP.
           IF BUSTAT-BUMP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUSTAT-BUMP-AMT * BUSTAT-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2100-FLAG-ADDON.
           IF BUSTAT-ADDON-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BUSTAT-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2110-FLAG-PRINCIPAL.
           IF BUSTAT-PRINCIPAL-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BUSTAT-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       2120-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUSTAT-INTEREST-YTD ROUNDED =
                   BUSTAT-PENALTY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BUSTAT-INTEREST-YTD
           END-IF
           ADD BUSTAT-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2130-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO BUSTAT-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF BUSTAT-INTEREST-YTD NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO BUSTAT-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUSTAT-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUSTAT-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E100 NAME REQUIRED
      * E892 ADDON AMT NEGATIVE
      * E677 ADDON AMT OVER 5000
      * E509 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2140-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUSTAT-NAME = SPACES
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN BUSTAT-ADDON-AMT < 0
                   MOVE 'E892' TO WS-REJECT-CODE
               WHEN BUSTAT-ADDON-AMT > 5000.00
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN BUSTAT-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN MATURITY DATE IS PAST
      * DGU -> GWJ WHEN MATURITY DATE IS PAST
      * GWJ -> RJL WHEN MATURITY DATE IS PAST
      * RJL -> UPM WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2150-SET-STEP-PLAN.
           EVALUATE BUSTAT-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BUSTAT-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BUSTAT-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BUSTAT-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BUSTAT-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2160-DESCRIBE-SOURCE.
           EVALUATE BUSTAT-SOURCE-CD
               WHEN 'AG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY ELECTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2170-ACCUM-ELECTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUSTAT-ELECTION-CD
                   ADD BUSTAT-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUSTAT-ELECTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUSTAT-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2180-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUMP AMT.
      *----------------------------------------------------------------*
       2190-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUSTAT-BUMP-AMT ROUNDED =
                   BUSTAT-ADDON-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BUSTAT-BUMP-AMT
           END-IF
           ADD BUSTAT-BUMP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2200-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BUSTAT-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUSTAT-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BUSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E990 ADDR REQUIRED
      * E841 INTEREST YTD NEGATIVE
      * E387 INTEREST YTD OVER 1000000
      * E613 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2210-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUSTAT-ADDR = SPACES
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN BUSTAT-INTEREST-YTD < 0
                   MOVE 'E841' TO WS-REJECT-CODE
               WHEN BUSTAT-INTEREST-YTD > 1000000.00
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN BUSTAT-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E613' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2220-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BUSTAT-LAST-STMT-DATE > 0 AND BUSTAT-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BUSTAT-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BUSTAT-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BUSTAT-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BUSTAT-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BUSTAT-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY YTD BY STEP PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2230-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUSTAT-STEP-PLAN-CD
                   ADD BUSTAT-PENALTY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUSTAT-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUSTAT-PENALTY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2240-LIMIT-PRINCIPAL.
           IF BUSTAT-PRINCIPAL-AMT < 0.00
               MOVE 0.00 TO BUSTAT-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUSTAT-PRINCIPAL-AMT > 5000.00
               MOVE 5000.00 TO BUSTAT-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADDON BAND TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2250-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ADDON-BAND-RATE(1) TO BUSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BUSTAT-ADDON-AMT NOT < WS-ADDON-BAND-LOW(WS-TX)
                   MOVE WS-ADDON-BAND-RATE(WS-TX) TO BUSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E044 ADDR REQUIRED
      * E511 INTEREST YTD NEGATIVE
      * E686 INTEREST YTD OVER 5000
      * E341 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2260-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUSTAT-ADDR = SPACES
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN BUSTAT-INTEREST-YTD < 0
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN BUSTAT-INTEREST-YTD > 5000.00
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN BUSTAT-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E341' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2270-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BUSTAT-STOP-FLAG = 'Y'
                   MOVE ZERO TO BUSTAT-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUSTAT-REVIEW-FLAG = 'Y'
                   COMPUTE BUSTAT-PENALTY-AMT ROUNDED = WS-WORK-AMT *
                       BUSTAT-PENALTY-PCT / 200
               WHEN BUSTAT-CHANNEL-CD = 'EK'
                   MOVE 3.00 TO BUSTAT-PENALTY-AMT
               WHEN BUSTAT-CHANNEL-CD = 'JE'
                   MOVE 12.00 TO BUSTAT-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BUSTAT-PENALTY-AMT ROUNDED = WS-WORK-AMT *
                       BUSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF BUSTAT-PENALTY-AMT > 0 AND BUSTAT-PENALTY-AMT < 1.00
               MOVE 1.00 TO BUSTAT-PENALTY-AMT
           END-IF
           IF BUSTAT-PENALTY-AMT > 75.00
               MOVE 75.00 TO BUSTAT-PENALTY-AMT
           END-IF
           ADD BUSTAT-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-PRINCIPAL.
           IF BUSTAT-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUSTAT-PRINCIPAL-AMT * BUSTAT-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BUSTAT-PENALTY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2290-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BUSTAT-PRINCIPAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BUSTAT-PRINCIPAL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE BUSTAT-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-SOURCE.
           EVALUATE BUSTAT-SOURCE-CD
               WHEN 'AG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E017 ADDR REQUIRED
      * E417 PENALTY AMT NEGATIVE
      * E870 PENALTY AMT OVER 99999
      * E032 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2310-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUSTAT-ADDR = SPACES
                   MOVE 'E017' TO WS-REJECT-CODE
               WHEN BUSTAT-PENALTY-AMT < 0
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN BUSTAT-PENALTY-AMT > 99999.00
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN BUSTAT-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E032' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2320-COUNT-CYCLE.
           IF BUSTAT-CYCLE-CNT < 9999999
               ADD 1 TO BUSTAT-CYCLE-CNT
           END-IF
           IF BUSTAT-CYCLE-CNT NOT < 12
               MOVE 'Y' TO BUSTAT-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2330-LIMIT-PENALTY.
           IF BUSTAT-PENALTY-YTD < 10.00
               MOVE 10.00 TO BUSTAT-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUSTAT-PENALTY-YTD > 1000.00
               MOVE 1000.00 TO BUSTAT-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2340-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BUSTAT-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BUSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE INTEREST YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2350-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BUSTAT-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BUSTAT-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUSTAT-REVIEW-FLAG = 'Y'
                   COMPUTE BUSTAT-PENALTY-AMT ROUNDED =
                       BUSTAT-INTEREST-YTD * BUSTAT-PENALTY-PCT / 200
               WHEN BUSTAT-CHANNEL-CD = 'EK'
                   MOVE 15.00 TO BUSTAT-PENALTY-AMT
               WHEN BUSTAT-CHANNEL-CD = 'JE'
                   MOVE 10.00 TO BUSTAT-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BUSTAT-PENALTY-AMT ROUNDED =
                       BUSTAT-INTEREST-YTD * BUSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF BUSTAT-PENALTY-AMT > 0 AND BUSTAT-PENALTY-AMT < 10.00
               MOVE 10.00 TO BUSTAT-PENALTY-AMT
           END-IF
           IF BUSTAT-PENALTY-AMT > 25.00
               MOVE 25.00 TO BUSTAT-PENALTY-AMT
           END-IF
           ADD BUSTAT-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-ITEM.
           IF BUSTAT-ITEM-CNT < 9999999
               ADD 1 TO BUSTAT-ITEM-CNT
           END-IF
           IF BUSTAT-ITEM-CNT NOT < 6
               MOVE 'Y' TO BUSTAT-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2370-LIMIT-PENALTY.
           IF BUSTAT-PENALTY-AMT < 0.00
               MOVE 0.00 TO BUSTAT-PENALTY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUSTAT-PENALTY-AMT > 10000.00
               MOVE 10000.00 TO BUSTAT-PENALTY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2380-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE BUSTAT-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * EK -> JE WHEN LAST PAY DATE IS PAST
      * JE -> PN WHEN LAST PAY DATE IS PAST
      * PN -> WV WHEN LAST PAY DATE IS PAST
      * WV -> EK WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2390-SET-CHANNEL.
           EVALUATE BUSTAT-CHANNEL-CD
               WHEN 'EK'
                   IF BUSTAT-LAST-PAY-DATE > 0 AND BUSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JE' TO BUSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF BUSTAT-LAST-PAY-DATE > 0 AND BUSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PN' TO BUSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PN'
                   IF BUSTAT-LAST-PAY-DATE > 0 AND BUSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WV' TO BUSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF BUSTAT-LAST-PAY-DATE > 0 AND BUSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EK' TO BUSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       2400-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUSTAT-PRINCIPAL-AMT ROUNDED =
                   BUSTAT-BUMP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BUSTAT-PRINCIPAL-AMT
           END-IF
           ADD BUSTAT-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN EXPIRY DATE IS PAST
      * DGU -> GWJ WHEN EXPIRY DATE IS PAST
      * GWJ -> RJL WHEN EXPIRY DATE IS PAST
      * RJL -> UPM WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2410-SET-STEP-PLAN.
           EVALUATE BUSTAT-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BUSTAT-EXPIRY-DATE > 0 AND BUSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BUSTAT-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BUSTAT-EXPIRY-DATE > 0 AND BUSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BUSTAT-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BUSTAT-EXPIRY-DATE > 0 AND BUSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BUSTAT-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BUSTAT-EXPIRY-DATE > 0 AND BUSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BUSTAT-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN MATURITY DATE IS PAST
      * BF -> HF WHEN MATURITY DATE IS PAST
      * HF -> HR WHEN MATURITY DATE IS PAST
      * HR -> UF WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2420-SET-SOURCE.
           EVALUATE BUSTAT-SOURCE-CD
               WHEN 'AG'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN EXPIRY DATE IS PAST
      * M -> N WHEN EXPIRY DATE IS PAST
      * N -> R WHEN EXPIRY DATE IS PAST
      * R -> V WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2430-SET-ELECTION.
           EVALUATE BUSTAT-ELECTION-CD
               WHEN 'L'
                   IF BUSTAT-EXPIRY-DATE > 0 AND BUSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO BUSTAT-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BUSTAT-EXPIRY-DATE > 0 AND BUSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO BUSTAT-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BUSTAT-EXPIRY-DATE > 0 AND BUSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO BUSTAT-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BUSTAT-EXPIRY-DATE > 0 AND BUSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO BUSTAT-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2440-LIMIT-PENALTY.
           IF BUSTAT-PENALTY-YTD < 0.00
               MOVE 0.00 TO BUSTAT-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUSTAT-PENALTY-YTD > 1000.00
               MOVE 1000.00 TO BUSTAT-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2450-ACCRUE-PENALTY.
           IF BUSTAT-PENALTY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUSTAT-PENALTY-YTD * BUSTAT-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2460-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUSTAT-SOURCE-CD
                   ADD BUSTAT-BUMP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUSTAT-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUSTAT-BUMP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2470-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2480-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BUSTAT-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUSTAT-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BUSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE PENALTY YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2490-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BUSTAT-STOP-FLAG = 'Y'
                   MOVE ZERO TO BUSTAT-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUSTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE BUSTAT-ADDON-AMT ROUNDED = BUSTAT-PENALTY-YTD
                       * BUSTAT-RATE / 200
               WHEN BUSTAT-ELECTION-CD = 'L'
                   MOVE 10.00 TO BUSTAT-ADDON-AMT
               WHEN BUSTAT-ELECTION-CD = 'M'
                   MOVE 3.00 TO BUSTAT-ADDON-AMT
               WHEN OTHER
                   COMPUTE BUSTAT-ADDON-AMT ROUNDED = BUSTAT-PENALTY-YTD
                       * BUSTAT-RATE / 100
           END-EVALUATE
           IF BUSTAT-ADDON-AMT > 0 AND BUSTAT-ADDON-AMT < 1.00
               MOVE 1.00 TO BUSTAT-ADDON-AMT
           END-IF
           IF BUSTAT-ADDON-AMT > 35.00
               MOVE 35.00 TO BUSTAT-ADDON-AMT
           END-IF
           ADD BUSTAT-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE ADDON BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2500-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ADDON-BAND-RATE(1) TO BUSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-ADDON-BAND-LOW(WS-TX)
                   MOVE WS-ADDON-BAND-RATE(WS-TX) TO BUSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2510-DESCRIBE-ELECTION.
           EVALUATE BUSTAT-ELECTION-CD
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'N'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE PENALTY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2520-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO BUSTAT-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF BUSTAT-PENALTY-YTD NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO BUSTAT-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUSTAT-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUSTAT-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2530-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUSTAT-SOURCE-CD
                   ADD BUSTAT-BUMP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUSTAT-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUSTAT-BUMP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2540-DESCRIBE-SOURCE.
           EVALUATE BUSTAT-SOURCE-CD
               WHEN 'AG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STEP PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2550-DESCRIBE-STEP-PLAN.
           EVALUATE BUSTAT-STEP-PLAN-CD
               WHEN 'AYX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DGU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GWJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RJL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UPM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VCX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YPY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2560-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BUSTAT-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BUSTAT-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUSTAT-STOP-FLAG = 'Y'
                   COMPUTE BUSTAT-INTEREST-YTD ROUNDED =
                       BUSTAT-PRINCIPAL-AMT * BUSTAT-PENALTY-PCT / 200
               WHEN BUSTAT-STEP-PLAN-CD = 'AYX'
                   MOVE 10.00 TO BUSTAT-INTEREST-YTD
               WHEN BUSTAT-STEP-PLAN-CD = 'DGU'
                   MOVE 10.00 TO BUSTAT-INTEREST-YTD
               WHEN OTHER
                   COMPUTE BUSTAT-INTEREST-YTD ROUNDED =
                       BUSTAT-PRINCIPAL-AMT * BUSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF BUSTAT-INTEREST-YTD > 0 AND BUSTAT-INTEREST-YTD < 5.00
               MOVE 5.00 TO BUSTAT-INTEREST-YTD
           END-IF
           IF BUSTAT-INTEREST-YTD > 150.00
               MOVE 150.00 TO BUSTAT-INTEREST-YTD
           END-IF
           ADD BUSTAT-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN LAST PAY DATE IS PAST
      * M -> N WHEN LAST PAY DATE IS PAST
      * N -> R WHEN LAST PAY DATE IS PAST
      * R -> V WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2570-SET-ELECTION.
           EVALUATE BUSTAT-ELECTION-CD
               WHEN 'L'
                   IF BUSTAT-LAST-PAY-DATE > 0 AND BUSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO BUSTAT-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BUSTAT-LAST-PAY-DATE > 0 AND BUSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'N' TO BUSTAT-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BUSTAT-LAST-PAY-DATE > 0 AND BUSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO BUSTAT-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BUSTAT-LAST-PAY-DATE > 0 AND BUSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO BUSTAT-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2580-COUNT-ITEM.
           IF BUSTAT-ITEM-CNT < 9999999
               ADD 1 TO BUSTAT-ITEM-CNT
           END-IF
           IF BUSTAT-ITEM-CNT NOT < 12
               MOVE 'Y' TO BUSTAT-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2590-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BUSTAT-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BUSTAT-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUSTAT-REVIEW-FLAG = 'Y'
                   COMPUTE BUSTAT-ADDON-AMT ROUNDED =
                       BUSTAT-PRINCIPAL-AMT * BUSTAT-RATE / 200
               WHEN BUSTAT-SOURCE-CD = 'AG'
                   MOVE 3.00 TO BUSTAT-ADDON-AMT
               WHEN BUSTAT-SOURCE-CD = 'BF'
                   MOVE 10.00 TO BUSTAT-ADDON-AMT
               WHEN OTHER
                   COMPUTE BUSTAT-ADDON-AMT ROUNDED =
                       BUSTAT-PRINCIPAL-AMT * BUSTAT-RATE / 100
           END-EVALUATE
           IF BUSTAT-ADDON-AMT > 0 AND BUSTAT-ADDON-AMT < 2.00
               MOVE 2.00 TO BUSTAT-ADDON-AMT
           END-IF
           IF BUSTAT-ADDON-AMT > 250.00
               MOVE 250.00 TO BUSTAT-ADDON-AMT
           END-IF
           ADD BUSTAT-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2600-COUNT-CYCLE.
           IF BUSTAT-CYCLE-CNT < 9999999
               ADD 1 TO BUSTAT-CYCLE-CNT
           END-IF
           IF BUSTAT-CYCLE-CNT NOT < 3
               MOVE 'Y' TO BUSTAT-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN MATURITY DATE IS PAST
      * BF -> HF WHEN MATURITY DATE IS PAST
      * HF -> HR WHEN MATURITY DATE IS PAST
      * HR -> UF WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2610-SET-SOURCE.
           EVALUATE BUSTAT-SOURCE-CD
               WHEN 'AG'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUSTAT-MATURITY-DATE > 0 AND BUSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E600 ADDR REQUIRED
      * E199 PENALTY AMT NEGATIVE
      * E767 PENALTY AMT OVER 10000
      * E133 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2620-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUSTAT-ADDR = SPACES
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN BUSTAT-PENALTY-AMT < 0
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN BUSTAT-PENALTY-AMT > 10000.00
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN BUSTAT-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BUSTAT-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BUSTAT-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E301 NAME REQUIRED
      * E268 WORK AMT 2 NEGATIVE
      * E866 WORK AMT 2 OVER 25000
      * E444 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2640-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUSTAT-NAME = SPACES
                   MOVE 'E301' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E866' TO WS-REJECT-CODE
               WHEN BUSTAT-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP AMT BY STEP PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2650-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUSTAT-STEP-PLAN-CD
                   ADD BUSTAT-BUMP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUSTAT-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUSTAT-BUMP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2660-FLAG-PENALTY.
           IF BUSTAT-PENALTY-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BUSTAT-PENALTY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO BUSTAT-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO BUSTAT-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO BUSTAT-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'BUSTAT EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
