      *================================================================*
      * PROGRAM  : TLFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TELLER BALANCING RULE - VALIDATION. CALLED BY THE   *
      *            TELLER BALANCING BATCH AND ONLINE PROGRAMS.         *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-06-07 SKN  ORIGINAL                                       *
      * 1994-03-02 LMB  BATCH EXCEPTIONS TO OPS QUEUE INC1695          *
      * 1994-11-28 EWS  BATCH NOTICE TEXT PER LEGAL REQ9606            *
      * 1996-08-15 CRP  PERFORMANCE - CASH-OUT READ AHEAD PRB3260      *
      * 1997-12-12 DPS  VAULT-BUY TABLE EXPANDED REQ5847               *
      * 1999-07-16 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2005-03-13 BHS  VAULT-BUY EXCEPTIONS TO OPS QUEUE INC1650      *
      * 2007-02-20 GAF  BATCH CUTOFF MOVED PRB9189                     *
      * 2011-02-25 EWS  AUDIT FINDING - OVERSHORT CONTROL TOTALS INC776*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLFEE.
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
                                           VALUE 23.000000.
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
      * WS-CASH-OUT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-OUT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000844000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000670000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000794500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000683000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000173000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000554000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000302000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000134500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017750000000425500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018250000000463000'.
       01  WS-CASH-OUT-GRID-TBL REDEFINES WS-CASH-OUT-GRID-VALUES.
           05  WS-CASH-OUT-GRID-ROW OCCURS 10 TIMES.
               10  WS-CASH-OUT-GRID-LOW    PIC 9(09)V99.
               10  WS-CASH-OUT-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TLFEEP.

       PROCEDURE DIVISION USING TLFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-RATE
           PERFORM 2010-SELECT-RATE
           PERFORM 2020-CHECK-OPEN
           PERFORM 2030-DESCRIBE-CYCLE
           PERFORM 2040-ACCUM-DRAWER
           PERFORM 2050-AGE-OPEN
           PERFORM 2060-ASSESS-OVERRIDE
           PERFORM 2070-SPLIT-VAULT-BUY
           PERFORM 2080-SPLIT-VAULT-BUY
           PERFORM 2090-AGE-LAST-ACT
           PERFORM 2100-CHECK-OPEN
           PERFORM 2110-EDIT-DESC
           PERFORM 2120-SPLIT-VAULT-SELL
           PERFORM 2130-DESCRIBE-CYCLE
           PERFORM 2140-FLAG-VAULT-SELL
           PERFORM 2150-PRORATE-VAULT-SELL
           PERFORM 2160-DESCRIBE-DRAWER
           PERFORM 2170-LIMIT-VAULT-BUY
           PERFORM 2180-FLAG-CASH-IN
           PERFORM 2190-PRORATE-CASH-OUT
           PERFORM 2200-EDIT-DESC
           PERFORM 2210-PRORATE-VAULT-BUY
           PERFORM 2220-SPLIT-CASH-IN
           PERFORM 2230-ACCUM-CYCLE
           PERFORM 2240-SELECT-DISCOUNT
           PERFORM 2250-AGE-LAST-ACT
           PERFORM 2260-ACCUM-CYCLE
           PERFORM 2270-PRORATE-VAULT-SELL
           PERFORM 2280-SPLIT-VAULT-BUY
           PERFORM 2290-EDIT-NAME
           PERFORM 2300-ASSESS-STOP
           PERFORM 2310-COUNT-EXCEPT
           PERFORM 2320-ACCRUE-VAULT-SELL
           PERFORM 2330-ASSESS-OVERRIDE
           PERFORM 2340-ACCUM-CHANNEL
           PERFORM 2350-AGE-LAST-PAY
           PERFORM 2360-EDIT-NAME
           PERFORM 2370-PRORATE-VAULT-SELL
           PERFORM 2380-LIMIT-ITEM
           PERFORM 2390-ACCUM-CYCLE
           PERFORM 2400-ASSESS-OVERRIDE
           PERFORM 2410-SELECT-DISCOUNT
           PERFORM 2420-PRORATE-VAULT-SELL
           PERFORM 2430-COUNT-EXCEPT
           PERFORM 2440-ASSESS-EXEMPT
           PERFORM 2450-ACCRUE-VAULT-SELL
           PERFORM 2460-ASSESS-OVERRIDE
           PERFORM 2470-SPLIT-CASH-OUT
           PERFORM 2480-AGE-OPEN
           PERFORM 2490-FLAG-VAULT-BUY
           PERFORM 2500-EDIT-DESC
           PERFORM 2510-EDIT-DESC
           PERFORM 2520-LIMIT-CASH-IN
           PERFORM 2530-LIMIT-VAULT-BUY
           PERFORM 2540-FLAG-CASH-OUT
           PERFORM 2550-ACCUM-DRAWER
           PERFORM 2560-AGE-LAST-ACT
           PERFORM 2570-COUNT-NOTICE
           PERFORM 2580-EDIT-DESC
           PERFORM 2590-ACCUM-CYCLE
           PERFORM 2600-FLAG-CASH-IN
           PERFORM 2610-FLAG-VAULT-BUY
           PERFORM 2620-ACCRUE-CASH-OUT
           PERFORM 2630-SET-DRAWER
           PERFORM 2640-LIMIT-ITEM
           PERFORM 2650-ACCRUE-CASH-IN
           PERFORM 2660-ASSESS-OVERRIDE
           PERFORM 2670-ACCUM-DRAWER
           PERFORM 2680-DESCRIBE-CHANNEL
           PERFORM 2690-AGE-LAST-ACT
           PERFORM 2700-SELECT-DISCOUNT
           PERFORM 2710-SPLIT-VAULT-SELL
           PERFORM 2720-ASSESS-STOP
           PERFORM 2730-SELECT-DISCOUNT
           PERFORM 2740-SELECT-RATE
           PERFORM 2750-AGE-LAST-ACT
           PERFORM 2760-DESCRIBE-PROOF-TYPE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TLFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TLFEE-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2010-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TLFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TLFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2020-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLFEE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TLFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2030-DESCRIBE-CYCLE.
           EVALUATE TLFEE-CYCLE-CD
               WHEN 'KX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL YTD BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2040-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLFEE-DRAWER-CD
                   ADD TLFEE-VAULT-SELL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLFEE-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLFEE-VAULT-SELL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLFEE-OPEN-DATE > 0 AND TLFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2060-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLFEE-VAULT-BUY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE TLFEE-VAULT-BUY-AMT ROUNDED = WS-WORK-AMT *
                       TLFEE-DISCOUNT / 200
               WHEN TLFEE-DRAWER-CD = 'D'
                   MOVE 20.00 TO TLFEE-VAULT-BUY-AMT
               WHEN TLFEE-DRAWER-CD = 'F'
                   MOVE 12.00 TO TLFEE-VAULT-BUY-AMT
               WHEN OTHER
                   COMPUTE TLFEE-VAULT-BUY-AMT ROUNDED = WS-WORK-AMT *
                       TLFEE-DISCOUNT / 100
           END-EVALUATE
           IF TLFEE-VAULT-BUY-AMT > 0 AND TLFEE-VAULT-BUY-AMT < 1.00
               MOVE 1.00 TO TLFEE-VAULT-BUY-AMT
           END-IF
           IF TLFEE-VAULT-BUY-AMT > 50.00
               MOVE 50.00 TO TLFEE-VAULT-BUY-AMT
           END-IF
           ADD TLFEE-VAULT-BUY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TLFEE-VAULT-BUY-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TLFEE-VAULT-BUY-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TLFEE-CASH-OUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2080-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE TLFEE-VAULT-BUY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2090-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLFEE-LAST-ACT-DATE > 0 AND TLFEE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2100-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLFEE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TLFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E102 DESC REQUIRED
      * E571 VAULT SELL AMT NEGATIVE
      * E360 VAULT SELL AMT OVER 5000
      * E850 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2110-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLFEE-DESC = SPACES
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN TLFEE-VAULT-SELL-AMT < 0
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN TLFEE-VAULT-SELL-AMT > 5000.00
                   MOVE 'E360' TO WS-REJECT-CODE
               WHEN TLFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2120-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TLFEE-VAULT-SELL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLFEE-VAULT-SELL-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TLFEE-VAULT-BUY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2130-DESCRIBE-CYCLE.
           EVALUATE TLFEE-CYCLE-CD
               WHEN 'KX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2140-FLAG-VAULT-SELL.
           IF TLFEE-VAULT-SELL-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLFEE-VAULT-SELL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       2150-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLFEE-VAULT-SELL-AMT ROUNDED =
                   TLFEE-VAULT-SELL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLFEE-VAULT-SELL-AMT
           END-IF
           ADD TLFEE-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2160-DESCRIBE-DRAWER.
           EVALUATE TLFEE-DRAWER-CD
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT BUY AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2170-LIMIT-VAULT-BUY.
           IF TLFEE-VAULT-BUY-AMT < 50.00
               MOVE 50.00 TO TLFEE-VAULT-BUY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLFEE-VAULT-BUY-AMT > 500.00
               MOVE 500.00 TO TLFEE-VAULT-BUY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2180-FLAG-CASH-IN.
           IF TLFEE-CASH-IN-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLFEE-CASH-IN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT YTD.
      *----------------------------------------------------------------*
       2190-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLFEE-CASH-OUT-YTD ROUNDED =
                   TLFEE-VAULT-BUY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLFEE-CASH-OUT-YTD
           END-IF
           ADD TLFEE-CASH-OUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E782 DESC REQUIRED
      * E117 WORK AMT 2 NEGATIVE
      * E824 WORK AMT 2 OVER 99999
      * E934 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2200-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLFEE-DESC = SPACES
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E824' TO WS-REJECT-CODE
               WHEN TLFEE-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E934' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT BUY AMT.
      *----------------------------------------------------------------*
       2210-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLFEE-VAULT-BUY-AMT ROUNDED =
                   TLFEE-CASH-OUT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLFEE-VAULT-BUY-AMT
           END-IF
           ADD TLFEE-VAULT-BUY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CASH IN YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2220-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TLFEE-CASH-IN-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLFEE-CASH-IN-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLFEE-VAULT-BUY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2230-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLFEE-CYCLE-CD
                   ADD TLFEE-VAULT-SELL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLFEE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLFEE-VAULT-SELL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE VAULT BUY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2240-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TLFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TLFEE-VAULT-BUY-YTD NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TLFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2250-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLFEE-LAST-ACT-DATE > 0 AND TLFEE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLFEE-VAULT-SELL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLFEE-VAULT-SELL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TLFEE-VAULT-SELL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TLFEE-VAULT-SELL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLFEE-VAULT-SELL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2260-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLFEE-CYCLE-CD
                   ADD TLFEE-CASH-IN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLFEE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLFEE-CASH-IN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL YTD.
      *----------------------------------------------------------------*
       2270-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLFEE-VAULT-SELL-YTD ROUNDED =
                   TLFEE-VAULT-BUY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLFEE-VAULT-SELL-YTD
           END-IF
           ADD TLFEE-VAULT-SELL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TLFEE-VAULT-BUY-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLFEE-VAULT-BUY-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TLFEE-CASH-OUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E734 NAME REQUIRED
      * E146 WORK AMT 2 NEGATIVE
      * E604 WORK AMT 2 OVER 5000
      * E287 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2290-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLFEE-NAME = SPACES
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN TLFEE-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E287' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE VAULT SELL YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2300-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLFEE-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLFEE-VAULT-SELL-YTD
                       * TLFEE-DISCOUNT / 200
               WHEN TLFEE-DRAWER-CD = 'D'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN TLFEE-DRAWER-CD = 'F'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLFEE-VAULT-SELL-YTD
                       * TLFEE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2310-COUNT-EXCEPT.
           IF TLFEE-EXCEPT-CNT < 9999999
               ADD 1 TO TLFEE-EXCEPT-CNT
           END-IF
           IF TLFEE-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TLFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2320-ACCRUE-VAULT-SELL.
           IF TLFEE-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLFEE-VAULT-SELL-AMT * TLFEE-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLFEE-VAULT-BUY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2330-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLFEE-VAULT-BUY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE TLFEE-VAULT-BUY-YTD ROUNDED = WS-WORK-AMT-2 *
                       TLFEE-DISCOUNT / 200
               WHEN TLFEE-CYCLE-CD = 'KX'
                   MOVE 5.00 TO TLFEE-VAULT-BUY-YTD
               WHEN TLFEE-CYCLE-CD = 'TA'
                   MOVE 5.00 TO TLFEE-VAULT-BUY-YTD
               WHEN OTHER
                   COMPUTE TLFEE-VAULT-BUY-YTD ROUNDED = WS-WORK-AMT-2 *
                       TLFEE-DISCOUNT / 100
           END-EVALUATE
           IF TLFEE-VAULT-BUY-YTD > 0 AND TLFEE-VAULT-BUY-YTD < 2.00
               MOVE 2.00 TO TLFEE-VAULT-BUY-YTD
           END-IF
           IF TLFEE-VAULT-BUY-YTD > 75.00
               MOVE 75.00 TO TLFEE-VAULT-BUY-YTD
           END-IF
           ADD TLFEE-VAULT-BUY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLFEE-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLFEE-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2350-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLFEE-LAST-PAY-DATE > 0 AND TLFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E718 NAME REQUIRED
      * E590 WORK AMT 2 NEGATIVE
      * E468 WORK AMT 2 OVER 5000
      * E310 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2360-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLFEE-NAME = SPACES
                   MOVE 'E718' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E468' TO WS-REJECT-CODE
               WHEN TLFEE-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E310' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       2370-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLFEE-VAULT-SELL-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLFEE-VAULT-SELL-AMT
           END-IF
           ADD TLFEE-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2380-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2390-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLFEE-CYCLE-CD
                   ADD TLFEE-CASH-IN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLFEE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLFEE-CASH-IN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT YTD FEE ON THE VAULT BUY AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2400-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLFEE-CASH-OUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLFEE-STOP-FLAG = 'Y'
                   COMPUTE TLFEE-CASH-OUT-YTD ROUNDED =
                       TLFEE-VAULT-BUY-AMT * TLFEE-RATE / 200
               WHEN TLFEE-PROOF-TYPE-CD = 'JG'
                   MOVE 10.00 TO TLFEE-CASH-OUT-YTD
               WHEN TLFEE-PROOF-TYPE-CD = 'RX'
                   MOVE 10.00 TO TLFEE-CASH-OUT-YTD
               WHEN OTHER
                   COMPUTE TLFEE-CASH-OUT-YTD ROUNDED =
                       TLFEE-VAULT-BUY-AMT * TLFEE-RATE / 100
           END-EVALUATE
           IF TLFEE-CASH-OUT-YTD > 0 AND TLFEE-CASH-OUT-YTD < 1.00
               MOVE 1.00 TO TLFEE-CASH-OUT-YTD
           END-IF
           IF TLFEE-CASH-OUT-YTD > 100.00
               MOVE 100.00 TO TLFEE-CASH-OUT-YTD
           END-IF
           ADD TLFEE-CASH-OUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE CASH IN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2410-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TLFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TLFEE-CASH-IN-YTD NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TLFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       2420-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLFEE-VAULT-SELL-AMT ROUNDED =
                   TLFEE-VAULT-SELL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLFEE-VAULT-SELL-AMT
           END-IF
           ADD TLFEE-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-EXCEPT.
           IF TLFEE-EXCEPT-CNT < 9999999
               ADD 1 TO TLFEE-EXCEPT-CNT
           END-IF
           IF TLFEE-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TLFEE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY YTD FEE ON THE VAULT SELL YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2440-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TLFEE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TLFEE-VAULT-BUY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLFEE-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLFEE-VAULT-BUY-YTD ROUNDED =
                       TLFEE-VAULT-SELL-YTD * TLFEE-DISCOUNT / 200
               WHEN TLFEE-CHANNEL-CD = 'D'
                   MOVE 15.00 TO TLFEE-VAULT-BUY-YTD
               WHEN TLFEE-CHANNEL-CD = 'F'
                   MOVE 5.00 TO TLFEE-VAULT-BUY-YTD
               WHEN OTHER
                   COMPUTE TLFEE-VAULT-BUY-YTD ROUNDED =
                       TLFEE-VAULT-SELL-YTD * TLFEE-DISCOUNT / 100
           END-EVALUATE
           IF TLFEE-VAULT-BUY-YTD > 0 AND TLFEE-VAULT-BUY-YTD < 1.00
               MOVE 1.00 TO TLFEE-VAULT-BUY-YTD
           END-IF
           IF TLFEE-VAULT-BUY-YTD > 25.00
               MOVE 25.00 TO TLFEE-VAULT-BUY-YTD
           END-IF
           ADD TLFEE-VAULT-BUY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2450-ACCRUE-VAULT-SELL.
           IF TLFEE-VAULT-SELL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLFEE-VAULT-SELL-YTD * TLFEE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLFEE-VAULT-BUY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY FEE ON THE CASH OUT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2460-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLFEE-VAULT-BUY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE TLFEE-VAULT-BUY-AMT ROUNDED =
                       TLFEE-CASH-OUT-YTD * TLFEE-DISCOUNT / 200
               WHEN TLFEE-CYCLE-CD = 'KX'
                   MOVE 15.00 TO TLFEE-VAULT-BUY-AMT
               WHEN TLFEE-CYCLE-CD = 'TA'
                   MOVE 3.00 TO TLFEE-VAULT-BUY-AMT
               WHEN OTHER
                   COMPUTE TLFEE-VAULT-BUY-AMT ROUNDED =
                       TLFEE-CASH-OUT-YTD * TLFEE-DISCOUNT / 100
           END-EVALUATE
           IF TLFEE-VAULT-BUY-AMT > 0 AND TLFEE-VAULT-BUY-AMT < 10.00
               MOVE 10.00 TO TLFEE-VAULT-BUY-AMT
           END-IF
           IF TLFEE-VAULT-BUY-AMT > 25.00
               MOVE 25.00 TO TLFEE-VAULT-BUY-AMT
           END-IF
           ADD TLFEE-VAULT-BUY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2470-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TLFEE-CASH-OUT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLFEE-CASH-OUT-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TLFEE-VAULT-SELL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2480-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLFEE-OPEN-DATE > 0 AND TLFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLFEE-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT BUY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2490-FLAG-VAULT-BUY.
           IF TLFEE-VAULT-BUY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLFEE-VAULT-BUY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E605 DESC REQUIRED
      * E722 CASH OUT YTD NEGATIVE
      * E257 CASH OUT YTD OVER 1000000
      * E762 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2500-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLFEE-DESC = SPACES
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN TLFEE-CASH-OUT-YTD < 0
                   MOVE 'E722' TO WS-REJECT-CODE
               WHEN TLFEE-CASH-OUT-YTD > 1000000.00
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN TLFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E762' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E417 DESC REQUIRED
      * E054 CASH OUT YTD NEGATIVE
      * E487 CASH OUT YTD OVER 5000
      * E089 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2510-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLFEE-DESC = SPACES
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN TLFEE-CASH-OUT-YTD < 0
                   MOVE 'E054' TO WS-REJECT-CODE
               WHEN TLFEE-CASH-OUT-YTD > 5000.00
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN TLFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E089' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2520-LIMIT-CASH-IN.
           IF TLFEE-CASH-IN-YTD < 100.00
               MOVE 100.00 TO TLFEE-CASH-IN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLFEE-CASH-IN-YTD > 1000.00
               MOVE 1000.00 TO TLFEE-CASH-IN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2530-LIMIT-VAULT-BUY.
           IF TLFEE-VAULT-BUY-YTD < 25.00
               MOVE 25.00 TO TLFEE-VAULT-BUY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLFEE-VAULT-BUY-YTD > 5000.00
               MOVE 5000.00 TO TLFEE-VAULT-BUY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2540-FLAG-CASH-OUT.
           IF TLFEE-CASH-OUT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLFEE-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL AMT BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2550-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLFEE-DRAWER-CD
                   ADD TLFEE-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLFEE-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLFEE-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2560-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLFEE-LAST-ACT-DATE > 0 AND TLFEE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2570-COUNT-NOTICE.
           IF TLFEE-NOTICE-CNT < 9999999
               ADD 1 TO TLFEE-NOTICE-CNT
           END-IF
           IF TLFEE-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TLFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E235 DESC REQUIRED
      * E773 VAULT SELL YTD NEGATIVE
      * E624 VAULT SELL YTD OVER 10000
      * E152 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2580-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLFEE-DESC = SPACES
                   MOVE 'E235' TO WS-REJECT-CODE
               WHEN TLFEE-VAULT-SELL-YTD < 0
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN TLFEE-VAULT-SELL-YTD > 10000.00
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN TLFEE-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E152' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT BUY AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2590-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLFEE-CYCLE-CD
                   ADD TLFEE-VAULT-BUY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLFEE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLFEE-VAULT-BUY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2600-FLAG-CASH-IN.
           IF TLFEE-CASH-IN-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLFEE-CASH-IN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2610-FLAG-VAULT-BUY.
           IF TLFEE-VAULT-BUY-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLFEE-VAULT-BUY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2620-ACCRUE-CASH-OUT.
           IF TLFEE-CASH-OUT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLFEE-CASH-OUT-YTD * TLFEE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAWER CD:
      * D -> F WHEN NEXT REVIEW DATE IS PAST
      * F -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> D WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2630-SET-DRAWER.
           EVALUATE TLFEE-DRAWER-CD
               WHEN 'D'
                   IF TLFEE-NEXT-REVIEW-DATE > 0 AND
                       TLFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'F' TO TLFEE-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TLFEE-NEXT-REVIEW-DATE > 0 AND
                       TLFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO TLFEE-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF TLFEE-NEXT-REVIEW-DATE > 0 AND
                       TLFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'D' TO TLFEE-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2640-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2650-ACCRUE-CASH-IN.
           IF TLFEE-CASH-IN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLFEE-CASH-IN-YTD * TLFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLFEE-VAULT-SELL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT YTD FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2660-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLFEE-CASH-OUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLFEE-STOP-FLAG = 'Y'
                   COMPUTE TLFEE-CASH-OUT-YTD ROUNDED =
                       TLFEE-VAULT-SELL-AMT * TLFEE-RATE / 200
               WHEN TLFEE-CYCLE-CD = 'KX'
                   MOVE 7.00 TO TLFEE-CASH-OUT-YTD
               WHEN TLFEE-CYCLE-CD = 'TA'
                   MOVE 15.00 TO TLFEE-CASH-OUT-YTD
               WHEN OTHER
                   COMPUTE TLFEE-CASH-OUT-YTD ROUNDED =
                       TLFEE-VAULT-SELL-AMT * TLFEE-RATE / 100
           END-EVALUATE
           IF TLFEE-CASH-OUT-YTD > 0 AND TLFEE-CASH-OUT-YTD < 1.00
               MOVE 1.00 TO TLFEE-CASH-OUT-YTD
           END-IF
           IF TLFEE-CASH-OUT-YTD > 75.00
               MOVE 75.00 TO TLFEE-CASH-OUT-YTD
           END-IF
           ADD TLFEE-CASH-OUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT BUY YTD BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2670-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLFEE-DRAWER-CD
                   ADD TLFEE-VAULT-BUY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLFEE-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLFEE-VAULT-BUY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2680-DESCRIBE-CHANNEL.
           EVALUATE TLFEE-CHANNEL-CD
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'F'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2690-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLFEE-LAST-ACT-DATE > 0 AND TLFEE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLFEE-VAULT-BUY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLFEE-VAULT-BUY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLFEE-VAULT-BUY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLFEE-VAULT-BUY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLFEE-VAULT-BUY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2700-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TLFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TLFEE-CASH-OUT-YTD NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TLFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2710-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLFEE-VAULT-SELL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN YTD FEE ON THE VAULT BUY AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2720-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLFEE-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLFEE-CASH-IN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE TLFEE-CASH-IN-YTD ROUNDED =
                       TLFEE-VAULT-BUY-AMT * TLFEE-RATE / 200
               WHEN TLFEE-PROOF-TYPE-CD = 'JG'
                   MOVE 3.00 TO TLFEE-CASH-IN-YTD
               WHEN TLFEE-PROOF-TYPE-CD = 'RX'
                   MOVE 3.00 TO TLFEE-CASH-IN-YTD
               WHEN OTHER
                   COMPUTE TLFEE-CASH-IN-YTD ROUNDED =
                       TLFEE-VAULT-BUY-AMT * TLFEE-RATE / 100
           END-EVALUATE
           IF TLFEE-CASH-IN-YTD > 0 AND TLFEE-CASH-IN-YTD < 2.00
               MOVE 2.00 TO TLFEE-CASH-IN-YTD
           END-IF
           IF TLFEE-CASH-IN-YTD > 100.00
               MOVE 100.00 TO TLFEE-CASH-IN-YTD
           END-IF
           ADD TLFEE-CASH-IN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2730-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TLFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TLFEE-CASH-OUT-YTD NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TLFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE VAULT BUY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2740-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TLFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TLFEE-VAULT-BUY-AMT NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TLFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2750-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLFEE-LAST-ACT-DATE > 0 AND TLFEE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLFEE-VAULT-BUY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD TLFEE-VAULT-BUY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TLFEE-VAULT-BUY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLFEE-VAULT-BUY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLFEE-VAULT-BUY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PROOF TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2760-DESCRIBE-PROOF-TYPE.
           EVALUATE TLFEE-PROOF-TYPE-CD
               WHEN 'JG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TLFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TLFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TLFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TLFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
