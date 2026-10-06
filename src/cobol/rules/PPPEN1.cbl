      *================================================================*
      * PROGRAM  : PPPEN1                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : POSITIVE PAY RULE - FEES. CALLED BY THE POSITIVE PAY*
      *            BATCH AND ONLINE PROGRAMS.                          *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-02-16 PQR  ORIGINAL                                       *
      * 1991-03-07 SKN  EXCEPT RERUN PROTECTION CHG9239                *
      * 1992-09-07 JLM  VOID LIMIT REVISED CHG8467                     *
      * 1993-06-28 JLM  EXCEPT CALCULATION CORRECTED INC6646           *
      * 1993-04-08 GAF  ISSUED CUTOFF MOVED CHG4481                    *
      * 1993-02-22 CRP  PERFORMANCE - ISSUED READ AHEAD PRB3392        *
      * 1994-10-17 BHS  ISSUED TABLE EXPANDED CHG0942                  *
      * 1997-05-16 TWH  PAID FEE SCHEDULE UPDATE INC2489               *
      * 1999-07-15 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-07-02 CRP  VOID FEE SCHEDULE UPDATE PRB9165               *
      * 2000-11-21 EWS  PAID RERUN PROTECTION PRB6683                  *
      * 2002-09-18 SKN  EXCEPT TABLE EXPANDED REQ9295                  *
      * 2002-03-07 JLM  STALE EXCEPTIONS TO OPS QUEUE CHG1310          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPPEN1.
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
                                           VALUE 22.000000.
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
      * WS-STALE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STALE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000649000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000759500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000094000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000503000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000270000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015350000000165500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020350000000450000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020850000000419500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000750000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021600000000793500'.
       01  WS-STALE-BAND-TBL REDEFINES WS-STALE-BAND-VALUES.
           05  WS-STALE-BAND-ROW OCCURS 10 TIMES.
               10  WS-STALE-BAND-LOW       PIC 9(09)V99.
               10  WS-STALE-BAND-RATE      PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PPPEN1P.

       PROCEDURE DIVISION USING PPPEN1-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SELECT-DISCOUNT
           PERFORM 2010-ACCUM-REGION
           PERFORM 2020-SPLIT-STALE
           PERFORM 2030-SELECT-DISCOUNT
           PERFORM 2040-LIMIT-ITEM
           PERFORM 2050-FLAG-EXCEPT
           PERFORM 2060-CHECK-LAST-MAINT
           PERFORM 2070-CHECK-LAST-MAINT
           PERFORM 2080-PRORATE-VOID
           PERFORM 2090-LIMIT-ITEM
           PERFORM 2100-EDIT-DESC
           PERFORM 2110-ACCRUE-PAID
           PERFORM 2120-COUNT-CYCLE
           PERFORM 2130-AGE-LAST-MAINT
           PERFORM 2140-LIMIT-ITEM
           PERFORM 2150-SET-DECISION
           PERFORM 2160-LIMIT-ISSUED
           PERFORM 2170-SELECT-RATE
           PERFORM 2180-EDIT-DESC
           PERFORM 2190-ACCUM-REASON
           PERFORM 2200-SELECT-DISCOUNT
           PERFORM 2210-ASSESS-OVERRIDE
           PERFORM 2220-PRORATE-ISSUED
           PERFORM 2230-DESCRIBE-REASON
           PERFORM 2240-EDIT-CUST-ID
           PERFORM 2250-CHECK-EXPIRY
           PERFORM 2260-FLAG-ITEM
           PERFORM 2270-AGE-OPEN
           PERFORM 2280-SELECT-RATE
           PERFORM 2290-ACCRUE-PAID
           PERFORM 2300-CHECK-CLOSE
           PERFORM 2310-SET-REGION
           PERFORM 2320-ACCRUE-ISSUED
           PERFORM 2330-CHECK-LAST-MAINT
           PERFORM 2340-SET-DECISION
           PERFORM 2350-LIMIT-STALE
           PERFORM 2360-SELECT-DISCOUNT
           PERFORM 2370-SELECT-DISCOUNT
           PERFORM 2380-FLAG-EXCEPT
           PERFORM 2390-PRORATE-PAID
           PERFORM 2400-AGE-EXPIRY
           PERFORM 2410-SET-CYCLE
           PERFORM 2420-AGE-LAST-MAINT
           PERFORM 2430-SPLIT-STALE
           PERFORM 2440-PRORATE-VOID
           PERFORM 2450-LIMIT-PAID
           PERFORM 2460-CHECK-EXPIRY
           PERFORM 2470-ACCUM-REGION
           PERFORM 2480-SET-REASON
           PERFORM 2490-SELECT-RATE
           PERFORM 2500-ACCRUE-RATE
           PERFORM 2510-COUNT-CYCLE
           PERFORM 2520-AGE-LAST-MAINT
           PERFORM 2530-PRORATE-VOID
           PERFORM 2540-LIMIT-ITEM
           PERFORM 2550-AGE-LAST-MAINT
           PERFORM 2560-DESCRIBE-REGION
           PERFORM 2570-ACCRUE-ISSUED
           PERFORM 2580-ACCRUE-DISCOUNT
           PERFORM 2590-FLAG-ITEM
           PERFORM 2600-ACCRUE-DISCOUNT
           PERFORM 2610-COUNT-CYCLE
           PERFORM 2620-LIMIT-PAID
           PERFORM 2630-ASSESS-HOLD
           PERFORM 2640-DESCRIBE-CYCLE
           PERFORM 2650-SPLIT-VOID
           PERFORM 2660-CHECK-OPEN
           PERFORM 2670-SPLIT-PAID
           PERFORM 2680-FLAG-ITEM
           PERFORM 2690-ASSESS-HOLD
           PERFORM 2700-COUNT-PAYMENT
           PERFORM 2710-FLAG-STALE
           PERFORM 2720-ACCRUE-PAID
           PERFORM 2730-ACCRUE-STALE
           PERFORM 2740-PRORATE-PAID
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PPPEN1-RETURN-CODE.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE EXCEPT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2000-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPPEN1-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPPEN1-EXCEPT-AMT NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPPEN1-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPPEN1-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPPEN1-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPPEN1-REGION-CD
                   ADD PPPEN1-STALE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPPEN1-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPPEN1-STALE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STALE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2020-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPPEN1-STALE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PPPEN1-STALE-YTD - (WS-WORK-AMT * 2)
           COMPUTE PPPEN1-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2030-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPPEN1-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPPEN1-VOID-AMT NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPPEN1-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPPEN1-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPPEN1-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2040-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2050-FLAG-EXCEPT.
           IF PPPEN1-EXCEPT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPPEN1-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2060-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PPPEN1-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2070-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PPPEN1-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2080-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PPPEN1-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2090-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E908 DESC REQUIRED
      * E281 WORK AMT NEGATIVE
      * E717 WORK AMT OVER 5000
      * E241 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPPEN1-DESC = SPACES
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E281' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E717' TO WS-REJECT-CODE
               WHEN PPPEN1-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2110-ACCRUE-PAID.
           IF PPPEN1-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPPEN1-PAID-YTD * PPPEN1-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPPEN1-STALE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-CYCLE.
           IF PPPEN1-CYCLE-CNT < 9999999
               ADD 1 TO PPPEN1-CYCLE-CNT
           END-IF
           IF PPPEN1-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PPPEN1-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2130-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-LAST-MAINT-DATE > 0 AND PPPEN1-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPPEN1-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PPPEN1-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPPEN1-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPPEN1-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPPEN1-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2140-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DECISION CD:
      * AND -> BER WHEN LAST MAINT DATE IS PAST
      * BER -> CZW WHEN LAST MAINT DATE IS PAST
      * CZW -> SBV WHEN LAST MAINT DATE IS PAST
      * SBV -> ZNG WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2150-SET-DECISION.
           EVALUATE PPPEN1-DECISION-CD
               WHEN 'AND'
                   IF PPPEN1-LAST-MAINT-DATE > 0 AND
                       PPPEN1-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'BER' TO PPPEN1-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BER'
                   IF PPPEN1-LAST-MAINT-DATE > 0 AND
                       PPPEN1-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'CZW' TO PPPEN1-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZW'
                   IF PPPEN1-LAST-MAINT-DATE > 0 AND
                       PPPEN1-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'SBV' TO PPPEN1-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SBV'
                   IF PPPEN1-LAST-MAINT-DATE > 0 AND
                       PPPEN1-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZNG' TO PPPEN1-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2160-LIMIT-ISSUED.
           IF PPPEN1-ISSUED-AMT < 10.00
               MOVE 10.00 TO PPPEN1-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPPEN1-ISSUED-AMT > 5000.00
               MOVE 5000.00 TO PPPEN1-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2170-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPPEN1-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPPEN1-VOID-AMT NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPPEN1-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPPEN1-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPPEN1-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E341 DESC REQUIRED
      * E416 WORK AMT NEGATIVE
      * E833 WORK AMT OVER 250000
      * E628 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2180-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPPEN1-DESC = SPACES
                   MOVE 'E341' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN PPPEN1-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E628' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2190-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPPEN1-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPPEN1-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2200-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPPEN1-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPPEN1-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPPEN1-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPPEN1-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2210-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPPEN1-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPPEN1-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPPEN1-HOLD-FLAG = 'Y'
                   COMPUTE PPPEN1-VOID-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPPEN1-RATE / 200
               WHEN PPPEN1-CYCLE-CD = 'A'
                   MOVE 5.00 TO PPPEN1-VOID-AMT
               WHEN PPPEN1-CYCLE-CD = 'B'
                   MOVE 3.00 TO PPPEN1-VOID-AMT
               WHEN OTHER
                   COMPUTE PPPEN1-VOID-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPPEN1-RATE / 100
           END-EVALUATE
           IF PPPEN1-VOID-AMT > 0 AND PPPEN1-VOID-AMT < 10.00
               MOVE 10.00 TO PPPEN1-VOID-AMT
           END-IF
           IF PPPEN1-VOID-AMT > 25.00
               MOVE 25.00 TO PPPEN1-VOID-AMT
           END-IF
           ADD PPPEN1-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2220-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PPPEN1-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2230-DESCRIBE-REASON.
           EVALUATE PPPEN1-REASON-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E849 CUST ID REQUIRED
      * E017 PAID AMT NEGATIVE
      * E519 PAID AMT OVER 250000
      * E786 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2240-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPPEN1-CUST-ID = SPACES
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN PPPEN1-PAID-AMT < 0
                   MOVE 'E017' TO WS-REJECT-CODE
               WHEN PPPEN1-PAID-AMT > 250000.00
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN PPPEN1-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2250-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PPPEN1-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2260-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-OPEN-DATE > 0 AND PPPEN1-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPPEN1-EXCEPT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PPPEN1-EXCEPT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PPPEN1-EXCEPT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPPEN1-EXCEPT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPPEN1-EXCEPT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2280-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPPEN1-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPPEN1-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPPEN1-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPPEN1-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2290-ACCRUE-PAID.
           IF PPPEN1-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPPEN1-PAID-AMT * PPPEN1-RATE / 100 / 360
               ADD WS-WORK-AMT TO PPPEN1-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PPPEN1-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN CLOSE DATE IS PAST
      * B -> D WHEN CLOSE DATE IS PAST
      * D -> H WHEN CLOSE DATE IS PAST
      * H -> N WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-REGION.
           EVALUATE PPPEN1-REGION-CD
               WHEN 'A'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPPEN1-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PPPEN1-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PPPEN1-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPPEN1-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2320-ACCRUE-ISSUED.
           IF PPPEN1-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPPEN1-ISSUED-AMT * PPPEN1-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO PPPEN1-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2330-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PPPEN1-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DECISION CD:
      * AND -> BER WHEN CLOSE DATE IS PAST
      * BER -> CZW WHEN CLOSE DATE IS PAST
      * CZW -> SBV WHEN CLOSE DATE IS PAST
      * SBV -> ZNG WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2340-SET-DECISION.
           EVALUATE PPPEN1-DECISION-CD
               WHEN 'AND'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BER' TO PPPEN1-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BER'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZW' TO PPPEN1-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZW'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SBV' TO PPPEN1-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SBV'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZNG' TO PPPEN1-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STALE YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2350-LIMIT-STALE.
           IF PPPEN1-STALE-YTD < 10.00
               MOVE 10.00 TO PPPEN1-STALE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPPEN1-STALE-YTD > 5000.00
               MOVE 5000.00 TO PPPEN1-STALE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2360-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPPEN1-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPPEN1-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPPEN1-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPPEN1-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2370-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPPEN1-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPPEN1-ISSUED-AMT NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPPEN1-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPPEN1-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPPEN1-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2380-FLAG-EXCEPT.
           IF PPPEN1-EXCEPT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPPEN1-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       2390-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPPEN1-PAID-AMT ROUNDED =
                   PPPEN1-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPPEN1-PAID-AMT
           END-IF
           ADD PPPEN1-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2400-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-EXPIRY-DATE > 0 AND PPPEN1-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPPEN1-PAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPPEN1-PAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPPEN1-PAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPPEN1-PAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPPEN1-PAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> B WHEN LAST MAINT DATE IS PAST
      * B -> C WHEN LAST MAINT DATE IS PAST
      * C -> R WHEN LAST MAINT DATE IS PAST
      * R -> T WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2410-SET-CYCLE.
           EVALUATE PPPEN1-CYCLE-CD
               WHEN 'A'
                   IF PPPEN1-LAST-MAINT-DATE > 0 AND
                       PPPEN1-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'B' TO PPPEN1-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPPEN1-LAST-MAINT-DATE > 0 AND
                       PPPEN1-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'C' TO PPPEN1-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PPPEN1-LAST-MAINT-DATE > 0 AND
                       PPPEN1-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO PPPEN1-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF PPPEN1-LAST-MAINT-DATE > 0 AND
                       PPPEN1-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'T' TO PPPEN1-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2420-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-LAST-MAINT-DATE > 0 AND PPPEN1-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPPEN1-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPPEN1-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPPEN1-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PPPEN1-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPPEN1-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STALE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2430-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPPEN1-STALE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PPPEN1-STALE-YTD - (WS-WORK-AMT * 2)
           COMPUTE PPPEN1-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       2440-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPPEN1-VOID-AMT ROUNDED =
                   PPPEN1-PAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPPEN1-VOID-AMT
           END-IF
           ADD PPPEN1-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2450-LIMIT-PAID.
           IF PPPEN1-PAID-AMT < 50.00
               MOVE 50.00 TO PPPEN1-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPPEN1-PAID-AMT > 50000.00
               MOVE 50000.00 TO PPPEN1-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2460-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PPPEN1-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2470-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPPEN1-REGION-CD
                   ADD PPPEN1-EXCEPT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPPEN1-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPPEN1-EXCEPT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN CLOSE DATE IS PAST
      * M -> N WHEN CLOSE DATE IS PAST
      * N -> U WHEN CLOSE DATE IS PAST
      * U -> V WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2480-SET-REASON.
           EVALUATE PPPEN1-REASON-CD
               WHEN 'B'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPPEN1-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPPEN1-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPPEN1-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPPEN1-CLOSE-DATE > 0 AND PPPEN1-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPPEN1-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE STALE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2490-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPPEN1-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPPEN1-STALE-YTD NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPPEN1-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPPEN1-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPPEN1-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PPPEN1-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPPEN1-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2510-COUNT-CYCLE.
           IF PPPEN1-CYCLE-CNT < 9999999
               ADD 1 TO PPPEN1-CYCLE-CNT
           END-IF
           IF PPPEN1-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PPPEN1-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2520-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-LAST-MAINT-DATE > 0 AND PPPEN1-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPPEN1-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PPPEN1-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PPPEN1-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPPEN1-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPPEN1-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       2530-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPPEN1-VOID-AMT ROUNDED =
                   PPPEN1-PAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPPEN1-VOID-AMT
           END-IF
           ADD PPPEN1-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2540-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2550-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-LAST-MAINT-DATE > 0 AND PPPEN1-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2560-DESCRIBE-REGION.
           EVALUATE PPPEN1-REGION-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2570-ACCRUE-ISSUED.
           IF PPPEN1-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPPEN1-ISSUED-AMT * PPPEN1-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PPPEN1-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO PPPEN1-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2590-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2600-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PPPEN1-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO PPPEN1-EXCEPT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2610-COUNT-CYCLE.
           IF PPPEN1-CYCLE-CNT < 9999999
               ADD 1 TO PPPEN1-CYCLE-CNT
           END-IF
           IF PPPEN1-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PPPEN1-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2620-LIMIT-PAID.
           IF PPPEN1-PAID-YTD < 50.00
               MOVE 50.00 TO PPPEN1-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPPEN1-PAID-YTD > 10000.00
               MOVE 10000.00 TO PPPEN1-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PAID AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2630-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPPEN1-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPPEN1-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PPPEN1-PAID-AMT *
                       PPPEN1-DISCOUNT / 200
               WHEN PPPEN1-CYCLE-CD = 'A'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN PPPEN1-CYCLE-CD = 'B'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PPPEN1-PAID-AMT *
                       PPPEN1-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2640-DESCRIBE-CYCLE.
           EVALUATE PPPEN1-CYCLE-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'C'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPPEN1-VOID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPPEN1-VOID-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPPEN1-STALE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2660-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPPEN1-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPPEN1-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PPPEN1-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2670-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPPEN1-PAID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PPPEN1-PAID-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2680-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE WORK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2690-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPPEN1-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PPPEN1-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPPEN1-OVERRIDE-FLAG = 'Y'
                   COMPUTE PPPEN1-VOID-AMT ROUNDED = WS-WORK-AMT *
                       PPPEN1-RATE / 200
               WHEN PPPEN1-REGION-CD = 'A'
                   MOVE 15.00 TO PPPEN1-VOID-AMT
               WHEN PPPEN1-REGION-CD = 'B'
                   MOVE 5.00 TO PPPEN1-VOID-AMT
               WHEN OTHER
                   COMPUTE PPPEN1-VOID-AMT ROUNDED = WS-WORK-AMT *
                       PPPEN1-RATE / 100
           END-EVALUATE
           IF PPPEN1-VOID-AMT > 0 AND PPPEN1-VOID-AMT < 1.00
               MOVE 1.00 TO PPPEN1-VOID-AMT
           END-IF
           IF PPPEN1-VOID-AMT > 250.00
               MOVE 250.00 TO PPPEN1-VOID-AMT
           END-IF
           ADD PPPEN1-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2700-COUNT-PAYMENT.
           IF PPPEN1-PAYMENT-CNT < 9999999
               ADD 1 TO PPPEN1-PAYMENT-CNT
           END-IF
           IF PPPEN1-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO PPPEN1-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STALE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2710-FLAG-STALE.
           IF PPPEN1-STALE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPPEN1-STALE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2720-ACCRUE-PAID.
           IF PPPEN1-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPPEN1-PAID-AMT * PPPEN1-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2730-ACCRUE-STALE.
           IF PPPEN1-STALE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPPEN1-STALE-YTD * PPPEN1-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO PPPEN1-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       2740-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPPEN1-PAID-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPPEN1-PAID-AMT
           END-IF
           ADD PPPEN1-PAID-AMT TO WS-TOTAL-PRORATED.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PPPEN1-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PPPEN1-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PPPEN1-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PPPEN1 EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
