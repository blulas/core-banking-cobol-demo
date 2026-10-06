      *================================================================*
      * PROGRAM  : EUTIER                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : EURO CONVERSION RULE - STATUS. CALLED BY THE EURO   *
      *            CONVERSION BATCH AND ONLINE PROGRAMS.               *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-05-26 JLM  ORIGINAL                                       *
      * 1996-09-17 DPS  RESIDUAL CUTOFF MOVED REQ4696                  *
      * 2000-06-28 TWH  TRIANGULAR RERUN PROTECTION INC6093            *
      * 2000-11-25 EWS  CONVERTED LIMIT REVISED REQ8863                *
      * 2000-03-28 SKN  FEE FEE SCHEDULE UPDATE CHG3405                *
      * 2002-11-11 PQR  ROUNDING FEE SCHEDULE UPDATE INC1968           *
      * 2003-02-01 CRP  TRIANGULAR TABLE EXPANDED CHG3359              *
      * 2003-01-16 BHS  CONVERTED CALCULATION CORRECTED INC6548        *
      * 2006-09-01 BHS  NATIONAL CUTOFF MOVED PRB9576                  *
      * 2006-04-28 SKN  PERFORMANCE - FEE READ AHEAD REQ9204           *
      * 2006-09-10 JLM  NATIONAL STATUS CODES ADDED REQ8503            *
      * 2009-03-11 RJT  NATIONAL FEE SCHEDULE UPDATE PRB6166           *
      * 2010-09-03 PQR  NEW EURO REPORT COLUMNS INC7482                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUTIER.
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
                                           VALUE 19.000000.
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
      * WS-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000573000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000013500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000446000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000496000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000476500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011300000000408000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011400000000229000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011450000000569000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000491500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000637500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000720500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000704000'.
       01  WS-FEE-SCHED-TBL REDEFINES WS-FEE-SCHED-VALUES.
           05  WS-FEE-SCHED-ROW OCCURS 12 TIMES.
               10  WS-FEE-SCHED-LOW        PIC 9(09)V99.
               10  WS-FEE-SCHED-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY EUTIERP.

       PROCEDURE DIVISION USING EUTIER-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-DESCRIBE-STATUS
           PERFORM 2010-ASSESS-STOP
           PERFORM 2020-FLAG-FEE
           PERFORM 2030-SPLIT-NATIONAL
           PERFORM 2040-SELECT-FEE-PCT
           PERFORM 2050-DESCRIBE-STATUS
           PERFORM 2060-SPLIT-EURO
           PERFORM 2070-ACCUM-REGION
           PERFORM 2080-EDIT-REF-NO
           PERFORM 2090-PRORATE-NATIONAL
           PERFORM 2100-AGE-MATURITY
           PERFORM 2110-SET-REGION
           PERFORM 2120-SPLIT-FEE
           PERFORM 2130-LIMIT-ITEM
           PERFORM 2140-SELECT-PENALTY-PCT
           PERFORM 2150-ACCUM-CURRENCY
           PERFORM 2160-SELECT-FEE-PCT
           PERFORM 2170-DESCRIBE-REGION
           PERFORM 2180-ASSESS-EXEMPT
           PERFORM 2190-DESCRIBE-REGION
           PERFORM 2200-ASSESS-EXEMPT
           PERFORM 2210-AGE-LAST-STMT
           PERFORM 2220-CHECK-LAST-MAINT
           PERFORM 2230-ACCRUE-TRIANGULAR
           PERFORM 2240-PRORATE-ROUNDING
           PERFORM 2250-ACCUM-STATUS
           PERFORM 2260-ASSESS-EXEMPT
           PERFORM 2270-ACCUM-CHANNEL
           PERFORM 2280-ASSESS-STOP
           PERFORM 2290-AGE-LAST-MAINT
           PERFORM 2300-ACCUM-CURRENCY
           PERFORM 2310-PRORATE-FEE
           PERFORM 2320-FLAG-ITEM
           PERFORM 2330-AGE-NOTICE
           PERFORM 2340-ASSESS-AUTO-RENEW
           PERFORM 2350-CHECK-LAST-STMT
           PERFORM 2360-AGE-NOTICE
           PERFORM 2370-EDIT-CUST-ID
           PERFORM 2380-ASSESS-AUTO-RENEW
           PERFORM 2390-COUNT-PAYMENT
           PERFORM 2400-ACCRUE-NATIONAL
           PERFORM 2410-COUNT-EXCEPT
           PERFORM 2420-DESCRIBE-STATUS
           PERFORM 2430-FLAG-ITEM
           PERFORM 2440-ACCUM-CHANNEL
           PERFORM 2450-SPLIT-ROUNDING
           PERFORM 2460-PRORATE-EURO
           PERFORM 2470-PRORATE-ROUNDING
           PERFORM 2480-SPLIT-TRIANGULAR
           PERFORM 2490-LIMIT-TRIANGULAR
           PERFORM 2500-COUNT-EXCEPT
           PERFORM 2510-SPLIT-TRIANGULAR
           PERFORM 2520-ASSESS-EXEMPT
           PERFORM 2530-SELECT-PENALTY-PCT
           PERFORM 2540-SPLIT-EURO
           PERFORM 2550-AGE-NOTICE
           PERFORM 2560-EDIT-CUST-ID
           PERFORM 2570-EDIT-CUST-ID
           PERFORM 2580-ASSESS-STOP
           PERFORM 2590-ACCRUE-ROUNDING
           PERFORM 2600-SET-REGION
           PERFORM 2610-EDIT-CUST-ID
           PERFORM 2620-FLAG-ITEM
           PERFORM 2630-CHECK-MATURITY
           PERFORM 2640-ASSESS-EXEMPT
           PERFORM 2650-CHECK-MATURITY
           PERFORM 2660-DESCRIBE-CHANNEL
           PERFORM 2670-ACCUM-CURRENCY
           PERFORM 2680-EDIT-REF-NO
           PERFORM 2690-LIMIT-NATIONAL
           PERFORM 2700-SELECT-PENALTY-PCT
           PERFORM 2710-CHECK-NOTICE
           PERFORM 2720-EDIT-REF-NO
           PERFORM 2730-CHECK-LAST-STMT
           PERFORM 2740-SET-CURRENCY
           PERFORM 2750-AGE-LAST-STMT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO EUTIER-RETURN-CODE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2000-DESCRIBE-STATUS.
           EVALUATE EUTIER-STATUS-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE NATIONAL YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2010-ASSESS-STOP.
           EVALUATE TRUE
               WHEN EUTIER-STOP-FLAG = 'Y'
                   MOVE ZERO TO EUTIER-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUTIER-EXEMPT-FLAG = 'Y'
                   COMPUTE EUTIER-FEE-YTD ROUNDED = EUTIER-NATIONAL-YTD
                       * EUTIER-FEE-PCT / 200
               WHEN EUTIER-CURRENCY-CD = 'EW'
                   MOVE 15.00 TO EUTIER-FEE-YTD
               WHEN EUTIER-CURRENCY-CD = 'FJ'
                   MOVE 3.00 TO EUTIER-FEE-YTD
               WHEN OTHER
                   COMPUTE EUTIER-FEE-YTD ROUNDED = EUTIER-NATIONAL-YTD
                       * EUTIER-FEE-PCT / 100
           END-EVALUATE
           IF EUTIER-FEE-YTD > 0 AND EUTIER-FEE-YTD < 5.00
               MOVE 5.00 TO EUTIER-FEE-YTD
           END-IF
           IF EUTIER-FEE-YTD > 100.00
               MOVE 100.00 TO EUTIER-FEE-YTD
           END-IF
           ADD EUTIER-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2020-FLAG-FEE.
           IF EUTIER-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUTIER-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2030-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EUTIER-NATIONAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EUTIER-NATIONAL-YTD - (WS-WORK-AMT *
               4)
           COMPUTE EUTIER-ROUNDING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE EURO YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO EUTIER-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EUTIER-EURO-YTD NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO EUTIER-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUTIER-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUTIER-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2050-DESCRIBE-STATUS.
           EVALUATE EUTIER-STATUS-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'U'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE EUTIER-EURO-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRIANGULAR AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2070-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUTIER-REGION-CD
                   ADD EUTIER-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUTIER-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUTIER-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E282 REF NO REQUIRED
      * E527 TRIANGULAR AMT NEGATIVE
      * E424 TRIANGULAR AMT OVER 1000000
      * E118 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2080-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUTIER-REF-NO = SPACES
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN EUTIER-TRIANGULAR-AMT < 0
                   MOVE 'E527' TO WS-REJECT-CODE
               WHEN EUTIER-TRIANGULAR-AMT > 1000000.00
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN EUTIER-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E118' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NATIONAL YTD.
      *----------------------------------------------------------------*
       2090-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUTIER-NATIONAL-YTD ROUNDED =
                   EUTIER-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUTIER-NATIONAL-YTD
           END-IF
           ADD EUTIER-NATIONAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2100-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-MATURITY-DATE > 0 AND EUTIER-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-MATURITY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DTL -> HYD WHEN NOTICE DATE IS PAST
      * HYD -> JUL WHEN NOTICE DATE IS PAST
      * JUL -> WSE WHEN NOTICE DATE IS PAST
      * WSE -> YAU WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2110-SET-REGION.
           EVALUATE EUTIER-REGION-CD
               WHEN 'DTL'
                   IF EUTIER-NOTICE-DATE > 0 AND EUTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HYD' TO EUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HYD'
                   IF EUTIER-NOTICE-DATE > 0 AND EUTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JUL' TO EUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JUL'
                   IF EUTIER-NOTICE-DATE > 0 AND EUTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WSE' TO EUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WSE'
                   IF EUTIER-NOTICE-DATE > 0 AND EUTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YAU' TO EUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2120-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EUTIER-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = EUTIER-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE EUTIER-TRIANGULAR-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE EURO YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2140-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO EUTIER-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EUTIER-EURO-YTD NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO EUTIER-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUTIER-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUTIER-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2150-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUTIER-CURRENCY-CD
                   ADD EUTIER-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUTIER-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUTIER-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE ROUNDING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2160-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO EUTIER-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EUTIER-ROUNDING-YTD NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO EUTIER-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUTIER-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUTIER-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-REGION.
           EVALUATE EUTIER-REGION-CD
               WHEN 'DTL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ROUNDING FEE ON THE FEE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2180-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EUTIER-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO EUTIER-ROUNDING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUTIER-STOP-FLAG = 'Y'
                   COMPUTE EUTIER-ROUNDING-AMT ROUNDED = EUTIER-FEE-YTD
                       * EUTIER-FEE-PCT / 200
               WHEN EUTIER-STATUS-CD = 'B'
                   MOVE 7.00 TO EUTIER-ROUNDING-AMT
               WHEN EUTIER-STATUS-CD = 'F'
                   MOVE 20.00 TO EUTIER-ROUNDING-AMT
               WHEN OTHER
                   COMPUTE EUTIER-ROUNDING-AMT ROUNDED = EUTIER-FEE-YTD
                       * EUTIER-FEE-PCT / 100
           END-EVALUATE
           IF EUTIER-ROUNDING-AMT > 0 AND EUTIER-ROUNDING-AMT < 2.00
               MOVE 2.00 TO EUTIER-ROUNDING-AMT
           END-IF
           IF EUTIER-ROUNDING-AMT > 100.00
               MOVE 100.00 TO EUTIER-ROUNDING-AMT
           END-IF
           ADD EUTIER-ROUNDING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2190-DESCRIBE-REGION.
           EVALUATE EUTIER-REGION-CD
               WHEN 'DTL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2200-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EUTIER-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO EUTIER-NATIONAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUTIER-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE EUTIER-NATIONAL-YTD ROUNDED = WS-WORK-AMT *
                       EUTIER-PENALTY-PCT / 200
               WHEN EUTIER-CURRENCY-CD = 'EW'
                   MOVE 10.00 TO EUTIER-NATIONAL-YTD
               WHEN EUTIER-CURRENCY-CD = 'FJ'
                   MOVE 20.00 TO EUTIER-NATIONAL-YTD
               WHEN OTHER
                   COMPUTE EUTIER-NATIONAL-YTD ROUNDED = WS-WORK-AMT *
                       EUTIER-PENALTY-PCT / 100
           END-EVALUATE
           IF EUTIER-NATIONAL-YTD > 0 AND EUTIER-NATIONAL-YTD < 5.00
               MOVE 5.00 TO EUTIER-NATIONAL-YTD
           END-IF
           IF EUTIER-NATIONAL-YTD > 25.00
               MOVE 25.00 TO EUTIER-NATIONAL-YTD
           END-IF
           ADD EUTIER-NATIONAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2210-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-LAST-STMT-DATE > 0 AND EUTIER-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2220-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO EUTIER-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRIANGULAR AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2230-ACCRUE-TRIANGULAR.
           IF EUTIER-TRIANGULAR-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUTIER-TRIANGULAR-AMT * EUTIER-PENALTY-PCT / 100 /
                       360
               ADD WS-WORK-AMT TO EUTIER-ROUNDING-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROUNDING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2240-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   EUTIER-ROUNDING-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE ROUNDING AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUTIER-STATUS-CD
                   ADD EUTIER-ROUNDING-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUTIER-ROUNDING-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ROUNDING YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2260-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EUTIER-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUTIER-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EUTIER-ROUNDING-YTD *
                       EUTIER-FEE-PCT / 200
               WHEN EUTIER-CHANNEL-CD = 'AEA'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN EUTIER-CHANNEL-CD = 'EBM'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EUTIER-ROUNDING-YTD *
                       EUTIER-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ROUNDING YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2270-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUTIER-CHANNEL-CD
                   ADD EUTIER-ROUNDING-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUTIER-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUTIER-ROUNDING-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EURO YTD FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2280-ASSESS-STOP.
           EVALUATE TRUE
               WHEN EUTIER-STOP-FLAG = 'Y'
                   MOVE ZERO TO EUTIER-EURO-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUTIER-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE EUTIER-EURO-YTD ROUNDED = WS-WORK-AMT *
                       EUTIER-PENALTY-PCT / 200
               WHEN EUTIER-CHANNEL-CD = 'AEA'
                   MOVE 20.00 TO EUTIER-EURO-YTD
               WHEN EUTIER-CHANNEL-CD = 'EBM'
                   MOVE 12.00 TO EUTIER-EURO-YTD
               WHEN OTHER
                   COMPUTE EUTIER-EURO-YTD ROUNDED = WS-WORK-AMT *
                       EUTIER-PENALTY-PCT / 100
           END-EVALUATE
           IF EUTIER-EURO-YTD > 0 AND EUTIER-EURO-YTD < 5.00
               MOVE 5.00 TO EUTIER-EURO-YTD
           END-IF
           IF EUTIER-EURO-YTD > 50.00
               MOVE 50.00 TO EUTIER-EURO-YTD
           END-IF
           ADD EUTIER-EURO-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-LAST-MAINT-DATE > 0 AND EUTIER-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2300-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUTIER-CURRENCY-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUTIER-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROUNDING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       2310-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUTIER-FEE-YTD ROUNDED =
                   EUTIER-ROUNDING-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUTIER-FEE-YTD
           END-IF
           ADD EUTIER-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2320-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2330-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-NOTICE-DATE > 0 AND EUTIER-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ROUNDING YTD FEE ON THE ROUNDING AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2340-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EUTIER-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EUTIER-ROUNDING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUTIER-EXEMPT-FLAG = 'Y'
                   COMPUTE EUTIER-ROUNDING-YTD ROUNDED =
                       EUTIER-ROUNDING-AMT * EUTIER-FEE-PCT / 200
               WHEN EUTIER-CHANNEL-CD = 'AEA'
                   MOVE 20.00 TO EUTIER-ROUNDING-YTD
               WHEN EUTIER-CHANNEL-CD = 'EBM'
                   MOVE 20.00 TO EUTIER-ROUNDING-YTD
               WHEN OTHER
                   COMPUTE EUTIER-ROUNDING-YTD ROUNDED =
                       EUTIER-ROUNDING-AMT * EUTIER-FEE-PCT / 100
           END-EVALUATE
           IF EUTIER-ROUNDING-YTD > 0 AND EUTIER-ROUNDING-YTD < 5.00
               MOVE 5.00 TO EUTIER-ROUNDING-YTD
           END-IF
           IF EUTIER-ROUNDING-YTD > 150.00
               MOVE 150.00 TO EUTIER-ROUNDING-YTD
           END-IF
           ADD EUTIER-ROUNDING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO EUTIER-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-NOTICE-DATE > 0 AND EUTIER-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E040 CUST ID REQUIRED
      * E885 ROUNDING AMT NEGATIVE
      * E867 ROUNDING AMT OVER 5000
      * E304 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2370-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUTIER-CUST-ID = SPACES
                   MOVE 'E040' TO WS-REJECT-CODE
               WHEN EUTIER-ROUNDING-AMT < 0
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN EUTIER-ROUNDING-AMT > 5000.00
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN EUTIER-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E304' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2380-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EUTIER-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EUTIER-TRIANGULAR-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUTIER-EXEMPT-FLAG = 'Y'
                   COMPUTE EUTIER-TRIANGULAR-AMT ROUNDED = WS-WORK-AMT-2
                       * EUTIER-FEE-PCT / 200
               WHEN EUTIER-CHANNEL-CD = 'AEA'
                   MOVE 15.00 TO EUTIER-TRIANGULAR-AMT
               WHEN EUTIER-CHANNEL-CD = 'EBM'
                   MOVE 10.00 TO EUTIER-TRIANGULAR-AMT
               WHEN OTHER
                   COMPUTE EUTIER-TRIANGULAR-AMT ROUNDED = WS-WORK-AMT-2
                       * EUTIER-FEE-PCT / 100
           END-EVALUATE
           IF EUTIER-TRIANGULAR-AMT > 0 AND EUTIER-TRIANGULAR-AMT <
               10.00
               MOVE 10.00 TO EUTIER-TRIANGULAR-AMT
           END-IF
           IF EUTIER-TRIANGULAR-AMT > 35.00
               MOVE 35.00 TO EUTIER-TRIANGULAR-AMT
           END-IF
           ADD EUTIER-TRIANGULAR-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2390-COUNT-PAYMENT.
           IF EUTIER-PAYMENT-CNT < 9999999
               ADD 1 TO EUTIER-PAYMENT-CNT
           END-IF
           IF EUTIER-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO EUTIER-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2400-ACCRUE-NATIONAL.
           IF EUTIER-NATIONAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUTIER-NATIONAL-YTD * EUTIER-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO EUTIER-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-EXCEPT.
           IF EUTIER-EXCEPT-CNT < 9999999
               ADD 1 TO EUTIER-EXCEPT-CNT
           END-IF
           IF EUTIER-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO EUTIER-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2420-DESCRIBE-STATUS.
           EVALUATE EUTIER-STATUS-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'R'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2430-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRIANGULAR AMT BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2440-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUTIER-CHANNEL-CD
                   ADD EUTIER-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUTIER-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUTIER-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2450-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EUTIER-ROUNDING-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EUTIER-ROUNDING-AMT - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EURO YTD.
      *----------------------------------------------------------------*
       2460-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUTIER-EURO-YTD ROUNDED =
                   EUTIER-TRIANGULAR-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUTIER-EURO-YTD
           END-IF
           ADD EUTIER-EURO-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROUNDING AMT.
      *----------------------------------------------------------------*
       2470-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUTIER-ROUNDING-AMT ROUNDED =
                   EUTIER-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUTIER-ROUNDING-AMT
           END-IF
           ADD EUTIER-ROUNDING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2480-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EUTIER-TRIANGULAR-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EUTIER-TRIANGULAR-AMT - (WS-WORK-AMT
               * 3)
           COMPUTE EUTIER-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2490-LIMIT-TRIANGULAR.
           IF EUTIER-TRIANGULAR-AMT < 50.00
               MOVE 50.00 TO EUTIER-TRIANGULAR-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUTIER-TRIANGULAR-AMT > 5000.00
               MOVE 5000.00 TO EUTIER-TRIANGULAR-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2500-COUNT-EXCEPT.
           IF EUTIER-EXCEPT-CNT < 9999999
               ADD 1 TO EUTIER-EXCEPT-CNT
           END-IF
           IF EUTIER-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO EUTIER-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EUTIER-TRIANGULAR-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EUTIER-TRIANGULAR-AMT - (WS-WORK-AMT
               * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROUNDING YTD FEE ON THE NATIONAL YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2520-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EUTIER-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO EUTIER-ROUNDING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUTIER-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE EUTIER-ROUNDING-YTD ROUNDED =
                       EUTIER-NATIONAL-YTD * EUTIER-FEE-PCT / 200
               WHEN EUTIER-STATUS-CD = 'B'
                   MOVE 5.00 TO EUTIER-ROUNDING-YTD
               WHEN EUTIER-STATUS-CD = 'F'
                   MOVE 5.00 TO EUTIER-ROUNDING-YTD
               WHEN OTHER
                   COMPUTE EUTIER-ROUNDING-YTD ROUNDED =
                       EUTIER-NATIONAL-YTD * EUTIER-FEE-PCT / 100
           END-EVALUATE
           IF EUTIER-ROUNDING-YTD > 0 AND EUTIER-ROUNDING-YTD < 10.00
               MOVE 10.00 TO EUTIER-ROUNDING-YTD
           END-IF
           IF EUTIER-ROUNDING-YTD > 50.00
               MOVE 50.00 TO EUTIER-ROUNDING-YTD
           END-IF
           ADD EUTIER-ROUNDING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE TRIANGULAR AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO EUTIER-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EUTIER-TRIANGULAR-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO EUTIER-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUTIER-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUTIER-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2540-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EUTIER-EURO-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EUTIER-EURO-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2550-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-NOTICE-DATE > 0 AND EUTIER-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUTIER-EURO-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E190 CUST ID REQUIRED
      * E732 EURO YTD NEGATIVE
      * E983 EURO YTD OVER 99999
      * E681 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2560-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUTIER-CUST-ID = SPACES
                   MOVE 'E190' TO WS-REJECT-CODE
               WHEN EUTIER-EURO-YTD < 0
                   MOVE 'E732' TO WS-REJECT-CODE
               WHEN EUTIER-EURO-YTD > 99999.00
                   MOVE 'E983' TO WS-REJECT-CODE
               WHEN EUTIER-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E925 CUST ID REQUIRED
      * E599 WORK AMT 2 NEGATIVE
      * E855 WORK AMT 2 OVER 99999
      * E505 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2570-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUTIER-CUST-ID = SPACES
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E855' TO WS-REJECT-CODE
               WHEN EUTIER-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EURO YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2580-ASSESS-STOP.
           EVALUATE TRUE
               WHEN EUTIER-STOP-FLAG = 'Y'
                   MOVE ZERO TO EUTIER-EURO-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUTIER-EXEMPT-FLAG = 'Y'
                   COMPUTE EUTIER-EURO-YTD ROUNDED = WS-WORK-AMT-2 *
                       EUTIER-FEE-PCT / 200
               WHEN EUTIER-CHANNEL-CD = 'AEA'
                   MOVE 10.00 TO EUTIER-EURO-YTD
               WHEN EUTIER-CHANNEL-CD = 'EBM'
                   MOVE 7.00 TO EUTIER-EURO-YTD
               WHEN OTHER
                   COMPUTE EUTIER-EURO-YTD ROUNDED = WS-WORK-AMT-2 *
                       EUTIER-FEE-PCT / 100
           END-EVALUATE
           IF EUTIER-EURO-YTD > 0 AND EUTIER-EURO-YTD < 10.00
               MOVE 10.00 TO EUTIER-EURO-YTD
           END-IF
           IF EUTIER-EURO-YTD > 35.00
               MOVE 35.00 TO EUTIER-EURO-YTD
           END-IF
           ADD EUTIER-EURO-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROUNDING YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-ROUNDING.
           IF EUTIER-ROUNDING-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUTIER-ROUNDING-YTD * EUTIER-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DTL -> HYD WHEN LAST MAINT DATE IS PAST
      * HYD -> JUL WHEN LAST MAINT DATE IS PAST
      * JUL -> WSE WHEN LAST MAINT DATE IS PAST
      * WSE -> YAU WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2600-SET-REGION.
           EVALUATE EUTIER-REGION-CD
               WHEN 'DTL'
                   IF EUTIER-LAST-MAINT-DATE > 0 AND
                       EUTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HYD' TO EUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HYD'
                   IF EUTIER-LAST-MAINT-DATE > 0 AND
                       EUTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JUL' TO EUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JUL'
                   IF EUTIER-LAST-MAINT-DATE > 0 AND
                       EUTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'WSE' TO EUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WSE'
                   IF EUTIER-LAST-MAINT-DATE > 0 AND
                       EUTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'YAU' TO EUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E094 CUST ID REQUIRED
      * E632 FEE YTD NEGATIVE
      * E060 FEE YTD OVER 5000
      * E736 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUTIER-CUST-ID = SPACES
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN EUTIER-FEE-YTD < 0
                   MOVE 'E632' TO WS-REJECT-CODE
               WHEN EUTIER-FEE-YTD > 5000.00
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN EUTIER-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E736' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2620-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EUTIER-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR FEE ON THE ROUNDING AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2640-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EUTIER-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO EUTIER-TRIANGULAR-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUTIER-STOP-FLAG = 'Y'
                   COMPUTE EUTIER-TRIANGULAR-AMT ROUNDED =
                       EUTIER-ROUNDING-AMT * EUTIER-FEE-PCT / 200
               WHEN EUTIER-CHANNEL-CD = 'AEA'
                   MOVE 7.00 TO EUTIER-TRIANGULAR-AMT
               WHEN EUTIER-CHANNEL-CD = 'EBM'
                   MOVE 3.00 TO EUTIER-TRIANGULAR-AMT
               WHEN OTHER
                   COMPUTE EUTIER-TRIANGULAR-AMT ROUNDED =
                       EUTIER-ROUNDING-AMT * EUTIER-FEE-PCT / 100
           END-EVALUATE
           IF EUTIER-TRIANGULAR-AMT > 0 AND EUTIER-TRIANGULAR-AMT < 2.00
               MOVE 2.00 TO EUTIER-TRIANGULAR-AMT
           END-IF
           IF EUTIER-TRIANGULAR-AMT > 75.00
               MOVE 75.00 TO EUTIER-TRIANGULAR-AMT
           END-IF
           ADD EUTIER-TRIANGULAR-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2650-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO EUTIER-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2660-DESCRIBE-CHANNEL.
           EVALUATE EUTIER-CHANNEL-CD
               WHEN 'AEA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'EBM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GAU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GSU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VWP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XDF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2670-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUTIER-CURRENCY-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUTIER-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E269 REF NO REQUIRED
      * E788 WORK AMT 2 NEGATIVE
      * E901 WORK AMT 2 OVER 10000
      * E440 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2680-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUTIER-REF-NO = SPACES
                   MOVE 'E269' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E788' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E901' TO WS-REJECT-CODE
               WHEN EUTIER-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2690-LIMIT-NATIONAL.
           IF EUTIER-NATIONAL-YTD < 100.00
               MOVE 100.00 TO EUTIER-NATIONAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUTIER-NATIONAL-YTD > 50000.00
               MOVE 50000.00 TO EUTIER-NATIONAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE ROUNDING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2700-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO EUTIER-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EUTIER-ROUNDING-YTD NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO EUTIER-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUTIER-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUTIER-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2710-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO EUTIER-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E963 REF NO REQUIRED
      * E608 ROUNDING AMT NEGATIVE
      * E589 ROUNDING AMT OVER 5000
      * E363 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2720-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUTIER-REF-NO = SPACES
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN EUTIER-ROUNDING-AMT < 0
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN EUTIER-ROUNDING-AMT > 5000.00
                   MOVE 'E589' TO WS-REJECT-CODE
               WHEN EUTIER-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2730-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EUTIER-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * EW -> FJ WHEN LAST MAINT DATE IS PAST
      * FJ -> JC WHEN LAST MAINT DATE IS PAST
      * JC -> EW WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2740-SET-CURRENCY.
           EVALUATE EUTIER-CURRENCY-CD
               WHEN 'EW'
                   IF EUTIER-LAST-MAINT-DATE > 0 AND
                       EUTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FJ' TO EUTIER-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF EUTIER-LAST-MAINT-DATE > 0 AND
                       EUTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JC' TO EUTIER-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF EUTIER-LAST-MAINT-DATE > 0 AND
                       EUTIER-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'EW' TO EUTIER-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2750-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EUTIER-LAST-STMT-DATE > 0 AND EUTIER-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUTIER-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUTIER-ROUNDING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EUTIER-ROUNDING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUTIER-ROUNDING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EUTIER-ROUNDING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUTIER-ROUNDING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO EUTIER-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO EUTIER-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO EUTIER-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'EUTIER EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
