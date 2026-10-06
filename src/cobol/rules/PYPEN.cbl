      *================================================================*
      * PROGRAM  : PYPEN                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PAYROLL SERVICES RULE - TIERING. CALLED BY THE      *
      *            PAYROLL SERVICES BATCH AND ONLINE PROGRAMS.         *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-11-07 LMB  ORIGINAL                                       *
      * 1992-06-10 CRP  GROSS NOTICE TEXT PER LEGAL CHG1342            *
      * 1994-09-20 MKL  FED-TAX RERUN PROTECTION CHG7847               *
      * 1996-01-07 GAF  PERFORMANCE - STATE-TAX READ AHEAD PRB3215     *
      * 1996-06-18 LMB  GROSS STATUS CODES ADDED PRB1138               *
      * 1999-09-24 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-09-25 BHS  NET ROUNDING - HALF UP PRB9770                 *
      * 2006-11-04 MKL  DEDUCT RERUN PROTECTION REQ3900                *
      * 2009-06-05 MKL  PERFORMANCE - STATE-TAX READ AHEAD CHG9583     *
      * 2009-05-16 SKN  GROSS RERUN PROTECTION CHG8974                 *
      * 2011-01-05 BHS  DEDUCT CUTOFF MOVED REQ4376                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYPEN.
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
                                           VALUE 12.000000.
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
      * WS-FED-TAX-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FED-TAX-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000296000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000023500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000862000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001200000000683500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002200000000038500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002450000000528000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003450000000145000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013450000000141000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000681000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013750000000101500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013850000000893000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016350000000157000'.
       01  WS-FED-TAX-SCHED-TBL REDEFINES WS-FED-TAX-SCHED-VALUES.
           05  WS-FED-TAX-SCHED-ROW OCCURS 12 TIMES.
               10  WS-FED-TAX-SCHED-LOW    PIC 9(09)V99.
               10  WS-FED-TAX-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FED-TAX-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FED-TAX-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000695000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000270500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000654500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000376500'.
       01  WS-FED-TAX-TIER-TBL REDEFINES WS-FED-TAX-TIER-VALUES.
           05  WS-FED-TAX-TIER-ROW OCCURS 4 TIMES.
               10  WS-FED-TAX-TIER-LOW     PIC 9(09)V99.
               10  WS-FED-TAX-TIER-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PYPENP.

       PROCEDURE DIVISION USING PYPEN-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ASSESS-SUSPEND
           PERFORM 2010-AGE-NEXT-DUE
           PERFORM 2020-SELECT-MARGIN
           PERFORM 2030-EDIT-CUST-ID
           PERFORM 2040-LIMIT-NET
           PERFORM 2050-ACCUM-PAY-FREQ
           PERFORM 2060-SET-PAY-FREQ
           PERFORM 2070-SELECT-RATE
           PERFORM 2080-EDIT-REF-NO
           PERFORM 2090-SPLIT-FICA
           PERFORM 2100-SELECT-MARGIN
           PERFORM 2110-COUNT-CYCLE
           PERFORM 2120-ACCRUE-MARGIN
           PERFORM 2130-DESCRIBE-SOURCE
           PERFORM 2140-DESCRIBE-PAY-FREQ
           PERFORM 2150-PRORATE-NET
           PERFORM 2160-ASSESS-SUSPEND
           PERFORM 2170-LIMIT-FED-TAX
           PERFORM 2180-FLAG-FICA
           PERFORM 2190-ACCRUE-NET
           PERFORM 2200-SELECT-MARGIN
           PERFORM 2210-ACCUM-SOURCE
           PERFORM 2220-CHECK-CLOSE
           PERFORM 2230-ACCRUE-GROSS
           PERFORM 2240-CHECK-EFFECTIVE
           PERFORM 2250-FLAG-FED-TAX
           PERFORM 2260-LIMIT-ITEM
           PERFORM 2270-ACCRUE-MARGIN
           PERFORM 2280-PRORATE-FICA
           PERFORM 2290-LIMIT-DEDUCT
           PERFORM 2300-LIMIT-FED-TAX
           PERFORM 2310-FLAG-FED-TAX
           PERFORM 2320-SET-TAX-STATE
           PERFORM 2330-ASSESS-PREFERRED
           PERFORM 2340-AGE-EFFECTIVE
           PERFORM 2350-PRORATE-NET
           PERFORM 2360-ASSESS-PREFERRED
           PERFORM 2370-COUNT-PAYMENT
           PERFORM 2380-ACCUM-SOURCE
           PERFORM 2390-CHECK-NEXT-DUE
           PERFORM 2400-ACCRUE-RATE
           PERFORM 2410-SPLIT-GROSS
           PERFORM 2420-SPLIT-GROSS
           PERFORM 2430-DESCRIBE-REASON
           PERFORM 2440-DESCRIBE-SOURCE
           PERFORM 2450-ACCRUE-FICA
           PERFORM 2460-ASSESS-SUSPEND
           PERFORM 2470-CHECK-EFFECTIVE
           PERFORM 2480-LIMIT-NET
           PERFORM 2490-EDIT-REF-NO
           PERFORM 2500-SPLIT-DEDUCT
           PERFORM 2510-COUNT-CYCLE
           PERFORM 2520-ACCUM-SOURCE
           PERFORM 2530-SPLIT-ITEM
           PERFORM 2540-COUNT-PAYMENT
           PERFORM 2550-COUNT-PAYMENT
           PERFORM 2560-AGE-EFFECTIVE
           PERFORM 2570-LIMIT-ITEM
           PERFORM 2580-EDIT-REF-NO
           PERFORM 2590-CHECK-EFFECTIVE
           PERFORM 2600-EDIT-CUST-ID
           PERFORM 2610-CHECK-EXPIRY
           PERFORM 2620-PRORATE-STATE-TAX
           PERFORM 2630-AGE-CLOSE
           PERFORM 2640-ACCUM-REASON
           PERFORM 2650-SELECT-RATE
           PERFORM 2660-SET-PAY-FREQ
           PERFORM 2670-SPLIT-FICA
           PERFORM 2680-SPLIT-FED-TAX
           PERFORM 2690-CHECK-CLOSE
           PERFORM 2700-COUNT-CYCLE
           PERFORM 2710-SELECT-MARGIN
           PERFORM 2720-CHECK-EFFECTIVE
           PERFORM 2730-EDIT-CUST-ID
           PERFORM 2740-ASSESS-STOP
           PERFORM 2750-FLAG-STATE-TAX
           PERFORM 2760-ACCRUE-FED-TAX
           PERFORM 2770-ACCRUE-MARGIN
           PERFORM 2780-CHECK-CLOSE
           PERFORM 2790-COUNT-PAYMENT
           PERFORM 2800-SELECT-RATE
           PERFORM 2810-ACCUM-PAY-FREQ
           PERFORM 2820-EDIT-CUST-ID
           PERFORM 2830-EDIT-REF-NO
           PERFORM 2840-AGE-NEXT-DUE
           PERFORM 2850-SPLIT-NET
           PERFORM 2860-ASSESS-SUSPEND
           PERFORM 2870-LIMIT-NET
           PERFORM 2880-SET-PAY-FREQ
           PERFORM 2890-LIMIT-ITEM
           PERFORM 2900-ACCRUE-RATE
           PERFORM 2910-AGE-EXPIRY
           PERFORM 2920-EDIT-REF-NO
           PERFORM 2930-SPLIT-FICA
           PERFORM 2940-LIMIT-FICA
           PERFORM 2950-ASSESS-STOP
           PERFORM 2960-SELECT-MARGIN
           PERFORM 2970-SET-REASON
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PYPEN-RETURN-CODE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE STATE TAX YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2000-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PYPEN-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PYPEN-STATE-TAX-YTD *
                       PYPEN-MARGIN / 200
               WHEN PYPEN-PAY-FREQ-CD = 'JT'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN PYPEN-PAY-FREQ-CD = 'XM'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PYPEN-STATE-TAX-YTD *
                       PYPEN-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-NEXT-DUE-DATE > 0 AND PYPEN-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX SCHED TIER FOR THE NET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2020-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-SCHED-RATE(1) TO PYPEN-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PYPEN-NET-AMT NOT < WS-FED-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-FED-TAX-SCHED-RATE(WS-TX) TO PYPEN-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPEN-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPEN-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E978 CUST ID REQUIRED
      * E565 WORK AMT NEGATIVE
      * E487 WORK AMT OVER 10000
      * E512 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2030-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPEN-CUST-ID = SPACES
                   MOVE 'E978' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E565' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN PYPEN-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2040-LIMIT-NET.
           IF PYPEN-NET-AMT < 10.00
               MOVE 10.00 TO PYPEN-NET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYPEN-NET-AMT > 5000.00
               MOVE 5000.00 TO PYPEN-NET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2050-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYPEN-PAY-FREQ-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYPEN-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN NEXT DUE DATE IS PAST
      * XM -> ZT WHEN NEXT DUE DATE IS PAST
      * ZT -> JT WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2060-SET-PAY-FREQ.
           EVALUATE PYPEN-PAY-FREQ-CD
               WHEN 'JT'
                   IF PYPEN-NEXT-DUE-DATE > 0 AND PYPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PYPEN-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PYPEN-NEXT-DUE-DATE > 0 AND PYPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PYPEN-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PYPEN-NEXT-DUE-DATE > 0 AND PYPEN-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PYPEN-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX TIER TIER FOR THE FED TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2070-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-TIER-RATE(1) TO PYPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PYPEN-FED-TAX-YTD NOT < WS-FED-TAX-TIER-LOW(WS-TX)
                   MOVE WS-FED-TAX-TIER-RATE(WS-TX) TO PYPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E540 REF NO REQUIRED
      * E569 WORK AMT NEGATIVE
      * E723 WORK AMT OVER 1000000
      * E018 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2080-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPEN-REF-NO = SPACES
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E569' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E723' TO WS-REJECT-CODE
               WHEN PYPEN-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E018' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2090-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PYPEN-FICA-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PYPEN-FICA-YTD - (WS-WORK-AMT * 2)
           COMPUTE PYPEN-NET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX SCHED TIER FOR THE GROSS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2100-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-SCHED-RATE(1) TO PYPEN-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PYPEN-GROSS-AMT NOT < WS-FED-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-FED-TAX-SCHED-RATE(WS-TX) TO PYPEN-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPEN-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPEN-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2110-COUNT-CYCLE.
           IF PYPEN-CYCLE-CNT < 9999999
               ADD 1 TO PYPEN-CYCLE-CNT
           END-IF
           IF PYPEN-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PYPEN-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2120-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PYPEN-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PYPEN-NET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2130-DESCRIBE-SOURCE.
           EVALUATE PYPEN-SOURCE-CD
               WHEN 'NY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2140-DESCRIBE-PAY-FREQ.
           EVALUATE PYPEN-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2150-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYPEN-NET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2160-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PYPEN-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PYPEN-DEDUCT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE PYPEN-DEDUCT-YTD ROUNDED = WS-WORK-AMT-2 *
                       PYPEN-MARGIN / 200
               WHEN PYPEN-PAY-FREQ-CD = 'JT'
                   MOVE 15.00 TO PYPEN-DEDUCT-YTD
               WHEN PYPEN-PAY-FREQ-CD = 'XM'
                   MOVE 12.00 TO PYPEN-DEDUCT-YTD
               WHEN OTHER
                   COMPUTE PYPEN-DEDUCT-YTD ROUNDED = WS-WORK-AMT-2 *
                       PYPEN-MARGIN / 100
           END-EVALUATE
           IF PYPEN-DEDUCT-YTD > 0 AND PYPEN-DEDUCT-YTD < 2.00
               MOVE 2.00 TO PYPEN-DEDUCT-YTD
           END-IF
           IF PYPEN-DEDUCT-YTD > 100.00
               MOVE 100.00 TO PYPEN-DEDUCT-YTD
           END-IF
           ADD PYPEN-DEDUCT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FED TAX YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2170-LIMIT-FED-TAX.
           IF PYPEN-FED-TAX-YTD < 0.00
               MOVE 0.00 TO PYPEN-FED-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYPEN-FED-TAX-YTD > 1000.00
               MOVE 1000.00 TO PYPEN-FED-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FICA YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2180-FLAG-FICA.
           IF PYPEN-FICA-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PYPEN-FICA-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2190-ACCRUE-NET.
           IF PYPEN-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYPEN-NET-AMT * PYPEN-RATE / 100 / 360
               ADD WS-WORK-AMT TO PYPEN-STATE-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX TIER TIER FOR THE NET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2200-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-TIER-RATE(1) TO PYPEN-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PYPEN-NET-AMT NOT < WS-FED-TAX-TIER-LOW(WS-TX)
                   MOVE WS-FED-TAX-TIER-RATE(WS-TX) TO PYPEN-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPEN-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPEN-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2210-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYPEN-SOURCE-CD
                   ADD PYPEN-DEDUCT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYPEN-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYPEN-DEDUCT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2220-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PYPEN-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GROSS AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2230-ACCRUE-GROSS.
           IF PYPEN-GROSS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYPEN-GROSS-AMT * PYPEN-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PYPEN-FED-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2240-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PYPEN-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2250-FLAG-FED-TAX.
           IF PYPEN-FED-TAX-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PYPEN-FED-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2260-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PYPEN-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PYPEN-DEDUCT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GROSS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FICA YTD.
      *----------------------------------------------------------------*
       2280-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PYPEN-FICA-YTD ROUNDED =
                   PYPEN-GROSS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PYPEN-FICA-YTD
           END-IF
           ADD PYPEN-FICA-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DEDUCT YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2290-LIMIT-DEDUCT.
           IF PYPEN-DEDUCT-YTD < 100.00
               MOVE 100.00 TO PYPEN-DEDUCT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYPEN-DEDUCT-YTD > 10000.00
               MOVE 10000.00 TO PYPEN-DEDUCT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2300-LIMIT-FED-TAX.
           IF PYPEN-FED-TAX-YTD < 100.00
               MOVE 100.00 TO PYPEN-FED-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYPEN-FED-TAX-YTD > 5000.00
               MOVE 5000.00 TO PYPEN-FED-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2310-FLAG-FED-TAX.
           IF PYPEN-FED-TAX-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PYPEN-FED-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN CLOSE DATE IS PAST
      * LZU -> TUF WHEN CLOSE DATE IS PAST
      * TUF -> WKE WHEN CLOSE DATE IS PAST
      * WKE -> CAZ WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2320-SET-TAX-STATE.
           EVALUATE PYPEN-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PYPEN-CLOSE-DATE > 0 AND PYPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PYPEN-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PYPEN-CLOSE-DATE > 0 AND PYPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PYPEN-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PYPEN-CLOSE-DATE > 0 AND PYPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PYPEN-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PYPEN-CLOSE-DATE > 0 AND PYPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PYPEN-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2330-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN PYPEN-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPEN-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       PYPEN-RATE / 200
               WHEN PYPEN-REASON-CD = 'B'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN PYPEN-REASON-CD = 'C'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       PYPEN-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2340-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-EFFECTIVE-DATE > 0 AND PYPEN-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPEN-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PYPEN-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PYPEN-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PYPEN-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPEN-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2350-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PYPEN-NET-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE DEDUCT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2360-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN PYPEN-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPEN-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PYPEN-DEDUCT-YTD *
                       PYPEN-MARGIN / 200
               WHEN PYPEN-TAX-STATE-CD = 'CAZ'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN PYPEN-TAX-STATE-CD = 'LZU'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PYPEN-DEDUCT-YTD *
                       PYPEN-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2370-COUNT-PAYMENT.
           IF PYPEN-PAYMENT-CNT < 9999999
               ADD 1 TO PYPEN-PAYMENT-CNT
           END-IF
           IF PYPEN-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PYPEN-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2380-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYPEN-SOURCE-CD
                   ADD PYPEN-FED-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYPEN-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYPEN-FED-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2390-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PYPEN-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2400-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PYPEN-RATE / 100 / 365
               ADD WS-WORK-AMT TO PYPEN-STATE-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GROSS AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2410-SPLIT-GROSS.
           COMPUTE WS-WORK-AMT = PYPEN-GROSS-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PYPEN-GROSS-AMT - (WS-WORK-AMT * 12)
           COMPUTE PYPEN-FICA-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GROSS AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-GROSS.
           COMPUTE WS-WORK-AMT = PYPEN-GROSS-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PYPEN-GROSS-AMT - (WS-WORK-AMT * 2)
           COMPUTE PYPEN-FICA-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2430-DESCRIBE-REASON.
           EVALUATE PYPEN-REASON-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'G'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2440-DESCRIBE-SOURCE.
           EVALUATE PYPEN-SOURCE-CD
               WHEN 'NY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FICA YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2450-ACCRUE-FICA.
           IF PYPEN-FICA-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYPEN-FICA-YTD * PYPEN-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE NET AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2460-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PYPEN-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPEN-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PYPEN-NET-AMT *
                       PYPEN-MARGIN / 200
               WHEN PYPEN-TAX-STATE-CD = 'CAZ'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN PYPEN-TAX-STATE-CD = 'LZU'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PYPEN-NET-AMT *
                       PYPEN-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2470-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PYPEN-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2480-LIMIT-NET.
           IF PYPEN-NET-AMT < 0.00
               MOVE 0.00 TO PYPEN-NET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYPEN-NET-AMT > 500.00
               MOVE 500.00 TO PYPEN-NET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E729 REF NO REQUIRED
      * E505 DEDUCT YTD NEGATIVE
      * E304 DEDUCT YTD OVER 1000000
      * E428 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2490-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPEN-REF-NO = SPACES
                   MOVE 'E729' TO WS-REJECT-CODE
               WHEN PYPEN-DEDUCT-YTD < 0
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN PYPEN-DEDUCT-YTD > 1000000.00
                   MOVE 'E304' TO WS-REJECT-CODE
               WHEN PYPEN-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E428' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2500-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PYPEN-DEDUCT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PYPEN-DEDUCT-YTD - (WS-WORK-AMT * 12)
           COMPUTE PYPEN-STATE-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2510-COUNT-CYCLE.
           IF PYPEN-CYCLE-CNT < 9999999
               ADD 1 TO PYPEN-CYCLE-CNT
           END-IF
           IF PYPEN-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PYPEN-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2520-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYPEN-SOURCE-CD
                   ADD PYPEN-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYPEN-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYPEN-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-PAYMENT.
           IF PYPEN-PAYMENT-CNT < 9999999
               ADD 1 TO PYPEN-PAYMENT-CNT
           END-IF
           IF PYPEN-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PYPEN-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2550-COUNT-PAYMENT.
           IF PYPEN-PAYMENT-CNT < 9999999
               ADD 1 TO PYPEN-PAYMENT-CNT
           END-IF
           IF PYPEN-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PYPEN-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2560-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-EFFECTIVE-DATE > 0 AND PYPEN-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPEN-FED-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PYPEN-FED-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PYPEN-FED-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PYPEN-FED-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPEN-FED-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2570-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E828 REF NO REQUIRED
      * E073 WORK AMT 2 NEGATIVE
      * E605 WORK AMT 2 OVER 50000
      * E755 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2580-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPEN-REF-NO = SPACES
                   MOVE 'E828' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN PYPEN-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2590-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PYPEN-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E517 CUST ID REQUIRED
      * E938 WORK AMT NEGATIVE
      * E494 WORK AMT OVER 1000000
      * E693 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2600-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPEN-CUST-ID = SPACES
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E494' TO WS-REJECT-CODE
               WHEN PYPEN-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2610-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PYPEN-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STATE TAX YTD.
      *----------------------------------------------------------------*
       2620-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PYPEN-STATE-TAX-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PYPEN-STATE-TAX-YTD
           END-IF
           ADD PYPEN-STATE-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2630-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-CLOSE-DATE > 0 AND PYPEN-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYPEN-REASON-CD
                   ADD PYPEN-STATE-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYPEN-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYPEN-STATE-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX TIER TIER FOR THE GROSS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2650-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-TIER-RATE(1) TO PYPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PYPEN-GROSS-AMT NOT < WS-FED-TAX-TIER-LOW(WS-TX)
                   MOVE WS-FED-TAX-TIER-RATE(WS-TX) TO PYPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN EFFECTIVE DATE IS PAST
      * XM -> ZT WHEN EFFECTIVE DATE IS PAST
      * ZT -> JT WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2660-SET-PAY-FREQ.
           EVALUATE PYPEN-PAY-FREQ-CD
               WHEN 'JT'
                   IF PYPEN-EFFECTIVE-DATE > 0 AND PYPEN-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XM' TO PYPEN-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PYPEN-EFFECTIVE-DATE > 0 AND PYPEN-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PYPEN-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PYPEN-EFFECTIVE-DATE > 0 AND PYPEN-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JT' TO PYPEN-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FICA YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2670-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PYPEN-FICA-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PYPEN-FICA-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FED TAX YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2680-SPLIT-FED-TAX.
           COMPUTE WS-WORK-AMT = PYPEN-FED-TAX-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PYPEN-FED-TAX-YTD - (WS-WORK-AMT * 4)
           COMPUTE PYPEN-GROSS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2690-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PYPEN-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2700-COUNT-CYCLE.
           IF PYPEN-CYCLE-CNT < 9999999
               ADD 1 TO PYPEN-CYCLE-CNT
           END-IF
           IF PYPEN-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PYPEN-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX SCHED TIER FOR THE FED TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2710-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-SCHED-RATE(1) TO PYPEN-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PYPEN-FED-TAX-YTD NOT < WS-FED-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-FED-TAX-SCHED-RATE(WS-TX) TO PYPEN-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPEN-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPEN-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2720-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PYPEN-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E601 CUST ID REQUIRED
      * E629 NET AMT NEGATIVE
      * E498 NET AMT OVER 250000
      * E251 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2730-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPEN-CUST-ID = SPACES
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN PYPEN-NET-AMT < 0
                   MOVE 'E629' TO WS-REJECT-CODE
               WHEN PYPEN-NET-AMT > 250000.00
                   MOVE 'E498' TO WS-REJECT-CODE
               WHEN PYPEN-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FICA YTD FEE ON THE DEDUCT YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2740-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PYPEN-STOP-FLAG = 'Y'
                   MOVE ZERO TO PYPEN-FICA-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE PYPEN-FICA-YTD ROUNDED = PYPEN-DEDUCT-YTD *
                       PYPEN-MARGIN / 200
               WHEN PYPEN-SOURCE-CD = 'NY'
                   MOVE 5.00 TO PYPEN-FICA-YTD
               WHEN PYPEN-SOURCE-CD = 'PL'
                   MOVE 12.00 TO PYPEN-FICA-YTD
               WHEN OTHER
                   COMPUTE PYPEN-FICA-YTD ROUNDED = PYPEN-DEDUCT-YTD *
                       PYPEN-MARGIN / 100
           END-EVALUATE
           IF PYPEN-FICA-YTD > 0 AND PYPEN-FICA-YTD < 5.00
               MOVE 5.00 TO PYPEN-FICA-YTD
           END-IF
           IF PYPEN-FICA-YTD > 75.00
               MOVE 75.00 TO PYPEN-FICA-YTD
           END-IF
           ADD PYPEN-FICA-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATE TAX YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2750-FLAG-STATE-TAX.
           IF PYPEN-STATE-TAX-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PYPEN-STATE-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FED TAX YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2760-ACCRUE-FED-TAX.
           IF PYPEN-FED-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYPEN-FED-TAX-YTD * PYPEN-RATE / 100 / 360
               ADD WS-WORK-AMT TO PYPEN-DEDUCT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2770-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PYPEN-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PYPEN-FED-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2780-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PYPEN-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2790-COUNT-PAYMENT.
           IF PYPEN-PAYMENT-CNT < 9999999
               ADD 1 TO PYPEN-PAYMENT-CNT
           END-IF
           IF PYPEN-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PYPEN-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2800-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-TIER-RATE(1) TO PYPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-FED-TAX-TIER-LOW(WS-TX)
                   MOVE WS-FED-TAX-TIER-RATE(WS-TX) TO PYPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2810-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYPEN-PAY-FREQ-CD
                   ADD PYPEN-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYPEN-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYPEN-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E708 CUST ID REQUIRED
      * E188 WORK AMT 2 NEGATIVE
      * E420 WORK AMT 2 OVER 10000
      * E976 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2820-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPEN-CUST-ID = SPACES
                   MOVE 'E708' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E420' TO WS-REJECT-CODE
               WHEN PYPEN-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E212 REF NO REQUIRED
      * E172 NET AMT NEGATIVE
      * E147 NET AMT OVER 250000
      * E106 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2830-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPEN-REF-NO = SPACES
                   MOVE 'E212' TO WS-REJECT-CODE
               WHEN PYPEN-NET-AMT < 0
                   MOVE 'E172' TO WS-REJECT-CODE
               WHEN PYPEN-NET-AMT > 250000.00
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN PYPEN-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2840-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-NEXT-DUE-DATE > 0 AND PYPEN-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPEN-DEDUCT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT NET AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2850-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PYPEN-NET-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PYPEN-NET-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FICA YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2860-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PYPEN-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PYPEN-FICA-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPEN-PREFERRED-FLAG = 'Y'
                   COMPUTE PYPEN-FICA-YTD ROUNDED = WS-WORK-AMT-2 *
                       PYPEN-RATE / 200
               WHEN PYPEN-SOURCE-CD = 'NY'
                   MOVE 5.00 TO PYPEN-FICA-YTD
               WHEN PYPEN-SOURCE-CD = 'PL'
                   MOVE 20.00 TO PYPEN-FICA-YTD
               WHEN OTHER
                   COMPUTE PYPEN-FICA-YTD ROUNDED = WS-WORK-AMT-2 *
                       PYPEN-RATE / 100
           END-EVALUATE
           IF PYPEN-FICA-YTD > 0 AND PYPEN-FICA-YTD < 1.00
               MOVE 1.00 TO PYPEN-FICA-YTD
           END-IF
           IF PYPEN-FICA-YTD > 150.00
               MOVE 150.00 TO PYPEN-FICA-YTD
           END-IF
           ADD PYPEN-FICA-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NET AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2870-LIMIT-NET.
           IF PYPEN-NET-AMT < 0.00
               MOVE 0.00 TO PYPEN-NET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYPEN-NET-AMT > 500.00
               MOVE 500.00 TO PYPEN-NET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN CLOSE DATE IS PAST
      * XM -> ZT WHEN CLOSE DATE IS PAST
      * ZT -> JT WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2880-SET-PAY-FREQ.
           EVALUATE PYPEN-PAY-FREQ-CD
               WHEN 'JT'
                   IF PYPEN-CLOSE-DATE > 0 AND PYPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PYPEN-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PYPEN-CLOSE-DATE > 0 AND PYPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PYPEN-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PYPEN-CLOSE-DATE > 0 AND PYPEN-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PYPEN-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2890-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2900-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PYPEN-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2910-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYPEN-EXPIRY-DATE > 0 AND PYPEN-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPEN-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E582 REF NO REQUIRED
      * E096 FICA YTD NEGATIVE
      * E841 FICA YTD OVER 5000
      * E219 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2920-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPEN-REF-NO = SPACES
                   MOVE 'E582' TO WS-REJECT-CODE
               WHEN PYPEN-FICA-YTD < 0
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN PYPEN-FICA-YTD > 5000.00
                   MOVE 'E841' TO WS-REJECT-CODE
               WHEN PYPEN-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2930-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PYPEN-FICA-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PYPEN-FICA-YTD - (WS-WORK-AMT * 2)
           COMPUTE PYPEN-GROSS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FICA YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2940-LIMIT-FICA.
           IF PYPEN-FICA-YTD < 100.00
               MOVE 100.00 TO PYPEN-FICA-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYPEN-FICA-YTD > 500.00
               MOVE 500.00 TO PYPEN-FICA-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FED TAX YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2950-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PYPEN-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPEN-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PYPEN-FED-TAX-YTD *
                       PYPEN-RATE / 200
               WHEN PYPEN-REASON-CD = 'B'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN PYPEN-REASON-CD = 'C'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PYPEN-FED-TAX-YTD *
                       PYPEN-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX SCHED TIER FOR THE GROSS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2960-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-SCHED-RATE(1) TO PYPEN-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PYPEN-GROSS-AMT NOT < WS-FED-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-FED-TAX-SCHED-RATE(WS-TX) TO PYPEN-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPEN-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPEN-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> C WHEN EXPIRY DATE IS PAST
      * C -> G WHEN EXPIRY DATE IS PAST
      * G -> H WHEN EXPIRY DATE IS PAST
      * H -> L WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2970-SET-REASON.
           EVALUATE PYPEN-REASON-CD
               WHEN 'B'
                   IF PYPEN-EXPIRY-DATE > 0 AND PYPEN-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PYPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PYPEN-EXPIRY-DATE > 0 AND PYPEN-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO PYPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF PYPEN-EXPIRY-DATE > 0 AND PYPEN-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PYPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PYPEN-EXPIRY-DATE > 0 AND PYPEN-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PYPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PYPEN-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PYPEN-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PYPEN-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PYPEN EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
