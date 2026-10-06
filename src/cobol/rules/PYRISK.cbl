      *================================================================*
      * PROGRAM  : PYRISK                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PAYROLL SERVICES RULE - ELIGIBILITY. CALLED BY THE  *
      *            PAYROLL SERVICES BATCH AND ONLINE PROGRAMS.         *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-04-13 LMB  ORIGINAL                                       *
      * 1996-06-22 CRP  AUDIT FINDING - STATE-TAX CONTROL TOTALS INC095*
      * 1996-10-16 MKL  GROSS CALCULATION CORRECTED PRB6987            *
      * 1999-05-19 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-12-21 BHS  STATE-TAX LIMIT REVISED INC8796                *
      * 2002-10-14 MKL  STATE-TAX NOTICE TEXT PER LEGAL PRB9263        *
      * 2003-10-06 LMB  DEDUCT RERUN PROTECTION CHG1213                *
      * 2009-09-19 DPS  AUDIT FINDING - DEDUCT CONTROL TOTALS PRB5062  *
      * 2009-06-22 CRP  NET LIMIT REVISED INC6701                      *
      * 2012-01-06 RJT  FED-TAX FEE SCHEDULE UPDATE CHG3354            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYRISK.
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
                                           VALUE 11.000000.
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
      * WS-DEDUCT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEDUCT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000176000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000896000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000756500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000764000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008050000000684000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018050000000331500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018100000000258000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018150000000171000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018250000000126000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018300000000105500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018550000000558500'.
       01  WS-DEDUCT-SCHED-TBL REDEFINES WS-DEDUCT-SCHED-VALUES.
           05  WS-DEDUCT-SCHED-ROW OCCURS 11 TIMES.
               10  WS-DEDUCT-SCHED-LOW     PIC 9(09)V99.
               10  WS-DEDUCT-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FICA-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FICA-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000860500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000325000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000514500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000280000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000027000'.
       01  WS-FICA-GRID-TBL REDEFINES WS-FICA-GRID-VALUES.
           05  WS-FICA-GRID-ROW OCCURS 5 TIMES.
               10  WS-FICA-GRID-LOW        PIC 9(09)V99.
               10  WS-FICA-GRID-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PYRISKP.

       PROCEDURE DIVISION USING PYRISK-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-GROSS
           PERFORM 2010-ACCRUE-GROSS
           PERFORM 2020-SPLIT-DEDUCT
           PERFORM 2030-ACCUM-TAX-STATE
           PERFORM 2040-ACCUM-SOURCE
           PERFORM 2050-FLAG-ITEM
           PERFORM 2060-LIMIT-FICA
           PERFORM 2070-DESCRIBE-TAX-STATE
           PERFORM 2080-ACCRUE-FICA
           PERFORM 2090-PRORATE-GROSS
           PERFORM 2100-EDIT-REF-NO
           PERFORM 2110-LIMIT-DEDUCT
           PERFORM 2120-ACCRUE-STATE-TAX
           PERFORM 2130-ASSESS-STOP
           PERFORM 2140-ASSESS-NOTICE-SENT
           PERFORM 2150-PRORATE-GROSS
           PERFORM 2160-SELECT-MARGIN
           PERFORM 2170-ASSESS-NOTICE-SENT
           PERFORM 2180-ACCRUE-GROSS
           PERFORM 2190-SET-STATUS
           PERFORM 2200-DESCRIBE-REGION
           PERFORM 2210-ASSESS-NOTICE-SENT
           PERFORM 2220-AGE-EFFECTIVE
           PERFORM 2230-FLAG-STATE-TAX
           PERFORM 2240-EDIT-NAME
           PERFORM 2250-ASSESS-STOP
           PERFORM 2260-AGE-EFFECTIVE
           PERFORM 2270-LIMIT-FICA
           PERFORM 2280-LIMIT-FICA
           PERFORM 2290-DESCRIBE-REGION
           PERFORM 2300-SET-TAX-STATE
           PERFORM 2310-CHECK-EFFECTIVE
           PERFORM 2320-AGE-OPEN
           PERFORM 2330-ACCRUE-GROSS
           PERFORM 2340-LIMIT-DEDUCT
           PERFORM 2350-SPLIT-GROSS
           PERFORM 2360-ACCRUE-DEDUCT
           PERFORM 2370-SET-TAX-STATE
           PERFORM 2380-SET-REGION
           PERFORM 2390-SET-SOURCE
           PERFORM 2400-ASSESS-NOTICE-SENT
           PERFORM 2410-CHECK-NEXT-DUE
           PERFORM 2420-SELECT-RATE
           PERFORM 2430-DESCRIBE-TAX-STATE
           PERFORM 2440-SET-STATUS
           PERFORM 2450-PRORATE-GROSS
           PERFORM 2460-LIMIT-ITEM
           PERFORM 2470-ASSESS-AUTO-RENEW
           PERFORM 2480-EDIT-NAME
           PERFORM 2490-LIMIT-ITEM
           PERFORM 2500-SELECT-MARGIN
           PERFORM 2510-SPLIT-DEDUCT
           PERFORM 2520-DESCRIBE-STATUS
           PERFORM 2530-SELECT-MARGIN
           PERFORM 2540-SPLIT-FICA
           PERFORM 2550-CHECK-NEXT-REVIEW
           PERFORM 2560-PRORATE-DEDUCT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PYRISK-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-GROSS.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE PYRISK-GROSS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GROSS YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2010-ACCRUE-GROSS.
           IF PYRISK-GROSS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYRISK-GROSS-YTD * PYRISK-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2020-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PYRISK-DEDUCT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PYRISK-DEDUCT-YTD - (WS-WORK-AMT * 2)
           COMPUTE PYRISK-DEDUCT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS AMT BY TAX STATE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2030-ACCUM-TAX-STATE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYRISK-TAX-STATE-CD
                   ADD PYRISK-GROSS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYRISK-TAX-STATE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYRISK-GROSS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2040-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYRISK-SOURCE-CD
                   ADD PYRISK-STATE-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYRISK-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYRISK-STATE-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2050-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FICA AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2060-LIMIT-FICA.
           IF PYRISK-FICA-AMT < 25.00
               MOVE 25.00 TO PYRISK-FICA-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYRISK-FICA-AMT > 10000.00
               MOVE 10000.00 TO PYRISK-FICA-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TAX STATE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2070-DESCRIBE-TAX-STATE.
           EVALUATE PYRISK-TAX-STATE-CD
               WHEN 'CAZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LZU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TUF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WKE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FICA AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2080-ACCRUE-FICA.
           IF PYRISK-FICA-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYRISK-FICA-AMT * PYRISK-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PYRISK-DEDUCT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GROSS YTD.
      *----------------------------------------------------------------*
       2090-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PYRISK-GROSS-YTD ROUNDED =
                   PYRISK-DEDUCT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PYRISK-GROSS-YTD
           END-IF
           ADD PYRISK-GROSS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E798 REF NO REQUIRED
      * E688 DEDUCT YTD NEGATIVE
      * E431 DEDUCT YTD OVER 10000
      * E150 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYRISK-REF-NO = SPACES
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN PYRISK-DEDUCT-YTD < 0
                   MOVE 'E688' TO WS-REJECT-CODE
               WHEN PYRISK-DEDUCT-YTD > 10000.00
                   MOVE 'E431' TO WS-REJECT-CODE
               WHEN PYRISK-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E150' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2110-LIMIT-DEDUCT.
           IF PYRISK-DEDUCT-YTD < 10.00
               MOVE 10.00 TO PYRISK-DEDUCT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYRISK-DEDUCT-YTD > 500.00
               MOVE 500.00 TO PYRISK-DEDUCT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2120-ACCRUE-STATE-TAX.
           IF PYRISK-STATE-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYRISK-STATE-TAX-YTD * PYRISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PYRISK-FICA-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE STATE TAX YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2130-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PYRISK-STOP-FLAG = 'Y'
                   MOVE ZERO TO PYRISK-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYRISK-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PYRISK-DEDUCT-AMT ROUNDED =
                       PYRISK-STATE-TAX-YTD * PYRISK-RATE / 200
               WHEN PYRISK-SOURCE-CD = 'NY'
                   MOVE 10.00 TO PYRISK-DEDUCT-AMT
               WHEN PYRISK-SOURCE-CD = 'PL'
                   MOVE 12.00 TO PYRISK-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PYRISK-DEDUCT-AMT ROUNDED =
                       PYRISK-STATE-TAX-YTD * PYRISK-RATE / 100
           END-EVALUATE
           IF PYRISK-DEDUCT-AMT > 0 AND PYRISK-DEDUCT-AMT < 1.00
               MOVE 1.00 TO PYRISK-DEDUCT-AMT
           END-IF
           IF PYRISK-DEDUCT-AMT > 250.00
               MOVE 250.00 TO PYRISK-DEDUCT-AMT
           END-IF
           ADD PYRISK-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FICA FEE ON THE DEDUCT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2140-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PYRISK-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PYRISK-FICA-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYRISK-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PYRISK-FICA-AMT ROUNDED = PYRISK-DEDUCT-AMT *
                       PYRISK-RATE / 200
               WHEN PYRISK-REGION-CD = 'DP'
                   MOVE 20.00 TO PYRISK-FICA-AMT
               WHEN PYRISK-REGION-CD = 'TU'
                   MOVE 7.00 TO PYRISK-FICA-AMT
               WHEN OTHER
                   COMPUTE PYRISK-FICA-AMT ROUNDED = PYRISK-DEDUCT-AMT *
                       PYRISK-RATE / 100
           END-EVALUATE
           IF PYRISK-FICA-AMT > 0 AND PYRISK-FICA-AMT < 10.00
               MOVE 10.00 TO PYRISK-FICA-AMT
           END-IF
           IF PYRISK-FICA-AMT > 75.00
               MOVE 75.00 TO PYRISK-FICA-AMT
           END-IF
           ADD PYRISK-FICA-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE GROSS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2150-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PYRISK-GROSS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FICA GRID TIER FOR THE GROSS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2160-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-GRID-RATE(1) TO PYRISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PYRISK-GROSS-YTD NOT < WS-FICA-GRID-LOW(WS-TX)
                   MOVE WS-FICA-GRID-RATE(WS-TX) TO PYRISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYRISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYRISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FICA FEE ON THE DEDUCT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2170-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PYRISK-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PYRISK-FICA-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYRISK-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PYRISK-FICA-AMT ROUNDED = PYRISK-DEDUCT-AMT *
                       PYRISK-RATE / 200
               WHEN PYRISK-REGION-CD = 'DP'
                   MOVE 7.00 TO PYRISK-FICA-AMT
               WHEN PYRISK-REGION-CD = 'TU'
                   MOVE 3.00 TO PYRISK-FICA-AMT
               WHEN OTHER
                   COMPUTE PYRISK-FICA-AMT ROUNDED = PYRISK-DEDUCT-AMT *
                       PYRISK-RATE / 100
           END-EVALUATE
           IF PYRISK-FICA-AMT > 0 AND PYRISK-FICA-AMT < 5.00
               MOVE 5.00 TO PYRISK-FICA-AMT
           END-IF
           IF PYRISK-FICA-AMT > 100.00
               MOVE 100.00 TO PYRISK-FICA-AMT
           END-IF
           ADD PYRISK-FICA-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GROSS YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2180-ACCRUE-GROSS.
           IF PYRISK-GROSS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYRISK-GROSS-YTD * PYRISK-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN NEXT DUE DATE IS PAST
      * RD -> ST WHEN NEXT DUE DATE IS PAST
      * ST -> TJ WHEN NEXT DUE DATE IS PAST
      * TJ -> UC WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2190-SET-STATUS.
           EVALUATE PYRISK-STATUS-CD
               WHEN 'MV'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RD' TO PYRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ST' TO PYRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PYRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UC' TO PYRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2200-DESCRIBE-REGION.
           EVALUATE PYRISK-REGION-CD
               WHEN 'DP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE GROSS YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2210-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PYRISK-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PYRISK-GROSS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYRISK-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PYRISK-GROSS-YTD ROUNDED = WS-WORK-AMT *
                       PYRISK-MARGIN / 200
               WHEN PYRISK-SOURCE-CD = 'NY'
                   MOVE 3.00 TO PYRISK-GROSS-YTD
               WHEN PYRISK-SOURCE-CD = 'PL'
                   MOVE 20.00 TO PYRISK-GROSS-YTD
               WHEN OTHER
                   COMPUTE PYRISK-GROSS-YTD ROUNDED = WS-WORK-AMT *
                       PYRISK-MARGIN / 100
           END-EVALUATE
           IF PYRISK-GROSS-YTD > 0 AND PYRISK-GROSS-YTD < 5.00
               MOVE 5.00 TO PYRISK-GROSS-YTD
           END-IF
           IF PYRISK-GROSS-YTD > 100.00
               MOVE 100.00 TO PYRISK-GROSS-YTD
           END-IF
           ADD PYRISK-GROSS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2220-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PYRISK-EFFECTIVE-DATE > 0 AND PYRISK-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYRISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYRISK-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PYRISK-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PYRISK-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PYRISK-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYRISK-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATE TAX YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2230-FLAG-STATE-TAX.
           IF PYRISK-STATE-TAX-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PYRISK-STATE-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E176 NAME REQUIRED
      * E938 WORK AMT NEGATIVE
      * E273 WORK AMT OVER 1000000
      * E686 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2240-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYRISK-NAME = SPACES
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E273' TO WS-REJECT-CODE
               WHEN PYRISK-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2250-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PYRISK-STOP-FLAG = 'Y'
                   MOVE ZERO TO PYRISK-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYRISK-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PYRISK-DEDUCT-AMT ROUNDED = WS-WORK-AMT *
                       PYRISK-MARGIN / 200
               WHEN PYRISK-SOURCE-CD = 'NY'
                   MOVE 5.00 TO PYRISK-DEDUCT-AMT
               WHEN PYRISK-SOURCE-CD = 'PL'
                   MOVE 12.00 TO PYRISK-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PYRISK-DEDUCT-AMT ROUNDED = WS-WORK-AMT *
                       PYRISK-MARGIN / 100
           END-EVALUATE
           IF PYRISK-DEDUCT-AMT > 0 AND PYRISK-DEDUCT-AMT < 1.00
               MOVE 1.00 TO PYRISK-DEDUCT-AMT
           END-IF
           IF PYRISK-DEDUCT-AMT > 35.00
               MOVE 35.00 TO PYRISK-DEDUCT-AMT
           END-IF
           ADD PYRISK-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PYRISK-EFFECTIVE-DATE > 0 AND PYRISK-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYRISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYRISK-GROSS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PYRISK-GROSS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PYRISK-GROSS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PYRISK-GROSS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYRISK-GROSS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FICA AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2270-LIMIT-FICA.
           IF PYRISK-FICA-AMT < 50.00
               MOVE 50.00 TO PYRISK-FICA-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYRISK-FICA-AMT > 500.00
               MOVE 500.00 TO PYRISK-FICA-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FICA AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2280-LIMIT-FICA.
           IF PYRISK-FICA-AMT < 100.00
               MOVE 100.00 TO PYRISK-FICA-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYRISK-FICA-AMT > 50000.00
               MOVE 50000.00 TO PYRISK-FICA-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2290-DESCRIBE-REGION.
           EVALUATE PYRISK-REGION-CD
               WHEN 'DP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN NEXT DUE DATE IS PAST
      * LZU -> TUF WHEN NEXT DUE DATE IS PAST
      * TUF -> WKE WHEN NEXT DUE DATE IS PAST
      * WKE -> CAZ WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2300-SET-TAX-STATE.
           EVALUATE PYRISK-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PYRISK-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PYRISK-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PYRISK-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PYRISK-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PYRISK-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYRISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PYRISK-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2320-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PYRISK-OPEN-DATE > 0 AND PYRISK-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYRISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYRISK-DEDUCT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PYRISK-DEDUCT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PYRISK-DEDUCT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PYRISK-DEDUCT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYRISK-DEDUCT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GROSS AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2330-ACCRUE-GROSS.
           IF PYRISK-GROSS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYRISK-GROSS-AMT * PYRISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2340-LIMIT-DEDUCT.
           IF PYRISK-DEDUCT-YTD < 50.00
               MOVE 50.00 TO PYRISK-DEDUCT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYRISK-DEDUCT-YTD > 500.00
               MOVE 500.00 TO PYRISK-DEDUCT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GROSS YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2350-SPLIT-GROSS.
           COMPUTE WS-WORK-AMT = PYRISK-GROSS-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PYRISK-GROSS-YTD - (WS-WORK-AMT * 12)
           COMPUTE PYRISK-FICA-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2360-ACCRUE-DEDUCT.
           IF PYRISK-DEDUCT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYRISK-DEDUCT-YTD * PYRISK-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN OPEN DATE IS PAST
      * LZU -> TUF WHEN OPEN DATE IS PAST
      * TUF -> WKE WHEN OPEN DATE IS PAST
      * WKE -> CAZ WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2370-SET-TAX-STATE.
           EVALUATE PYRISK-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PYRISK-OPEN-DATE > 0 AND PYRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PYRISK-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PYRISK-OPEN-DATE > 0 AND PYRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PYRISK-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PYRISK-OPEN-DATE > 0 AND PYRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PYRISK-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PYRISK-OPEN-DATE > 0 AND PYRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PYRISK-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DP -> TU WHEN NEXT DUE DATE IS PAST
      * TU -> WD WHEN NEXT DUE DATE IS PAST
      * WD -> DP WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2380-SET-REGION.
           EVALUATE PYRISK-REGION-CD
               WHEN 'DP'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TU' TO PYRISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TU'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WD' TO PYRISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DP' TO PYRISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN NEXT DUE DATE IS PAST
      * PL -> VK WHEN NEXT DUE DATE IS PAST
      * VK -> WB WHEN NEXT DUE DATE IS PAST
      * WB -> XL WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2390-SET-SOURCE.
           EVALUATE PYRISK-SOURCE-CD
               WHEN 'NY'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PL' TO PYRISK-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VK' TO PYRISK-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WB' TO PYRISK-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XL' TO PYRISK-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FICA AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2400-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PYRISK-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYRISK-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PYRISK-FICA-AMT *
                       PYRISK-MARGIN / 200
               WHEN PYRISK-STATUS-CD = 'MV'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN PYRISK-STATUS-CD = 'RD'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PYRISK-FICA-AMT *
                       PYRISK-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2410-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PYRISK-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PYRISK-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-SCHED-RATE(1) TO PYRISK-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-DEDUCT-SCHED-LOW(WS-TX)
                   MOVE WS-DEDUCT-SCHED-RATE(WS-TX) TO PYRISK-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYRISK-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYRISK-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TAX STATE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2430-DESCRIBE-TAX-STATE.
           EVALUATE PYRISK-TAX-STATE-CD
               WHEN 'CAZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LZU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TUF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WKE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN NEXT DUE DATE IS PAST
      * RD -> ST WHEN NEXT DUE DATE IS PAST
      * ST -> TJ WHEN NEXT DUE DATE IS PAST
      * TJ -> UC WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2440-SET-STATUS.
           EVALUATE PYRISK-STATUS-CD
               WHEN 'MV'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RD' TO PYRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ST' TO PYRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PYRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PYRISK-NEXT-DUE-DATE > 0 AND PYRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UC' TO PYRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE GROSS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2450-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PYRISK-GROSS-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2460-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE GROSS AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2470-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PYRISK-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYRISK-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PYRISK-GROSS-AMT *
                       PYRISK-MARGIN / 200
               WHEN PYRISK-SOURCE-CD = 'NY'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN PYRISK-SOURCE-CD = 'PL'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PYRISK-GROSS-AMT *
                       PYRISK-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E414 NAME REQUIRED
      * E357 FICA AMT NEGATIVE
      * E534 FICA AMT OVER 50000
      * E030 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2480-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYRISK-NAME = SPACES
                   MOVE 'E414' TO WS-REJECT-CODE
               WHEN PYRISK-FICA-AMT < 0
                   MOVE 'E357' TO WS-REJECT-CODE
               WHEN PYRISK-FICA-AMT > 50000.00
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN PYRISK-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E030' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2490-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT SCHED TIER FOR THE DEDUCT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2500-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-SCHED-RATE(1) TO PYRISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PYRISK-DEDUCT-AMT NOT < WS-DEDUCT-SCHED-LOW(WS-TX)
                   MOVE WS-DEDUCT-SCHED-RATE(WS-TX) TO PYRISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYRISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYRISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PYRISK-DEDUCT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PYRISK-DEDUCT-YTD - (WS-WORK-AMT * 4)
           COMPUTE PYRISK-GROSS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2520-DESCRIBE-STATUS.
           EVALUATE PYRISK-STATUS-CD
               WHEN 'MV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ST'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-SCHED-RATE(1) TO PYRISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-DEDUCT-SCHED-LOW(WS-TX)
                   MOVE WS-DEDUCT-SCHED-RATE(WS-TX) TO PYRISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYRISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYRISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2540-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE PYRISK-FICA-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2550-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PYRISK-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYRISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PYRISK-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GROSS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEDUCT YTD.
      *----------------------------------------------------------------*
       2560-PRORATE-DEDUCT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PYRISK-DEDUCT-YTD ROUNDED =
                   PYRISK-GROSS-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PYRISK-DEDUCT-YTD
           END-IF
           ADD PYRISK-DEDUCT-YTD TO WS-TOTAL-PRORATED.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PYRISK-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PYRISK-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PYRISK-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PYRISK EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
