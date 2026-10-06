      *================================================================*
      * PROGRAM  : IRPRC                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : IRA CUSTODY RULE - FEES. CALLED BY THE IRA CUSTODY  *
      *            BATCH AND ONLINE PROGRAMS.                          *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-27 DPS  ORIGINAL                                       *
      * 2002-01-19 EWS  AUDIT FINDING - DISTRIB CONTROL TOTALS CHG1473 *
      * 2003-08-11 DPS  RMD STATUS CODES ADDED REQ8818                 *
      * 2005-10-15 SKN  DISTRIB EXCEPTIONS TO OPS QUEUE CHG9549        *
      * 2008-02-04 EWS  CONTRIB FEE SCHEDULE UPDATE PRB5395            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRPRC.
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
                                           VALUE 20.000000.
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
      * WS-RMD-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RMD-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000543500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000763000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000101000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000601000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010200000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010450000000733500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010950000000098000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013450000000372500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018450000000865000'.
       01  WS-RMD-SCHED-TBL REDEFINES WS-RMD-SCHED-VALUES.
           05  WS-RMD-SCHED-ROW OCCURS 9 TIMES.
               10  WS-RMD-SCHED-LOW        PIC 9(09)V99.
               10  WS-RMD-SCHED-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WITHHOLD-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WITHHOLD-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000370500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000195000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000705000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000165500'.
       01  WS-WITHHOLD-GRID-TBL REDEFINES WS-WITHHOLD-GRID-VALUES.
           05  WS-WITHHOLD-GRID-ROW OCCURS 4 TIMES.
               10  WS-WITHHOLD-GRID-LOW    PIC 9(09)V99.
               10  WS-WITHHOLD-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RMD-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RMD-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000274000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000298500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000615500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000728500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000737000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000209000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015150000000275000'.
       01  WS-RMD-BAND-TBL REDEFINES WS-RMD-BAND-VALUES.
           05  WS-RMD-BAND-ROW OCCURS 7 TIMES.
               10  WS-RMD-BAND-LOW         PIC 9(09)V99.
               10  WS-RMD-BAND-RATE        PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY IRPRCP.

       PROCEDURE DIVISION USING IRPRC-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-LIMIT-ROLLOVER
           PERFORM 2010-DESCRIBE-IRA-TYPE
           PERFORM 2020-FLAG-ITEM
           PERFORM 2030-AGE-LAST-ACT
           PERFORM 2040-SELECT-FEE-PCT
           PERFORM 2050-SPLIT-DISTRIB
           PERFORM 2060-FLAG-ROLLOVER
           PERFORM 2070-ACCUM-IRA-TYPE
           PERFORM 2080-FLAG-ROLLOVER
           PERFORM 2090-ASSESS-AUTO-RENEW
           PERFORM 2100-ACCRUE-DISTRIB
           PERFORM 2110-AGE-LAST-STMT
           PERFORM 2120-ASSESS-AUTO-RENEW
           PERFORM 2130-PRORATE-EXCESS
           PERFORM 2140-SPLIT-RMD
           PERFORM 2150-LIMIT-RMD
           PERFORM 2160-EDIT-ADDR
           PERFORM 2170-SELECT-FEE-PCT
           PERFORM 2180-DESCRIBE-TYPE
           PERFORM 2190-ASSESS-SUSPEND
           PERFORM 2200-SPLIT-CONTRIB
           PERFORM 2210-PRORATE-RMD
           PERFORM 2220-SPLIT-ROLLOVER
           PERFORM 2230-CHECK-NEXT-REVIEW
           PERFORM 2240-LIMIT-CONTRIB
           PERFORM 2250-CHECK-NEXT-REVIEW
           PERFORM 2260-DESCRIBE-CHANNEL
           PERFORM 2270-SET-TYPE
           PERFORM 2280-ACCRUE-EXCESS
           PERFORM 2290-ACCUM-IRA-TYPE
           PERFORM 2300-ASSESS-SUSPEND
           PERFORM 2310-AGE-NEXT-REVIEW
           PERFORM 2320-LIMIT-CONTRIB
           PERFORM 2330-ACCUM-REASON
           PERFORM 2340-SELECT-FEE-PCT
           PERFORM 2350-SPLIT-EXCESS
           PERFORM 2360-SPLIT-ITEM
           PERFORM 2370-DESCRIBE-REASON
           PERFORM 2380-ACCRUE-RMD
           PERFORM 2390-ACCUM-IRA-TYPE
           PERFORM 2400-ACCRUE-ROLLOVER
           PERFORM 2410-AGE-EXPIRY
           PERFORM 2420-COUNT-RETRY
           PERFORM 2430-ACCUM-IRA-TYPE
           PERFORM 2440-SET-CHANNEL
           PERFORM 2450-ASSESS-AUTO-RENEW
           PERFORM 2460-LIMIT-RMD
           PERFORM 2470-ACCRUE-ROLLOVER
           PERFORM 2480-DESCRIBE-IRA-TYPE
           PERFORM 2490-AGE-NEXT-REVIEW
           PERFORM 2500-SPLIT-ROLLOVER
           PERFORM 2510-PRORATE-RMD
           PERFORM 2520-SPLIT-ROLLOVER
           PERFORM 2530-SELECT-FEE-PCT
           PERFORM 2540-ACCUM-TYPE
           PERFORM 2550-DESCRIBE-TYPE
           PERFORM 2560-ACCUM-IRA-TYPE
           PERFORM 2570-PRORATE-EXCESS
           PERFORM 2580-SELECT-FEE-PCT
           PERFORM 2590-AGE-LAST-STMT
           PERFORM 2600-CHECK-NEXT-REVIEW
           PERFORM 2610-FLAG-EXCESS
           PERFORM 2620-CHECK-LAST-ACT
           PERFORM 2630-PRORATE-DISTRIB
           PERFORM 2640-AGE-NEXT-REVIEW
           PERFORM 2650-ACCUM-TYPE
           PERFORM 2660-SPLIT-FMV
           PERFORM 2670-SELECT-FEE-PCT
           PERFORM 2680-AGE-NEXT-REVIEW
           PERFORM 2690-PRORATE-EXCESS
           PERFORM 2700-SELECT-FEE-PCT
           PERFORM 2710-SPLIT-RMD
           PERFORM 2720-PRORATE-RMD
           PERFORM 2730-SET-CHANNEL
           PERFORM 2740-EDIT-ADDR
           PERFORM 2750-CHECK-EXPIRY
           PERFORM 2760-ACCUM-REASON
           PERFORM 2770-CHECK-EXPIRY
           PERFORM 2780-PRORATE-RMD
           PERFORM 2790-COUNT-PAYMENT
           PERFORM 2800-EDIT-NAME
           PERFORM 2810-ACCRUE-CONTRIB
           PERFORM 2820-ACCUM-CHANNEL
           PERFORM 2830-EDIT-ADDR
           PERFORM 2840-ACCUM-IRA-TYPE
           PERFORM 2850-COUNT-PAYMENT
           PERFORM 2860-SPLIT-FMV
           PERFORM 2870-FLAG-DISTRIB
           PERFORM 2880-ACCUM-REASON
           PERFORM 2890-SET-IRA-TYPE
           PERFORM 2900-CHECK-LAST-ACT
           PERFORM 2910-ACCRUE-EXCESS
           PERFORM 2920-SPLIT-ITEM
           PERFORM 2930-PRORATE-DISTRIB
           PERFORM 2940-PRORATE-ITEM
           PERFORM 2950-EDIT-NAME
           PERFORM 2960-ACCRUE-ROLLOVER
           PERFORM 2970-AGE-EXPIRY
           PERFORM 2980-FLAG-EXCESS
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO IRPRC-RETURN-CODE.

      *----------------------------------------------------------------*
      * ROLLOVER AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2000-LIMIT-ROLLOVER.
           IF IRPRC-ROLLOVER-AMT < 25.00
               MOVE 25.00 TO IRPRC-ROLLOVER-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRPRC-ROLLOVER-AMT > 2500.00
               MOVE 2500.00 TO IRPRC-ROLLOVER-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE IRA TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2010-DESCRIBE-IRA-TYPE.
           EVALUATE IRPRC-IRA-TYPE-CD
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2020-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-LAST-ACT-DATE > 0 AND IRPRC-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRPRC-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRPRC-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRPRC-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRPRC-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRPRC-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RMD SCHED TIER FOR THE EXCESS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RMD-SCHED-RATE(1) TO IRPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF IRPRC-EXCESS-YTD NOT < WS-RMD-SCHED-LOW(WS-TX)
                   MOVE WS-RMD-SCHED-RATE(WS-TX) TO IRPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2050-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = IRPRC-DISTRIB-YTD / 4
           COMPUTE WS-WORK-AMT-2 = IRPRC-DISTRIB-YTD - (WS-WORK-AMT * 4)
           COMPUTE IRPRC-FMV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2060-FLAG-ROLLOVER.
           IF IRPRC-ROLLOVER-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRPRC-ROLLOVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB YTD BY IRA TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2070-ACCUM-IRA-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-IRA-TYPE-CD
                   ADD IRPRC-CONTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-IRA-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRPRC-CONTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2080-FLAG-ROLLOVER.
           IF IRPRC-ROLLOVER-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRPRC-ROLLOVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE RMD YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2090-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN IRPRC-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRPRC-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = IRPRC-RMD-YTD *
                       IRPRC-MARGIN / 200
               WHEN IRPRC-REASON-CD = 'CMR'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN IRPRC-REASON-CD = 'LJS'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = IRPRC-RMD-YTD *
                       IRPRC-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2100-ACCRUE-DISTRIB.
           IF IRPRC-DISTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRPRC-DISTRIB-YTD * IRPRC-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO IRPRC-EXCESS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRPRC-EXCESS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD IRPRC-EXCESS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRPRC-EXCESS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD IRPRC-EXCESS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRPRC-EXCESS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE DISTRIB YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2120-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN IRPRC-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO IRPRC-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRPRC-OVERRIDE-FLAG = 'Y'
                   COMPUTE IRPRC-CONTRIB-YTD ROUNDED = IRPRC-DISTRIB-YTD
                       * IRPRC-FEE-PCT / 200
               WHEN IRPRC-TYPE-CD = 'EA'
                   MOVE 12.00 TO IRPRC-CONTRIB-YTD
               WHEN IRPRC-TYPE-CD = 'HR'
                   MOVE 20.00 TO IRPRC-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE IRPRC-CONTRIB-YTD ROUNDED = IRPRC-DISTRIB-YTD
                       * IRPRC-FEE-PCT / 100
           END-EVALUATE
           IF IRPRC-CONTRIB-YTD > 0 AND IRPRC-CONTRIB-YTD < 10.00
               MOVE 10.00 TO IRPRC-CONTRIB-YTD
           END-IF
           IF IRPRC-CONTRIB-YTD > 100.00
               MOVE 100.00 TO IRPRC-CONTRIB-YTD
           END-IF
           ADD IRPRC-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCESS YTD.
      *----------------------------------------------------------------*
       2130-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRPRC-EXCESS-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRPRC-EXCESS-YTD
           END-IF
           ADD IRPRC-EXCESS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT RMD YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2140-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = IRPRC-RMD-YTD / 12
           COMPUTE WS-WORK-AMT-2 = IRPRC-RMD-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RMD YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2150-LIMIT-RMD.
           IF IRPRC-RMD-YTD < 10.00
               MOVE 10.00 TO IRPRC-RMD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRPRC-RMD-YTD > 2500.00
               MOVE 2500.00 TO IRPRC-RMD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E759 ADDR REQUIRED
      * E519 FMV AMT NEGATIVE
      * E581 FMV AMT OVER 50000
      * E060 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2160-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRPRC-ADDR = SPACES
                   MOVE 'E759' TO WS-REJECT-CODE
               WHEN IRPRC-FMV-AMT < 0
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN IRPRC-FMV-AMT > 50000.00
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN IRPRC-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RMD BAND TIER FOR THE EXCESS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2170-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RMD-BAND-RATE(1) TO IRPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRPRC-EXCESS-YTD NOT < WS-RMD-BAND-LOW(WS-TX)
                   MOVE WS-RMD-BAND-RATE(WS-TX) TO IRPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2180-DESCRIBE-TYPE.
           EVALUATE IRPRC-TYPE-CD
               WHEN 'EA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE DISTRIB YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2190-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN IRPRC-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO IRPRC-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRPRC-OVERRIDE-FLAG = 'Y'
                   COMPUTE IRPRC-CONTRIB-YTD ROUNDED = IRPRC-DISTRIB-YTD
                       * IRPRC-MARGIN / 200
               WHEN IRPRC-CHANNEL-CD = 'AS'
                   MOVE 5.00 TO IRPRC-CONTRIB-YTD
               WHEN IRPRC-CHANNEL-CD = 'AY'
                   MOVE 15.00 TO IRPRC-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE IRPRC-CONTRIB-YTD ROUNDED = IRPRC-DISTRIB-YTD
                       * IRPRC-MARGIN / 100
           END-EVALUATE
           IF IRPRC-CONTRIB-YTD > 0 AND IRPRC-CONTRIB-YTD < 1.00
               MOVE 1.00 TO IRPRC-CONTRIB-YTD
           END-IF
           IF IRPRC-CONTRIB-YTD > 50.00
               MOVE 50.00 TO IRPRC-CONTRIB-YTD
           END-IF
           ADD IRPRC-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2200-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE IRPRC-CONTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCESS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD YTD.
      *----------------------------------------------------------------*
       2210-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRPRC-RMD-YTD ROUNDED =
                   IRPRC-EXCESS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRPRC-RMD-YTD
           END-IF
           ADD IRPRC-RMD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2220-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRPRC-ROLLOVER-AMT / 12
           COMPUTE WS-WORK-AMT-2 = IRPRC-ROLLOVER-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2230-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO IRPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2240-LIMIT-CONTRIB.
           IF IRPRC-CONTRIB-YTD < 25.00
               MOVE 25.00 TO IRPRC-CONTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRPRC-CONTRIB-YTD > 2500.00
               MOVE 2500.00 TO IRPRC-CONTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2250-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO IRPRC-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2260-DESCRIBE-CHANNEL.
           EVALUATE IRPRC-CHANNEL-CD
               WHEN 'AS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'AY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN LAST STMT DATE IS PAST
      * HR -> XZ WHEN LAST STMT DATE IS PAST
      * XZ -> EA WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2270-SET-TYPE.
           EVALUATE IRPRC-TYPE-CD
               WHEN 'EA'
                   IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRPRC-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRPRC-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRPRC-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCESS YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-EXCESS.
           IF IRPRC-EXCESS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRPRC-EXCESS-YTD * IRPRC-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO IRPRC-FMV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER AMT BY IRA TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-IRA-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-IRA-TYPE-CD
                   ADD IRPRC-ROLLOVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-IRA-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRPRC-ROLLOVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER FEE ON THE EXCESS YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2300-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN IRPRC-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO IRPRC-ROLLOVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRPRC-OVERRIDE-FLAG = 'Y'
                   COMPUTE IRPRC-ROLLOVER-AMT ROUNDED = IRPRC-EXCESS-YTD
                       * IRPRC-FEE-PCT / 200
               WHEN IRPRC-CHANNEL-CD = 'AS'
                   MOVE 15.00 TO IRPRC-ROLLOVER-AMT
               WHEN IRPRC-CHANNEL-CD = 'AY'
                   MOVE 12.00 TO IRPRC-ROLLOVER-AMT
               WHEN OTHER
                   COMPUTE IRPRC-ROLLOVER-AMT ROUNDED = IRPRC-EXCESS-YTD
                       * IRPRC-FEE-PCT / 100
           END-EVALUATE
           IF IRPRC-ROLLOVER-AMT > 0 AND IRPRC-ROLLOVER-AMT < 1.00
               MOVE 1.00 TO IRPRC-ROLLOVER-AMT
           END-IF
           IF IRPRC-ROLLOVER-AMT > 250.00
               MOVE 250.00 TO IRPRC-ROLLOVER-AMT
           END-IF
           ADD IRPRC-ROLLOVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2310-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-NEXT-REVIEW-DATE > 0 AND IRPRC-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRPRC-EXCESS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRPRC-EXCESS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRPRC-EXCESS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRPRC-EXCESS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRPRC-EXCESS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTRIB YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2320-LIMIT-CONTRIB.
           IF IRPRC-CONTRIB-YTD < 0.00
               MOVE 0.00 TO IRPRC-CONTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRPRC-CONTRIB-YTD > 50000.00
               MOVE 50000.00 TO IRPRC-CONTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2330-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD GRID TIER FOR THE ROLLOVER AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2340-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-GRID-RATE(1) TO IRPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRPRC-ROLLOVER-AMT NOT < WS-WITHHOLD-GRID-LOW(WS-TX)
                   MOVE WS-WITHHOLD-GRID-RATE(WS-TX) TO IRPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCESS YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2350-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRPRC-EXCESS-YTD / 12
           COMPUTE WS-WORK-AMT-2 = IRPRC-EXCESS-YTD - (WS-WORK-AMT * 12)
           COMPUTE IRPRC-CONTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2360-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-REASON.
           EVALUATE IRPRC-REASON-CD
               WHEN 'CMR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LJS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NRT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UGA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WBS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZYU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-RMD.
           IF IRPRC-RMD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRPRC-RMD-YTD * IRPRC-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO IRPRC-DISTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB YTD BY IRA TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2390-ACCUM-IRA-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-IRA-TYPE-CD
                   ADD IRPRC-CONTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-IRA-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRPRC-CONTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROLLOVER AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2400-ACCRUE-ROLLOVER.
           IF IRPRC-ROLLOVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRPRC-ROLLOVER-AMT * IRPRC-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO IRPRC-RMD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2410-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-EXPIRY-DATE > 0 AND IRPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRPRC-ROLLOVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRPRC-ROLLOVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRPRC-ROLLOVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRPRC-ROLLOVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRPRC-ROLLOVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2420-COUNT-RETRY.
           IF IRPRC-RETRY-CNT < 9999999
               ADD 1 TO IRPRC-RETRY-CNT
           END-IF
           IF IRPRC-RETRY-CNT NOT < 5
               MOVE 'Y' TO IRPRC-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB YTD BY IRA TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2430-ACCUM-IRA-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-IRA-TYPE-CD
                   ADD IRPRC-CONTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-IRA-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRPRC-CONTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AS -> AY WHEN LAST STMT DATE IS PAST
      * AY -> JR WHEN LAST STMT DATE IS PAST
      * JR -> TA WHEN LAST STMT DATE IS PAST
      * TA -> UM WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2440-SET-CHANNEL.
           EVALUATE IRPRC-CHANNEL-CD
               WHEN 'AS'
                   IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AY' TO IRPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AY'
                   IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JR' TO IRPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JR'
                   IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TA' TO IRPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UM' TO IRPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2450-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN IRPRC-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO IRPRC-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRPRC-OVERRIDE-FLAG = 'Y'
                   COMPUTE IRPRC-CONTRIB-YTD ROUNDED =
                       IRPRC-ROLLOVER-AMT * IRPRC-FEE-PCT / 200
               WHEN IRPRC-IRA-TYPE-CD = 'F'
                   MOVE 10.00 TO IRPRC-CONTRIB-YTD
               WHEN IRPRC-IRA-TYPE-CD = 'V'
                   MOVE 10.00 TO IRPRC-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE IRPRC-CONTRIB-YTD ROUNDED =
                       IRPRC-ROLLOVER-AMT * IRPRC-FEE-PCT / 100
           END-EVALUATE
           IF IRPRC-CONTRIB-YTD > 0 AND IRPRC-CONTRIB-YTD < 2.00
               MOVE 2.00 TO IRPRC-CONTRIB-YTD
           END-IF
           IF IRPRC-CONTRIB-YTD > 35.00
               MOVE 35.00 TO IRPRC-CONTRIB-YTD
           END-IF
           ADD IRPRC-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RMD YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2460-LIMIT-RMD.
           IF IRPRC-RMD-YTD < 10.00
               MOVE 10.00 TO IRPRC-RMD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRPRC-RMD-YTD > 50000.00
               MOVE 50000.00 TO IRPRC-RMD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROLLOVER AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2470-ACCRUE-ROLLOVER.
           IF IRPRC-ROLLOVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRPRC-ROLLOVER-AMT * IRPRC-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE IRA TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-IRA-TYPE.
           EVALUATE IRPRC-IRA-TYPE-CD
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'V'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'W'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2490-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-NEXT-REVIEW-DATE > 0 AND IRPRC-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRPRC-CONTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRPRC-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRPRC-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD IRPRC-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRPRC-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2500-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRPRC-ROLLOVER-AMT / 12
           COMPUTE WS-WORK-AMT-2 = IRPRC-ROLLOVER-AMT - (WS-WORK-AMT *
               12)
           COMPUTE IRPRC-RMD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD YTD.
      *----------------------------------------------------------------*
       2510-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRPRC-RMD-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRPRC-RMD-YTD
           END-IF
           ADD IRPRC-RMD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2520-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRPRC-ROLLOVER-AMT / 12
           COMPUTE WS-WORK-AMT-2 = IRPRC-ROLLOVER-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RMD SCHED TIER FOR THE EXCESS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RMD-SCHED-RATE(1) TO IRPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF IRPRC-EXCESS-YTD NOT < WS-RMD-SCHED-LOW(WS-TX)
                   MOVE WS-RMD-SCHED-RATE(WS-TX) TO IRPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-TYPE-CD
                   ADD IRPRC-EXCESS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRPRC-EXCESS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2550-DESCRIBE-TYPE.
           EVALUATE IRPRC-TYPE-CD
               WHEN 'EA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER AMT BY IRA TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-IRA-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-IRA-TYPE-CD
                   ADD IRPRC-ROLLOVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-IRA-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRPRC-ROLLOVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCESS YTD.
      *----------------------------------------------------------------*
       2570-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRPRC-EXCESS-YTD ROUNDED =
                   IRPRC-ROLLOVER-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRPRC-EXCESS-YTD
           END-IF
           ADD IRPRC-EXCESS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE RMD SCHED TIER FOR THE ROLLOVER AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2580-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RMD-SCHED-RATE(1) TO IRPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF IRPRC-ROLLOVER-AMT NOT < WS-RMD-SCHED-LOW(WS-TX)
                   MOVE WS-RMD-SCHED-RATE(WS-TX) TO IRPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2590-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRPRC-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRPRC-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRPRC-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRPRC-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRPRC-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2600-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO IRPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCESS YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2610-FLAG-EXCESS.
           IF IRPRC-EXCESS-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRPRC-EXCESS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2620-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO IRPRC-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       2630-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRPRC-DISTRIB-YTD ROUNDED =
                   IRPRC-RMD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRPRC-DISTRIB-YTD
           END-IF
           ADD IRPRC-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2640-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-NEXT-REVIEW-DATE > 0 AND IRPRC-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRPRC-CONTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRPRC-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRPRC-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD IRPRC-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRPRC-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2650-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FMV AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2660-SPLIT-FMV.
           COMPUTE WS-WORK-AMT = IRPRC-FMV-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRPRC-FMV-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRPRC-CONTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD GRID TIER FOR THE FMV AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2670-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-GRID-RATE(1) TO IRPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRPRC-FMV-AMT NOT < WS-WITHHOLD-GRID-LOW(WS-TX)
                   MOVE WS-WITHHOLD-GRID-RATE(WS-TX) TO IRPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2680-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-NEXT-REVIEW-DATE > 0 AND IRPRC-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRPRC-FMV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRPRC-FMV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRPRC-FMV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRPRC-FMV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRPRC-FMV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FMV AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCESS YTD.
      *----------------------------------------------------------------*
       2690-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRPRC-EXCESS-YTD ROUNDED =
                   IRPRC-FMV-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRPRC-EXCESS-YTD
           END-IF
           ADD IRPRC-EXCESS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE RMD SCHED TIER FOR THE ROLLOVER AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2700-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RMD-SCHED-RATE(1) TO IRPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF IRPRC-ROLLOVER-AMT NOT < WS-RMD-SCHED-LOW(WS-TX)
                   MOVE WS-RMD-SCHED-RATE(WS-TX) TO IRPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2710-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE IRPRC-RMD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD YTD.
      *----------------------------------------------------------------*
       2720-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRPRC-RMD-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRPRC-RMD-YTD
           END-IF
           ADD IRPRC-RMD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AS -> AY WHEN NEXT REVIEW DATE IS PAST
      * AY -> JR WHEN NEXT REVIEW DATE IS PAST
      * JR -> TA WHEN NEXT REVIEW DATE IS PAST
      * TA -> UM WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2730-SET-CHANNEL.
           EVALUATE IRPRC-CHANNEL-CD
               WHEN 'AS'
                   IF IRPRC-NEXT-REVIEW-DATE > 0 AND
                       IRPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AY' TO IRPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AY'
                   IF IRPRC-NEXT-REVIEW-DATE > 0 AND
                       IRPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JR' TO IRPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JR'
                   IF IRPRC-NEXT-REVIEW-DATE > 0 AND
                       IRPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TA' TO IRPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF IRPRC-NEXT-REVIEW-DATE > 0 AND
                       IRPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UM' TO IRPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E701 ADDR REQUIRED
      * E681 EXCESS YTD NEGATIVE
      * E274 EXCESS YTD OVER 1000000
      * E515 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2740-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRPRC-ADDR = SPACES
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN IRPRC-EXCESS-YTD < 0
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN IRPRC-EXCESS-YTD > 1000000.00
                   MOVE 'E274' TO WS-REJECT-CODE
               WHEN IRPRC-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E515' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2750-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO IRPRC-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2760-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-REASON-CD
                   ADD IRPRC-EXCESS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRPRC-EXCESS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2770-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO IRPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RMD YTD.
      *----------------------------------------------------------------*
       2780-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRPRC-RMD-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRPRC-RMD-YTD
           END-IF
           ADD IRPRC-RMD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2790-COUNT-PAYMENT.
           IF IRPRC-PAYMENT-CNT < 9999999
               ADD 1 TO IRPRC-PAYMENT-CNT
           END-IF
           IF IRPRC-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO IRPRC-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E606 NAME REQUIRED
      * E808 EXCESS YTD NEGATIVE
      * E893 EXCESS YTD OVER 25000
      * E441 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2800-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRPRC-NAME = SPACES
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN IRPRC-EXCESS-YTD < 0
                   MOVE 'E808' TO WS-REJECT-CODE
               WHEN IRPRC-EXCESS-YTD > 25000.00
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN IRPRC-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E441' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2810-ACCRUE-CONTRIB.
           IF IRPRC-CONTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRPRC-CONTRIB-YTD * IRPRC-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO IRPRC-DISTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2820-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E973 ADDR REQUIRED
      * E845 EXCESS YTD NEGATIVE
      * E713 EXCESS YTD OVER 10000
      * E542 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2830-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRPRC-ADDR = SPACES
                   MOVE 'E973' TO WS-REJECT-CODE
               WHEN IRPRC-EXCESS-YTD < 0
                   MOVE 'E845' TO WS-REJECT-CODE
               WHEN IRPRC-EXCESS-YTD > 10000.00
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN IRPRC-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER AMT BY IRA TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2840-ACCUM-IRA-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-IRA-TYPE-CD
                   ADD IRPRC-ROLLOVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-IRA-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRPRC-ROLLOVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2850-COUNT-PAYMENT.
           IF IRPRC-PAYMENT-CNT < 9999999
               ADD 1 TO IRPRC-PAYMENT-CNT
           END-IF
           IF IRPRC-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO IRPRC-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FMV AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2860-SPLIT-FMV.
           COMPUTE WS-WORK-AMT = IRPRC-FMV-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRPRC-FMV-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2870-FLAG-DISTRIB.
           IF IRPRC-DISTRIB-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRPRC-DISTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2880-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRPRC-REASON-CD
                   ADD IRPRC-EXCESS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRPRC-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRPRC-EXCESS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR IRA TYPE CD:
      * F -> V WHEN LAST STMT DATE IS PAST
      * V -> W WHEN LAST STMT DATE IS PAST
      * W -> Y WHEN LAST STMT DATE IS PAST
      * Y -> F WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2890-SET-IRA-TYPE.
           EVALUATE IRPRC-IRA-TYPE-CD
               WHEN 'F'
                   IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO IRPRC-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'W' TO IRPRC-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'Y' TO IRPRC-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF IRPRC-LAST-STMT-DATE > 0 AND IRPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'F' TO IRPRC-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2900-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO IRPRC-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCESS YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2910-ACCRUE-EXCESS.
           IF IRPRC-EXCESS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRPRC-EXCESS-YTD * IRPRC-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO IRPRC-RMD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2920-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       2930-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRPRC-DISTRIB-YTD ROUNDED =
                   IRPRC-CONTRIB-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRPRC-DISTRIB-YTD
           END-IF
           ADD IRPRC-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2940-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E492 NAME REQUIRED
      * E549 CONTRIB YTD NEGATIVE
      * E879 CONTRIB YTD OVER 250000
      * E693 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2950-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRPRC-NAME = SPACES
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN IRPRC-CONTRIB-YTD < 0
                   MOVE 'E549' TO WS-REJECT-CODE
               WHEN IRPRC-CONTRIB-YTD > 250000.00
                   MOVE 'E879' TO WS-REJECT-CODE
               WHEN IRPRC-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROLLOVER AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2960-ACCRUE-ROLLOVER.
           IF IRPRC-ROLLOVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRPRC-ROLLOVER-AMT * IRPRC-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO IRPRC-RMD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2970-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRPRC-EXPIRY-DATE > 0 AND IRPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCESS YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2980-FLAG-EXCESS.
           IF IRPRC-EXCESS-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRPRC-EXCESS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO IRPRC-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO IRPRC-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO IRPRC-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'IRPRC EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
