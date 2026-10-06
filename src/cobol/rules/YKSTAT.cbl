      *================================================================*
      * PROGRAM  : YKSTAT                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DATE WINDOW BRIDGE RULE - VALIDATION. CALLED BY THE *
      *            DATE WINDOW BRIDGE BATCH AND ONLINE PROGRAMS.       *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-02 JLM  ORIGINAL                                       *
      * 1994-01-08 JLM  CONTROL FEE SCHEDULE UPDATE INC8299            *
      * 1995-12-12 EWS  AMOUNT-1 NOTICE TEXT PER LEGAL REQ1253         *
      * 1996-06-03 LMB  AMOUNT-1 NOTICE TEXT PER LEGAL INC6497         *
      * 1998-06-14 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-17 GAF  PERFORMANCE - AMOUNT-2 READ AHEAD REQ0624      *
      * 1999-02-03 DPS  CONTROL STATUS CODES ADDED CHG3566             *
      * 2001-07-25 CRP  NEW AMOUNT-1 REPORT COLUMNS CHG7624            *
      * 2002-10-08 JLM  AMOUNT-1 NOTICE TEXT PER LEGAL REQ8159         *
      * 2006-09-06 BHS  CONTROL EXCEPTIONS TO OPS QUEUE INC3280        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKSTAT.
       AUTHOR. D P SHAW.

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
                                           VALUE 8.000000.
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
      * WS-AMOUNT-1-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-1-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000717500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000850000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000646500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000662000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000504000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017800000000488000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022800000000514500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023800000000444500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028800000000387500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029800000000252000'.
       01  WS-AMOUNT-1-BAND-TBL REDEFINES WS-AMOUNT-1-BAND-VALUES.
           05  WS-AMOUNT-1-BAND-ROW OCCURS 10 TIMES.
               10  WS-AMOUNT-1-BAND-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-1-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-AMOUNT-2-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-2-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000189000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000803000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000413500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000318500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000151000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000828500'.
       01  WS-AMOUNT-2-BAND-TBL REDEFINES WS-AMOUNT-2-BAND-VALUES.
           05  WS-AMOUNT-2-BAND-ROW OCCURS 6 TIMES.
               10  WS-AMOUNT-2-BAND-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-2-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONTROL-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000199000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000345000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000586500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000572000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000456000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000771000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000184000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000754000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000060500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006750000000129000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006800000000776000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006850000000631500'.
       01  WS-CONTROL-SCHED-TBL REDEFINES WS-CONTROL-SCHED-VALUES.
           05  WS-CONTROL-SCHED-ROW OCCURS 12 TIMES.
               10  WS-CONTROL-SCHED-LOW    PIC 9(09)V99.
               10  WS-CONTROL-SCHED-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY YKSTATP.

       PROCEDURE DIVISION USING YKSTAT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCUM-REGION
           PERFORM 2010-DESCRIBE-CYCLE
           PERFORM 2020-PRORATE-AMOUNT-1
           PERFORM 2030-AGE-LAST-PAY
           PERFORM 2040-ACCUM-REGION
           PERFORM 2050-AGE-LAST-STMT
           PERFORM 2060-LIMIT-ITEM
           PERFORM 2070-SELECT-MARGIN
           PERFORM 2080-SPLIT-AMOUNT-2
           PERFORM 2090-ACCUM-REASON
           PERFORM 2100-FLAG-CONTROL
           PERFORM 2110-SPLIT-AMOUNT-1
           PERFORM 2120-COUNT-RETRY
           PERFORM 2130-AGE-LAST-PAY
           PERFORM 2140-PRORATE-CONTROL
           PERFORM 2150-CHECK-LAST-MAINT
           PERFORM 2160-SPLIT-CONTROL
           PERFORM 2170-SET-SOURCE
           PERFORM 2180-DESCRIBE-CYCLE
           PERFORM 2190-SET-CYCLE
           PERFORM 2200-DESCRIBE-REASON
           PERFORM 2210-SET-REGION
           PERFORM 2220-LIMIT-AMOUNT-2
           PERFORM 2230-SELECT-MARGIN
           PERFORM 2240-PRORATE-CONTROL
           PERFORM 2250-LIMIT-AMOUNT-1
           PERFORM 2260-AGE-NEXT-DUE
           PERFORM 2270-SELECT-PENALTY-PCT
           PERFORM 2280-FLAG-AMOUNT-1
           PERFORM 2290-PRORATE-ITEM
           PERFORM 2300-COUNT-CYCLE
           PERFORM 2310-SELECT-PENALTY-PCT
           PERFORM 2320-SPLIT-ITEM
           PERFORM 2330-SET-REGION
           PERFORM 2340-PRORATE-AMOUNT-2
           PERFORM 2350-EDIT-DESC
           PERFORM 2360-COUNT-RETRY
           PERFORM 2370-DESCRIBE-CYCLE
           PERFORM 2380-CHECK-NEXT-DUE
           PERFORM 2390-EDIT-DESC
           PERFORM 2400-SET-CYCLE
           PERFORM 2410-LIMIT-AMOUNT-1
           PERFORM 2420-PRORATE-CONTROL
           PERFORM 2430-ACCUM-CYCLE
           PERFORM 2440-ACCRUE-AMOUNT-2
           PERFORM 2450-COUNT-CYCLE
           PERFORM 2460-COUNT-RETRY
           PERFORM 2470-SET-SOURCE
           PERFORM 2480-DESCRIBE-SOURCE
           PERFORM 2490-SET-SOURCE
           PERFORM 2500-AGE-LAST-PAY
           PERFORM 2510-CHECK-LAST-PAY
           PERFORM 2520-LIMIT-AMOUNT-1
           PERFORM 2530-SPLIT-AMOUNT-2
           PERFORM 2540-PRORATE-AMOUNT-2
           PERFORM 2550-COUNT-CYCLE
           PERFORM 2560-AGE-LAST-MAINT
           PERFORM 2570-ACCRUE-MARGIN
           PERFORM 2580-AGE-LAST-STMT
           PERFORM 2590-LIMIT-ITEM
           PERFORM 2600-SET-SOURCE
           PERFORM 2610-EDIT-DESC
           PERFORM 2620-AGE-NEXT-DUE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO YKSTAT-RETURN-CODE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2000-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKSTAT-REGION-CD
                   ADD YKSTAT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKSTAT-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKSTAT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2010-DESCRIBE-CYCLE.
           EVALUATE YKSTAT-CYCLE-CD
               WHEN 'FJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       2020-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKSTAT-AMOUNT-1-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YKSTAT-AMOUNT-1-AMT
           END-IF
           ADD YKSTAT-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YKSTAT-LAST-PAY-DATE > 0 AND YKSTAT-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKSTAT-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKSTAT-AMOUNT-1-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YKSTAT-AMOUNT-1-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YKSTAT-AMOUNT-1-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD YKSTAT-AMOUNT-1-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKSTAT-AMOUNT-1-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2040-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKSTAT-REGION-CD
                   ADD YKSTAT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKSTAT-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKSTAT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YKSTAT-LAST-STMT-DATE > 0 AND YKSTAT-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2060-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL SCHED TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2070-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-SCHED-RATE(1) TO YKSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF YKSTAT-CONTROL-AMT NOT < WS-CONTROL-SCHED-LOW(WS-TX)
                   MOVE WS-CONTROL-SCHED-RATE(WS-TX) TO YKSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2080-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YKSTAT-AMOUNT-2-AMT / 4
           COMPUTE WS-WORK-AMT-2 = YKSTAT-AMOUNT-2-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2090-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKSTAT-REASON-CD
                   ADD YKSTAT-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKSTAT-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKSTAT-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2100-FLAG-CONTROL.
           IF YKSTAT-CONTROL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YKSTAT-CONTROL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2110-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YKSTAT-AMOUNT-1-YTD / 4
           COMPUTE WS-WORK-AMT-2 = YKSTAT-AMOUNT-1-YTD - (WS-WORK-AMT *
               4)
           COMPUTE YKSTAT-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-RETRY.
           IF YKSTAT-RETRY-CNT < 9999999
               ADD 1 TO YKSTAT-RETRY-CNT
           END-IF
           IF YKSTAT-RETRY-CNT NOT < 5
               MOVE 'Y' TO YKSTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2130-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YKSTAT-LAST-PAY-DATE > 0 AND YKSTAT-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKSTAT-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKSTAT-AMOUNT-2-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YKSTAT-AMOUNT-2-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD YKSTAT-AMOUNT-2-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YKSTAT-AMOUNT-2-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKSTAT-AMOUNT-2-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2140-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKSTAT-CONTROL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2150-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YKSTAT-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKSTAT-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO YKSTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YKSTAT-CONTROL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = YKSTAT-CONTROL-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN NEXT DUE DATE IS PAST
      * FTB -> GAP WHEN NEXT DUE DATE IS PAST
      * GAP -> PAP WHEN NEXT DUE DATE IS PAST
      * PAP -> VGC WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2170-SET-SOURCE.
           EVALUATE YKSTAT-SOURCE-CD
               WHEN 'BNX'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2180-DESCRIBE-CYCLE.
           EVALUATE YKSTAT-CYCLE-CD
               WHEN 'FJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * FJ -> LA WHEN LAST MAINT DATE IS PAST
      * LA -> SP WHEN LAST MAINT DATE IS PAST
      * SP -> UC WHEN LAST MAINT DATE IS PAST
      * UC -> FJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2190-SET-CYCLE.
           EVALUATE YKSTAT-CYCLE-CD
               WHEN 'FJ'
                   IF YKSTAT-LAST-MAINT-DATE > 0 AND
                       YKSTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'LA' TO YKSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LA'
                   IF YKSTAT-LAST-MAINT-DATE > 0 AND
                       YKSTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'SP' TO YKSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF YKSTAT-LAST-MAINT-DATE > 0 AND
                       YKSTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UC' TO YKSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UC'
                   IF YKSTAT-LAST-MAINT-DATE > 0 AND
                       YKSTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FJ' TO YKSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2200-DESCRIBE-REASON.
           EVALUATE YKSTAT-REASON-CD
               WHEN 'AX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> H WHEN LAST PAY DATE IS PAST
      * H -> K WHEN LAST PAY DATE IS PAST
      * K -> S WHEN LAST PAY DATE IS PAST
      * S -> V WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-REGION.
           EVALUATE YKSTAT-REGION-CD
               WHEN 'C'
                   IF YKSTAT-LAST-PAY-DATE > 0 AND YKSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO YKSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF YKSTAT-LAST-PAY-DATE > 0 AND YKSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO YKSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF YKSTAT-LAST-PAY-DATE > 0 AND YKSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO YKSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YKSTAT-LAST-PAY-DATE > 0 AND YKSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO YKSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2220-LIMIT-AMOUNT-2.
           IF YKSTAT-AMOUNT-2-YTD < 10.00
               MOVE 10.00 TO YKSTAT-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKSTAT-AMOUNT-2-YTD > 5000.00
               MOVE 5000.00 TO YKSTAT-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 BAND TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2230-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-BAND-RATE(1) TO YKSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF YKSTAT-CONTROL-AMT NOT < WS-AMOUNT-1-BAND-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-BAND-RATE(WS-TX) TO YKSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       2240-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKSTAT-CONTROL-AMT ROUNDED =
                   YKSTAT-CONTROL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YKSTAT-CONTROL-AMT
           END-IF
           ADD YKSTAT-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2250-LIMIT-AMOUNT-1.
           IF YKSTAT-AMOUNT-1-YTD < 25.00
               MOVE 25.00 TO YKSTAT-AMOUNT-1-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKSTAT-AMOUNT-1-YTD > 2500.00
               MOVE 2500.00 TO YKSTAT-AMOUNT-1-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKSTAT-AMOUNT-1-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YKSTAT-AMOUNT-1-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD YKSTAT-AMOUNT-1-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD YKSTAT-AMOUNT-1-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKSTAT-AMOUNT-1-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2270-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-SCHED-RATE(1) TO YKSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-CONTROL-SCHED-LOW(WS-TX)
                   MOVE WS-CONTROL-SCHED-RATE(WS-TX) TO
                       YKSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2280-FLAG-AMOUNT-1.
           IF YKSTAT-AMOUNT-1-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YKSTAT-AMOUNT-1-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2290-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2300-COUNT-CYCLE.
           IF YKSTAT-CYCLE-CNT < 9999999
               ADD 1 TO YKSTAT-CYCLE-CNT
           END-IF
           IF YKSTAT-CYCLE-CNT NOT < 10
               MOVE 'Y' TO YKSTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 2 BAND TIER FOR THE AMOUNT 1 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2310-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-2-BAND-RATE(1) TO YKSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF YKSTAT-AMOUNT-1-YTD NOT < WS-AMOUNT-2-BAND-LOW(WS-TX)
                   MOVE WS-AMOUNT-2-BAND-RATE(WS-TX) TO
                       YKSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> H WHEN NEXT DUE DATE IS PAST
      * H -> K WHEN NEXT DUE DATE IS PAST
      * K -> S WHEN NEXT DUE DATE IS PAST
      * S -> V WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2330-SET-REGION.
           EVALUATE YKSTAT-REGION-CD
               WHEN 'C'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO YKSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO YKSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO YKSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO YKSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       2340-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKSTAT-AMOUNT-2-AMT ROUNDED =
                   YKSTAT-AMOUNT-1-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YKSTAT-AMOUNT-2-AMT
           END-IF
           ADD YKSTAT-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E325 DESC REQUIRED
      * E676 AMOUNT 2 AMT NEGATIVE
      * E226 AMOUNT 2 AMT OVER 10000
      * E074 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2350-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKSTAT-DESC = SPACES
                   MOVE 'E325' TO WS-REJECT-CODE
               WHEN YKSTAT-AMOUNT-2-AMT < 0
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN YKSTAT-AMOUNT-2-AMT > 10000.00
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN YKSTAT-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E074' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-RETRY.
           IF YKSTAT-RETRY-CNT < 9999999
               ADD 1 TO YKSTAT-RETRY-CNT
           END-IF
           IF YKSTAT-RETRY-CNT NOT < 12
               MOVE 'Y' TO YKSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-CYCLE.
           EVALUATE YKSTAT-CYCLE-CD
               WHEN 'FJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2380-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YKSTAT-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO YKSTAT-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E100 DESC REQUIRED
      * E506 CONTROL AMT NEGATIVE
      * E961 CONTROL AMT OVER 50000
      * E773 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2390-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKSTAT-DESC = SPACES
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN YKSTAT-CONTROL-AMT < 0
                   MOVE 'E506' TO WS-REJECT-CODE
               WHEN YKSTAT-CONTROL-AMT > 50000.00
                   MOVE 'E961' TO WS-REJECT-CODE
               WHEN YKSTAT-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * FJ -> LA WHEN LAST MAINT DATE IS PAST
      * LA -> SP WHEN LAST MAINT DATE IS PAST
      * SP -> UC WHEN LAST MAINT DATE IS PAST
      * UC -> FJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2400-SET-CYCLE.
           EVALUATE YKSTAT-CYCLE-CD
               WHEN 'FJ'
                   IF YKSTAT-LAST-MAINT-DATE > 0 AND
                       YKSTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'LA' TO YKSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LA'
                   IF YKSTAT-LAST-MAINT-DATE > 0 AND
                       YKSTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'SP' TO YKSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF YKSTAT-LAST-MAINT-DATE > 0 AND
                       YKSTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UC' TO YKSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UC'
                   IF YKSTAT-LAST-MAINT-DATE > 0 AND
                       YKSTAT-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FJ' TO YKSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2410-LIMIT-AMOUNT-1.
           IF YKSTAT-AMOUNT-1-YTD < 0.00
               MOVE 0.00 TO YKSTAT-AMOUNT-1-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKSTAT-AMOUNT-1-YTD > 1000.00
               MOVE 1000.00 TO YKSTAT-AMOUNT-1-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       2420-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKSTAT-CONTROL-AMT ROUNDED =
                   YKSTAT-AMOUNT-2-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YKSTAT-CONTROL-AMT
           END-IF
           ADD YKSTAT-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2430-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKSTAT-CYCLE-CD
                   ADD YKSTAT-CONTROL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKSTAT-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKSTAT-CONTROL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2440-ACCRUE-AMOUNT-2.
           IF YKSTAT-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKSTAT-AMOUNT-2-AMT * YKSTAT-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2450-COUNT-CYCLE.
           IF YKSTAT-CYCLE-CNT < 9999999
               ADD 1 TO YKSTAT-CYCLE-CNT
           END-IF
           IF YKSTAT-CYCLE-CNT NOT < 6
               MOVE 'Y' TO YKSTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2460-COUNT-RETRY.
           IF YKSTAT-RETRY-CNT < 9999999
               ADD 1 TO YKSTAT-RETRY-CNT
           END-IF
           IF YKSTAT-RETRY-CNT NOT < 3
               MOVE 'Y' TO YKSTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN NEXT DUE DATE IS PAST
      * FTB -> GAP WHEN NEXT DUE DATE IS PAST
      * GAP -> PAP WHEN NEXT DUE DATE IS PAST
      * PAP -> VGC WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-SOURCE.
           EVALUATE YKSTAT-SOURCE-CD
               WHEN 'BNX'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-SOURCE.
           EVALUATE YKSTAT-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN LAST STMT DATE IS PAST
      * FTB -> GAP WHEN LAST STMT DATE IS PAST
      * GAP -> PAP WHEN LAST STMT DATE IS PAST
      * PAP -> VGC WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-SOURCE.
           EVALUATE YKSTAT-SOURCE-CD
               WHEN 'BNX'
                   IF YKSTAT-LAST-STMT-DATE > 0 AND
                       YKSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YKSTAT-LAST-STMT-DATE > 0 AND
                       YKSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YKSTAT-LAST-STMT-DATE > 0 AND
                       YKSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YKSTAT-LAST-STMT-DATE > 0 AND
                       YKSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2500-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YKSTAT-LAST-PAY-DATE > 0 AND YKSTAT-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKSTAT-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2510-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YKSTAT-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKSTAT-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO YKSTAT-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2520-LIMIT-AMOUNT-1.
           IF YKSTAT-AMOUNT-1-YTD < 100.00
               MOVE 100.00 TO YKSTAT-AMOUNT-1-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKSTAT-AMOUNT-1-YTD > 5000.00
               MOVE 5000.00 TO YKSTAT-AMOUNT-1-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YKSTAT-AMOUNT-2-YTD / 4
           COMPUTE WS-WORK-AMT-2 = YKSTAT-AMOUNT-2-YTD - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 YTD.
      *----------------------------------------------------------------*
       2540-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKSTAT-AMOUNT-2-YTD ROUNDED =
                   YKSTAT-CONTROL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YKSTAT-AMOUNT-2-YTD
           END-IF
           ADD YKSTAT-AMOUNT-2-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2550-COUNT-CYCLE.
           IF YKSTAT-CYCLE-CNT < 9999999
               ADD 1 TO YKSTAT-CYCLE-CNT
           END-IF
           IF YKSTAT-CYCLE-CNT NOT < 3
               MOVE 'Y' TO YKSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2560-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YKSTAT-LAST-MAINT-DATE > 0 AND YKSTAT-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKSTAT-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2570-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * YKSTAT-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YKSTAT-AMOUNT-2-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2580-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YKSTAT-LAST-STMT-DATE > 0 AND YKSTAT-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKSTAT-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YKSTAT-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YKSTAT-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YKSTAT-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKSTAT-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2590-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN LAST PAY DATE IS PAST
      * FTB -> GAP WHEN LAST PAY DATE IS PAST
      * GAP -> PAP WHEN LAST PAY DATE IS PAST
      * PAP -> VGC WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2600-SET-SOURCE.
           EVALUATE YKSTAT-SOURCE-CD
               WHEN 'BNX'
                   IF YKSTAT-LAST-PAY-DATE > 0 AND YKSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YKSTAT-LAST-PAY-DATE > 0 AND YKSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YKSTAT-LAST-PAY-DATE > 0 AND YKSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YKSTAT-LAST-PAY-DATE > 0 AND YKSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YKSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E038 DESC REQUIRED
      * E821 AMOUNT 2 YTD NEGATIVE
      * E625 AMOUNT 2 YTD OVER 25000
      * E257 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKSTAT-DESC = SPACES
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN YKSTAT-AMOUNT-2-YTD < 0
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN YKSTAT-AMOUNT-2-YTD > 25000.00
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN YKSTAT-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2620-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YKSTAT-NEXT-DUE-DATE > 0 AND YKSTAT-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKSTAT-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YKSTAT-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YKSTAT-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD YKSTAT-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKSTAT-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO YKSTAT-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO YKSTAT-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO YKSTAT-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'YKSTAT EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
