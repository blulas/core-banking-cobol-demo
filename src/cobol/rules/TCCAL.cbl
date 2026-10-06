      *================================================================*
      * PROGRAM  : TCCAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TRAVELERS CHEQUES RULE - LIMITS. CALLED BY THE      *
      *            TRAVELERS CHEQUES BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-05-04 BHS  ORIGINAL                                       *
      * 1992-07-12 GAF  FACE CUTOFF MOVED INC2318                      *
      * 1996-07-08 DPS  FACE NOTICE TEXT PER LEGAL PRB5194             *
      * 1999-01-05 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-08-13 RJT  CONSIGN STATUS CODES ADDED PRB8278             *
      * 2005-08-01 MKL  REFUND EXCEPTIONS TO OPS QUEUE PRB2997         *
      * 2006-09-25 CRP  FACE FEE SCHEDULE UPDATE INC1223               *
      * 2006-11-14 SKN  SHORTAGE RERUN PROTECTION REQ2005              *
      * 2007-07-10 RJT  REFUND STATUS CODES ADDED REQ0565              *
      * 2014-06-14 SKN  SHORTAGE ROUNDING - HALF UP REQ7549            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCCAL.
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
                                           VALUE 24.000000.
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
      * WS-FACE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000075000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000540000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000292000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007550000000531000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000787000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000137000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020550000000045500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021050000000159500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021150000000464500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022150000000381000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023150000000679000'.
       01  WS-FACE-GRID-TBL REDEFINES WS-FACE-GRID-VALUES.
           05  WS-FACE-GRID-ROW OCCURS 11 TIMES.
               10  WS-FACE-GRID-LOW        PIC 9(09)V99.
               10  WS-FACE-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONSIGN-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONSIGN-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000508500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000195000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000667000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000576000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010400000000835000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015400000000607500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015900000000314000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015950000000229000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000739500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000120000'.
       01  WS-CONSIGN-TIER-TBL REDEFINES WS-CONSIGN-TIER-VALUES.
           05  WS-CONSIGN-TIER-ROW OCCURS 10 TIMES.
               10  WS-CONSIGN-TIER-LOW     PIC 9(09)V99.
               10  WS-CONSIGN-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONSIGN-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONSIGN-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000811500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000819000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000255500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000586000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022600000000019500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022650000000327000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022750000000238500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032750000000340000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035250000000361500'.
       01  WS-CONSIGN-SCHED-TBL REDEFINES WS-CONSIGN-SCHED-VALUES.
           05  WS-CONSIGN-SCHED-ROW OCCURS 9 TIMES.
               10  WS-CONSIGN-SCHED-LOW    PIC 9(09)V99.
               10  WS-CONSIGN-SCHED-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TCCALP.

       PROCEDURE DIVISION USING TCCAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-COMMISSION
           PERFORM 2010-FLAG-ITEM
           PERFORM 2020-SET-ISSUER
           PERFORM 2030-SELECT-FEE-PCT
           PERFORM 2040-ACCUM-SOURCE
           PERFORM 2050-ASSESS-STOP
           PERFORM 2060-SET-ISSUER
           PERFORM 2070-AGE-LAST-ACT
           PERFORM 2080-ACCUM-BRANCH
           PERFORM 2090-CHECK-MATURITY
           PERFORM 2100-PRORATE-CONSIGN
           PERFORM 2110-ACCUM-SOURCE
           PERFORM 2120-LIMIT-ITEM
           PERFORM 2130-CHECK-MATURITY
           PERFORM 2140-AGE-NEXT-DUE
           PERFORM 2150-DESCRIBE-BRANCH
           PERFORM 2160-PRORATE-COMMISSION
           PERFORM 2170-CHECK-EFFECTIVE
           PERFORM 2180-CHECK-MATURITY
           PERFORM 2190-SPLIT-COMMISSION
           PERFORM 2200-COUNT-EXCEPT
           PERFORM 2210-COUNT-ITEM
           PERFORM 2220-CHECK-NEXT-DUE
           PERFORM 2230-EDIT-NAME
           PERFORM 2240-COUNT-EXCEPT
           PERFORM 2250-ASSESS-REVIEW
           PERFORM 2260-DESCRIBE-ISSUER
           PERFORM 2270-COUNT-EXCEPT
           PERFORM 2280-EDIT-ADDR
           PERFORM 2290-LIMIT-SHORTAGE
           PERFORM 2300-ACCUM-ISSUER
           PERFORM 2310-ASSESS-EXEMPT
           PERFORM 2320-DESCRIBE-ISSUER
           PERFORM 2330-ACCUM-ISSUER
           PERFORM 2340-AGE-EFFECTIVE
           PERFORM 2350-AGE-EFFECTIVE
           PERFORM 2360-DESCRIBE-BRANCH
           PERFORM 2370-AGE-MATURITY
           PERFORM 2380-PRORATE-CONSIGN
           PERFORM 2390-DESCRIBE-DENOM
           PERFORM 2400-ASSESS-EXEMPT
           PERFORM 2410-ACCRUE-RATE
           PERFORM 2420-AGE-MATURITY
           PERFORM 2430-AGE-NEXT-DUE
           PERFORM 2440-SET-BRANCH
           PERFORM 2450-COUNT-EXCEPT
           PERFORM 2460-AGE-MATURITY
           PERFORM 2470-AGE-EFFECTIVE
           PERFORM 2480-ASSESS-STOP
           PERFORM 2490-LIMIT-CONSIGN
           PERFORM 2500-ACCUM-SOURCE
           PERFORM 2510-ACCRUE-CONSIGN
           PERFORM 2520-SPLIT-COMMISSION
           PERFORM 2530-ASSESS-EXEMPT
           PERFORM 2540-SPLIT-SHORTAGE
           PERFORM 2550-ASSESS-REVIEW
           PERFORM 2560-ACCRUE-SHORTAGE
           PERFORM 2570-EDIT-NAME
           PERFORM 2580-ACCRUE-SHORTAGE
           PERFORM 2590-ACCRUE-SHORTAGE
           PERFORM 2600-FLAG-COMMISSION
           PERFORM 2610-LIMIT-CONSIGN
           PERFORM 2620-SPLIT-SHORTAGE
           PERFORM 2630-DESCRIBE-BRANCH
           PERFORM 2640-LIMIT-ITEM
           PERFORM 2650-SET-DENOM
           PERFORM 2660-EDIT-ADDR
           PERFORM 2670-LIMIT-SHORTAGE
           PERFORM 2680-ACCRUE-COMMISSION
           PERFORM 2690-EDIT-ADDR
           PERFORM 2700-CHECK-LAST-ACT
           PERFORM 2710-DESCRIBE-BRANCH
           PERFORM 2720-SELECT-RATE
           PERFORM 2730-EDIT-NAME
           PERFORM 2740-SET-BRANCH
           PERFORM 2750-DESCRIBE-DENOM
           PERFORM 2760-ACCRUE-RATE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TCCAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COMMISSION YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-COMMISSION.
           IF TCCAL-COMMISSION-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCCAL-COMMISSION-YTD * TCCAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TCCAL-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2010-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUER CD:
      * AF -> CJ WHEN LAST ACT DATE IS PAST
      * CJ -> DU WHEN LAST ACT DATE IS PAST
      * DU -> JY WHEN LAST ACT DATE IS PAST
      * JY -> RR WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2020-SET-ISSUER.
           EVALUATE TCCAL-ISSUER-CD
               WHEN 'AF'
                   IF TCCAL-LAST-ACT-DATE > 0 AND TCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJ' TO TCCAL-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJ'
                   IF TCCAL-LAST-ACT-DATE > 0 AND TCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO TCCAL-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DU'
                   IF TCCAL-LAST-ACT-DATE > 0 AND TCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO TCCAL-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF TCCAL-LAST-ACT-DATE > 0 AND TCCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RR' TO TCCAL-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN TIER TIER FOR THE COMMISSION YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2030-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-TIER-RATE(1) TO TCCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TCCAL-COMMISSION-YTD NOT < WS-CONSIGN-TIER-LOW(WS-TX)
                   MOVE WS-CONSIGN-TIER-RATE(WS-TX) TO TCCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2040-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCCAL-SOURCE-CD
                   ADD TCCAL-CONSIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCCAL-CONSIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CONSIGN YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2050-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TCCAL-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCCAL-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TCCAL-CONSIGN-YTD *
                       TCCAL-FEE-PCT / 200
               WHEN TCCAL-SOURCE-CD = 'EG'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN TCCAL-SOURCE-CD = 'GH'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TCCAL-CONSIGN-YTD *
                       TCCAL-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUER CD:
      * AF -> CJ WHEN NEXT DUE DATE IS PAST
      * CJ -> DU WHEN NEXT DUE DATE IS PAST
      * DU -> JY WHEN NEXT DUE DATE IS PAST
      * JY -> RR WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2060-SET-ISSUER.
           EVALUATE TCCAL-ISSUER-CD
               WHEN 'AF'
                   IF TCCAL-NEXT-DUE-DATE > 0 AND TCCAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJ' TO TCCAL-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJ'
                   IF TCCAL-NEXT-DUE-DATE > 0 AND TCCAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO TCCAL-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DU'
                   IF TCCAL-NEXT-DUE-DATE > 0 AND TCCAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO TCCAL-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF TCCAL-NEXT-DUE-DATE > 0 AND TCCAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RR' TO TCCAL-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-LAST-ACT-DATE > 0 AND TCCAL-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCCAL-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TCCAL-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TCCAL-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TCCAL-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCCAL-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2080-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCCAL-BRANCH-CD
                   ADD TCCAL-SHORTAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCCAL-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCCAL-SHORTAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2090-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TCCAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONSIGN AMT.
      *----------------------------------------------------------------*
       2100-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCCAL-CONSIGN-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TCCAL-CONSIGN-AMT
           END-IF
           ADD TCCAL-CONSIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE COMMISSION AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCCAL-SOURCE-CD
                   ADD TCCAL-COMMISSION-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCCAL-COMMISSION-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2120-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2130-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TCCAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-NEXT-DUE-DATE > 0 AND TCCAL-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCCAL-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TCCAL-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TCCAL-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TCCAL-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCCAL-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2150-DESCRIBE-BRANCH.
           EVALUATE TCCAL-BRANCH-CD
               WHEN 'EG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONSIGN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COMMISSION AMT.
      *----------------------------------------------------------------*
       2160-PRORATE-COMMISSION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCCAL-COMMISSION-AMT ROUNDED =
                   TCCAL-CONSIGN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TCCAL-COMMISSION-AMT
           END-IF
           ADD TCCAL-COMMISSION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2170-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TCCAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TCCAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2190-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TCCAL-COMMISSION-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TCCAL-COMMISSION-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TCCAL-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-EXCEPT.
           IF TCCAL-EXCEPT-CNT < 9999999
               ADD 1 TO TCCAL-EXCEPT-CNT
           END-IF
           IF TCCAL-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TCCAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2210-COUNT-ITEM.
           IF TCCAL-ITEM-CNT < 9999999
               ADD 1 TO TCCAL-ITEM-CNT
           END-IF
           IF TCCAL-ITEM-CNT NOT < 3
               MOVE 'Y' TO TCCAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2220-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TCCAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E733 NAME REQUIRED
      * E463 COMMISSION YTD NEGATIVE
      * E821 COMMISSION YTD OVER 25000
      * E240 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCCAL-NAME = SPACES
                   MOVE 'E733' TO WS-REJECT-CODE
               WHEN TCCAL-COMMISSION-YTD < 0
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN TCCAL-COMMISSION-YTD > 25000.00
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN TCCAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-EXCEPT.
           IF TCCAL-EXCEPT-CNT < 9999999
               ADD 1 TO TCCAL-EXCEPT-CNT
           END-IF
           IF TCCAL-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TCCAL-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION FEE ON THE CONSIGN YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2250-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TCCAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TCCAL-COMMISSION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCCAL-EXEMPT-FLAG = 'Y'
                   COMPUTE TCCAL-COMMISSION-AMT ROUNDED =
                       TCCAL-CONSIGN-YTD * TCCAL-FEE-PCT / 200
               WHEN TCCAL-BRANCH-CD = 'EG'
                   MOVE 10.00 TO TCCAL-COMMISSION-AMT
               WHEN TCCAL-BRANCH-CD = 'HZ'
                   MOVE 10.00 TO TCCAL-COMMISSION-AMT
               WHEN OTHER
                   COMPUTE TCCAL-COMMISSION-AMT ROUNDED =
                       TCCAL-CONSIGN-YTD * TCCAL-FEE-PCT / 100
           END-EVALUATE
           IF TCCAL-COMMISSION-AMT > 0 AND TCCAL-COMMISSION-AMT < 5.00
               MOVE 5.00 TO TCCAL-COMMISSION-AMT
           END-IF
           IF TCCAL-COMMISSION-AMT > 50.00
               MOVE 50.00 TO TCCAL-COMMISSION-AMT
           END-IF
           ADD TCCAL-COMMISSION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2260-DESCRIBE-ISSUER.
           EVALUATE TCCAL-ISSUER-CD
               WHEN 'AF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2270-COUNT-EXCEPT.
           IF TCCAL-EXCEPT-CNT < 9999999
               ADD 1 TO TCCAL-EXCEPT-CNT
           END-IF
           IF TCCAL-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO TCCAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E219 ADDR REQUIRED
      * E786 COMMISSION AMT NEGATIVE
      * E729 COMMISSION AMT OVER 50000
      * E586 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2280-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCCAL-ADDR = SPACES
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN TCCAL-COMMISSION-AMT < 0
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN TCCAL-COMMISSION-AMT > 50000.00
                   MOVE 'E729' TO WS-REJECT-CODE
               WHEN TCCAL-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E586' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2290-LIMIT-SHORTAGE.
           IF TCCAL-SHORTAGE-YTD < 10.00
               MOVE 10.00 TO TCCAL-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TCCAL-SHORTAGE-YTD > 500.00
               MOVE 500.00 TO TCCAL-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COMMISSION YTD BY ISSUER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2300-ACCUM-ISSUER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCCAL-ISSUER-CD
                   ADD TCCAL-COMMISSION-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCCAL-ISSUER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCCAL-COMMISSION-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION FEE ON THE CONSIGN YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2310-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TCCAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TCCAL-COMMISSION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCCAL-STOP-FLAG = 'Y'
                   COMPUTE TCCAL-COMMISSION-AMT ROUNDED =
                       TCCAL-CONSIGN-YTD * TCCAL-FEE-PCT / 200
               WHEN TCCAL-BRANCH-CD = 'EG'
                   MOVE 7.00 TO TCCAL-COMMISSION-AMT
               WHEN TCCAL-BRANCH-CD = 'HZ'
                   MOVE 20.00 TO TCCAL-COMMISSION-AMT
               WHEN OTHER
                   COMPUTE TCCAL-COMMISSION-AMT ROUNDED =
                       TCCAL-CONSIGN-YTD * TCCAL-FEE-PCT / 100
           END-EVALUATE
           IF TCCAL-COMMISSION-AMT > 0 AND TCCAL-COMMISSION-AMT < 5.00
               MOVE 5.00 TO TCCAL-COMMISSION-AMT
           END-IF
           IF TCCAL-COMMISSION-AMT > 35.00
               MOVE 35.00 TO TCCAL-COMMISSION-AMT
           END-IF
           ADD TCCAL-COMMISSION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2320-DESCRIBE-ISSUER.
           EVALUATE TCCAL-ISSUER-CD
               WHEN 'AF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN YTD BY ISSUER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2330-ACCUM-ISSUER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCCAL-ISSUER-CD
                   ADD TCCAL-CONSIGN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCCAL-ISSUER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCCAL-CONSIGN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2340-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-EFFECTIVE-DATE > 0 AND TCCAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCCAL-COMMISSION-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TCCAL-COMMISSION-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TCCAL-COMMISSION-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TCCAL-COMMISSION-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCCAL-COMMISSION-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2350-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-EFFECTIVE-DATE > 0 AND TCCAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2360-DESCRIBE-BRANCH.
           EVALUATE TCCAL-BRANCH-CD
               WHEN 'EG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2370-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-MATURITY-DATE > 0 AND TCCAL-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCCAL-COMMISSION-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TCCAL-COMMISSION-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TCCAL-COMMISSION-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TCCAL-COMMISSION-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCCAL-COMMISSION-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONSIGN AMT.
      *----------------------------------------------------------------*
       2380-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCCAL-CONSIGN-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TCCAL-CONSIGN-AMT
           END-IF
           ADD TCCAL-CONSIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2390-DESCRIBE-DENOM.
           EVALUATE TCCAL-DENOM-CD
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2400-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TCCAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TCCAL-COMMISSION-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCCAL-STOP-FLAG = 'Y'
                   COMPUTE TCCAL-COMMISSION-YTD ROUNDED = WS-WORK-AMT-2
                       * TCCAL-RATE / 200
               WHEN TCCAL-ISSUER-CD = 'AF'
                   MOVE 10.00 TO TCCAL-COMMISSION-YTD
               WHEN TCCAL-ISSUER-CD = 'CJ'
                   MOVE 20.00 TO TCCAL-COMMISSION-YTD
               WHEN OTHER
                   COMPUTE TCCAL-COMMISSION-YTD ROUNDED = WS-WORK-AMT-2
                       * TCCAL-RATE / 100
           END-EVALUATE
           IF TCCAL-COMMISSION-YTD > 0 AND TCCAL-COMMISSION-YTD < 1.00
               MOVE 1.00 TO TCCAL-COMMISSION-YTD
           END-IF
           IF TCCAL-COMMISSION-YTD > 25.00
               MOVE 25.00 TO TCCAL-COMMISSION-YTD
           END-IF
           ADD TCCAL-COMMISSION-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2410-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TCCAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO TCCAL-CONSIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2420-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-MATURITY-DATE > 0 AND TCCAL-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCCAL-CONSIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TCCAL-CONSIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TCCAL-CONSIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TCCAL-CONSIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCCAL-CONSIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2430-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-NEXT-DUE-DATE > 0 AND TCCAL-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-NEXT-DUE-DATE TO DTU-DATE-1
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
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EG -> HZ WHEN EFFECTIVE DATE IS PAST
      * HZ -> JF WHEN EFFECTIVE DATE IS PAST
      * JF -> NZ WHEN EFFECTIVE DATE IS PAST
      * NZ -> EG WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2440-SET-BRANCH.
           EVALUATE TCCAL-BRANCH-CD
               WHEN 'EG'
                   IF TCCAL-EFFECTIVE-DATE > 0 AND TCCAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HZ' TO TCCAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HZ'
                   IF TCCAL-EFFECTIVE-DATE > 0 AND TCCAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JF' TO TCCAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF TCCAL-EFFECTIVE-DATE > 0 AND TCCAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NZ' TO TCCAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF TCCAL-EFFECTIVE-DATE > 0 AND TCCAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EG' TO TCCAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2450-COUNT-EXCEPT.
           IF TCCAL-EXCEPT-CNT < 9999999
               ADD 1 TO TCCAL-EXCEPT-CNT
           END-IF
           IF TCCAL-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TCCAL-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2460-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-MATURITY-DATE > 0 AND TCCAL-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCCAL-SHORTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD TCCAL-SHORTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TCCAL-SHORTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TCCAL-SHORTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCCAL-SHORTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2470-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-EFFECTIVE-DATE > 0 AND TCCAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2480-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TCCAL-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCCAL-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TCCAL-SHORTAGE-AMT *
                       TCCAL-FEE-PCT / 200
               WHEN TCCAL-ISSUER-CD = 'AF'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN TCCAL-ISSUER-CD = 'CJ'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TCCAL-SHORTAGE-AMT *
                       TCCAL-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CONSIGN AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2490-LIMIT-CONSIGN.
           IF TCCAL-CONSIGN-AMT < 0.00
               MOVE 0.00 TO TCCAL-CONSIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TCCAL-CONSIGN-AMT > 2500.00
               MOVE 2500.00 TO TCCAL-CONSIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCCAL-SOURCE-CD
                   ADD TCCAL-SHORTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCCAL-SHORTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2510-ACCRUE-CONSIGN.
           IF TCCAL-CONSIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCCAL-CONSIGN-AMT * TCCAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TCCAL-COMMISSION-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2520-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE TCCAL-COMMISSION-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CONSIGN YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2530-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TCCAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCCAL-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TCCAL-CONSIGN-YTD *
                       TCCAL-FEE-PCT / 200
               WHEN TCCAL-ISSUER-CD = 'AF'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN TCCAL-ISSUER-CD = 'CJ'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TCCAL-CONSIGN-YTD *
                       TCCAL-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2540-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE TCCAL-SHORTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE COMMISSION AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2550-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TCCAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCCAL-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TCCAL-COMMISSION-AMT *
                       TCCAL-RATE / 200
               WHEN TCCAL-SOURCE-CD = 'EG'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN TCCAL-SOURCE-CD = 'GH'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TCCAL-COMMISSION-AMT *
                       TCCAL-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2560-ACCRUE-SHORTAGE.
           IF TCCAL-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCCAL-SHORTAGE-AMT * TCCAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO TCCAL-COMMISSION-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E112 NAME REQUIRED
      * E998 CONSIGN YTD NEGATIVE
      * E437 CONSIGN YTD OVER 25000
      * E118 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2570-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCCAL-NAME = SPACES
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN TCCAL-CONSIGN-YTD < 0
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN TCCAL-CONSIGN-YTD > 25000.00
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN TCCAL-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E118' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-SHORTAGE.
           IF TCCAL-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCCAL-SHORTAGE-AMT * TCCAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO TCCAL-COMMISSION-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-SHORTAGE.
           IF TCCAL-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCCAL-SHORTAGE-AMT * TCCAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO TCCAL-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COMMISSION YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2600-FLAG-COMMISSION.
           IF TCCAL-COMMISSION-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TCCAL-COMMISSION-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2610-LIMIT-CONSIGN.
           IF TCCAL-CONSIGN-YTD < 100.00
               MOVE 100.00 TO TCCAL-CONSIGN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TCCAL-CONSIGN-YTD > 50000.00
               MOVE 50000.00 TO TCCAL-CONSIGN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2620-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = TCCAL-SHORTAGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TCCAL-SHORTAGE-YTD - (WS-WORK-AMT *
               4)
           COMPUTE TCCAL-COMMISSION-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2630-DESCRIBE-BRANCH.
           EVALUATE TCCAL-BRANCH-CD
               WHEN 'EG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2640-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * G -> M WHEN EFFECTIVE DATE IS PAST
      * M -> N WHEN EFFECTIVE DATE IS PAST
      * N -> P WHEN EFFECTIVE DATE IS PAST
      * P -> V WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2650-SET-DENOM.
           EVALUATE TCCAL-DENOM-CD
               WHEN 'G'
                   IF TCCAL-EFFECTIVE-DATE > 0 AND TCCAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO TCCAL-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TCCAL-EFFECTIVE-DATE > 0 AND TCCAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'N' TO TCCAL-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TCCAL-EFFECTIVE-DATE > 0 AND TCCAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'P' TO TCCAL-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TCCAL-EFFECTIVE-DATE > 0 AND TCCAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO TCCAL-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E263 ADDR REQUIRED
      * E650 COMMISSION YTD NEGATIVE
      * E498 COMMISSION YTD OVER 1000000
      * E176 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2660-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCCAL-ADDR = SPACES
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN TCCAL-COMMISSION-YTD < 0
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN TCCAL-COMMISSION-YTD > 1000000.00
                   MOVE 'E498' TO WS-REJECT-CODE
               WHEN TCCAL-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2670-LIMIT-SHORTAGE.
           IF TCCAL-SHORTAGE-YTD < 0.00
               MOVE 0.00 TO TCCAL-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TCCAL-SHORTAGE-YTD > 500.00
               MOVE 500.00 TO TCCAL-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COMMISSION AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2680-ACCRUE-COMMISSION.
           IF TCCAL-COMMISSION-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCCAL-COMMISSION-AMT * TCCAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO TCCAL-CONSIGN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E477 ADDR REQUIRED
      * E881 COMMISSION AMT NEGATIVE
      * E247 COMMISSION AMT OVER 25000
      * E434 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2690-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCCAL-ADDR = SPACES
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN TCCAL-COMMISSION-AMT < 0
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN TCCAL-COMMISSION-AMT > 25000.00
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN TCCAL-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E434' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2700-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TCCAL-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TCCAL-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2710-DESCRIBE-BRANCH.
           EVALUATE TCCAL-BRANCH-CD
               WHEN 'EG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2720-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-GRID-RATE(1) TO TCCAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-FACE-GRID-LOW(WS-TX)
                   MOVE WS-FACE-GRID-RATE(WS-TX) TO TCCAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCCAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCCAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E109 NAME REQUIRED
      * E397 SHORTAGE AMT NEGATIVE
      * E880 SHORTAGE AMT OVER 250000
      * E507 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2730-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCCAL-NAME = SPACES
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN TCCAL-SHORTAGE-AMT < 0
                   MOVE 'E397' TO WS-REJECT-CODE
               WHEN TCCAL-SHORTAGE-AMT > 250000.00
                   MOVE 'E880' TO WS-REJECT-CODE
               WHEN TCCAL-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EG -> HZ WHEN MATURITY DATE IS PAST
      * HZ -> JF WHEN MATURITY DATE IS PAST
      * JF -> NZ WHEN MATURITY DATE IS PAST
      * NZ -> EG WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2740-SET-BRANCH.
           EVALUATE TCCAL-BRANCH-CD
               WHEN 'EG'
                   IF TCCAL-MATURITY-DATE > 0 AND TCCAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HZ' TO TCCAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HZ'
                   IF TCCAL-MATURITY-DATE > 0 AND TCCAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO TCCAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF TCCAL-MATURITY-DATE > 0 AND TCCAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO TCCAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF TCCAL-MATURITY-DATE > 0 AND TCCAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EG' TO TCCAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2750-DESCRIBE-DENOM.
           EVALUATE TCCAL-DENOM-CD
               WHEN 'G'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2760-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TCCAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO TCCAL-CONSIGN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TCCAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TCCAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TCCAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TCCAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
