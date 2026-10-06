      *================================================================*
      * PROGRAM  : ATFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : ATM NETWORK SETTLEMENT RULE - PRICING. CALLED BY THE*
      *            ATM NETWORK SETTLEMENT BATCH AND ONLINE PROGRAMS.   *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-14 MKL  ORIGINAL                                       *
      * 1996-02-25 SKN  DISPENSED ROUNDING - HALF UP PRB2853           *
      * 1996-03-13 EWS  SETTLE ROUNDING - HALF UP REQ9255              *
      * 1998-01-27 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-09-16 MKL  DISPENSED NOTICE TEXT PER LEGAL REQ1147        *
      * 2001-11-19 LMB  PERFORMANCE - DISPENSED READ AHEAD INC3269     *
      * 2002-09-16 TWH  AUDIT FINDING - CASSETTE CONTROL TOTALS CHG4195*
      * 2002-07-26 EWS  ADJUST EXCEPTIONS TO OPS QUEUE REQ7537         *
      * 2004-12-02 TWH  SURCHG CALCULATION CORRECTED INC8503           *
      * 2004-10-17 TWH  NEW ADJUST REPORT COLUMNS REQ5967              *
      * 2010-10-17 LMB  NEW CASSETTE REPORT COLUMNS CHG3175            *
      * 2012-10-07 LMB  DISPENSED STATUS CODES ADDED INC4686           *
      * 2014-03-22 GAF  SURCHG CALCULATION CORRECTED PRB1074           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATFEE.
       AUTHOR. M K LOWE.

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
                                           VALUE 25.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 91.
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
      * WS-SURCHG-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURCHG-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000273500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000407500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000457500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006250000000708000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000626000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021250000000554000'.
       01  WS-SURCHG-TIER-TBL REDEFINES WS-SURCHG-TIER-VALUES.
           05  WS-SURCHG-TIER-ROW OCCURS 6 TIMES.
               10  WS-SURCHG-TIER-LOW      PIC 9(09)V99.
               10  WS-SURCHG-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTERCHG-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTERCHG-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000181000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000542500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000501000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000777500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000267500'.
       01  WS-INTERCHG-GRID-TBL REDEFINES WS-INTERCHG-GRID-VALUES.
           05  WS-INTERCHG-GRID-ROW OCCURS 5 TIMES.
               10  WS-INTERCHG-GRID-LOW    PIC 9(09)V99.
               10  WS-INTERCHG-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ATFEEP.

       PROCEDURE DIVISION USING ATFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SET-TERMINAL
           PERFORM 2010-PRORATE-DISPENSED
           PERFORM 2020-AGE-NOTICE
           PERFORM 2030-ASSESS-REVIEW
           PERFORM 2040-AGE-LAST-PAY
           PERFORM 2050-DESCRIBE-REGION
           PERFORM 2060-DESCRIBE-REGION
           PERFORM 2070-ACCUM-BRANCH
           PERFORM 2080-ACCRUE-FEE-PCT
           PERFORM 2090-COUNT-RETRY
           PERFORM 2100-SET-REGION
           PERFORM 2110-DESCRIBE-REGION
           PERFORM 2120-ACCUM-STATUS
           PERFORM 2130-FLAG-ITEM
           PERFORM 2140-SELECT-DISCOUNT
           PERFORM 2150-ACCRUE-SETTLE
           PERFORM 2160-AGE-OPEN
           PERFORM 2170-COUNT-RETRY
           PERFORM 2180-ACCRUE-ADJUST
           PERFORM 2190-ACCUM-TERMINAL
           PERFORM 2200-AGE-NOTICE
           PERFORM 2210-COUNT-RETRY
           PERFORM 2220-EDIT-REF-NO
           PERFORM 2230-SPLIT-INTERCHG
           PERFORM 2240-ACCRUE-INTERCHG
           PERFORM 2250-FLAG-DISPENSED
           PERFORM 2260-FLAG-SETTLE
           PERFORM 2270-COUNT-RETRY
           PERFORM 2280-LIMIT-ITEM
           PERFORM 2290-ACCRUE-DISPENSED
           PERFORM 2300-SET-BRANCH
           PERFORM 2310-SET-BRANCH
           PERFORM 2320-LIMIT-SETTLE
           PERFORM 2330-ACCUM-REGION
           PERFORM 2340-SELECT-FEE-PCT
           PERFORM 2350-LIMIT-SETTLE
           PERFORM 2360-ACCUM-TERMINAL
           PERFORM 2370-CHECK-MATURITY
           PERFORM 2380-SELECT-FEE-PCT
           PERFORM 2390-FLAG-ITEM
           PERFORM 2400-ASSESS-STOP
           PERFORM 2410-LIMIT-ADJUST
           PERFORM 2420-FLAG-DISPENSED
           PERFORM 2430-DESCRIBE-REGION
           PERFORM 2440-CHECK-NOTICE
           PERFORM 2450-LIMIT-ITEM
           PERFORM 2460-SELECT-FEE-PCT
           PERFORM 2470-SELECT-FEE-PCT
           PERFORM 2480-FLAG-CASSETTE
           PERFORM 2490-COUNT-NOTICE
           PERFORM 2500-CHECK-NOTICE
           PERFORM 2510-LIMIT-SETTLE
           PERFORM 2520-ACCRUE-FEE-PCT
           PERFORM 2530-SELECT-FEE-PCT
           PERFORM 2540-DESCRIBE-STATUS
           PERFORM 2550-SET-BRANCH
           PERFORM 2560-EDIT-REF-NO
           PERFORM 2570-SPLIT-ADJUST
           PERFORM 2580-SPLIT-INTERCHG
           PERFORM 2590-COUNT-RETRY
           PERFORM 2600-PRORATE-SETTLE
           PERFORM 2610-AGE-OPEN
           PERFORM 2620-COUNT-RETRY
           PERFORM 2630-CHECK-OPEN
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ATFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN OPEN DATE IS PAST
      * JJ -> JZ WHEN OPEN DATE IS PAST
      * JZ -> KK WHEN OPEN DATE IS PAST
      * KK -> BJ WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2000-SET-TERMINAL.
           EVALUATE ATFEE-TERMINAL-CD
               WHEN 'BJ'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JJ' TO ATFEE-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JJ'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO ATFEE-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KK' TO ATFEE-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KK'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO ATFEE-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISPENSED YTD.
      *----------------------------------------------------------------*
       2010-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATFEE-DISPENSED-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATFEE-DISPENSED-YTD
           END-IF
           ADD ATFEE-DISPENSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2020-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATFEE-NOTICE-DATE > 0 AND ATFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATFEE-INTERCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATFEE-INTERCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATFEE-INTERCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATFEE-INTERCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATFEE-INTERCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CASSETTE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2030-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATFEE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATFEE-CASSETTE-AMT *
                       ATFEE-FEE-PCT / 200
               WHEN ATFEE-BRANCH-CD = 'DU'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN ATFEE-BRANCH-CD = 'HX'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATFEE-CASSETTE-AMT *
                       ATFEE-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2040-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ATFEE-LAST-PAY-DATE > 0 AND ATFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATFEE-SETTLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATFEE-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATFEE-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATFEE-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATFEE-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2050-DESCRIBE-REGION.
           EVALUATE ATFEE-REGION-CD
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'G'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2060-DESCRIBE-REGION.
           EVALUATE ATFEE-REGION-CD
               WHEN 'F'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'G'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2070-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATFEE-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATFEE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2080-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ATFEE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2090-COUNT-RETRY.
           IF ATFEE-RETRY-CNT < 9999999
               ADD 1 TO ATFEE-RETRY-CNT
           END-IF
           IF ATFEE-RETRY-CNT NOT < 3
               MOVE 'Y' TO ATFEE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN OPEN DATE IS PAST
      * G -> H WHEN OPEN DATE IS PAST
      * H -> P WHEN OPEN DATE IS PAST
      * P -> V WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2100-SET-REGION.
           EVALUATE ATFEE-REGION-CD
               WHEN 'F'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ATFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO ATFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ATFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2110-DESCRIBE-REGION.
           EVALUATE ATFEE-REGION-CD
               WHEN 'F'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'G'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2120-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATFEE-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATFEE-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2130-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTERCHG GRID TIER FOR THE DISPENSED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2140-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INTERCHG-GRID-RATE(1) TO ATFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ATFEE-DISPENSED-YTD NOT < WS-INTERCHG-GRID-LOW(WS-TX)
                   MOVE WS-INTERCHG-GRID-RATE(WS-TX) TO ATFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2150-ACCRUE-SETTLE.
           IF ATFEE-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATFEE-SETTLE-AMT * ATFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2160-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATFEE-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATFEE-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATFEE-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATFEE-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATFEE-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2170-COUNT-RETRY.
           IF ATFEE-RETRY-CNT < 9999999
               ADD 1 TO ATFEE-RETRY-CNT
           END-IF
           IF ATFEE-RETRY-CNT NOT < 3
               MOVE 'Y' TO ATFEE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2180-ACCRUE-ADJUST.
           IF ATFEE-ADJUST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATFEE-ADJUST-YTD * ATFEE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2190-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATFEE-TERMINAL-CD
                   ADD ATFEE-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATFEE-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATFEE-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2200-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATFEE-NOTICE-DATE > 0 AND ATFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATFEE-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATFEE-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATFEE-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATFEE-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATFEE-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2210-COUNT-RETRY.
           IF ATFEE-RETRY-CNT < 9999999
               ADD 1 TO ATFEE-RETRY-CNT
           END-IF
           IF ATFEE-RETRY-CNT NOT < 6
               MOVE 'Y' TO ATFEE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E799 REF NO REQUIRED
      * E244 SETTLE YTD NEGATIVE
      * E220 SETTLE YTD OVER 1000000
      * E716 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2220-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATFEE-REF-NO = SPACES
                   MOVE 'E799' TO WS-REJECT-CODE
               WHEN ATFEE-SETTLE-YTD < 0
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN ATFEE-SETTLE-YTD > 1000000.00
                   MOVE 'E220' TO WS-REJECT-CODE
               WHEN ATFEE-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTERCHG YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2230-SPLIT-INTERCHG.
           COMPUTE WS-WORK-AMT = ATFEE-INTERCHG-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ATFEE-INTERCHG-YTD - (WS-WORK-AMT *
               4)
           COMPUTE ATFEE-CASSETTE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTERCHG YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2240-ACCRUE-INTERCHG.
           IF ATFEE-INTERCHG-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATFEE-INTERCHG-YTD * ATFEE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ATFEE-DISPENSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2250-FLAG-DISPENSED.
           IF ATFEE-DISPENSED-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATFEE-DISPENSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2260-FLAG-SETTLE.
           IF ATFEE-SETTLE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATFEE-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2270-COUNT-RETRY.
           IF ATFEE-RETRY-CNT < 9999999
               ADD 1 TO ATFEE-RETRY-CNT
           END-IF
           IF ATFEE-RETRY-CNT NOT < 5
               MOVE 'Y' TO ATFEE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2280-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2290-ACCRUE-DISPENSED.
           IF ATFEE-DISPENSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATFEE-DISPENSED-YTD * ATFEE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATFEE-CASSETTE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN OPEN DATE IS PAST
      * HX -> YC WHEN OPEN DATE IS PAST
      * YC -> ZB WHEN OPEN DATE IS PAST
      * ZB -> DU WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2300-SET-BRANCH.
           EVALUATE ATFEE-BRANCH-CD
               WHEN 'DU'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN OPEN DATE IS PAST
      * HX -> YC WHEN OPEN DATE IS PAST
      * YC -> ZB WHEN OPEN DATE IS PAST
      * ZB -> DU WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-BRANCH.
           EVALUATE ATFEE-BRANCH-CD
               WHEN 'DU'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2320-LIMIT-SETTLE.
           IF ATFEE-SETTLE-YTD < 10.00
               MOVE 10.00 TO ATFEE-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATFEE-SETTLE-YTD > 500.00
               MOVE 500.00 TO ATFEE-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2330-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATFEE-REGION-CD
                   ADD ATFEE-CASSETTE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATFEE-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATFEE-CASSETTE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTERCHG GRID TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2340-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTERCHG-GRID-RATE(1) TO ATFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ATFEE-INTERCHG-YTD NOT < WS-INTERCHG-GRID-LOW(WS-TX)
                   MOVE WS-INTERCHG-GRID-RATE(WS-TX) TO ATFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2350-LIMIT-SETTLE.
           IF ATFEE-SETTLE-AMT < 10.00
               MOVE 10.00 TO ATFEE-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATFEE-SETTLE-AMT > 500.00
               MOVE 500.00 TO ATFEE-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE AMT BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2360-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATFEE-TERMINAL-CD
                   ADD ATFEE-CASSETTE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATFEE-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATFEE-CASSETTE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2370-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATFEE-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATFEE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ATFEE-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG TIER TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2380-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-TIER-RATE(1) TO ATFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ATFEE-SETTLE-AMT NOT < WS-SURCHG-TIER-LOW(WS-TX)
                   MOVE WS-SURCHG-TIER-RATE(WS-TX) TO ATFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2390-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE INTERCHG YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2400-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATFEE-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATFEE-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ATFEE-INTERCHG-YTD *
                       ATFEE-DISCOUNT / 200
               WHEN ATFEE-TERMINAL-CD = 'BJ'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN ATFEE-TERMINAL-CD = 'JJ'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ATFEE-INTERCHG-YTD *
                       ATFEE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ADJUST YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2410-LIMIT-ADJUST.
           IF ATFEE-ADJUST-YTD < 10.00
               MOVE 10.00 TO ATFEE-ADJUST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATFEE-ADJUST-YTD > 2500.00
               MOVE 2500.00 TO ATFEE-ADJUST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2420-FLAG-DISPENSED.
           IF ATFEE-DISPENSED-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATFEE-DISPENSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2430-DESCRIBE-REGION.
           EVALUATE ATFEE-REGION-CD
               WHEN 'F'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'G'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'V'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATFEE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ATFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2450-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-TIER-RATE(1) TO ATFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-SURCHG-TIER-LOW(WS-TX)
                   MOVE WS-SURCHG-TIER-RATE(WS-TX) TO ATFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTERCHG GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTERCHG-GRID-RATE(1) TO ATFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-INTERCHG-GRID-LOW(WS-TX)
                   MOVE WS-INTERCHG-GRID-RATE(WS-TX) TO ATFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2480-FLAG-CASSETTE.
           IF ATFEE-CASSETTE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATFEE-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2490-COUNT-NOTICE.
           IF ATFEE-NOTICE-CNT < 9999999
               ADD 1 TO ATFEE-NOTICE-CNT
           END-IF
           IF ATFEE-NOTICE-CNT NOT < 5
               MOVE 'Y' TO ATFEE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2500-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATFEE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ATFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2510-LIMIT-SETTLE.
           IF ATFEE-SETTLE-YTD < 10.00
               MOVE 10.00 TO ATFEE-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATFEE-SETTLE-YTD > 1000.00
               MOVE 1000.00 TO ATFEE-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ATFEE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATFEE-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG TIER TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-TIER-RATE(1) TO ATFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ATFEE-INTERCHG-YTD NOT < WS-SURCHG-TIER-LOW(WS-TX)
                   MOVE WS-SURCHG-TIER-RATE(WS-TX) TO ATFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2540-DESCRIBE-STATUS.
           EVALUATE ATFEE-STATUS-CD
               WHEN 'DF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN OPEN DATE IS PAST
      * HX -> YC WHEN OPEN DATE IS PAST
      * YC -> ZB WHEN OPEN DATE IS PAST
      * ZB -> DU WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2550-SET-BRANCH.
           EVALUATE ATFEE-BRANCH-CD
               WHEN 'DU'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E871 REF NO REQUIRED
      * E107 SETTLE YTD NEGATIVE
      * E587 SETTLE YTD OVER 10000
      * E696 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2560-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATFEE-REF-NO = SPACES
                   MOVE 'E871' TO WS-REJECT-CODE
               WHEN ATFEE-SETTLE-YTD < 0
                   MOVE 'E107' TO WS-REJECT-CODE
               WHEN ATFEE-SETTLE-YTD > 10000.00
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN ATFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E696' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2570-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATFEE-ADJUST-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ATFEE-ADJUST-YTD - (WS-WORK-AMT * 2)
           COMPUTE ATFEE-DISPENSED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTERCHG YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2580-SPLIT-INTERCHG.
           COMPUTE WS-WORK-AMT = ATFEE-INTERCHG-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ATFEE-INTERCHG-YTD - (WS-WORK-AMT *
               4)
           COMPUTE ATFEE-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-RETRY.
           IF ATFEE-RETRY-CNT < 9999999
               ADD 1 TO ATFEE-RETRY-CNT
           END-IF
           IF ATFEE-RETRY-CNT NOT < 5
               MOVE 'Y' TO ATFEE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2600-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATFEE-SETTLE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2610-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATFEE-OPEN-DATE > 0 AND ATFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATFEE-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ATFEE-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATFEE-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATFEE-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATFEE-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2620-COUNT-RETRY.
           IF ATFEE-RETRY-CNT < 9999999
               ADD 1 TO ATFEE-RETRY-CNT
           END-IF
           IF ATFEE-RETRY-CNT NOT < 6
               MOVE 'Y' TO ATFEE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATFEE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ATFEE-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ATFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ATFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ATFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ATFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
