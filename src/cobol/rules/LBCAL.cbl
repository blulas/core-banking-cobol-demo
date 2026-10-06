      *================================================================*
      * PROGRAM  : LBCAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : LOCKBOX REMITTANCE RULE - DATE RULES. CALLED BY THE *
      *            LOCKBOX REMITTANCE BATCH AND ONLINE PROGRAMS.       *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-06-06 GAF  ORIGINAL                                       *
      * 1995-07-24 JLM  NEW FEE REPORT COLUMNS REQ4597                 *
      * 1995-07-20 EWS  FLOAT LIMIT REVISED REQ0459                    *
      * 2002-06-28 MKL  NEW CHECK REPORT COLUMNS PRB8019               *
      * 2013-01-05 EWS  AUDIT FINDING - REMIT CONTROL TOTALS REQ8005   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBCAL.
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
                                           VALUE 9.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 90.
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
      * WS-DEPOSIT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEPOSIT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000452000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000039000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000594500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000462000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000676000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012250000000211500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022250000000852000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000601000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022550000000398000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022650000000107000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023650000000326000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026150000000414000'.
       01  WS-DEPOSIT-TIER-TBL REDEFINES WS-DEPOSIT-TIER-VALUES.
           05  WS-DEPOSIT-TIER-ROW OCCURS 12 TIMES.
               10  WS-DEPOSIT-TIER-LOW     PIC 9(09)V99.
               10  WS-DEPOSIT-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CHECK-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CHECK-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000653000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000519000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000448500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000270000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000473000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007800000000214000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000787000'.
       01  WS-CHECK-BAND-TBL REDEFINES WS-CHECK-BAND-VALUES.
           05  WS-CHECK-BAND-ROW OCCURS 7 TIMES.
               10  WS-CHECK-BAND-LOW       PIC 9(09)V99.
               10  WS-CHECK-BAND-RATE      PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY LBCALP.

       PROCEDURE DIVISION USING LBCAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CHECK-NOTICE
           PERFORM 2010-ACCRUE-CHECK
           PERFORM 2020-CHECK-OPEN
           PERFORM 2030-AGE-EFFECTIVE
           PERFORM 2040-SPLIT-CHECK
           PERFORM 2050-DESCRIBE-LOCKBOX
           PERFORM 2060-CHECK-EFFECTIVE
           PERFORM 2070-CHECK-OPEN
           PERFORM 2080-COUNT-EXCEPT
           PERFORM 2090-AGE-NEXT-DUE
           PERFORM 2100-ASSESS-PREFERRED
           PERFORM 2110-LIMIT-DEPOSIT
           PERFORM 2120-COUNT-EXCEPT
           PERFORM 2130-FLAG-REMIT
           PERFORM 2140-ACCRUE-CASH-ON-HAND
           PERFORM 2150-FLAG-CASH-ON-HAND
           PERFORM 2160-ASSESS-HOLD
           PERFORM 2170-ACCUM-REGION
           PERFORM 2180-COUNT-RETRY
           PERFORM 2190-SELECT-DISCOUNT
           PERFORM 2200-ACCUM-BATCH-TYPE
           PERFORM 2210-ACCUM-REASON
           PERFORM 2220-SET-LOCKBOX
           PERFORM 2230-COUNT-RETRY
           PERFORM 2240-PRORATE-FLOAT
           PERFORM 2250-SPLIT-DEPOSIT
           PERFORM 2260-ACCUM-REGION
           PERFORM 2270-LIMIT-REMIT
           PERFORM 2280-ACCUM-REGION
           PERFORM 2290-ACCUM-REASON
           PERFORM 2300-SET-REGION
           PERFORM 2310-ASSESS-HOLD
           PERFORM 2320-SELECT-PENALTY-PCT
           PERFORM 2330-DESCRIBE-REGION
           PERFORM 2340-CHECK-NEXT-DUE
           PERFORM 2350-EDIT-CONTACT
           PERFORM 2360-AGE-NEXT-DUE
           PERFORM 2370-AGE-OPEN
           PERFORM 2380-LIMIT-FLOAT
           PERFORM 2390-PRORATE-CASH-ON-HAND
           PERFORM 2400-DESCRIBE-REGION
           PERFORM 2410-SELECT-PENALTY-PCT
           PERFORM 2420-SELECT-DISCOUNT
           PERFORM 2430-SELECT-DISCOUNT
           PERFORM 2440-ASSESS-HOLD
           PERFORM 2450-EDIT-CONTACT
           PERFORM 2460-SELECT-PENALTY-PCT
           PERFORM 2470-EDIT-CONTACT
           PERFORM 2480-CHECK-OPEN
           PERFORM 2490-EDIT-CONTACT
           PERFORM 2500-PRORATE-FLOAT
           PERFORM 2510-COUNT-RETRY
           PERFORM 2520-PRORATE-CHECK
           PERFORM 2530-ASSESS-HOLD
           PERFORM 2540-SELECT-PENALTY-PCT
           PERFORM 2550-PRORATE-REMIT
           PERFORM 2560-ASSESS-PREFERRED
           PERFORM 2570-CHECK-EFFECTIVE
           PERFORM 2580-AGE-NOTICE
           PERFORM 2590-SET-BATCH-TYPE
           PERFORM 2600-CHECK-NEXT-DUE
           PERFORM 2610-EDIT-DESC
           PERFORM 2620-ASSESS-PREFERRED
           PERFORM 2630-DESCRIBE-LOCKBOX
           PERFORM 2640-ASSESS-HOLD
           PERFORM 2650-ACCRUE-PENALTY-PCT
           PERFORM 2660-ACCUM-REGION
           PERFORM 2670-EDIT-DESC
           PERFORM 2680-ACCRUE-CHECK
           PERFORM 2690-CHECK-NEXT-DUE
           PERFORM 2700-SPLIT-REMIT
           PERFORM 2710-DESCRIBE-REGION
           PERFORM 2720-SELECT-PENALTY-PCT
           PERFORM 2730-ASSESS-HOLD
           PERFORM 2740-SELECT-PENALTY-PCT
           PERFORM 2750-COUNT-EXCEPT
           PERFORM 2760-SPLIT-DEPOSIT
           PERFORM 2770-LIMIT-CASH-ON-HAND
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO LBCAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2000-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO LBCAL-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHECK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2010-ACCRUE-CHECK.
           IF LBCAL-CHECK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBCAL-CHECK-AMT * LBCAL-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2020-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO LBCAL-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-EFFECTIVE-DATE > 0 AND LBCAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBCAL-DEPOSIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBCAL-DEPOSIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBCAL-DEPOSIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBCAL-DEPOSIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBCAL-DEPOSIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CHECK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2040-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBCAL-CHECK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBCAL-CHECK-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBCAL-REMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOCKBOX CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2050-DESCRIBE-LOCKBOX.
           EVALUATE LBCAL-LOCKBOX-CD
               WHEN 'DX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2060-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO LBCAL-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2070-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO LBCAL-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-EXCEPT.
           IF LBCAL-EXCEPT-CNT < 9999999
               ADD 1 TO LBCAL-EXCEPT-CNT
           END-IF
           IF LBCAL-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO LBCAL-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2090-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-NEXT-DUE-DATE > 0 AND LBCAL-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBCAL-CHECK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD LBCAL-CHECK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD LBCAL-CHECK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBCAL-CHECK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBCAL-CHECK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE CHECK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2100-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBCAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO LBCAL-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBCAL-HOLD-FLAG = 'Y'
                   COMPUTE LBCAL-DEPOSIT-AMT ROUNDED = LBCAL-CHECK-AMT *
                       LBCAL-PENALTY-PCT / 200
               WHEN LBCAL-LOCKBOX-CD = 'DX'
                   MOVE 5.00 TO LBCAL-DEPOSIT-AMT
               WHEN LBCAL-LOCKBOX-CD = 'JM'
                   MOVE 12.00 TO LBCAL-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE LBCAL-DEPOSIT-AMT ROUNDED = LBCAL-CHECK-AMT *
                       LBCAL-PENALTY-PCT / 100
           END-EVALUATE
           IF LBCAL-DEPOSIT-AMT > 0 AND LBCAL-DEPOSIT-AMT < 5.00
               MOVE 5.00 TO LBCAL-DEPOSIT-AMT
           END-IF
           IF LBCAL-DEPOSIT-AMT > 150.00
               MOVE 150.00 TO LBCAL-DEPOSIT-AMT
           END-IF
           ADD LBCAL-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2110-LIMIT-DEPOSIT.
           IF LBCAL-DEPOSIT-YTD < 100.00
               MOVE 100.00 TO LBCAL-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBCAL-DEPOSIT-YTD > 5000.00
               MOVE 5000.00 TO LBCAL-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-EXCEPT.
           IF LBCAL-EXCEPT-CNT < 9999999
               ADD 1 TO LBCAL-EXCEPT-CNT
           END-IF
           IF LBCAL-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO LBCAL-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REMIT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2130-FLAG-REMIT.
           IF LBCAL-REMIT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBCAL-REMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ON HAND YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2140-ACCRUE-CASH-ON-HAND.
           IF LBCAL-CASH-ON-HAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBCAL-CASH-ON-HAND-YTD * LBCAL-PENALTY-PCT / 100 /
                       360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2150-FLAG-CASH-ON-HAND.
           IF LBCAL-CASH-ON-HAND-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBCAL-CASH-ON-HAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT YTD FEE ON THE CASH ON HAND YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2160-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBCAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBCAL-DEPOSIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBCAL-PREFERRED-FLAG = 'Y'
                   COMPUTE LBCAL-DEPOSIT-YTD ROUNDED =
                       LBCAL-CASH-ON-HAND-YTD * LBCAL-DISCOUNT / 200
               WHEN LBCAL-REGION-CD = 'DVP'
                   MOVE 5.00 TO LBCAL-DEPOSIT-YTD
               WHEN LBCAL-REGION-CD = 'FMZ'
                   MOVE 10.00 TO LBCAL-DEPOSIT-YTD
               WHEN OTHER
                   COMPUTE LBCAL-DEPOSIT-YTD ROUNDED =
                       LBCAL-CASH-ON-HAND-YTD * LBCAL-DISCOUNT / 100
           END-EVALUATE
           IF LBCAL-DEPOSIT-YTD > 0 AND LBCAL-DEPOSIT-YTD < 1.00
               MOVE 1.00 TO LBCAL-DEPOSIT-YTD
           END-IF
           IF LBCAL-DEPOSIT-YTD > 100.00
               MOVE 100.00 TO LBCAL-DEPOSIT-YTD
           END-IF
           ADD LBCAL-DEPOSIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2170-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBCAL-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBCAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2180-COUNT-RETRY.
           IF LBCAL-RETRY-CNT < 9999999
               ADD 1 TO LBCAL-RETRY-CNT
           END-IF
           IF LBCAL-RETRY-CNT NOT < 6
               MOVE 'Y' TO LBCAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2190-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO LBCAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF LBCAL-DEPOSIT-AMT NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO LBCAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBCAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBCAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT YTD BY BATCH TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2200-ACCUM-BATCH-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBCAL-BATCH-TYPE-CD
                   ADD LBCAL-DEPOSIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBCAL-BATCH-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBCAL-DEPOSIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2210-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBCAL-REASON-CD
                   ADD LBCAL-REMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBCAL-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBCAL-REMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN OPEN DATE IS PAST
      * JM -> WW WHEN OPEN DATE IS PAST
      * WW -> XE WHEN OPEN DATE IS PAST
      * XE -> DX WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2220-SET-LOCKBOX.
           EVALUATE LBCAL-LOCKBOX-CD
               WHEN 'DX'
                   IF LBCAL-OPEN-DATE > 0 AND LBCAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBCAL-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBCAL-OPEN-DATE > 0 AND LBCAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBCAL-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBCAL-OPEN-DATE > 0 AND LBCAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBCAL-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBCAL-OPEN-DATE > 0 AND LBCAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBCAL-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-RETRY.
           IF LBCAL-RETRY-CNT < 9999999
               ADD 1 TO LBCAL-RETRY-CNT
           END-IF
           IF LBCAL-RETRY-CNT NOT < 3
               MOVE 'Y' TO LBCAL-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FLOAT YTD.
      *----------------------------------------------------------------*
       2240-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBCAL-FLOAT-YTD ROUNDED =
                   LBCAL-CASH-ON-HAND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBCAL-FLOAT-YTD
           END-IF
           ADD LBCAL-FLOAT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2250-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE LBCAL-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2260-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBCAL-REGION-CD
                   ADD LBCAL-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBCAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBCAL-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * REMIT YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2270-LIMIT-REMIT.
           IF LBCAL-REMIT-YTD < 0.00
               MOVE 0.00 TO LBCAL-REMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBCAL-REMIT-YTD > 500.00
               MOVE 500.00 TO LBCAL-REMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2280-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBCAL-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBCAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBCAL-REASON-CD
                   ADD LBCAL-REMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBCAL-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBCAL-REMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DVP -> FMZ WHEN NOTICE DATE IS PAST
      * FMZ -> KSL WHEN NOTICE DATE IS PAST
      * KSL -> NYK WHEN NOTICE DATE IS PAST
      * NYK -> DVP WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2300-SET-REGION.
           EVALUATE LBCAL-REGION-CD
               WHEN 'DVP'
                   IF LBCAL-NOTICE-DATE > 0 AND LBCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FMZ' TO LBCAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FMZ'
                   IF LBCAL-NOTICE-DATE > 0 AND LBCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KSL' TO LBCAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KSL'
                   IF LBCAL-NOTICE-DATE > 0 AND LBCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NYK' TO LBCAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NYK'
                   IF LBCAL-NOTICE-DATE > 0 AND LBCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DVP' TO LBCAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2310-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBCAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBCAL-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = LBCAL-DEPOSIT-AMT *
                       LBCAL-DISCOUNT / 200
               WHEN LBCAL-REASON-CD = 'GF'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN LBCAL-REASON-CD = 'UK'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = LBCAL-DEPOSIT-AMT *
                       LBCAL-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CHECK BAND TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2320-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CHECK-BAND-RATE(1) TO LBCAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBCAL-DEPOSIT-YTD NOT < WS-CHECK-BAND-LOW(WS-TX)
                   MOVE WS-CHECK-BAND-RATE(WS-TX) TO LBCAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBCAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBCAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2330-DESCRIBE-REGION.
           EVALUATE LBCAL-REGION-CD
               WHEN 'DVP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2340-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO LBCAL-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E098 CONTACT REQUIRED
      * E009 CASH ON HAND YTD NEGATIVE
      * E486 CASH ON HAND YTD OVER 25000
      * E894 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2350-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBCAL-CONTACT = SPACES
                   MOVE 'E098' TO WS-REJECT-CODE
               WHEN LBCAL-CASH-ON-HAND-YTD < 0
                   MOVE 'E009' TO WS-REJECT-CODE
               WHEN LBCAL-CASH-ON-HAND-YTD > 25000.00
                   MOVE 'E486' TO WS-REJECT-CODE
               WHEN LBCAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-NEXT-DUE-DATE > 0 AND LBCAL-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBCAL-REMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBCAL-REMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBCAL-REMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD LBCAL-REMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBCAL-REMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2370-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-OPEN-DATE > 0 AND LBCAL-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBCAL-DEPOSIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBCAL-DEPOSIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBCAL-DEPOSIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBCAL-DEPOSIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBCAL-DEPOSIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FLOAT YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2380-LIMIT-FLOAT.
           IF LBCAL-FLOAT-YTD < 10.00
               MOVE 10.00 TO LBCAL-FLOAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBCAL-FLOAT-YTD > 5000.00
               MOVE 5000.00 TO LBCAL-FLOAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2390-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   LBCAL-CASH-ON-HAND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2400-DESCRIBE-REGION.
           EVALUATE LBCAL-REGION-CD
               WHEN 'DVP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2410-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO LBCAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF LBCAL-DEPOSIT-YTD NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO LBCAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBCAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBCAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHECK BAND TIER FOR THE CASH ON HAND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CHECK-BAND-RATE(1) TO LBCAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBCAL-CASH-ON-HAND-YTD NOT < WS-CHECK-BAND-LOW(WS-TX)
                   MOVE WS-CHECK-BAND-RATE(WS-TX) TO LBCAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBCAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBCAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHECK BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2430-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CHECK-BAND-RATE(1) TO LBCAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-CHECK-BAND-LOW(WS-TX)
                   MOVE WS-CHECK-BAND-RATE(WS-TX) TO LBCAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBCAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBCAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND YTD FEE ON THE WORK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2440-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBCAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBCAL-CASH-ON-HAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBCAL-PREFERRED-FLAG = 'Y'
                   COMPUTE LBCAL-CASH-ON-HAND-YTD ROUNDED = WS-WORK-AMT
                       * LBCAL-DISCOUNT / 200
               WHEN LBCAL-LOCKBOX-CD = 'DX'
                   MOVE 20.00 TO LBCAL-CASH-ON-HAND-YTD
               WHEN LBCAL-LOCKBOX-CD = 'JM'
                   MOVE 10.00 TO LBCAL-CASH-ON-HAND-YTD
               WHEN OTHER
                   COMPUTE LBCAL-CASH-ON-HAND-YTD ROUNDED = WS-WORK-AMT
                       * LBCAL-DISCOUNT / 100
           END-EVALUATE
           IF LBCAL-CASH-ON-HAND-YTD > 0 AND LBCAL-CASH-ON-HAND-YTD <
               10.00
               MOVE 10.00 TO LBCAL-CASH-ON-HAND-YTD
           END-IF
           IF LBCAL-CASH-ON-HAND-YTD > 75.00
               MOVE 75.00 TO LBCAL-CASH-ON-HAND-YTD
           END-IF
           ADD LBCAL-CASH-ON-HAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E770 CONTACT REQUIRED
      * E847 CASH ON HAND YTD NEGATIVE
      * E359 CASH ON HAND YTD OVER 10000
      * E493 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2450-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBCAL-CONTACT = SPACES
                   MOVE 'E770' TO WS-REJECT-CODE
               WHEN LBCAL-CASH-ON-HAND-YTD < 0
                   MOVE 'E847' TO WS-REJECT-CODE
               WHEN LBCAL-CASH-ON-HAND-YTD > 10000.00
                   MOVE 'E359' TO WS-REJECT-CODE
               WHEN LBCAL-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO LBCAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO LBCAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBCAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBCAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E963 CONTACT REQUIRED
      * E417 DEPOSIT AMT NEGATIVE
      * E610 DEPOSIT AMT OVER 10000
      * E821 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2470-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBCAL-CONTACT = SPACES
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN LBCAL-DEPOSIT-AMT < 0
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN LBCAL-DEPOSIT-AMT > 10000.00
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN LBCAL-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2480-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO LBCAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E215 CONTACT REQUIRED
      * E149 DEPOSIT YTD NEGATIVE
      * E542 DEPOSIT YTD OVER 99999
      * E798 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2490-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBCAL-CONTACT = SPACES
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN LBCAL-DEPOSIT-YTD < 0
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN LBCAL-DEPOSIT-YTD > 99999.00
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN LBCAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FLOAT YTD.
      *----------------------------------------------------------------*
       2500-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBCAL-FLOAT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBCAL-FLOAT-YTD
           END-IF
           ADD LBCAL-FLOAT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2510-COUNT-RETRY.
           IF LBCAL-RETRY-CNT < 9999999
               ADD 1 TO LBCAL-RETRY-CNT
           END-IF
           IF LBCAL-RETRY-CNT NOT < 10
               MOVE 'Y' TO LBCAL-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHECK AMT.
      *----------------------------------------------------------------*
       2520-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBCAL-CHECK-AMT ROUNDED =
                   LBCAL-REMIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBCAL-CHECK-AMT
           END-IF
           ADD LBCAL-CHECK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND YTD FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2530-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBCAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBCAL-CASH-ON-HAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBCAL-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBCAL-CASH-ON-HAND-YTD ROUNDED =
                       LBCAL-DEPOSIT-YTD * LBCAL-PENALTY-PCT / 200
               WHEN LBCAL-LOCKBOX-CD = 'DX'
                   MOVE 15.00 TO LBCAL-CASH-ON-HAND-YTD
               WHEN LBCAL-LOCKBOX-CD = 'JM'
                   MOVE 12.00 TO LBCAL-CASH-ON-HAND-YTD
               WHEN OTHER
                   COMPUTE LBCAL-CASH-ON-HAND-YTD ROUNDED =
                       LBCAL-DEPOSIT-YTD * LBCAL-PENALTY-PCT / 100
           END-EVALUATE
           IF LBCAL-CASH-ON-HAND-YTD > 0 AND LBCAL-CASH-ON-HAND-YTD <
               2.00
               MOVE 2.00 TO LBCAL-CASH-ON-HAND-YTD
           END-IF
           IF LBCAL-CASH-ON-HAND-YTD > 35.00
               MOVE 35.00 TO LBCAL-CASH-ON-HAND-YTD
           END-IF
           ADD LBCAL-CASH-ON-HAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2540-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO LBCAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF LBCAL-REMIT-YTD NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO LBCAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBCAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBCAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REMIT YTD.
      *----------------------------------------------------------------*
       2550-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBCAL-REMIT-YTD ROUNDED =
                   LBCAL-DEPOSIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBCAL-REMIT-YTD
           END-IF
           ADD LBCAL-REMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CHECK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2560-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBCAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBCAL-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = LBCAL-CHECK-AMT *
                       LBCAL-DISCOUNT / 200
               WHEN LBCAL-REGION-CD = 'DVP'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN LBCAL-REGION-CD = 'FMZ'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = LBCAL-CHECK-AMT *
                       LBCAL-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO LBCAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2580-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-NOTICE-DATE > 0 AND LBCAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBCAL-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBCAL-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD LBCAL-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD LBCAL-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBCAL-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BATCH TYPE CD:
      * C -> D WHEN NOTICE DATE IS PAST
      * D -> F WHEN NOTICE DATE IS PAST
      * F -> K WHEN NOTICE DATE IS PAST
      * K -> U WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2590-SET-BATCH-TYPE.
           EVALUATE LBCAL-BATCH-TYPE-CD
               WHEN 'C'
                   IF LBCAL-NOTICE-DATE > 0 AND LBCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO LBCAL-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF LBCAL-NOTICE-DATE > 0 AND LBCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO LBCAL-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF LBCAL-NOTICE-DATE > 0 AND LBCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO LBCAL-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF LBCAL-NOTICE-DATE > 0 AND LBCAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO LBCAL-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2600-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO LBCAL-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E367 DESC REQUIRED
      * E639 WORK AMT NEGATIVE
      * E510 WORK AMT OVER 25000
      * E142 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBCAL-DESC = SPACES
                   MOVE 'E367' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E639' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN LBCAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2620-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBCAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO LBCAL-DEPOSIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBCAL-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBCAL-DEPOSIT-YTD ROUNDED = WS-WORK-AMT-2 *
                       LBCAL-PENALTY-PCT / 200
               WHEN LBCAL-REASON-CD = 'GF'
                   MOVE 20.00 TO LBCAL-DEPOSIT-YTD
               WHEN LBCAL-REASON-CD = 'UK'
                   MOVE 15.00 TO LBCAL-DEPOSIT-YTD
               WHEN OTHER
                   COMPUTE LBCAL-DEPOSIT-YTD ROUNDED = WS-WORK-AMT-2 *
                       LBCAL-PENALTY-PCT / 100
           END-EVALUATE
           IF LBCAL-DEPOSIT-YTD > 0 AND LBCAL-DEPOSIT-YTD < 1.00
               MOVE 1.00 TO LBCAL-DEPOSIT-YTD
           END-IF
           IF LBCAL-DEPOSIT-YTD > 75.00
               MOVE 75.00 TO LBCAL-DEPOSIT-YTD
           END-IF
           ADD LBCAL-DEPOSIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE LOCKBOX CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2630-DESCRIBE-LOCKBOX.
           EVALUATE LBCAL-LOCKBOX-CD
               WHEN 'DX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT YTD FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2640-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBCAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBCAL-FLOAT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBCAL-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBCAL-FLOAT-YTD ROUNDED = LBCAL-DEPOSIT-AMT *
                       LBCAL-DISCOUNT / 200
               WHEN LBCAL-REASON-CD = 'GF'
                   MOVE 3.00 TO LBCAL-FLOAT-YTD
               WHEN LBCAL-REASON-CD = 'UK'
                   MOVE 10.00 TO LBCAL-FLOAT-YTD
               WHEN OTHER
                   COMPUTE LBCAL-FLOAT-YTD ROUNDED = LBCAL-DEPOSIT-AMT *
                       LBCAL-DISCOUNT / 100
           END-EVALUATE
           IF LBCAL-FLOAT-YTD > 0 AND LBCAL-FLOAT-YTD < 1.00
               MOVE 1.00 TO LBCAL-FLOAT-YTD
           END-IF
           IF LBCAL-FLOAT-YTD > 25.00
               MOVE 25.00 TO LBCAL-FLOAT-YTD
           END-IF
           ADD LBCAL-FLOAT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2650-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * LBCAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBCAL-REMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND YTD BY REGION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2660-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBCAL-REGION-CD
                   ADD LBCAL-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBCAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBCAL-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E076 DESC REQUIRED
      * E477 REMIT YTD NEGATIVE
      * E951 REMIT YTD OVER 25000
      * E010 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2670-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBCAL-DESC = SPACES
                   MOVE 'E076' TO WS-REJECT-CODE
               WHEN LBCAL-REMIT-YTD < 0
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN LBCAL-REMIT-YTD > 25000.00
                   MOVE 'E951' TO WS-REJECT-CODE
               WHEN LBCAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E010' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHECK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2680-ACCRUE-CHECK.
           IF LBCAL-CHECK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBCAL-CHECK-AMT * LBCAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBCAL-DEPOSIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2690-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBCAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBCAL-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2700-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBCAL-REMIT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBCAL-REMIT-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBCAL-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2710-DESCRIBE-REGION.
           EVALUATE LBCAL-REGION-CD
               WHEN 'DVP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2720-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO LBCAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF LBCAL-DEPOSIT-AMT NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO LBCAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBCAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBCAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT YTD FEE ON THE REMIT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2730-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBCAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBCAL-FLOAT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBCAL-PREFERRED-FLAG = 'Y'
                   COMPUTE LBCAL-FLOAT-YTD ROUNDED = LBCAL-REMIT-YTD *
                       LBCAL-PENALTY-PCT / 200
               WHEN LBCAL-BATCH-TYPE-CD = 'C'
                   MOVE 12.00 TO LBCAL-FLOAT-YTD
               WHEN LBCAL-BATCH-TYPE-CD = 'D'
                   MOVE 12.00 TO LBCAL-FLOAT-YTD
               WHEN OTHER
                   COMPUTE LBCAL-FLOAT-YTD ROUNDED = LBCAL-REMIT-YTD *
                       LBCAL-PENALTY-PCT / 100
           END-EVALUATE
           IF LBCAL-FLOAT-YTD > 0 AND LBCAL-FLOAT-YTD < 10.00
               MOVE 10.00 TO LBCAL-FLOAT-YTD
           END-IF
           IF LBCAL-FLOAT-YTD > 150.00
               MOVE 150.00 TO LBCAL-FLOAT-YTD
           END-IF
           ADD LBCAL-FLOAT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE CHECK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2740-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO LBCAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF LBCAL-CHECK-AMT NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO LBCAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBCAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBCAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2750-COUNT-EXCEPT.
           IF LBCAL-EXCEPT-CNT < 9999999
               ADD 1 TO LBCAL-EXCEPT-CNT
           END-IF
           IF LBCAL-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO LBCAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2760-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = LBCAL-DEPOSIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBCAL-DEPOSIT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE LBCAL-CHECK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND YTD IS FLOORED AT $100.00 AND CAPPED AT
      * $50000.00.
      *----------------------------------------------------------------*
       2770-LIMIT-CASH-ON-HAND.
           IF LBCAL-CASH-ON-HAND-YTD < 100.00
               MOVE 100.00 TO LBCAL-CASH-ON-HAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBCAL-CASH-ON-HAND-YTD > 50000.00
               MOVE 50000.00 TO LBCAL-CASH-ON-HAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO LBCAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO LBCAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO LBCAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'LBCAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
