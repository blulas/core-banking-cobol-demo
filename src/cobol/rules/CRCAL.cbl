      *================================================================*
      * PROGRAM  : CRCAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PROPRIETARY CREDIT CARD RULE - DATE RULES. CALLED BY*
      *            THE PROPRIETARY CREDIT CARD BATCH AND ONLINE        *
      *            PROGRAMS.                                           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-10 BHS  ORIGINAL                                       *
      * 2000-03-28 SKN  NEW LIMIT REPORT COLUMNS REQ5998               *
      * 2001-01-06 MKL  NEW FINANCE REPORT COLUMNS REQ9386             *
      * 2001-11-10 EWS  FINANCE STATUS CODES ADDED PRB5080             *
      * 2003-07-07 JLM  AUDIT FINDING - MIN-DUE CONTROL TOTALS INC2017 *
      * 2003-09-11 JLM  AUDIT FINDING - MIN-DUE CONTROL TOTALS REQ2865 *
      * 2003-06-16 TWH  AUDIT FINDING - PAYMENT CONTROL TOTALS CHG0255 *
      * 2003-11-13 DPS  LIMIT TABLE EXPANDED PRB1111                   *
      * 2006-11-14 JLM  LATE-FEE RERUN PROTECTION REQ9406              *
      * 2006-09-14 MKL  LIMIT LIMIT REVISED CHG2845                    *
      * 2007-04-08 RJT  FINANCE CALCULATION CORRECTED PRB1100          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRCAL.
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
                                           VALUE 17.000000.
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
      * WS-FINANCE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FINANCE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000155500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000775000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000311500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000632500'.
       01  WS-FINANCE-TIER-TBL REDEFINES WS-FINANCE-TIER-VALUES.
           05  WS-FINANCE-TIER-ROW OCCURS 4 TIMES.
               10  WS-FINANCE-TIER-LOW     PIC 9(09)V99.
               10  WS-FINANCE-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FINANCE-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FINANCE-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000691500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000065000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000462000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000209500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013350000000515500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015850000000048500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016850000000867000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016900000000749500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026900000000841500'.
       01  WS-FINANCE-TIERX-TBL REDEFINES WS-FINANCE-TIERX-VALUES.
           05  WS-FINANCE-TIERX-ROW OCCURS 10 TIMES.
               10  WS-FINANCE-TIERX-LOW    PIC 9(09)V99.
               10  WS-FINANCE-TIERX-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CRCALP.

       PROCEDURE DIVISION USING CRCAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-PENALTY-PCT
           PERFORM 2010-CHECK-EFFECTIVE
           PERFORM 2020-PRORATE-LATE-FEE
           PERFORM 2030-DESCRIBE-CARD-TYPE
           PERFORM 2040-CHECK-EFFECTIVE
           PERFORM 2050-AGE-EFFECTIVE
           PERFORM 2060-ACCUM-CARD-TYPE
           PERFORM 2070-COUNT-CYCLE
           PERFORM 2080-CHECK-CLOSE
           PERFORM 2090-ACCRUE-CASH-ADV
           PERFORM 2100-ACCUM-SOURCE
           PERFORM 2110-PRORATE-FINANCE
           PERFORM 2120-ACCUM-SOURCE
           PERFORM 2130-SELECT-PENALTY-PCT
           PERFORM 2140-AGE-EXPIRY
           PERFORM 2150-LIMIT-ITEM
           PERFORM 2160-ASSESS-EXEMPT
           PERFORM 2170-SET-CARD-TYPE
           PERFORM 2180-ACCUM-TYPE
           PERFORM 2190-FLAG-LATE-FEE
           PERFORM 2200-CHECK-EFFECTIVE
           PERFORM 2210-EDIT-CUST-ID
           PERFORM 2220-AGE-EFFECTIVE
           PERFORM 2230-DESCRIBE-SOURCE
           PERFORM 2240-PRORATE-LIMIT
           PERFORM 2250-PRORATE-MIN-DUE
           PERFORM 2260-ACCRUE-CASH-ADV
           PERFORM 2270-ACCRUE-LATE-FEE
           PERFORM 2280-SPLIT-CASH-ADV
           PERFORM 2290-ACCUM-CARD-TYPE
           PERFORM 2300-PRORATE-MIN-DUE
           PERFORM 2310-FLAG-CASH-ADV
           PERFORM 2320-ACCRUE-LIMIT
           PERFORM 2330-ASSESS-EXEMPT
           PERFORM 2340-SELECT-RATE
           PERFORM 2350-ASSESS-AUTO-RENEW
           PERFORM 2360-AGE-MATURITY
           PERFORM 2370-FLAG-LATE-FEE
           PERFORM 2380-PRORATE-MIN-DUE
           PERFORM 2390-ACCUM-TYPE
           PERFORM 2400-COUNT-CYCLE
           PERFORM 2410-ACCRUE-FINANCE
           PERFORM 2420-DESCRIBE-TYPE
           PERFORM 2430-CHECK-CLOSE
           PERFORM 2440-SELECT-RATE
           PERFORM 2450-DESCRIBE-STATUS
           PERFORM 2460-SPLIT-LATE-FEE
           PERFORM 2470-AGE-EFFECTIVE
           PERFORM 2480-DESCRIBE-SOURCE
           PERFORM 2490-ACCUM-SOURCE
           PERFORM 2500-FLAG-CASH-ADV
           PERFORM 2510-EDIT-CUST-ID
           PERFORM 2520-CHECK-CLOSE
           PERFORM 2530-SPLIT-MIN-DUE
           PERFORM 2540-AGE-MATURITY
           PERFORM 2550-DESCRIBE-STATUS
           PERFORM 2560-ACCRUE-LIMIT
           PERFORM 2570-ACCRUE-FINANCE
           PERFORM 2580-COUNT-RETRY
           PERFORM 2590-ACCUM-SOURCE
           PERFORM 2600-EDIT-DESC
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CRCAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CRCAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2010-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CRCAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LATE FEE AMT.
      *----------------------------------------------------------------*
       2020-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCAL-LATE-FEE-AMT ROUNDED =
                   CRCAL-MIN-DUE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCAL-LATE-FEE-AMT
           END-IF
           ADD CRCAL-LATE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2030-DESCRIBE-CARD-TYPE.
           EVALUATE CRCAL-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2040-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CRCAL-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-EFFECTIVE-DATE > 0 AND CRCAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCAL-MIN-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCAL-MIN-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCAL-MIN-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCAL-MIN-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCAL-MIN-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2060-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCAL-CARD-TYPE-CD
                   ADD CRCAL-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCAL-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCAL-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2070-COUNT-CYCLE.
           IF CRCAL-CYCLE-CNT < 9999999
               ADD 1 TO CRCAL-CYCLE-CNT
           END-IF
           IF CRCAL-CYCLE-CNT NOT < 3
               MOVE 'Y' TO CRCAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2080-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CRCAL-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ADV AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2090-ACCRUE-CASH-ADV.
           IF CRCAL-CASH-ADV-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCAL-CASH-ADV-AMT * CRCAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCAL-MIN-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2100-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCAL-SOURCE-CD
                   ADD CRCAL-MIN-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCAL-MIN-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FINANCE YTD.
      *----------------------------------------------------------------*
       2110-PRORATE-FINANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCAL-FINANCE-YTD ROUNDED =
                   CRCAL-MIN-DUE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCAL-FINANCE-YTD
           END-IF
           ADD CRCAL-FINANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2120-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCAL-SOURCE-CD
                   ADD CRCAL-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCAL-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FINANCE TIER TIER FOR THE MIN DUE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2130-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FINANCE-TIER-RATE(1) TO CRCAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCAL-MIN-DUE-YTD NOT < WS-FINANCE-TIER-LOW(WS-TX)
                   MOVE WS-FINANCE-TIER-RATE(WS-TX) TO CRCAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-EXPIRY-DATE > 0 AND CRCAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCAL-CASH-ADV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CRCAL-CASH-ADV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCAL-CASH-ADV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCAL-CASH-ADV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCAL-CASH-ADV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2150-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2160-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRCAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRCAL-CASH-ADV-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCAL-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CRCAL-CASH-ADV-YTD ROUNDED = WS-WORK-AMT-2 *
                       CRCAL-PENALTY-PCT / 200
               WHEN CRCAL-STATUS-CD = 'BAL'
                   MOVE 3.00 TO CRCAL-CASH-ADV-YTD
               WHEN CRCAL-STATUS-CD = 'DDY'
                   MOVE 15.00 TO CRCAL-CASH-ADV-YTD
               WHEN OTHER
                   COMPUTE CRCAL-CASH-ADV-YTD ROUNDED = WS-WORK-AMT-2 *
                       CRCAL-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCAL-CASH-ADV-YTD > 0 AND CRCAL-CASH-ADV-YTD < 2.00
               MOVE 2.00 TO CRCAL-CASH-ADV-YTD
           END-IF
           IF CRCAL-CASH-ADV-YTD > 25.00
               MOVE 25.00 TO CRCAL-CASH-ADV-YTD
           END-IF
           ADD CRCAL-CASH-ADV-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARD TYPE CD:
      * PGU -> TRN WHEN CLOSE DATE IS PAST
      * TRN -> WHR WHEN CLOSE DATE IS PAST
      * WHR -> ZWB WHEN CLOSE DATE IS PAST
      * ZWB -> PGU WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2170-SET-CARD-TYPE.
           EVALUATE CRCAL-CARD-TYPE-CD
               WHEN 'PGU'
                   IF CRCAL-CLOSE-DATE > 0 AND CRCAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TRN' TO CRCAL-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TRN'
                   IF CRCAL-CLOSE-DATE > 0 AND CRCAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WHR' TO CRCAL-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WHR'
                   IF CRCAL-CLOSE-DATE > 0 AND CRCAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWB' TO CRCAL-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZWB'
                   IF CRCAL-CLOSE-DATE > 0 AND CRCAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PGU' TO CRCAL-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ADV AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2180-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCAL-TYPE-CD
                   ADD CRCAL-CASH-ADV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCAL-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCAL-CASH-ADV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2190-FLAG-LATE-FEE.
           IF CRCAL-LATE-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCAL-LATE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2200-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CRCAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E007 CUST ID REQUIRED
      * E662 CASH ADV AMT NEGATIVE
      * E752 CASH ADV AMT OVER 99999
      * E322 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2210-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCAL-CUST-ID = SPACES
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN CRCAL-CASH-ADV-AMT < 0
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN CRCAL-CASH-ADV-AMT > 99999.00
                   MOVE 'E752' TO WS-REJECT-CODE
               WHEN CRCAL-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E322' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2220-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-EFFECTIVE-DATE > 0 AND CRCAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCAL-FINANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCAL-FINANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCAL-FINANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCAL-FINANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCAL-FINANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2230-DESCRIBE-SOURCE.
           EVALUATE CRCAL-SOURCE-CD
               WHEN 'C'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FINANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       2240-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCAL-LIMIT-AMT ROUNDED =
                   CRCAL-FINANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCAL-LIMIT-AMT
           END-IF
           ADD CRCAL-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       2250-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCAL-MIN-DUE-YTD ROUNDED =
                   CRCAL-LIMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCAL-MIN-DUE-YTD
           END-IF
           ADD CRCAL-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ADV YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2260-ACCRUE-CASH-ADV.
           IF CRCAL-CASH-ADV-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCAL-CASH-ADV-YTD * CRCAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCAL-MIN-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-LATE-FEE.
           IF CRCAL-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCAL-LATE-FEE-AMT * CRCAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCAL-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE CRCAL-CASH-ADV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE YTD BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCAL-CARD-TYPE-CD
                   ADD CRCAL-MIN-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCAL-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCAL-MIN-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ADV AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       2300-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCAL-MIN-DUE-YTD ROUNDED =
                   CRCAL-CASH-ADV-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCAL-MIN-DUE-YTD
           END-IF
           ADD CRCAL-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH ADV AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2310-FLAG-CASH-ADV.
           IF CRCAL-CASH-ADV-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCAL-CASH-ADV-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2320-ACCRUE-LIMIT.
           IF CRCAL-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCAL-LIMIT-AMT * CRCAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE FEE ON THE LIMIT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2330-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRCAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRCAL-LATE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCAL-LATE-FEE-AMT ROUNDED = CRCAL-LIMIT-AMT
                       * CRCAL-RATE / 200
               WHEN CRCAL-SOURCE-CD = 'C'
                   MOVE 12.00 TO CRCAL-LATE-FEE-AMT
               WHEN CRCAL-SOURCE-CD = 'H'
                   MOVE 5.00 TO CRCAL-LATE-FEE-AMT
               WHEN OTHER
                   COMPUTE CRCAL-LATE-FEE-AMT ROUNDED = CRCAL-LIMIT-AMT
                       * CRCAL-RATE / 100
           END-EVALUATE
           IF CRCAL-LATE-FEE-AMT > 0 AND CRCAL-LATE-FEE-AMT < 1.00
               MOVE 1.00 TO CRCAL-LATE-FEE-AMT
           END-IF
           IF CRCAL-LATE-FEE-AMT > 75.00
               MOVE 75.00 TO CRCAL-LATE-FEE-AMT
           END-IF
           ADD CRCAL-LATE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FINANCE TIER TIER FOR THE CASH ADV YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2340-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FINANCE-TIER-RATE(1) TO CRCAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCAL-CASH-ADV-YTD NOT < WS-FINANCE-TIER-LOW(WS-TX)
                   MOVE WS-FINANCE-TIER-RATE(WS-TX) TO CRCAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV FEE ON THE MIN DUE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2350-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCAL-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRCAL-CASH-ADV-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCAL-EXEMPT-FLAG = 'Y'
                   COMPUTE CRCAL-CASH-ADV-AMT ROUNDED =
                       CRCAL-MIN-DUE-YTD * CRCAL-PENALTY-PCT / 200
               WHEN CRCAL-SOURCE-CD = 'C'
                   MOVE 7.00 TO CRCAL-CASH-ADV-AMT
               WHEN CRCAL-SOURCE-CD = 'H'
                   MOVE 15.00 TO CRCAL-CASH-ADV-AMT
               WHEN OTHER
                   COMPUTE CRCAL-CASH-ADV-AMT ROUNDED =
                       CRCAL-MIN-DUE-YTD * CRCAL-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCAL-CASH-ADV-AMT > 0 AND CRCAL-CASH-ADV-AMT < 1.00
               MOVE 1.00 TO CRCAL-CASH-ADV-AMT
           END-IF
           IF CRCAL-CASH-ADV-AMT > 150.00
               MOVE 150.00 TO CRCAL-CASH-ADV-AMT
           END-IF
           ADD CRCAL-CASH-ADV-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-MATURITY-DATE > 0 AND CRCAL-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCAL-FINANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCAL-FINANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CRCAL-FINANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CRCAL-FINANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCAL-FINANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LATE FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2370-FLAG-LATE-FEE.
           IF CRCAL-LATE-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCAL-LATE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       2380-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCAL-MIN-DUE-YTD ROUNDED =
                   CRCAL-LATE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCAL-MIN-DUE-YTD
           END-IF
           ADD CRCAL-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ADV AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2390-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCAL-TYPE-CD
                   ADD CRCAL-CASH-ADV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCAL-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCAL-CASH-ADV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2400-COUNT-CYCLE.
           IF CRCAL-CYCLE-CNT < 9999999
               ADD 1 TO CRCAL-CYCLE-CNT
           END-IF
           IF CRCAL-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CRCAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2410-ACCRUE-FINANCE.
           IF CRCAL-FINANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCAL-FINANCE-YTD * CRCAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCAL-CASH-ADV-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2420-DESCRIBE-TYPE.
           EVALUATE CRCAL-TYPE-CD
               WHEN 'GP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2430-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CRCAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FINANCE TIERX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FINANCE-TIERX-RATE(1) TO CRCAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-FINANCE-TIERX-LOW(WS-TX)
                   MOVE WS-FINANCE-TIERX-RATE(WS-TX) TO CRCAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2450-DESCRIBE-STATUS.
           EVALUATE CRCAL-STATUS-CD
               WHEN 'BAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2460-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRCAL-LATE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRCAL-LATE-FEE-AMT - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2470-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-EFFECTIVE-DATE > 0 AND CRCAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCAL-CASH-ADV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRCAL-CASH-ADV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCAL-CASH-ADV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCAL-CASH-ADV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCAL-CASH-ADV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-SOURCE.
           EVALUATE CRCAL-SOURCE-CD
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCAL-SOURCE-CD
                   ADD CRCAL-MIN-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCAL-MIN-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2500-FLAG-CASH-ADV.
           IF CRCAL-CASH-ADV-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCAL-CASH-ADV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E438 CUST ID REQUIRED
      * E853 CASH ADV AMT NEGATIVE
      * E872 CASH ADV AMT OVER 10000
      * E740 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2510-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCAL-CUST-ID = SPACES
                   MOVE 'E438' TO WS-REJECT-CODE
               WHEN CRCAL-CASH-ADV-AMT < 0
                   MOVE 'E853' TO WS-REJECT-CODE
               WHEN CRCAL-CASH-ADV-AMT > 10000.00
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN CRCAL-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2520-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CRCAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MIN DUE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-MIN-DUE.
           COMPUTE WS-WORK-AMT = CRCAL-MIN-DUE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRCAL-MIN-DUE-YTD - (WS-WORK-AMT * 3)
           COMPUTE CRCAL-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRCAL-MATURITY-DATE > 0 AND CRCAL-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCAL-MATURITY-DATE TO DTU-DATE-1
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
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2550-DESCRIBE-STATUS.
           EVALUATE CRCAL-STATUS-CD
               WHEN 'BAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2560-ACCRUE-LIMIT.
           IF CRCAL-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCAL-LIMIT-AMT * CRCAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRCAL-FINANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2570-ACCRUE-FINANCE.
           IF CRCAL-FINANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCAL-FINANCE-YTD * CRCAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRCAL-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2580-COUNT-RETRY.
           IF CRCAL-RETRY-CNT < 9999999
               ADD 1 TO CRCAL-RETRY-CNT
           END-IF
           IF CRCAL-RETRY-CNT NOT < 3
               MOVE 'Y' TO CRCAL-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2590-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCAL-SOURCE-CD
                   ADD CRCAL-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCAL-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E114 DESC REQUIRED
      * E695 FINANCE YTD NEGATIVE
      * E562 FINANCE YTD OVER 25000
      * E507 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2600-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCAL-DESC = SPACES
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN CRCAL-FINANCE-YTD < 0
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN CRCAL-FINANCE-YTD > 25000.00
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN CRCAL-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CRCAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CRCAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CRCAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CRCAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
