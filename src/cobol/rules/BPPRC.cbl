      *================================================================*
      * PROGRAM  : BPPRC                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : BILL PAYMENT SERVICE RULE - PRICING. CALLED BY THE  *
      *            BILL PAYMENT SERVICE BATCH AND ONLINE PROGRAMS.     *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-01-18 DPS  ORIGINAL                                       *
      * 1993-04-22 EWS  RETURNED CALCULATION CORRECTED CHG3251         *
      * 1993-03-16 DPS  FEE EXCEPTIONS TO OPS QUEUE PRB6711            *
      * 1994-10-13 TWH  PAYMENT STATUS CODES ADDED CHG8028             *
      * 1994-02-12 CRP  REFUND FEE SCHEDULE UPDATE INC5574             *
      * 1996-06-03 TWH  REFUND ROUNDING - HALF UP INC1240              *
      * 1999-12-07 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-06-19 PQR  NEW FEE REPORT COLUMNS PRB6679                 *
      * 2002-06-15 GAF  RETURNED STATUS CODES ADDED INC0764            *
      * 2004-03-19 SKN  AUDIT FINDING - RETURNED CONTROL TOTALS PRB2125*
      * 2004-01-24 RJT  RETURNED ROUNDING - HALF UP CHG4904            *
      * 2007-02-09 EWS  REFUND FEE SCHEDULE UPDATE REQ3498             *
      * 2008-02-04 DPS  REFUND FEE SCHEDULE UPDATE CHG7173             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPPRC.
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
      * WS-REFUND-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REFUND-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000875000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000053000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000090000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000198000'.
       01  WS-REFUND-TIER-TBL REDEFINES WS-REFUND-TIER-VALUES.
           05  WS-REFUND-TIER-ROW OCCURS 4 TIMES.
               10  WS-REFUND-TIER-LOW      PIC 9(09)V99.
               10  WS-REFUND-TIER-RATE     PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY BPPRCP.

       PROCEDURE DIVISION USING BPPRC-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-COUNT-EXCEPT
           PERFORM 2010-PRORATE-PAYMENT
           PERFORM 2020-SET-REMIT-METHOD
           PERFORM 2030-ACCRUE-FEE
           PERFORM 2040-COUNT-CYCLE
           PERFORM 2050-PRORATE-REFUND
           PERFORM 2060-COUNT-CYCLE
           PERFORM 2070-ACCRUE-PAYMENT
           PERFORM 2080-SPLIT-REFUND
           PERFORM 2090-DESCRIBE-REASON
           PERFORM 2100-CHECK-EXPIRY
           PERFORM 2110-SELECT-MARGIN
           PERFORM 2120-ACCRUE-PAYMENT
           PERFORM 2130-PRORATE-FEE
           PERFORM 2140-CHECK-EFFECTIVE
           PERFORM 2150-CHECK-LAST-ACT
           PERFORM 2160-EDIT-ADDR
           PERFORM 2170-CHECK-NEXT-DUE
           PERFORM 2180-SET-STATUS
           PERFORM 2190-ASSESS-OVERRIDE
           PERFORM 2200-SPLIT-FEE
           PERFORM 2210-SELECT-PENALTY-PCT
           PERFORM 2220-COUNT-CYCLE
           PERFORM 2230-PRORATE-FEE
           PERFORM 2240-SELECT-MARGIN
           PERFORM 2250-ACCUM-STATUS
           PERFORM 2260-EDIT-ADDR
           PERFORM 2270-COUNT-EXCEPT
           PERFORM 2280-ACCRUE-PENALTY-PCT
           PERFORM 2290-SPLIT-FEE
           PERFORM 2300-CHECK-EXPIRY
           PERFORM 2310-FLAG-RETURNED
           PERFORM 2320-CHECK-NEXT-DUE
           PERFORM 2330-LIMIT-ITEM
           PERFORM 2340-LIMIT-FEE
           PERFORM 2350-ACCRUE-FEE
           PERFORM 2360-DESCRIBE-STATUS
           PERFORM 2370-COUNT-EXCEPT
           PERFORM 2380-FLAG-ITEM
           PERFORM 2390-LIMIT-FEE
           PERFORM 2400-DESCRIBE-SOURCE
           PERFORM 2410-FLAG-PAYMENT
           PERFORM 2420-PRORATE-PAYMENT
           PERFORM 2430-COUNT-EXCEPT
           PERFORM 2440-EDIT-NAME
           PERFORM 2450-COUNT-CYCLE
           PERFORM 2460-DESCRIBE-STATUS
           PERFORM 2470-PRORATE-PAYMENT
           PERFORM 2480-ACCRUE-PENALTY-PCT
           PERFORM 2490-SET-REMIT-METHOD
           PERFORM 2500-SPLIT-FEE
           PERFORM 2510-LIMIT-FEE
           PERFORM 2520-ASSESS-OVERRIDE
           PERFORM 2530-PRORATE-PAYMENT
           PERFORM 2540-LIMIT-PAYMENT
           PERFORM 2550-ACCRUE-PAYMENT
           PERFORM 2560-SPLIT-FEE
           PERFORM 2570-SET-SOURCE
           PERFORM 2580-COUNT-CYCLE
           PERFORM 2590-ASSESS-HOLD
           PERFORM 2600-FLAG-RETURNED
           PERFORM 2610-ACCUM-SOURCE
           PERFORM 2620-DESCRIBE-REASON
           PERFORM 2630-DESCRIBE-STATUS
           PERFORM 2640-PRORATE-PAYMENT
           PERFORM 2650-FLAG-PAYMENT
           PERFORM 2660-PRORATE-REFUND
           PERFORM 2670-CHECK-NEXT-DUE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO BPPRC-RETURN-CODE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2000-COUNT-EXCEPT.
           IF BPPRC-EXCEPT-CNT < 9999999
               ADD 1 TO BPPRC-EXCEPT-CNT
           END-IF
           IF BPPRC-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO BPPRC-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       2010-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPPRC-PAYMENT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPPRC-PAYMENT-AMT
           END-IF
           ADD BPPRC-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN NEXT DUE DATE IS PAST
      * FNS -> NMS WHEN NEXT DUE DATE IS PAST
      * NMS -> TAF WHEN NEXT DUE DATE IS PAST
      * TAF -> VNK WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2020-SET-REMIT-METHOD.
           EVALUATE BPPRC-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPPRC-NEXT-DUE-DATE > 0 AND BPPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPPRC-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPPRC-NEXT-DUE-DATE > 0 AND BPPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPPRC-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPPRC-NEXT-DUE-DATE > 0 AND BPPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPPRC-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPPRC-NEXT-DUE-DATE > 0 AND BPPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPPRC-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2030-ACCRUE-FEE.
           IF BPPRC-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPPRC-FEE-AMT * BPPRC-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPPRC-RETURNED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-CYCLE.
           IF BPPRC-CYCLE-CNT < 9999999
               ADD 1 TO BPPRC-CYCLE-CNT
           END-IF
           IF BPPRC-CYCLE-CNT NOT < 6
               MOVE 'Y' TO BPPRC-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       2050-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPPRC-REFUND-YTD ROUNDED =
                   BPPRC-PAYMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPPRC-REFUND-YTD
           END-IF
           ADD BPPRC-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2060-COUNT-CYCLE.
           IF BPPRC-CYCLE-CNT < 9999999
               ADD 1 TO BPPRC-CYCLE-CNT
           END-IF
           IF BPPRC-CYCLE-CNT NOT < 6
               MOVE 'Y' TO BPPRC-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2070-ACCRUE-PAYMENT.
           IF BPPRC-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPPRC-PAYMENT-YTD * BPPRC-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPPRC-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2080-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPPRC-REFUND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BPPRC-REFUND-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2090-DESCRIBE-REASON.
           EVALUATE BPPRC-REASON-CD
               WHEN 'AX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2100-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BPPRC-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2110-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2120-ACCRUE-PAYMENT.
           IF BPPRC-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPPRC-PAYMENT-YTD * BPPRC-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       2130-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPPRC-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPPRC-FEE-YTD
           END-IF
           ADD BPPRC-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BPPRC-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPPRC-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BPPRC-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2150-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF BPPRC-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPPRC-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BPPRC-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E070 ADDR REQUIRED
      * E938 PAYMENT AMT NEGATIVE
      * E996 PAYMENT AMT OVER 1000000
      * E656 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2160-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPPRC-ADDR = SPACES
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN BPPRC-PAYMENT-AMT < 0
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN BPPRC-PAYMENT-AMT > 1000000.00
                   MOVE 'E996' TO WS-REJECT-CODE
               WHEN BPPRC-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E656' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2170-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPPRC-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BPPRC-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN EFFECTIVE DATE IS PAST
      * LR -> NZ WHEN EFFECTIVE DATE IS PAST
      * NZ -> RV WHEN EFFECTIVE DATE IS PAST
      * RV -> TM WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2180-SET-STATUS.
           EVALUATE BPPRC-STATUS-CD
               WHEN 'JX'
                   IF BPPRC-EFFECTIVE-DATE > 0 AND BPPRC-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPPRC-EFFECTIVE-DATE > 0 AND BPPRC-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPPRC-EFFECTIVE-DATE > 0 AND BPPRC-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPPRC-EFFECTIVE-DATE > 0 AND BPPRC-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE REFUND YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2190-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPPRC-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPPRC-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = BPPRC-REFUND-YTD *
                       BPPRC-PENALTY-PCT / 200
               WHEN BPPRC-STATUS-CD = 'JX'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN BPPRC-STATUS-CD = 'LR'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = BPPRC-REFUND-YTD *
                       BPPRC-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2200-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPPRC-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BPPRC-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2210-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPPRC-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BPPRC-FEE-AMT NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPPRC-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPPRC-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPPRC-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2220-COUNT-CYCLE.
           IF BPPRC-CYCLE-CNT < 9999999
               ADD 1 TO BPPRC-CYCLE-CNT
           END-IF
           IF BPPRC-CYCLE-CNT NOT < 6
               MOVE 'Y' TO BPPRC-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       2230-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPPRC-FEE-AMT ROUNDED =
                   BPPRC-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPPRC-FEE-AMT
           END-IF
           ADD BPPRC-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE REFUND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2240-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BPPRC-REFUND-YTD NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPPRC-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPPRC-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E022 ADDR REQUIRED
      * E480 WORK AMT 2 NEGATIVE
      * E808 WORK AMT 2 OVER 250000
      * E140 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2260-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPPRC-ADDR = SPACES
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E480' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E808' TO WS-REJECT-CODE
               WHEN BPPRC-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2270-COUNT-EXCEPT.
           IF BPPRC-EXCEPT-CNT < 9999999
               ADD 1 TO BPPRC-EXCEPT-CNT
           END-IF
           IF BPPRC-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO BPPRC-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BPPRC-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPPRC-REFUND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2290-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPPRC-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPPRC-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPPRC-RETURNED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BPPRC-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2310-FLAG-RETURNED.
           IF BPPRC-RETURNED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPPRC-RETURNED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2320-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPPRC-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BPPRC-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2330-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2340-LIMIT-FEE.
           IF BPPRC-FEE-YTD < 50.00
               MOVE 50.00 TO BPPRC-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPPRC-FEE-YTD > 50000.00
               MOVE 50000.00 TO BPPRC-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2350-ACCRUE-FEE.
           IF BPPRC-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPPRC-FEE-AMT * BPPRC-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPPRC-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2360-DESCRIBE-STATUS.
           EVALUATE BPPRC-STATUS-CD
               WHEN 'JX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2370-COUNT-EXCEPT.
           IF BPPRC-EXCEPT-CNT < 9999999
               ADD 1 TO BPPRC-EXCEPT-CNT
           END-IF
           IF BPPRC-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPPRC-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2380-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2390-LIMIT-FEE.
           IF BPPRC-FEE-AMT < 50.00
               MOVE 50.00 TO BPPRC-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPPRC-FEE-AMT > 50000.00
               MOVE 50000.00 TO BPPRC-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2400-DESCRIBE-SOURCE.
           EVALUATE BPPRC-SOURCE-CD
               WHEN 'CT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2410-FLAG-PAYMENT.
           IF BPPRC-PAYMENT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPPRC-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2420-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPPRC-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-EXCEPT.
           IF BPPRC-EXCEPT-CNT < 9999999
               ADD 1 TO BPPRC-EXCEPT-CNT
           END-IF
           IF BPPRC-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO BPPRC-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E196 NAME REQUIRED
      * E523 WORK AMT 2 NEGATIVE
      * E834 WORK AMT 2 OVER 50000
      * E023 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2440-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPPRC-NAME = SPACES
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E834' TO WS-REJECT-CODE
               WHEN BPPRC-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E023' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2450-COUNT-CYCLE.
           IF BPPRC-CYCLE-CNT < 9999999
               ADD 1 TO BPPRC-CYCLE-CNT
           END-IF
           IF BPPRC-CYCLE-CNT NOT < 3
               MOVE 'Y' TO BPPRC-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2460-DESCRIBE-STATUS.
           EVALUATE BPPRC-STATUS-CD
               WHEN 'JX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       2470-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPPRC-PAYMENT-YTD ROUNDED =
                   BPPRC-REFUND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPPRC-PAYMENT-YTD
           END-IF
           ADD BPPRC-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2480-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BPPRC-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN NEXT DUE DATE IS PAST
      * FNS -> NMS WHEN NEXT DUE DATE IS PAST
      * NMS -> TAF WHEN NEXT DUE DATE IS PAST
      * TAF -> VNK WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-REMIT-METHOD.
           EVALUATE BPPRC-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPPRC-NEXT-DUE-DATE > 0 AND BPPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPPRC-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPPRC-NEXT-DUE-DATE > 0 AND BPPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPPRC-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPPRC-NEXT-DUE-DATE > 0 AND BPPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPPRC-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPPRC-NEXT-DUE-DATE > 0 AND BPPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPPRC-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2500-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE BPPRC-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2510-LIMIT-FEE.
           IF BPPRC-FEE-YTD < 0.00
               MOVE 0.00 TO BPPRC-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPPRC-FEE-YTD > 50000.00
               MOVE 50000.00 TO BPPRC-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2520-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPPRC-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPPRC-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPPRC-HOLD-FLAG = 'Y'
                   COMPUTE BPPRC-PAYMENT-YTD ROUNDED = WS-WORK-AMT *
                       BPPRC-MARGIN / 200
               WHEN BPPRC-STATUS-CD = 'JX'
                   MOVE 5.00 TO BPPRC-PAYMENT-YTD
               WHEN BPPRC-STATUS-CD = 'LR'
                   MOVE 5.00 TO BPPRC-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE BPPRC-PAYMENT-YTD ROUNDED = WS-WORK-AMT *
                       BPPRC-MARGIN / 100
           END-EVALUATE
           IF BPPRC-PAYMENT-YTD > 0 AND BPPRC-PAYMENT-YTD < 2.00
               MOVE 2.00 TO BPPRC-PAYMENT-YTD
           END-IF
           IF BPPRC-PAYMENT-YTD > 75.00
               MOVE 75.00 TO BPPRC-PAYMENT-YTD
           END-IF
           ADD BPPRC-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       2530-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPPRC-PAYMENT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPPRC-PAYMENT-YTD
           END-IF
           ADD BPPRC-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2540-LIMIT-PAYMENT.
           IF BPPRC-PAYMENT-AMT < 50.00
               MOVE 50.00 TO BPPRC-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPPRC-PAYMENT-AMT > 50000.00
               MOVE 50000.00 TO BPPRC-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2550-ACCRUE-PAYMENT.
           IF BPPRC-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPPRC-PAYMENT-AMT * BPPRC-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BPPRC-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2560-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPPRC-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CT -> FV WHEN EXPIRY DATE IS PAST
      * FV -> HD WHEN EXPIRY DATE IS PAST
      * HD -> KD WHEN EXPIRY DATE IS PAST
      * KD -> NE WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2570-SET-SOURCE.
           EVALUATE BPPRC-SOURCE-CD
               WHEN 'CT'
                   IF BPPRC-EXPIRY-DATE > 0 AND BPPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FV' TO BPPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FV'
                   IF BPPRC-EXPIRY-DATE > 0 AND BPPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HD' TO BPPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HD'
                   IF BPPRC-EXPIRY-DATE > 0 AND BPPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO BPPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF BPPRC-EXPIRY-DATE > 0 AND BPPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO BPPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2580-COUNT-CYCLE.
           IF BPPRC-CYCLE-CNT < 9999999
               ADD 1 TO BPPRC-CYCLE-CNT
           END-IF
           IF BPPRC-CYCLE-CNT NOT < 6
               MOVE 'Y' TO BPPRC-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PAYMENT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2590-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPPRC-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPPRC-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPPRC-PAYMENT-YTD *
                       BPPRC-MARGIN / 200
               WHEN BPPRC-REMIT-METHOD-CD = 'CGV'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN BPPRC-REMIT-METHOD-CD = 'FNS'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPPRC-PAYMENT-YTD *
                       BPPRC-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETURNED YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2600-FLAG-RETURNED.
           IF BPPRC-RETURNED-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPPRC-RETURNED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2610-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPPRC-SOURCE-CD
                   ADD BPPRC-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPPRC-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPPRC-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2620-DESCRIBE-REASON.
           EVALUATE BPPRC-REASON-CD
               WHEN 'AX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2630-DESCRIBE-STATUS.
           EVALUATE BPPRC-STATUS-CD
               WHEN 'JX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       2640-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPPRC-PAYMENT-AMT ROUNDED =
                   BPPRC-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPPRC-PAYMENT-AMT
           END-IF
           ADD BPPRC-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2650-FLAG-PAYMENT.
           IF BPPRC-PAYMENT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPPRC-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       2660-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPPRC-REFUND-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPPRC-REFUND-YTD
           END-IF
           ADD BPPRC-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2670-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPPRC-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BPPRC-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO BPPRC-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO BPPRC-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO BPPRC-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'BPPRC EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
