      *================================================================*
      * PROGRAM  : SBLIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : SAVINGS BOND SERVICES RULE - VALIDATION. CALLED BY  *
      *            THE SAVINGS BOND SERVICES BATCH AND ONLINE PROGRAMS.*
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-09-28 DPS  ORIGINAL                                       *
      * 1999-01-17 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-07-16 MKL  PURCHASE CUTOFF MOVED CHG2024                  *
      * 2000-05-23 JLM  INTEREST STATUS CODES ADDED REQ4136            *
      * 2002-05-28 MKL  PURCHASE RERUN PROTECTION PRB3593              *
      * 2002-08-23 JLM  FACE CUTOFF MOVED INC3171                      *
      * 2004-05-22 LMB  REDEEM ROUNDING - HALF UP CHG3736              *
      * 2004-10-28 DPS  FACE FEE SCHEDULE UPDATE REQ2319               *
      * 2004-02-05 TWH  TAXABLE RERUN PROTECTION REQ1976               *
      * 2006-10-05 JLM  FACE RERUN PROTECTION PRB6607                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBLIM.
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
                                           VALUE 17.000000.
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
      * WS-ISSUE-PR-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUE-PR-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000580000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000813000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000803500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000581500'.
       01  WS-ISSUE-PR-SCHED-TBL REDEFINES WS-ISSUE-PR-SCHED-VALUES.
           05  WS-ISSUE-PR-SCHED-ROW OCCURS 4 TIMES.
               10  WS-ISSUE-PR-SCHED-LOW   PIC 9(09)V99.
               10  WS-ISSUE-PR-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SBLIMP.

       PROCEDURE DIVISION USING SBLIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CHECK-NOTICE
           PERFORM 2010-DESCRIBE-REASON
           PERFORM 2020-COUNT-EXCEPT
           PERFORM 2030-DESCRIBE-ISSUE-TYPE
           PERFORM 2040-EDIT-ADDR
           PERFORM 2050-SET-ISSUE-TYPE
           PERFORM 2060-DESCRIBE-SOURCE
           PERFORM 2070-PRORATE-TAXABLE
           PERFORM 2080-CHECK-NOTICE
           PERFORM 2090-AGE-MATURITY
           PERFORM 2100-ASSESS-HOLD
           PERFORM 2110-PRORATE-TAXABLE
           PERFORM 2120-FLAG-TAXABLE
           PERFORM 2130-AGE-NEXT-REVIEW
           PERFORM 2140-FLAG-FACE
           PERFORM 2150-LIMIT-TAXABLE
           PERFORM 2160-SELECT-PENALTY-PCT
           PERFORM 2170-SPLIT-PURCHASE
           PERFORM 2180-FLAG-PURCHASE
           PERFORM 2190-FLAG-PURCHASE
           PERFORM 2200-LIMIT-PURCHASE
           PERFORM 2210-LIMIT-TAXABLE
           PERFORM 2220-SPLIT-FACE
           PERFORM 2230-SET-ISSUE-TYPE
           PERFORM 2240-COUNT-EXCEPT
           PERFORM 2250-ACCUM-REASON
           PERFORM 2260-SET-SOURCE
           PERFORM 2270-ACCRUE-FACE
           PERFORM 2280-COUNT-PAYMENT
           PERFORM 2290-ACCRUE-PENALTY-PCT
           PERFORM 2300-PRORATE-ISSUE-PRICE
           PERFORM 2310-CHECK-NOTICE
           PERFORM 2320-SET-SERIES
           PERFORM 2330-LIMIT-ITEM
           PERFORM 2340-PRORATE-PURCHASE
           PERFORM 2350-DESCRIBE-SERIES
           PERFORM 2360-PRORATE-ISSUE-PRICE
           PERFORM 2370-ACCRUE-TAXABLE
           PERFORM 2380-ACCUM-SERIES
           PERFORM 2390-AGE-MATURITY
           PERFORM 2400-LIMIT-PURCHASE
           PERFORM 2410-EDIT-REF-NO
           PERFORM 2420-FLAG-ITEM
           PERFORM 2430-ASSESS-HOLD
           PERFORM 2440-ASSESS-PREFERRED
           PERFORM 2450-SET-SOURCE
           PERFORM 2460-EDIT-ADDR
           PERFORM 2470-ASSESS-HOLD
           PERFORM 2480-ACCUM-SERIES
           PERFORM 2490-SPLIT-ISSUE-PRICE
           PERFORM 2500-SELECT-FEE-PCT
           PERFORM 2510-SET-SOURCE
           PERFORM 2520-CHECK-NEXT-REVIEW
           PERFORM 2530-CHECK-MATURITY
           PERFORM 2540-FLAG-TAXABLE
           PERFORM 2550-SET-SERIES
           PERFORM 2560-SELECT-PENALTY-PCT
           PERFORM 2570-FLAG-TAXABLE
           PERFORM 2580-FLAG-PURCHASE
           PERFORM 2590-COUNT-PAYMENT
           PERFORM 2600-PRORATE-PURCHASE
           PERFORM 2610-SPLIT-TAXABLE
           PERFORM 2620-CHECK-NEXT-REVIEW
           PERFORM 2630-ASSESS-OVERRIDE
           PERFORM 2640-CHECK-NOTICE
           PERFORM 2650-FLAG-FACE
           PERFORM 2660-PRORATE-TAXABLE
           PERFORM 2670-SPLIT-PURCHASE
           PERFORM 2680-PRORATE-TAXABLE
           PERFORM 2690-SET-ISSUE-TYPE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SBLIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2000-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SBLIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2010-DESCRIBE-REASON.
           EVALUATE SBLIM-REASON-CD
               WHEN 'DG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2020-COUNT-EXCEPT.
           IF SBLIM-EXCEPT-CNT < 9999999
               ADD 1 TO SBLIM-EXCEPT-CNT
           END-IF
           IF SBLIM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO SBLIM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2030-DESCRIBE-ISSUE-TYPE.
           EVALUATE SBLIM-ISSUE-TYPE-CD
               WHEN 'CW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E228 ADDR REQUIRED
      * E311 TAXABLE AMT NEGATIVE
      * E471 TAXABLE AMT OVER 1000000
      * E189 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2040-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBLIM-ADDR = SPACES
                   MOVE 'E228' TO WS-REJECT-CODE
               WHEN SBLIM-TAXABLE-AMT < 0
                   MOVE 'E311' TO WS-REJECT-CODE
               WHEN SBLIM-TAXABLE-AMT > 1000000.00
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN SBLIM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN LAST MAINT DATE IS PAST
      * DY -> JA WHEN LAST MAINT DATE IS PAST
      * JA -> KT WHEN LAST MAINT DATE IS PAST
      * KT -> MF WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2050-SET-ISSUE-TYPE.
           EVALUATE SBLIM-ISSUE-TYPE-CD
               WHEN 'CW'
                   IF SBLIM-LAST-MAINT-DATE > 0 AND
                       SBLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'DY' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DY'
                   IF SBLIM-LAST-MAINT-DATE > 0 AND
                       SBLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JA' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF SBLIM-LAST-MAINT-DATE > 0 AND
                       SBLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KT' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KT'
                   IF SBLIM-LAST-MAINT-DATE > 0 AND
                       SBLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'MF' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2060-DESCRIBE-SOURCE.
           EVALUATE SBLIM-SOURCE-CD
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'J'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2070-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBLIM-TAXABLE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2080-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SBLIM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2090-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SBLIM-MATURITY-DATE > 0 AND SBLIM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBLIM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBLIM-ISSUE-PRICE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD SBLIM-ISSUE-PRICE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBLIM-ISSUE-PRICE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBLIM-ISSUE-PRICE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBLIM-ISSUE-PRICE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE TAXABLE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2100-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBLIM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBLIM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SBLIM-TAXABLE-YTD *
                       SBLIM-PENALTY-PCT / 200
               WHEN SBLIM-SOURCE-CD = 'E'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN SBLIM-SOURCE-CD = 'J'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SBLIM-TAXABLE-YTD *
                       SBLIM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAXABLE AMT.
      *----------------------------------------------------------------*
       2110-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBLIM-TAXABLE-AMT ROUNDED =
                   SBLIM-PURCHASE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBLIM-TAXABLE-AMT
           END-IF
           ADD SBLIM-TAXABLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TAXABLE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2120-FLAG-TAXABLE.
           IF SBLIM-TAXABLE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBLIM-TAXABLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2130-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SBLIM-NEXT-REVIEW-DATE > 0 AND SBLIM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBLIM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBLIM-TAXABLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBLIM-TAXABLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBLIM-TAXABLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBLIM-TAXABLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBLIM-TAXABLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2140-FLAG-FACE.
           IF SBLIM-FACE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBLIM-FACE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2150-LIMIT-TAXABLE.
           IF SBLIM-TAXABLE-YTD < 0.00
               MOVE 0.00 TO SBLIM-TAXABLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBLIM-TAXABLE-YTD > 1000.00
               MOVE 1000.00 TO SBLIM-TAXABLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2160-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHED-RATE(1) TO SBLIM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-ISSUE-PR-SCHED-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHED-RATE(WS-TX) TO
                       SBLIM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBLIM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBLIM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2170-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = SBLIM-PURCHASE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SBLIM-PURCHASE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE SBLIM-FACE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2180-FLAG-PURCHASE.
           IF SBLIM-PURCHASE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBLIM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2190-FLAG-PURCHASE.
           IF SBLIM-PURCHASE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBLIM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2200-LIMIT-PURCHASE.
           IF SBLIM-PURCHASE-AMT < 100.00
               MOVE 100.00 TO SBLIM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBLIM-PURCHASE-AMT > 1000.00
               MOVE 1000.00 TO SBLIM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2210-LIMIT-TAXABLE.
           IF SBLIM-TAXABLE-AMT < 0.00
               MOVE 0.00 TO SBLIM-TAXABLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBLIM-TAXABLE-AMT > 1000.00
               MOVE 1000.00 TO SBLIM-TAXABLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2220-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBLIM-FACE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBLIM-FACE-YTD - (WS-WORK-AMT * 4)
           COMPUTE SBLIM-PURCHASE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN NEXT REVIEW DATE IS PAST
      * DY -> JA WHEN NEXT REVIEW DATE IS PAST
      * JA -> KT WHEN NEXT REVIEW DATE IS PAST
      * KT -> MF WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2230-SET-ISSUE-TYPE.
           EVALUATE SBLIM-ISSUE-TYPE-CD
               WHEN 'CW'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DY' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DY'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JA' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KT' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KT'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MF' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-EXCEPT.
           IF SBLIM-EXCEPT-CNT < 9999999
               ADD 1 TO SBLIM-EXCEPT-CNT
           END-IF
           IF SBLIM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO SBLIM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAXABLE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBLIM-REASON-CD
                   ADD SBLIM-TAXABLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBLIM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBLIM-TAXABLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN MATURITY DATE IS PAST
      * J -> L WHEN MATURITY DATE IS PAST
      * L -> U WHEN MATURITY DATE IS PAST
      * U -> E WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2260-SET-SOURCE.
           EVALUATE SBLIM-SOURCE-CD
               WHEN 'E'
                   IF SBLIM-MATURITY-DATE > 0 AND SBLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBLIM-MATURITY-DATE > 0 AND SBLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBLIM-MATURITY-DATE > 0 AND SBLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBLIM-MATURITY-DATE > 0 AND SBLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-FACE.
           IF SBLIM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBLIM-FACE-YTD * SBLIM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-PAYMENT.
           IF SBLIM-PAYMENT-CNT < 9999999
               ADD 1 TO SBLIM-PAYMENT-CNT
           END-IF
           IF SBLIM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO SBLIM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2290-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SBLIM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBLIM-PURCHASE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUE PRICE YTD.
      *----------------------------------------------------------------*
       2300-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBLIM-ISSUE-PRICE-YTD ROUNDED =
                   SBLIM-PURCHASE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBLIM-ISSUE-PRICE-YTD
           END-IF
           ADD SBLIM-ISSUE-PRICE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SBLIM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN NEXT REVIEW DATE IS PAST
      * DF -> SS WHEN NEXT REVIEW DATE IS PAST
      * SS -> VJ WHEN NEXT REVIEW DATE IS PAST
      * VJ -> VX WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2320-SET-SERIES.
           EVALUATE SBLIM-SERIES-CD
               WHEN 'DD'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DF' TO SBLIM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SS' TO SBLIM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SS'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBLIM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJ'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VX' TO SBLIM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2330-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PURCHASE YTD.
      *----------------------------------------------------------------*
       2340-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBLIM-PURCHASE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBLIM-PURCHASE-YTD
           END-IF
           ADD SBLIM-PURCHASE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2350-DESCRIBE-SERIES.
           EVALUATE SBLIM-SERIES-CD
               WHEN 'DD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUE PRICE YTD.
      *----------------------------------------------------------------*
       2360-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBLIM-ISSUE-PRICE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBLIM-ISSUE-PRICE-YTD
           END-IF
           ADD SBLIM-ISSUE-PRICE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAXABLE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2370-ACCRUE-TAXABLE.
           IF SBLIM-TAXABLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBLIM-TAXABLE-YTD * SBLIM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBLIM-PURCHASE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE YTD BY SERIES CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2380-ACCUM-SERIES.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBLIM-SERIES-CD
                   ADD SBLIM-PURCHASE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBLIM-SERIES-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBLIM-PURCHASE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SBLIM-MATURITY-DATE > 0 AND SBLIM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBLIM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PURCHASE YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2400-LIMIT-PURCHASE.
           IF SBLIM-PURCHASE-YTD < 50.00
               MOVE 50.00 TO SBLIM-PURCHASE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBLIM-PURCHASE-YTD > 5000.00
               MOVE 5000.00 TO SBLIM-PURCHASE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E985 REF NO REQUIRED
      * E697 PURCHASE AMT NEGATIVE
      * E368 PURCHASE AMT OVER 99999
      * E295 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBLIM-REF-NO = SPACES
                   MOVE 'E985' TO WS-REJECT-CODE
               WHEN SBLIM-PURCHASE-AMT < 0
                   MOVE 'E697' TO WS-REJECT-CODE
               WHEN SBLIM-PURCHASE-AMT > 99999.00
                   MOVE 'E368' TO WS-REJECT-CODE
               WHEN SBLIM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E295' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2420-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUE PRICE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2430-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBLIM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBLIM-ISSUE-PRICE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBLIM-PREFERRED-FLAG = 'Y'
                   COMPUTE SBLIM-ISSUE-PRICE-YTD ROUNDED = WS-WORK-AMT-2
                       * SBLIM-PENALTY-PCT / 200
               WHEN SBLIM-ISSUE-TYPE-CD = 'CW'
                   MOVE 3.00 TO SBLIM-ISSUE-PRICE-YTD
               WHEN SBLIM-ISSUE-TYPE-CD = 'DY'
                   MOVE 3.00 TO SBLIM-ISSUE-PRICE-YTD
               WHEN OTHER
                   COMPUTE SBLIM-ISSUE-PRICE-YTD ROUNDED = WS-WORK-AMT-2
                       * SBLIM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBLIM-ISSUE-PRICE-YTD > 0 AND SBLIM-ISSUE-PRICE-YTD < 1.00
               MOVE 1.00 TO SBLIM-ISSUE-PRICE-YTD
           END-IF
           IF SBLIM-ISSUE-PRICE-YTD > 75.00
               MOVE 75.00 TO SBLIM-ISSUE-PRICE-YTD
           END-IF
           ADD SBLIM-ISSUE-PRICE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE TAXABLE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2440-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SBLIM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SBLIM-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBLIM-HOLD-FLAG = 'Y'
                   COMPUTE SBLIM-PURCHASE-YTD ROUNDED =
                       SBLIM-TAXABLE-AMT * SBLIM-PENALTY-PCT / 200
               WHEN SBLIM-REASON-CD = 'DG'
                   MOVE 15.00 TO SBLIM-PURCHASE-YTD
               WHEN SBLIM-REASON-CD = 'UK'
                   MOVE 12.00 TO SBLIM-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE SBLIM-PURCHASE-YTD ROUNDED =
                       SBLIM-TAXABLE-AMT * SBLIM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBLIM-PURCHASE-YTD > 0 AND SBLIM-PURCHASE-YTD < 10.00
               MOVE 10.00 TO SBLIM-PURCHASE-YTD
           END-IF
           IF SBLIM-PURCHASE-YTD > 250.00
               MOVE 250.00 TO SBLIM-PURCHASE-YTD
           END-IF
           ADD SBLIM-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN MATURITY DATE IS PAST
      * J -> L WHEN MATURITY DATE IS PAST
      * L -> U WHEN MATURITY DATE IS PAST
      * U -> E WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2450-SET-SOURCE.
           EVALUATE SBLIM-SOURCE-CD
               WHEN 'E'
                   IF SBLIM-MATURITY-DATE > 0 AND SBLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBLIM-MATURITY-DATE > 0 AND SBLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBLIM-MATURITY-DATE > 0 AND SBLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBLIM-MATURITY-DATE > 0 AND SBLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E268 ADDR REQUIRED
      * E741 PURCHASE AMT NEGATIVE
      * E007 PURCHASE AMT OVER 10000
      * E889 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2460-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBLIM-ADDR = SPACES
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN SBLIM-PURCHASE-AMT < 0
                   MOVE 'E741' TO WS-REJECT-CODE
               WHEN SBLIM-PURCHASE-AMT > 10000.00
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN SBLIM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE TAXABLE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2470-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBLIM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBLIM-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBLIM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBLIM-FACE-YTD ROUNDED = SBLIM-TAXABLE-YTD *
                       SBLIM-FEE-PCT / 200
               WHEN SBLIM-SERIES-CD = 'DD'
                   MOVE 3.00 TO SBLIM-FACE-YTD
               WHEN SBLIM-SERIES-CD = 'DF'
                   MOVE 15.00 TO SBLIM-FACE-YTD
               WHEN OTHER
                   COMPUTE SBLIM-FACE-YTD ROUNDED = SBLIM-TAXABLE-YTD *
                       SBLIM-FEE-PCT / 100
           END-EVALUATE
           IF SBLIM-FACE-YTD > 0 AND SBLIM-FACE-YTD < 2.00
               MOVE 2.00 TO SBLIM-FACE-YTD
           END-IF
           IF SBLIM-FACE-YTD > 25.00
               MOVE 25.00 TO SBLIM-FACE-YTD
           END-IF
           ADD SBLIM-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE TAXABLE YTD BY SERIES CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2480-ACCUM-SERIES.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBLIM-SERIES-CD
                   ADD SBLIM-TAXABLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBLIM-SERIES-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBLIM-TAXABLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUE PRICE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2490-SPLIT-ISSUE-PRICE.
           COMPUTE WS-WORK-AMT = SBLIM-ISSUE-PRICE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SBLIM-ISSUE-PRICE-YTD - (WS-WORK-AMT
               * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHED TIER FOR THE ISSUE PRICE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2500-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHED-RATE(1) TO SBLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF SBLIM-ISSUE-PRICE-YTD NOT <
                   WS-ISSUE-PR-SCHED-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHED-RATE(WS-TX) TO SBLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN LAST MAINT DATE IS PAST
      * J -> L WHEN LAST MAINT DATE IS PAST
      * L -> U WHEN LAST MAINT DATE IS PAST
      * U -> E WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2510-SET-SOURCE.
           EVALUATE SBLIM-SOURCE-CD
               WHEN 'E'
                   IF SBLIM-LAST-MAINT-DATE > 0 AND
                       SBLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'J' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBLIM-LAST-MAINT-DATE > 0 AND
                       SBLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBLIM-LAST-MAINT-DATE > 0 AND
                       SBLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'U' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBLIM-LAST-MAINT-DATE > 0 AND
                       SBLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'E' TO SBLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2520-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SBLIM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBLIM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SBLIM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2530-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SBLIM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBLIM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SBLIM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2540-FLAG-TAXABLE.
           IF SBLIM-TAXABLE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBLIM-TAXABLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN NEXT REVIEW DATE IS PAST
      * DF -> SS WHEN NEXT REVIEW DATE IS PAST
      * SS -> VJ WHEN NEXT REVIEW DATE IS PAST
      * VJ -> VX WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2550-SET-SERIES.
           EVALUATE SBLIM-SERIES-CD
               WHEN 'DD'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DF' TO SBLIM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SS' TO SBLIM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SS'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBLIM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJ'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VX' TO SBLIM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHED TIER FOR THE ISSUE PRICE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2560-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHED-RATE(1) TO SBLIM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF SBLIM-ISSUE-PRICE-YTD NOT <
                   WS-ISSUE-PR-SCHED-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHED-RATE(WS-TX) TO
                       SBLIM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBLIM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBLIM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2570-FLAG-TAXABLE.
           IF SBLIM-TAXABLE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBLIM-TAXABLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2580-FLAG-PURCHASE.
           IF SBLIM-PURCHASE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBLIM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-PAYMENT.
           IF SBLIM-PAYMENT-CNT < 9999999
               ADD 1 TO SBLIM-PAYMENT-CNT
           END-IF
           IF SBLIM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO SBLIM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PURCHASE AMT.
      *----------------------------------------------------------------*
       2600-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBLIM-PURCHASE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBLIM-PURCHASE-AMT
           END-IF
           ADD SBLIM-PURCHASE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2610-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBLIM-TAXABLE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SBLIM-TAXABLE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE SBLIM-TAXABLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2620-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SBLIM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBLIM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SBLIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAXABLE FEE ON THE PURCHASE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2630-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SBLIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SBLIM-TAXABLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBLIM-HOLD-FLAG = 'Y'
                   COMPUTE SBLIM-TAXABLE-AMT ROUNDED =
                       SBLIM-PURCHASE-AMT * SBLIM-FEE-PCT / 200
               WHEN SBLIM-SERIES-CD = 'DD'
                   MOVE 20.00 TO SBLIM-TAXABLE-AMT
               WHEN SBLIM-SERIES-CD = 'DF'
                   MOVE 7.00 TO SBLIM-TAXABLE-AMT
               WHEN OTHER
                   COMPUTE SBLIM-TAXABLE-AMT ROUNDED =
                       SBLIM-PURCHASE-AMT * SBLIM-FEE-PCT / 100
           END-EVALUATE
           IF SBLIM-TAXABLE-AMT > 0 AND SBLIM-TAXABLE-AMT < 5.00
               MOVE 5.00 TO SBLIM-TAXABLE-AMT
           END-IF
           IF SBLIM-TAXABLE-AMT > 150.00
               MOVE 150.00 TO SBLIM-TAXABLE-AMT
           END-IF
           ADD SBLIM-TAXABLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2640-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SBLIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2650-FLAG-FACE.
           IF SBLIM-FACE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBLIM-FACE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAXABLE AMT.
      *----------------------------------------------------------------*
       2660-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBLIM-TAXABLE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBLIM-TAXABLE-AMT
           END-IF
           ADD SBLIM-TAXABLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2670-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = SBLIM-PURCHASE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SBLIM-PURCHASE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAXABLE YTD.
      *----------------------------------------------------------------*
       2680-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBLIM-TAXABLE-YTD ROUNDED =
                   SBLIM-PURCHASE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBLIM-TAXABLE-YTD
           END-IF
           ADD SBLIM-TAXABLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN NEXT REVIEW DATE IS PAST
      * DY -> JA WHEN NEXT REVIEW DATE IS PAST
      * JA -> KT WHEN NEXT REVIEW DATE IS PAST
      * KT -> MF WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2690-SET-ISSUE-TYPE.
           EVALUATE SBLIM-ISSUE-TYPE-CD
               WHEN 'CW'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DY' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DY'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JA' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KT' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KT'
                   IF SBLIM-NEXT-REVIEW-DATE > 0 AND
                       SBLIM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MF' TO SBLIM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SBLIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SBLIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SBLIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SBLIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
