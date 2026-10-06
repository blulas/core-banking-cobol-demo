      *================================================================*
      * PROGRAM  : SBFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : SAVINGS BOND SERVICES RULE - VALIDATION. CALLED BY  *
      *            THE SAVINGS BOND SERVICES BATCH AND ONLINE PROGRAMS.*
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-28 RJT  ORIGINAL                                       *
      * 1999-09-17 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-11-23 MKL  ISSUE-PRICE TABLE EXPANDED REQ1267             *
      * 2004-11-08 EWS  INTEREST CUTOFF MOVED REQ4495                  *
      * 2005-01-03 GAF  NEW FACE REPORT COLUMNS INC7593                *
      * 2005-11-10 EWS  PURCHASE STATUS CODES ADDED CHG8098            *
      * 2005-05-27 EWS  TAXABLE LIMIT REVISED CHG7132                  *
      * 2005-03-22 SKN  REDEEM LIMIT REVISED REQ3155                   *
      * 2006-03-07 PQR  REDEEM CUTOFF MOVED PRB1685                    *
      * 2007-09-02 RJT  ISSUE-PRICE EXCEPTIONS TO OPS QUEUE REQ9877    *
      * 2007-10-16 EWS  ISSUE-PRICE CUTOFF MOVED CHG0720               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBFEE.
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
                                           VALUE 11.000000.
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
      * WS-PURCHASE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PURCHASE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000823500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000112500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000824000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000831000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030000000000045500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031000000000335000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031100000000771000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033600000000586000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00043600000000254000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00044600000000020500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00044850000000101000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00049850000000630000'.
       01  WS-PURCHASE-SCHED-TBL REDEFINES WS-PURCHASE-SCHED-VALUES.
           05  WS-PURCHASE-SCHED-ROW OCCURS 12 TIMES.
               10  WS-PURCHASE-SCHED-LOW   PIC 9(09)V99.
               10  WS-PURCHASE-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SBFEEP.

       PROCEDURE DIVISION USING SBFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-TAXABLE
           PERFORM 2010-FLAG-PURCHASE
           PERFORM 2020-DESCRIBE-BRANCH
           PERFORM 2030-COUNT-EXCEPT
           PERFORM 2040-ACCUM-REASON
           PERFORM 2050-AGE-OPEN
           PERFORM 2060-SPLIT-PURCHASE
           PERFORM 2070-SELECT-MARGIN
           PERFORM 2080-ASSESS-NOTICE-SENT
           PERFORM 2090-LIMIT-TAXABLE
           PERFORM 2100-SET-BRANCH
           PERFORM 2110-ACCUM-REASON
           PERFORM 2120-ACCRUE-TAXABLE
           PERFORM 2130-ACCUM-REASON
           PERFORM 2140-AGE-LAST-MAINT
           PERFORM 2150-ACCRUE-MARGIN
           PERFORM 2160-ASSESS-REVIEW
           PERFORM 2170-CHECK-NOTICE
           PERFORM 2180-AGE-NEXT-DUE
           PERFORM 2190-ACCUM-ISSUE-TYPE
           PERFORM 2200-SPLIT-FACE
           PERFORM 2210-FLAG-ACCRUAL
           PERFORM 2220-LIMIT-INTEREST
           PERFORM 2230-COUNT-EXCEPT
           PERFORM 2240-AGE-OPEN
           PERFORM 2250-COUNT-RETRY
           PERFORM 2260-FLAG-FACE
           PERFORM 2270-AGE-NOTICE
           PERFORM 2280-COUNT-RETRY
           PERFORM 2290-SET-REASON
           PERFORM 2300-SPLIT-REDEEM
           PERFORM 2310-COUNT-RETRY
           PERFORM 2320-LIMIT-FACE
           PERFORM 2330-DESCRIBE-REASON
           PERFORM 2340-EDIT-CONTACT
           PERFORM 2350-ASSESS-NOTICE-SENT
           PERFORM 2360-SELECT-PENALTY-PCT
           PERFORM 2370-FLAG-ACCRUAL
           PERFORM 2380-ACCRUE-ACCRUAL
           PERFORM 2390-SET-ISSUE-TYPE
           PERFORM 2400-ASSESS-REVIEW
           PERFORM 2410-SELECT-PENALTY-PCT
           PERFORM 2420-CHECK-NOTICE
           PERFORM 2430-SELECT-MARGIN
           PERFORM 2440-SELECT-PENALTY-PCT
           PERFORM 2450-ACCUM-ISSUE-TYPE
           PERFORM 2460-ACCRUE-MARGIN
           PERFORM 2470-SET-STATUS
           PERFORM 2480-DESCRIBE-REASON
           PERFORM 2490-AGE-OPEN
           PERFORM 2500-SET-REASON
           PERFORM 2510-ACCUM-REASON
           PERFORM 2520-ACCUM-ISSUE-TYPE
           PERFORM 2530-AGE-NEXT-DUE
           PERFORM 2540-COUNT-RETRY
           PERFORM 2550-COUNT-EXCEPT
           PERFORM 2560-EDIT-CONTACT
           PERFORM 2570-SET-BRANCH
           PERFORM 2580-ASSESS-NOTICE-SENT
           PERFORM 2590-EDIT-CONTACT
           PERFORM 2600-PRORATE-INTEREST
           PERFORM 2610-ASSESS-NOTICE-SENT
           PERFORM 2620-AGE-LAST-MAINT
           PERFORM 2630-ACCRUE-ACCRUAL
           PERFORM 2640-PRORATE-REDEEM
           PERFORM 2650-SELECT-MARGIN
           PERFORM 2660-EDIT-DESC
           PERFORM 2670-ACCRUE-INTEREST
           PERFORM 2680-SPLIT-REDEEM
           PERFORM 2690-ACCUM-REASON
           PERFORM 2700-DESCRIBE-BRANCH
           PERFORM 2710-ASSESS-REVIEW
           PERFORM 2720-ACCRUE-ACCRUAL
           PERFORM 2730-AGE-NOTICE
           PERFORM 2740-SPLIT-TAXABLE
           PERFORM 2750-DESCRIBE-BRANCH
           PERFORM 2760-ACCRUE-FACE
           PERFORM 2770-PRORATE-TAXABLE
           PERFORM 2780-PRORATE-REDEEM
           PERFORM 2790-PRORATE-FACE
           PERFORM 2800-SPLIT-FACE
           PERFORM 2810-SPLIT-FACE
           PERFORM 2820-PRORATE-TAXABLE
           PERFORM 2830-LIMIT-ITEM
           PERFORM 2840-SPLIT-FACE
           PERFORM 2850-EDIT-CONTACT
           PERFORM 2860-LIMIT-ITEM
           PERFORM 2870-ACCUM-STATUS
           PERFORM 2880-FLAG-ITEM
           PERFORM 2890-ASSESS-NOTICE-SENT
           PERFORM 2900-CHECK-LAST-MAINT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SBFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAXABLE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-TAXABLE.
           IF SBFEE-TAXABLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBFEE-TAXABLE-AMT * SBFEE-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2010-FLAG-PURCHASE.
           IF SBFEE-PURCHASE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBFEE-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2020-DESCRIBE-BRANCH.
           EVALUATE SBFEE-BRANCH-CD
               WHEN 'BD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2030-COUNT-EXCEPT.
           IF SBFEE-EXCEPT-CNT < 9999999
               ADD 1 TO SBFEE-EXCEPT-CNT
           END-IF
           IF SBFEE-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO SBFEE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2040-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBFEE-REASON-CD
                   ADD SBFEE-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBFEE-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-OPEN-DATE > 0 AND SBFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = SBFEE-PURCHASE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SBFEE-PURCHASE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE SBFEE-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE ACCRUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2070-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO SBFEE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SBFEE-ACCRUAL-AMT NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO SBFEE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBFEE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBFEE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE INTEREST YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2080-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBFEE-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SBFEE-INTEREST-YTD *
                       SBFEE-PENALTY-PCT / 200
               WHEN SBFEE-ISSUE-TYPE-CD = 'CW'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN SBFEE-ISSUE-TYPE-CD = 'DY'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SBFEE-INTEREST-YTD *
                       SBFEE-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TAXABLE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2090-LIMIT-TAXABLE.
           IF SBFEE-TAXABLE-AMT < 0.00
               MOVE 0.00 TO SBFEE-TAXABLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBFEE-TAXABLE-AMT > 500.00
               MOVE 500.00 TO SBFEE-TAXABLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN NOTICE DATE IS PAST
      * FC -> KR WHEN NOTICE DATE IS PAST
      * KR -> MP WHEN NOTICE DATE IS PAST
      * MP -> VJ WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2100-SET-BRANCH.
           EVALUATE SBFEE-BRANCH-CD
               WHEN 'BD'
                   IF SBFEE-NOTICE-DATE > 0 AND SBFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO SBFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF SBFEE-NOTICE-DATE > 0 AND SBFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO SBFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF SBFEE-NOTICE-DATE > 0 AND SBFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MP' TO SBFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MP'
                   IF SBFEE-NOTICE-DATE > 0 AND SBFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBFEE-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAXABLE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2120-ACCRUE-TAXABLE.
           IF SBFEE-TAXABLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBFEE-TAXABLE-AMT * SBFEE-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2130-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBFEE-REASON-CD
                   ADD SBFEE-REDEEM-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBFEE-REDEEM-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-LAST-MAINT-DATE > 0 AND SBFEE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBFEE-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBFEE-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBFEE-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBFEE-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBFEE-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2150-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SBFEE-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAXABLE FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2160-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBFEE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SBFEE-TAXABLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBFEE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SBFEE-TAXABLE-AMT ROUNDED = WS-WORK-AMT-2 *
                       SBFEE-PENALTY-PCT / 200
               WHEN SBFEE-STATUS-CD = 'CS'
                   MOVE 10.00 TO SBFEE-TAXABLE-AMT
               WHEN SBFEE-STATUS-CD = 'HF'
                   MOVE 7.00 TO SBFEE-TAXABLE-AMT
               WHEN OTHER
                   COMPUTE SBFEE-TAXABLE-AMT ROUNDED = WS-WORK-AMT-2 *
                       SBFEE-PENALTY-PCT / 100
           END-EVALUATE
           IF SBFEE-TAXABLE-AMT > 0 AND SBFEE-TAXABLE-AMT < 5.00
               MOVE 5.00 TO SBFEE-TAXABLE-AMT
           END-IF
           IF SBFEE-TAXABLE-AMT > 35.00
               MOVE 35.00 TO SBFEE-TAXABLE-AMT
           END-IF
           ADD SBFEE-TAXABLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2170-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SBFEE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2180-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-NEXT-DUE-DATE > 0 AND SBFEE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE AMT BY ISSUE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2190-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBFEE-ISSUE-TYPE-CD
                   ADD SBFEE-PURCHASE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBFEE-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBFEE-PURCHASE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2200-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBFEE-FACE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SBFEE-FACE-YTD - (WS-WORK-AMT * 2)
           COMPUTE SBFEE-REDEEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2210-FLAG-ACCRUAL.
           IF SBFEE-ACCRUAL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBFEE-ACCRUAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2220-LIMIT-INTEREST.
           IF SBFEE-INTEREST-YTD < 10.00
               MOVE 10.00 TO SBFEE-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBFEE-INTEREST-YTD > 5000.00
               MOVE 5000.00 TO SBFEE-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-EXCEPT.
           IF SBFEE-EXCEPT-CNT < 9999999
               ADD 1 TO SBFEE-EXCEPT-CNT
           END-IF
           IF SBFEE-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO SBFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2240-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-OPEN-DATE > 0 AND SBFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2250-COUNT-RETRY.
           IF SBFEE-RETRY-CNT < 9999999
               ADD 1 TO SBFEE-RETRY-CNT
           END-IF
           IF SBFEE-RETRY-CNT NOT < 12
               MOVE 'Y' TO SBFEE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2260-FLAG-FACE.
           IF SBFEE-FACE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBFEE-FACE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-NOTICE-DATE > 0 AND SBFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBFEE-REDEEM-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBFEE-REDEEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBFEE-REDEEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SBFEE-REDEEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBFEE-REDEEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-RETRY.
           IF SBFEE-RETRY-CNT < 9999999
               ADD 1 TO SBFEE-RETRY-CNT
           END-IF
           IF SBFEE-RETRY-CNT NOT < 3
               MOVE 'Y' TO SBFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DG -> UK WHEN LAST MAINT DATE IS PAST
      * UK -> VH WHEN LAST MAINT DATE IS PAST
      * VH -> XD WHEN LAST MAINT DATE IS PAST
      * XD -> DG WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2290-SET-REASON.
           EVALUATE SBFEE-REASON-CD
               WHEN 'DG'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UK' TO SBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VH' TO SBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VH'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XD' TO SBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'DG' TO SBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REDEEM YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2300-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBFEE-REDEEM-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SBFEE-REDEEM-YTD - (WS-WORK-AMT * 3)
           COMPUTE SBFEE-ACCRUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2310-COUNT-RETRY.
           IF SBFEE-RETRY-CNT < 9999999
               ADD 1 TO SBFEE-RETRY-CNT
           END-IF
           IF SBFEE-RETRY-CNT NOT < 5
               MOVE 'Y' TO SBFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2320-LIMIT-FACE.
           IF SBFEE-FACE-YTD < 0.00
               MOVE 0.00 TO SBFEE-FACE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBFEE-FACE-YTD > 5000.00
               MOVE 5000.00 TO SBFEE-FACE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2330-DESCRIBE-REASON.
           EVALUATE SBFEE-REASON-CD
               WHEN 'DG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E660 CONTACT REQUIRED
      * E866 FACE YTD NEGATIVE
      * E523 FACE YTD OVER 1000000
      * E084 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBFEE-CONTACT = SPACES
                   MOVE 'E660' TO WS-REJECT-CODE
               WHEN SBFEE-FACE-YTD < 0
                   MOVE 'E866' TO WS-REJECT-CODE
               WHEN SBFEE-FACE-YTD > 1000000.00
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN SBFEE-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E084' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2350-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SBFEE-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBFEE-REVIEW-FLAG = 'Y'
                   COMPUTE SBFEE-FACE-YTD ROUNDED = WS-WORK-AMT-2 *
                       SBFEE-MARGIN / 200
               WHEN SBFEE-ISSUE-TYPE-CD = 'CW'
                   MOVE 7.00 TO SBFEE-FACE-YTD
               WHEN SBFEE-ISSUE-TYPE-CD = 'DY'
                   MOVE 20.00 TO SBFEE-FACE-YTD
               WHEN OTHER
                   COMPUTE SBFEE-FACE-YTD ROUNDED = WS-WORK-AMT-2 *
                       SBFEE-MARGIN / 100
           END-EVALUATE
           IF SBFEE-FACE-YTD > 0 AND SBFEE-FACE-YTD < 1.00
               MOVE 1.00 TO SBFEE-FACE-YTD
           END-IF
           IF SBFEE-FACE-YTD > 75.00
               MOVE 75.00 TO SBFEE-FACE-YTD
           END-IF
           ADD SBFEE-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE FACE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2360-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO SBFEE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SBFEE-FACE-YTD NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO
                       SBFEE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBFEE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBFEE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2370-FLAG-ACCRUAL.
           IF SBFEE-ACCRUAL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBFEE-ACCRUAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUAL AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-ACCRUAL.
           IF SBFEE-ACCRUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBFEE-ACCRUAL-AMT * SBFEE-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBFEE-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN LAST MAINT DATE IS PAST
      * DY -> JA WHEN LAST MAINT DATE IS PAST
      * JA -> KT WHEN LAST MAINT DATE IS PAST
      * KT -> MF WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2390-SET-ISSUE-TYPE.
           EVALUATE SBFEE-ISSUE-TYPE-CD
               WHEN 'CW'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'DY' TO SBFEE-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DY'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JA' TO SBFEE-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KT' TO SBFEE-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KT'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'MF' TO SBFEE-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE REDEEM YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2400-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBFEE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SBFEE-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBFEE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SBFEE-PURCHASE-AMT ROUNDED = SBFEE-REDEEM-YTD
                       * SBFEE-MARGIN / 200
               WHEN SBFEE-BRANCH-CD = 'BD'
                   MOVE 15.00 TO SBFEE-PURCHASE-AMT
               WHEN SBFEE-BRANCH-CD = 'FC'
                   MOVE 5.00 TO SBFEE-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE SBFEE-PURCHASE-AMT ROUNDED = SBFEE-REDEEM-YTD
                       * SBFEE-MARGIN / 100
           END-EVALUATE
           IF SBFEE-PURCHASE-AMT > 0 AND SBFEE-PURCHASE-AMT < 10.00
               MOVE 10.00 TO SBFEE-PURCHASE-AMT
           END-IF
           IF SBFEE-PURCHASE-AMT > 25.00
               MOVE 25.00 TO SBFEE-PURCHASE-AMT
           END-IF
           ADD SBFEE-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2410-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO SBFEE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO
                       SBFEE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBFEE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBFEE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2420-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SBFEE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE REDEEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2430-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO SBFEE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SBFEE-REDEEM-YTD NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO SBFEE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBFEE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBFEE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE PURCHASE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO SBFEE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SBFEE-PURCHASE-AMT NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO
                       SBFEE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBFEE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBFEE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY ISSUE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2450-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBFEE-ISSUE-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBFEE-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SBFEE-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO SBFEE-ACCRUAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CS -> HF WHEN NEXT DUE DATE IS PAST
      * HF -> JZ WHEN NEXT DUE DATE IS PAST
      * JZ -> CS WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-STATUS.
           EVALUATE SBFEE-STATUS-CD
               WHEN 'CS'
                   IF SBFEE-NEXT-DUE-DATE > 0 AND SBFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO SBFEE-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF SBFEE-NEXT-DUE-DATE > 0 AND SBFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SBFEE-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SBFEE-NEXT-DUE-DATE > 0 AND SBFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CS' TO SBFEE-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-REASON.
           EVALUATE SBFEE-REASON-CD
               WHEN 'DG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2490-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-OPEN-DATE > 0 AND SBFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBFEE-ACCRUAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SBFEE-ACCRUAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBFEE-ACCRUAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBFEE-ACCRUAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBFEE-ACCRUAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DG -> UK WHEN OPEN DATE IS PAST
      * UK -> VH WHEN OPEN DATE IS PAST
      * VH -> XD WHEN OPEN DATE IS PAST
      * XD -> DG WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2500-SET-REASON.
           EVALUATE SBFEE-REASON-CD
               WHEN 'DG'
                   IF SBFEE-OPEN-DATE > 0 AND SBFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO SBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF SBFEE-OPEN-DATE > 0 AND SBFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VH' TO SBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VH'
                   IF SBFEE-OPEN-DATE > 0 AND SBFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO SBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF SBFEE-OPEN-DATE > 0 AND SBFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DG' TO SBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUAL AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBFEE-REASON-CD
                   ADD SBFEE-ACCRUAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBFEE-ACCRUAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE AMT BY ISSUE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2520-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBFEE-ISSUE-TYPE-CD
                   ADD SBFEE-PURCHASE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBFEE-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBFEE-PURCHASE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2530-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-NEXT-DUE-DATE > 0 AND SBFEE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBFEE-REDEEM-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SBFEE-REDEEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBFEE-REDEEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBFEE-REDEEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBFEE-REDEEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-RETRY.
           IF SBFEE-RETRY-CNT < 9999999
               ADD 1 TO SBFEE-RETRY-CNT
           END-IF
           IF SBFEE-RETRY-CNT NOT < 6
               MOVE 'Y' TO SBFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2550-COUNT-EXCEPT.
           IF SBFEE-EXCEPT-CNT < 9999999
               ADD 1 TO SBFEE-EXCEPT-CNT
           END-IF
           IF SBFEE-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO SBFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E647 CONTACT REQUIRED
      * E338 WORK AMT 2 NEGATIVE
      * E738 WORK AMT 2 OVER 99999
      * E201 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2560-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBFEE-CONTACT = SPACES
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E338' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E738' TO WS-REJECT-CODE
               WHEN SBFEE-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E201' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN LAST MAINT DATE IS PAST
      * FC -> KR WHEN LAST MAINT DATE IS PAST
      * KR -> MP WHEN LAST MAINT DATE IS PAST
      * MP -> VJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2570-SET-BRANCH.
           EVALUATE SBFEE-BRANCH-CD
               WHEN 'BD'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FC' TO SBFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KR' TO SBFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'MP' TO SBFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MP'
                   IF SBFEE-LAST-MAINT-DATE > 0 AND
                       SBFEE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE TAXABLE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2580-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SBFEE-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBFEE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SBFEE-FACE-YTD ROUNDED = SBFEE-TAXABLE-AMT *
                       SBFEE-MARGIN / 200
               WHEN SBFEE-REASON-CD = 'DG'
                   MOVE 7.00 TO SBFEE-FACE-YTD
               WHEN SBFEE-REASON-CD = 'UK'
                   MOVE 7.00 TO SBFEE-FACE-YTD
               WHEN OTHER
                   COMPUTE SBFEE-FACE-YTD ROUNDED = SBFEE-TAXABLE-AMT *
                       SBFEE-MARGIN / 100
           END-EVALUATE
           IF SBFEE-FACE-YTD > 0 AND SBFEE-FACE-YTD < 1.00
               MOVE 1.00 TO SBFEE-FACE-YTD
           END-IF
           IF SBFEE-FACE-YTD > 75.00
               MOVE 75.00 TO SBFEE-FACE-YTD
           END-IF
           ADD SBFEE-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E694 CONTACT REQUIRED
      * E059 INTEREST YTD NEGATIVE
      * E038 INTEREST YTD OVER 250000
      * E882 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2590-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBFEE-CONTACT = SPACES
                   MOVE 'E694' TO WS-REJECT-CODE
               WHEN SBFEE-INTEREST-YTD < 0
                   MOVE 'E059' TO WS-REJECT-CODE
               WHEN SBFEE-INTEREST-YTD > 250000.00
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN SBFEE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E882' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       2600-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBFEE-INTEREST-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBFEE-INTEREST-YTD
           END-IF
           ADD SBFEE-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2610-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SBFEE-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBFEE-REVIEW-FLAG = 'Y'
                   COMPUTE SBFEE-FACE-YTD ROUNDED = WS-WORK-AMT *
                       SBFEE-MARGIN / 200
               WHEN SBFEE-STATUS-CD = 'CS'
                   MOVE 12.00 TO SBFEE-FACE-YTD
               WHEN SBFEE-STATUS-CD = 'HF'
                   MOVE 5.00 TO SBFEE-FACE-YTD
               WHEN OTHER
                   COMPUTE SBFEE-FACE-YTD ROUNDED = WS-WORK-AMT *
                       SBFEE-MARGIN / 100
           END-EVALUATE
           IF SBFEE-FACE-YTD > 0 AND SBFEE-FACE-YTD < 5.00
               MOVE 5.00 TO SBFEE-FACE-YTD
           END-IF
           IF SBFEE-FACE-YTD > 25.00
               MOVE 25.00 TO SBFEE-FACE-YTD
           END-IF
           ADD SBFEE-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2620-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-LAST-MAINT-DATE > 0 AND SBFEE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUAL AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2630-ACCRUE-ACCRUAL.
           IF SBFEE-ACCRUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBFEE-ACCRUAL-AMT * SBFEE-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REDEEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2640-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBFEE-REDEEM-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2650-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO SBFEE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SBFEE-INTEREST-YTD NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO SBFEE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBFEE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBFEE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E409 DESC REQUIRED
      * E635 ACCRUAL AMT NEGATIVE
      * E091 ACCRUAL AMT OVER 1000000
      * E215 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2660-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBFEE-DESC = SPACES
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN SBFEE-ACCRUAL-AMT < 0
                   MOVE 'E635' TO WS-REJECT-CODE
               WHEN SBFEE-ACCRUAL-AMT > 1000000.00
                   MOVE 'E091' TO WS-REJECT-CODE
               WHEN SBFEE-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2670-ACCRUE-INTEREST.
           IF SBFEE-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBFEE-INTEREST-YTD * SBFEE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REDEEM YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2680-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBFEE-REDEEM-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBFEE-REDEEM-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2690-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBFEE-REASON-CD
                   ADD SBFEE-REDEEM-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBFEE-REDEEM-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2700-DESCRIBE-BRANCH.
           EVALUATE SBFEE-BRANCH-CD
               WHEN 'BD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE INTEREST YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2710-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBFEE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBFEE-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SBFEE-INTEREST-YTD *
                       SBFEE-PENALTY-PCT / 200
               WHEN SBFEE-REASON-CD = 'DG'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN SBFEE-REASON-CD = 'UK'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SBFEE-INTEREST-YTD *
                       SBFEE-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUAL AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2720-ACCRUE-ACCRUAL.
           IF SBFEE-ACCRUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBFEE-ACCRUAL-AMT * SBFEE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBFEE-PURCHASE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2730-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-NOTICE-DATE > 0 AND SBFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2740-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBFEE-TAXABLE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SBFEE-TAXABLE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SBFEE-REDEEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2750-DESCRIBE-BRANCH.
           EVALUATE SBFEE-BRANCH-CD
               WHEN 'BD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2760-ACCRUE-FACE.
           IF SBFEE-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBFEE-FACE-YTD * SBFEE-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBFEE-PURCHASE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2770-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBFEE-TAXABLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE REDEEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2780-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBFEE-REDEEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FACE YTD.
      *----------------------------------------------------------------*
       2790-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBFEE-FACE-YTD ROUNDED =
                   SBFEE-INTEREST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBFEE-FACE-YTD
           END-IF
           ADD SBFEE-FACE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2800-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBFEE-FACE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBFEE-FACE-YTD - (WS-WORK-AMT * 4)
           COMPUTE SBFEE-ACCRUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2810-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBFEE-FACE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBFEE-FACE-YTD - (WS-WORK-AMT * 4)
           COMPUTE SBFEE-REDEEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2820-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SBFEE-TAXABLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2830-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2840-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBFEE-FACE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SBFEE-FACE-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E440 CONTACT REQUIRED
      * E348 ACCRUAL AMT NEGATIVE
      * E418 ACCRUAL AMT OVER 10000
      * E289 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2850-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBFEE-CONTACT = SPACES
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN SBFEE-ACCRUAL-AMT < 0
                   MOVE 'E348' TO WS-REJECT-CODE
               WHEN SBFEE-ACCRUAL-AMT > 10000.00
                   MOVE 'E418' TO WS-REJECT-CODE
               WHEN SBFEE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E289' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2860-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2870-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBFEE-STATUS-CD
                   ADD SBFEE-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBFEE-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBFEE-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2880-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE ACCRUAL AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2890-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SBFEE-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBFEE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SBFEE-PURCHASE-AMT ROUNDED =
                       SBFEE-ACCRUAL-AMT * SBFEE-PENALTY-PCT / 200
               WHEN SBFEE-REASON-CD = 'DG'
                   MOVE 20.00 TO SBFEE-PURCHASE-AMT
               WHEN SBFEE-REASON-CD = 'UK'
                   MOVE 12.00 TO SBFEE-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE SBFEE-PURCHASE-AMT ROUNDED =
                       SBFEE-ACCRUAL-AMT * SBFEE-PENALTY-PCT / 100
           END-EVALUATE
           IF SBFEE-PURCHASE-AMT > 0 AND SBFEE-PURCHASE-AMT < 10.00
               MOVE 10.00 TO SBFEE-PURCHASE-AMT
           END-IF
           IF SBFEE-PURCHASE-AMT > 150.00
               MOVE 150.00 TO SBFEE-PURCHASE-AMT
           END-IF
           ADD SBFEE-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2900-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBFEE-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBFEE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SBFEE-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SBFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SBFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SBFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SBFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
