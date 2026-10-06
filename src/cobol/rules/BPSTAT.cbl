      *================================================================*
      * PROGRAM  : BPSTAT                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : BILL PAYMENT SERVICE RULE - PRICING. CALLED BY THE  *
      *            BILL PAYMENT SERVICE BATCH AND ONLINE PROGRAMS.     *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-06-14 MKL  ORIGINAL                                       *
      * 1996-05-10 EWS  PAYMENT STATUS CODES ADDED INC0297             *
      * 1999-08-14 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-05-18 RJT  AUDIT FINDING - PAYMENT CONTROL TOTALS INC5815 *
      * 2001-12-12 DPS  RETURNED CUTOFF MOVED PRB0697                  *
      * 2002-07-18 RJT  REFUND CUTOFF MOVED INC8349                    *
      * 2004-05-07 PQR  PAYMENT CALCULATION CORRECTED CHG5995          *
      * 2005-07-03 SKN  PAYMENT NOTICE TEXT PER LEGAL REQ4588          *
      * 2007-07-20 PQR  RETURNED CUTOFF MOVED PRB2063                  *
      * 2008-03-20 LMB  FEE TABLE EXPANDED CHG5424                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPSTAT.
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
                                           VALUE 12.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 30.
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
                                           VALUE '00000000000000068000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000458500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000095000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000028000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007650000000109000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000363000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010650000000672000'.
       01  WS-REFUND-TIER-TBL REDEFINES WS-REFUND-TIER-VALUES.
           05  WS-REFUND-TIER-ROW OCCURS 7 TIMES.
               10  WS-REFUND-TIER-LOW      PIC 9(09)V99.
               10  WS-REFUND-TIER-RATE     PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY BPSTATP.

       PROCEDURE DIVISION USING BPSTAT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ASSESS-SUSPEND
           PERFORM 2010-PRORATE-PAYMENT
           PERFORM 2020-DESCRIBE-STATUS
           PERFORM 2030-LIMIT-RETURNED
           PERFORM 2040-SELECT-PENALTY-PCT
           PERFORM 2050-EDIT-REF-NO
           PERFORM 2060-EDIT-CUST-ID
           PERFORM 2070-SPLIT-RETURNED
           PERFORM 2080-ASSESS-AUTO-RENEW
           PERFORM 2090-CHECK-MATURITY
           PERFORM 2100-EDIT-CUST-ID
           PERFORM 2110-ASSESS-SUSPEND
           PERFORM 2120-EDIT-CUST-ID
           PERFORM 2130-LIMIT-RETURNED
           PERFORM 2140-ACCRUE-FEE
           PERFORM 2150-ACCUM-STATUS
           PERFORM 2160-COUNT-PAYMENT
           PERFORM 2170-LIMIT-RETURNED
           PERFORM 2180-ACCRUE-REFUND
           PERFORM 2190-SELECT-RATE
           PERFORM 2200-ACCRUE-PAYMENT
           PERFORM 2210-DESCRIBE-REMIT-METHOD
           PERFORM 2220-CHECK-LAST-STMT
           PERFORM 2230-SET-REMIT-METHOD
           PERFORM 2240-CHECK-OPEN
           PERFORM 2250-SELECT-PENALTY-PCT
           PERFORM 2260-ACCUM-REMIT-METHOD
           PERFORM 2270-PRORATE-PAYMENT
           PERFORM 2280-EDIT-REF-NO
           PERFORM 2290-LIMIT-PAYMENT
           PERFORM 2300-ACCUM-REASON
           PERFORM 2310-LIMIT-REFUND
           PERFORM 2320-AGE-LAST-STMT
           PERFORM 2330-SPLIT-RETURNED
           PERFORM 2340-DESCRIBE-CYCLE
           PERFORM 2350-SELECT-PENALTY-PCT
           PERFORM 2360-SELECT-PENALTY-PCT
           PERFORM 2370-COUNT-EXCEPT
           PERFORM 2380-ACCUM-REMIT-METHOD
           PERFORM 2390-SPLIT-REFUND
           PERFORM 2400-EDIT-CUST-ID
           PERFORM 2410-PRORATE-FEE
           PERFORM 2420-FLAG-REFUND
           PERFORM 2430-DESCRIBE-CYCLE
           PERFORM 2440-EDIT-REF-NO
           PERFORM 2450-ACCUM-STATUS
           PERFORM 2460-FLAG-RETURNED
           PERFORM 2470-SPLIT-FEE
           PERFORM 2480-ASSESS-EXEMPT
           PERFORM 2490-CHECK-MATURITY
           PERFORM 2500-EDIT-CUST-ID
           PERFORM 2510-SPLIT-FEE
           PERFORM 2520-ACCRUE-REFUND
           PERFORM 2530-COUNT-PAYMENT
           PERFORM 2540-COUNT-EXCEPT
           PERFORM 2550-SPLIT-RETURNED
           PERFORM 2560-FLAG-ITEM
           PERFORM 2570-AGE-CLOSE
           PERFORM 2580-ACCUM-STATUS
           PERFORM 2590-SPLIT-PAYMENT
           PERFORM 2600-COUNT-PAYMENT
           PERFORM 2610-SPLIT-RETURNED
           PERFORM 2620-CHECK-CLOSE
           PERFORM 2630-SET-REASON
           PERFORM 2640-ACCUM-STATUS
           PERFORM 2650-ACCRUE-REFUND
           PERFORM 2660-FLAG-PAYMENT
           PERFORM 2670-DESCRIBE-STATUS
           PERFORM 2680-SPLIT-RETURNED
           PERFORM 2690-AGE-MATURITY
           PERFORM 2700-SELECT-PENALTY-PCT
           PERFORM 2710-LIMIT-PAYMENT
           PERFORM 2720-SELECT-PENALTY-PCT
           PERFORM 2730-LIMIT-PAYMENT
           PERFORM 2740-COUNT-PAYMENT
           PERFORM 2750-AGE-MATURITY
           PERFORM 2760-ACCUM-REMIT-METHOD
           PERFORM 2770-SET-REASON
           PERFORM 2780-DESCRIBE-STATUS
           PERFORM 2790-ACCRUE-PAYMENT
           PERFORM 2800-PRORATE-RETURNED
           PERFORM 2810-LIMIT-ITEM
           PERFORM 2820-SPLIT-RETURNED
           PERFORM 2830-SET-CYCLE
           PERFORM 2840-AGE-CLOSE
           PERFORM 2850-SELECT-PENALTY-PCT
           PERFORM 2860-ACCRUE-RETURNED
           PERFORM 2870-DESCRIBE-CYCLE
           PERFORM 2880-ASSESS-SUSPEND
           PERFORM 2890-CHECK-MATURITY
           PERFORM 2900-ASSESS-EXEMPT
           PERFORM 2910-COUNT-EXCEPT
           PERFORM 2920-LIMIT-RETURNED
           PERFORM 2930-CHECK-LAST-STMT
           PERFORM 2940-SPLIT-RETURNED
           PERFORM 2950-SELECT-RATE
           PERFORM 2960-SELECT-PENALTY-PCT
           PERFORM 2970-COUNT-EXCEPT
           PERFORM 2980-SELECT-PENALTY-PCT
           PERFORM 2990-ACCRUE-FEE
           PERFORM 3000-EDIT-CUST-ID
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO BPSTAT-RETURN-CODE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PAYMENT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2000-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPSTAT-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPSTAT-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = BPSTAT-PAYMENT-YTD *
                       BPSTAT-PENALTY-PCT / 200
               WHEN BPSTAT-STATUS-CD = 'JX'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN BPSTAT-STATUS-CD = 'LR'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = BPSTAT-PAYMENT-YTD *
                       BPSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2010-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BPSTAT-PAYMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2020-DESCRIBE-STATUS.
           EVALUATE BPSTAT-STATUS-CD
               WHEN 'JX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETURNED AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2030-LIMIT-RETURNED.
           IF BPSTAT-RETURNED-AMT < 100.00
               MOVE 100.00 TO BPSTAT-RETURNED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPSTAT-RETURNED-AMT > 500.00
               MOVE 500.00 TO BPSTAT-RETURNED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPSTAT-REFUND-AMT NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E479 REF NO REQUIRED
      * E033 RETURNED AMT NEGATIVE
      * E289 RETURNED AMT OVER 250000
      * E244 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPSTAT-REF-NO = SPACES
                   MOVE 'E479' TO WS-REJECT-CODE
               WHEN BPSTAT-RETURNED-AMT < 0
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN BPSTAT-RETURNED-AMT > 250000.00
                   MOVE 'E289' TO WS-REJECT-CODE
               WHEN BPSTAT-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E092 CUST ID REQUIRED
      * E893 REFUND AMT NEGATIVE
      * E823 REFUND AMT OVER 5000
      * E854 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2060-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPSTAT-CUST-ID = SPACES
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN BPSTAT-REFUND-AMT < 0
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN BPSTAT-REFUND-AMT > 5000.00
                   MOVE 'E823' TO WS-REJECT-CODE
               WHEN BPSTAT-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E854' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETURNED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPSTAT-RETURNED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BPSTAT-RETURNED-YTD - (WS-WORK-AMT *
               2)
           COMPUTE BPSTAT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2080-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN BPSTAT-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO BPSTAT-RETURNED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPSTAT-EXEMPT-FLAG = 'Y'
                   COMPUTE BPSTAT-RETURNED-YTD ROUNDED = WS-WORK-AMT-2 *
                       BPSTAT-RATE / 200
               WHEN BPSTAT-REASON-CD = 'AX'
                   MOVE 12.00 TO BPSTAT-RETURNED-YTD
               WHEN BPSTAT-REASON-CD = 'BX'
                   MOVE 20.00 TO BPSTAT-RETURNED-YTD
               WHEN OTHER
                   COMPUTE BPSTAT-RETURNED-YTD ROUNDED = WS-WORK-AMT-2 *
                       BPSTAT-RATE / 100
           END-EVALUATE
           IF BPSTAT-RETURNED-YTD > 0 AND BPSTAT-RETURNED-YTD < 1.00
               MOVE 1.00 TO BPSTAT-RETURNED-YTD
           END-IF
           IF BPSTAT-RETURNED-YTD > 50.00
               MOVE 50.00 TO BPSTAT-RETURNED-YTD
           END-IF
           ADD BPSTAT-RETURNED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2090-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BPSTAT-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E360 CUST ID REQUIRED
      * E603 RETURNED YTD NEGATIVE
      * E664 RETURNED YTD OVER 25000
      * E783 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPSTAT-CUST-ID = SPACES
                   MOVE 'E360' TO WS-REJECT-CODE
               WHEN BPSTAT-RETURNED-YTD < 0
                   MOVE 'E603' TO WS-REJECT-CODE
               WHEN BPSTAT-RETURNED-YTD > 25000.00
                   MOVE 'E664' TO WS-REJECT-CODE
               WHEN BPSTAT-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE RETURNED YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2110-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPSTAT-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPSTAT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPSTAT-EXEMPT-FLAG = 'Y'
                   COMPUTE BPSTAT-PAYMENT-AMT ROUNDED =
                       BPSTAT-RETURNED-YTD * BPSTAT-RATE / 200
               WHEN BPSTAT-REMIT-METHOD-CD = 'CGV'
                   MOVE 3.00 TO BPSTAT-PAYMENT-AMT
               WHEN BPSTAT-REMIT-METHOD-CD = 'FNS'
                   MOVE 5.00 TO BPSTAT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPSTAT-PAYMENT-AMT ROUNDED =
                       BPSTAT-RETURNED-YTD * BPSTAT-RATE / 100
           END-EVALUATE
           IF BPSTAT-PAYMENT-AMT > 0 AND BPSTAT-PAYMENT-AMT < 10.00
               MOVE 10.00 TO BPSTAT-PAYMENT-AMT
           END-IF
           IF BPSTAT-PAYMENT-AMT > 35.00
               MOVE 35.00 TO BPSTAT-PAYMENT-AMT
           END-IF
           ADD BPSTAT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E004 CUST ID REQUIRED
      * E890 RETURNED AMT NEGATIVE
      * E754 RETURNED AMT OVER 1000000
      * E125 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2120-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPSTAT-CUST-ID = SPACES
                   MOVE 'E004' TO WS-REJECT-CODE
               WHEN BPSTAT-RETURNED-AMT < 0
                   MOVE 'E890' TO WS-REJECT-CODE
               WHEN BPSTAT-RETURNED-AMT > 1000000.00
                   MOVE 'E754' TO WS-REJECT-CODE
               WHEN BPSTAT-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E125' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2130-LIMIT-RETURNED.
           IF BPSTAT-RETURNED-AMT < 100.00
               MOVE 100.00 TO BPSTAT-RETURNED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPSTAT-RETURNED-AMT > 500.00
               MOVE 500.00 TO BPSTAT-RETURNED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2140-ACCRUE-FEE.
           IF BPSTAT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPSTAT-FEE-AMT * BPSTAT-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RETURNED AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2150-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPSTAT-STATUS-CD
                   ADD BPSTAT-RETURNED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPSTAT-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPSTAT-RETURNED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-PAYMENT.
           IF BPSTAT-PAYMENT-CNT < 9999999
               ADD 1 TO BPSTAT-PAYMENT-CNT
           END-IF
           IF BPSTAT-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO BPSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2170-LIMIT-RETURNED.
           IF BPSTAT-RETURNED-AMT < 50.00
               MOVE 50.00 TO BPSTAT-RETURNED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPSTAT-RETURNED-AMT > 10000.00
               MOVE 10000.00 TO BPSTAT-RETURNED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2180-ACCRUE-REFUND.
           IF BPSTAT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPSTAT-REFUND-AMT * BPSTAT-RATE / 100 / 365
               ADD WS-WORK-AMT TO BPSTAT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2190-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPSTAT-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPSTAT-FEE-AMT NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPSTAT-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPSTAT-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPSTAT-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2200-ACCRUE-PAYMENT.
           IF BPSTAT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPSTAT-PAYMENT-AMT * BPSTAT-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPSTAT-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REMIT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2210-DESCRIBE-REMIT-METHOD.
           EVALUATE BPSTAT-REMIT-METHOD-CD
               WHEN 'CGV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FNS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NMS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TAF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VNK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WUA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XCT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2220-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BPSTAT-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN MATURITY DATE IS PAST
      * FNS -> NMS WHEN MATURITY DATE IS PAST
      * NMS -> TAF WHEN MATURITY DATE IS PAST
      * TAF -> VNK WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2230-SET-REMIT-METHOD.
           EVALUATE BPSTAT-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPSTAT-MATURITY-DATE > 0 AND BPSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPSTAT-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPSTAT-MATURITY-DATE > 0 AND BPSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPSTAT-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPSTAT-MATURITY-DATE > 0 AND BPSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPSTAT-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPSTAT-MATURITY-DATE > 0 AND BPSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPSTAT-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2240-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BPSTAT-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2250-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY REMIT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2260-ACCUM-REMIT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPSTAT-REMIT-METHOD-CD
                   ADD BPSTAT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPSTAT-REMIT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPSTAT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       2270-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPSTAT-PAYMENT-AMT ROUNDED =
                   BPSTAT-PAYMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPSTAT-PAYMENT-AMT
           END-IF
           ADD BPSTAT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E664 REF NO REQUIRED
      * E220 WORK AMT NEGATIVE
      * E821 WORK AMT OVER 5000
      * E583 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2280-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPSTAT-REF-NO = SPACES
                   MOVE 'E664' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E220' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN BPSTAT-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2290-LIMIT-PAYMENT.
           IF BPSTAT-PAYMENT-AMT < 0.00
               MOVE 0.00 TO BPSTAT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPSTAT-PAYMENT-AMT > 500.00
               MOVE 500.00 TO BPSTAT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2300-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPSTAT-REASON-CD
                   ADD BPSTAT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPSTAT-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPSTAT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2310-LIMIT-REFUND.
           IF BPSTAT-REFUND-AMT < 25.00
               MOVE 25.00 TO BPSTAT-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPSTAT-REFUND-AMT > 50000.00
               MOVE 50000.00 TO BPSTAT-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2320-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-LAST-STMT-DATE > 0 AND BPSTAT-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2330-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPSTAT-RETURNED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BPSTAT-RETURNED-AMT - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2340-DESCRIBE-CYCLE.
           EVALUATE BPSTAT-CYCLE-CD
               WHEN 'DA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2350-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2360-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPSTAT-PAYMENT-YTD NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2370-COUNT-EXCEPT.
           IF BPSTAT-EXCEPT-CNT < 9999999
               ADD 1 TO BPSTAT-EXCEPT-CNT
           END-IF
           IF BPSTAT-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BPSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY REMIT METHOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2380-ACCUM-REMIT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPSTAT-REMIT-METHOD-CD
                   ADD BPSTAT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPSTAT-REMIT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPSTAT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2390-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE BPSTAT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E163 CUST ID REQUIRED
      * E363 PAYMENT AMT NEGATIVE
      * E976 PAYMENT AMT OVER 1000000
      * E336 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2400-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPSTAT-CUST-ID = SPACES
                   MOVE 'E163' TO WS-REJECT-CODE
               WHEN BPSTAT-PAYMENT-AMT < 0
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN BPSTAT-PAYMENT-AMT > 1000000.00
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN BPSTAT-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E336' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       2410-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPSTAT-FEE-AMT ROUNDED =
                   BPSTAT-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPSTAT-FEE-AMT
           END-IF
           ADD BPSTAT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2420-FLAG-REFUND.
           IF BPSTAT-REFUND-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPSTAT-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2430-DESCRIBE-CYCLE.
           EVALUATE BPSTAT-CYCLE-CD
               WHEN 'DA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E189 REF NO REQUIRED
      * E386 RETURNED YTD NEGATIVE
      * E302 RETURNED YTD OVER 10000
      * E997 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2440-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPSTAT-REF-NO = SPACES
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN BPSTAT-RETURNED-YTD < 0
                   MOVE 'E386' TO WS-REJECT-CODE
               WHEN BPSTAT-RETURNED-YTD > 10000.00
                   MOVE 'E302' TO WS-REJECT-CODE
               WHEN BPSTAT-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2450-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPSTAT-STATUS-CD
                   ADD BPSTAT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPSTAT-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPSTAT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2460-FLAG-RETURNED.
           IF BPSTAT-RETURNED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPSTAT-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2470-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE BPSTAT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2480-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPSTAT-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPSTAT-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPSTAT-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPSTAT-RETURNED-AMT ROUNDED = WS-WORK-AMT *
                       BPSTAT-RATE / 200
               WHEN BPSTAT-REASON-CD = 'AX'
                   MOVE 15.00 TO BPSTAT-RETURNED-AMT
               WHEN BPSTAT-REASON-CD = 'BX'
                   MOVE 5.00 TO BPSTAT-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPSTAT-RETURNED-AMT ROUNDED = WS-WORK-AMT *
                       BPSTAT-RATE / 100
           END-EVALUATE
           IF BPSTAT-RETURNED-AMT > 0 AND BPSTAT-RETURNED-AMT < 1.00
               MOVE 1.00 TO BPSTAT-RETURNED-AMT
           END-IF
           IF BPSTAT-RETURNED-AMT > 50.00
               MOVE 50.00 TO BPSTAT-RETURNED-AMT
           END-IF
           ADD BPSTAT-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BPSTAT-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E013 CUST ID REQUIRED
      * E391 WORK AMT NEGATIVE
      * E133 WORK AMT OVER 5000
      * E308 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2500-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPSTAT-CUST-ID = SPACES
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E391' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN BPSTAT-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E308' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPSTAT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPSTAT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPSTAT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-REFUND.
           IF BPSTAT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPSTAT-REFUND-AMT * BPSTAT-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPSTAT-RETURNED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2530-COUNT-PAYMENT.
           IF BPSTAT-PAYMENT-CNT < 9999999
               ADD 1 TO BPSTAT-PAYMENT-CNT
           END-IF
           IF BPSTAT-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO BPSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-EXCEPT.
           IF BPSTAT-EXCEPT-CNT < 9999999
               ADD 1 TO BPSTAT-EXCEPT-CNT
           END-IF
           IF BPSTAT-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO BPSTAT-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETURNED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPSTAT-RETURNED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BPSTAT-RETURNED-YTD - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2560-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2570-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-CLOSE-DATE > 0 AND BPSTAT-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPSTAT-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPSTAT-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPSTAT-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BPSTAT-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPSTAT-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPSTAT-STATUS-CD
                   ADD BPSTAT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPSTAT-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPSTAT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2590-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE BPSTAT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2600-COUNT-PAYMENT.
           IF BPSTAT-PAYMENT-CNT < 9999999
               ADD 1 TO BPSTAT-PAYMENT-CNT
           END-IF
           IF BPSTAT-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO BPSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2610-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPSTAT-RETURNED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPSTAT-RETURNED-AMT - (WS-WORK-AMT *
               4)
           COMPUTE BPSTAT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2620-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BPSTAT-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN LAST STMT DATE IS PAST
      * BX -> GM WHEN LAST STMT DATE IS PAST
      * GM -> KU WHEN LAST STMT DATE IS PAST
      * KU -> LT WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2630-SET-REASON.
           EVALUATE BPSTAT-REASON-CD
               WHEN 'AX'
                   IF BPSTAT-LAST-STMT-DATE > 0 AND
                       BPSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPSTAT-LAST-STMT-DATE > 0 AND
                       BPSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPSTAT-LAST-STMT-DATE > 0 AND
                       BPSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPSTAT-LAST-STMT-DATE > 0 AND
                       BPSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RETURNED YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPSTAT-STATUS-CD
                   ADD BPSTAT-RETURNED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPSTAT-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPSTAT-RETURNED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2650-ACCRUE-REFUND.
           IF BPSTAT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPSTAT-REFUND-AMT * BPSTAT-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPSTAT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2660-FLAG-PAYMENT.
           IF BPSTAT-PAYMENT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPSTAT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2670-DESCRIBE-STATUS.
           EVALUATE BPSTAT-STATUS-CD
               WHEN 'JX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2680-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPSTAT-RETURNED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BPSTAT-RETURNED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE BPSTAT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2690-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-MATURITY-DATE > 0 AND BPSTAT-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPSTAT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPSTAT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPSTAT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BPSTAT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPSTAT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2700-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPSTAT-RETURNED-AMT NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2710-LIMIT-PAYMENT.
           IF BPSTAT-PAYMENT-AMT < 100.00
               MOVE 100.00 TO BPSTAT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPSTAT-PAYMENT-AMT > 1000.00
               MOVE 1000.00 TO BPSTAT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2720-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPSTAT-FEE-AMT NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2730-LIMIT-PAYMENT.
           IF BPSTAT-PAYMENT-YTD < 50.00
               MOVE 50.00 TO BPSTAT-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPSTAT-PAYMENT-YTD > 5000.00
               MOVE 5000.00 TO BPSTAT-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2740-COUNT-PAYMENT.
           IF BPSTAT-PAYMENT-CNT < 9999999
               ADD 1 TO BPSTAT-PAYMENT-CNT
           END-IF
           IF BPSTAT-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO BPSTAT-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2750-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-MATURITY-DATE > 0 AND BPSTAT-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPSTAT-RETURNED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BPSTAT-RETURNED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BPSTAT-RETURNED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPSTAT-RETURNED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPSTAT-RETURNED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY REMIT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2760-ACCUM-REMIT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPSTAT-REMIT-METHOD-CD
                   ADD BPSTAT-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPSTAT-REMIT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPSTAT-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN CLOSE DATE IS PAST
      * BX -> GM WHEN CLOSE DATE IS PAST
      * GM -> KU WHEN CLOSE DATE IS PAST
      * KU -> LT WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2770-SET-REASON.
           EVALUATE BPSTAT-REASON-CD
               WHEN 'AX'
                   IF BPSTAT-CLOSE-DATE > 0 AND BPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPSTAT-CLOSE-DATE > 0 AND BPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPSTAT-CLOSE-DATE > 0 AND BPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPSTAT-CLOSE-DATE > 0 AND BPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2780-DESCRIBE-STATUS.
           EVALUATE BPSTAT-STATUS-CD
               WHEN 'JX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2790-ACCRUE-PAYMENT.
           IF BPSTAT-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPSTAT-PAYMENT-YTD * BPSTAT-RATE / 100 / 365
               ADD WS-WORK-AMT TO BPSTAT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       2800-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPSTAT-RETURNED-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPSTAT-RETURNED-AMT
           END-IF
           ADD BPSTAT-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2810-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2820-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPSTAT-RETURNED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BPSTAT-RETURNED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN MATURITY DATE IS PAST
      * FP -> JH WHEN MATURITY DATE IS PAST
      * JH -> JV WHEN MATURITY DATE IS PAST
      * JV -> RM WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2830-SET-CYCLE.
           EVALUATE BPSTAT-CYCLE-CD
               WHEN 'DA'
                   IF BPSTAT-MATURITY-DATE > 0 AND BPSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPSTAT-MATURITY-DATE > 0 AND BPSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPSTAT-MATURITY-DATE > 0 AND BPSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPSTAT-MATURITY-DATE > 0 AND BPSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2840-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-CLOSE-DATE > 0 AND BPSTAT-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-CLOSE-DATE TO DTU-DATE-1
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
      * SELECT THE REFUND TIER TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2850-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPSTAT-RETURNED-AMT NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2860-ACCRUE-RETURNED.
           IF BPSTAT-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPSTAT-RETURNED-AMT * BPSTAT-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPSTAT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2870-DESCRIBE-CYCLE.
           EVALUATE BPSTAT-CYCLE-CD
               WHEN 'DA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED YTD FEE ON THE PAYMENT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2880-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPSTAT-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPSTAT-RETURNED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPSTAT-EXEMPT-FLAG = 'Y'
                   COMPUTE BPSTAT-RETURNED-YTD ROUNDED =
                       BPSTAT-PAYMENT-YTD * BPSTAT-RATE / 200
               WHEN BPSTAT-REASON-CD = 'AX'
                   MOVE 10.00 TO BPSTAT-RETURNED-YTD
               WHEN BPSTAT-REASON-CD = 'BX'
                   MOVE 20.00 TO BPSTAT-RETURNED-YTD
               WHEN OTHER
                   COMPUTE BPSTAT-RETURNED-YTD ROUNDED =
                       BPSTAT-PAYMENT-YTD * BPSTAT-RATE / 100
           END-EVALUATE
           IF BPSTAT-RETURNED-YTD > 0 AND BPSTAT-RETURNED-YTD < 2.00
               MOVE 2.00 TO BPSTAT-RETURNED-YTD
           END-IF
           IF BPSTAT-RETURNED-YTD > 35.00
               MOVE 35.00 TO BPSTAT-RETURNED-YTD
           END-IF
           ADD BPSTAT-RETURNED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2890-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BPSTAT-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE PAYMENT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2900-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPSTAT-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPSTAT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPSTAT-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPSTAT-PAYMENT-AMT ROUNDED =
                       BPSTAT-PAYMENT-YTD * BPSTAT-PENALTY-PCT / 200
               WHEN BPSTAT-REMIT-METHOD-CD = 'CGV'
                   MOVE 10.00 TO BPSTAT-PAYMENT-AMT
               WHEN BPSTAT-REMIT-METHOD-CD = 'FNS'
                   MOVE 10.00 TO BPSTAT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPSTAT-PAYMENT-AMT ROUNDED =
                       BPSTAT-PAYMENT-YTD * BPSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF BPSTAT-PAYMENT-AMT > 0 AND BPSTAT-PAYMENT-AMT < 10.00
               MOVE 10.00 TO BPSTAT-PAYMENT-AMT
           END-IF
           IF BPSTAT-PAYMENT-AMT > 50.00
               MOVE 50.00 TO BPSTAT-PAYMENT-AMT
           END-IF
           ADD BPSTAT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2910-COUNT-EXCEPT.
           IF BPSTAT-EXCEPT-CNT < 9999999
               ADD 1 TO BPSTAT-EXCEPT-CNT
           END-IF
           IF BPSTAT-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BPSTAT-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2920-LIMIT-RETURNED.
           IF BPSTAT-RETURNED-AMT < 25.00
               MOVE 25.00 TO BPSTAT-RETURNED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPSTAT-RETURNED-AMT > 1000.00
               MOVE 1000.00 TO BPSTAT-RETURNED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2930-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPSTAT-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BPSTAT-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETURNED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2940-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPSTAT-RETURNED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BPSTAT-RETURNED-YTD - (WS-WORK-AMT *
               2)
           COMPUTE BPSTAT-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE RETURNED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2950-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPSTAT-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPSTAT-RETURNED-YTD NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPSTAT-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPSTAT-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPSTAT-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2960-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPSTAT-FEE-AMT NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2970-COUNT-EXCEPT.
           IF BPSTAT-EXCEPT-CNT < 9999999
               ADD 1 TO BPSTAT-EXCEPT-CNT
           END-IF
           IF BPSTAT-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE RETURNED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2980-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPSTAT-RETURNED-YTD NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2990-ACCRUE-FEE.
           IF BPSTAT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPSTAT-FEE-AMT * BPSTAT-RATE / 100 / 365
               ADD WS-WORK-AMT TO BPSTAT-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E757 CUST ID REQUIRED
      * E051 PAYMENT AMT NEGATIVE
      * E284 PAYMENT AMT OVER 25000
      * E901 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPSTAT-CUST-ID = SPACES
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN BPSTAT-PAYMENT-AMT < 0
                   MOVE 'E051' TO WS-REJECT-CODE
               WHEN BPSTAT-PAYMENT-AMT > 25000.00
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN BPSTAT-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E901' TO WS-REJECT-CODE
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
                   MOVE 08 TO BPSTAT-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO BPSTAT-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO BPSTAT-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'BPSTAT EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
