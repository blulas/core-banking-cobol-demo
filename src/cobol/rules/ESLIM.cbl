      *================================================================*
      * PROGRAM  : ESLIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MORTGAGE ESCROW RULE - ELIGIBILITY. CALLED BY THE   *
      *            MORTGAGE ESCROW BATCH AND ONLINE PROGRAMS.          *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-07-17 CRP  ORIGINAL                                       *
      * 1996-06-20 DPS  CUSHION EXCEPTIONS TO OPS QUEUE CHG4543        *
      * 1998-08-05 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-09-16 EWS  CUSHION CUTOFF MOVED CHG0807                   *
      * 2002-06-05 MKL  PAYMENT RERUN PROTECTION CHG4508               *
      * 2002-08-11 JLM  SHORTAGE STATUS CODES ADDED REQ0477            *
      * 2002-05-16 SKN  PAYMENT CUTOFF MOVED REQ3780                   *
      * 2003-03-25 RJT  SURPLUS CALCULATION CORRECTED CHG5969          *
      * 2003-07-06 PQR  NEW PAYMENT REPORT COLUMNS PRB8451             *
      * 2007-08-23 RJT  PAYMENT CUTOFF MOVED REQ0846                   *
      * 2008-03-15 PQR  PERFORMANCE - PAYMENT READ AHEAD PRB3564       *
      * 2011-12-15 GAF  SHORTAGE STATUS CODES ADDED PRB1874            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESLIM.
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
      * WS-SHORTAGE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHORTAGE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000262000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000408000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000383500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000294500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000615000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011350000000311000'.
       01  WS-SHORTAGE-SCHED-TBL REDEFINES WS-SHORTAGE-SCHED-VALUES.
           05  WS-SHORTAGE-SCHED-ROW OCCURS 6 TIMES.
               10  WS-SHORTAGE-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHORTAGE-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ESLIMP.

       PROCEDURE DIVISION USING ESLIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-LAST-ACT
           PERFORM 2010-FLAG-TAX
           PERFORM 2020-PRORATE-ITEM
           PERFORM 2030-ACCUM-STATUS
           PERFORM 2040-SET-PAYEE-TYPE
           PERFORM 2050-DESCRIBE-PAYEE-TYPE
           PERFORM 2060-SELECT-RATE
           PERFORM 2070-AGE-EXPIRY
           PERFORM 2080-COUNT-RETRY
           PERFORM 2090-SELECT-RATE
           PERFORM 2100-SELECT-RATE
           PERFORM 2110-CHECK-LAST-PAY
           PERFORM 2120-SPLIT-SURPLUS
           PERFORM 2130-FLAG-INSURANCE
           PERFORM 2140-AGE-EXPIRY
           PERFORM 2150-PRORATE-TAX
           PERFORM 2160-SELECT-FEE-PCT
           PERFORM 2170-DESCRIBE-PAYEE-TYPE
           PERFORM 2180-ACCUM-PAYEE-TYPE
           PERFORM 2190-LIMIT-ITEM
           PERFORM 2200-SET-REASON
           PERFORM 2210-SET-TYPE
           PERFORM 2220-COUNT-EXCEPT
           PERFORM 2230-CHECK-LAST-ACT
           PERFORM 2240-ACCRUE-RATE
           PERFORM 2250-SELECT-RATE
           PERFORM 2260-PRORATE-SHORTAGE
           PERFORM 2270-CHECK-LAST-ACT
           PERFORM 2280-PRORATE-PAYMENT
           PERFORM 2290-AGE-LAST-ACT
           PERFORM 2300-CHECK-EXPIRY
           PERFORM 2310-FLAG-PAYMENT
           PERFORM 2320-LIMIT-SHORTAGE
           PERFORM 2330-PRORATE-INSURANCE
           PERFORM 2340-ACCUM-PAYEE-TYPE
           PERFORM 2350-PRORATE-INSURANCE
           PERFORM 2360-ACCRUE-SURPLUS
           PERFORM 2370-CHECK-NOTICE
           PERFORM 2380-SELECT-RATE
           PERFORM 2390-PRORATE-PAYMENT
           PERFORM 2400-SET-STATUS
           PERFORM 2410-ACCUM-STATUS
           PERFORM 2420-SELECT-FEE-PCT
           PERFORM 2430-PRORATE-PAYMENT
           PERFORM 2440-AGE-LAST-ACT
           PERFORM 2450-LIMIT-SHORTAGE
           PERFORM 2460-SET-PAYEE-TYPE
           PERFORM 2470-SELECT-RATE
           PERFORM 2480-DESCRIBE-PAYEE-TYPE
           PERFORM 2490-CHECK-LAST-PAY
           PERFORM 2500-ACCUM-STATUS
           PERFORM 2510-PRORATE-ITEM
           PERFORM 2520-DESCRIBE-PAYEE-TYPE
           PERFORM 2530-SET-PAYEE-TYPE
           PERFORM 2540-ACCUM-TYPE
           PERFORM 2550-LIMIT-PAYMENT
           PERFORM 2560-COUNT-EXCEPT
           PERFORM 2570-ACCRUE-PAYMENT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ESLIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ESLIM-LAST-ACT-DATE > 0 AND ESLIM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESLIM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESLIM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESLIM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESLIM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESLIM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESLIM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAX YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2010-FLAG-TAX.
           IF ESLIM-TAX-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESLIM-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2020-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2030-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESLIM-STATUS-CD
                   ADD ESLIM-SHORTAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESLIM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESLIM-SHORTAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN LAST PAY DATE IS PAST
      * UE -> UP WHEN LAST PAY DATE IS PAST
      * UP -> AM WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2040-SET-PAYEE-TYPE.
           EVALUATE ESLIM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESLIM-LAST-PAY-DATE > 0 AND ESLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESLIM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESLIM-LAST-PAY-DATE > 0 AND ESLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESLIM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESLIM-LAST-PAY-DATE > 0 AND ESLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESLIM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2050-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESLIM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2060-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ESLIM-EXPIRY-DATE > 0 AND ESLIM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESLIM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESLIM-INSURANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ESLIM-INSURANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ESLIM-INSURANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESLIM-INSURANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESLIM-INSURANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-RETRY.
           IF ESLIM-RETRY-CNT < 9999999
               ADD 1 TO ESLIM-RETRY-CNT
           END-IF
           IF ESLIM-RETRY-CNT NOT < 5
               MOVE 'Y' TO ESLIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ESLIM-TAX-YTD NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE SHORTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2100-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ESLIM-SHORTAGE-YTD NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2110-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESLIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ESLIM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURPLUS AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2120-SPLIT-SURPLUS.
           COMPUTE WS-WORK-AMT = ESLIM-SURPLUS-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESLIM-SURPLUS-AMT - (WS-WORK-AMT * 2)
           COMPUTE ESLIM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2130-FLAG-INSURANCE.
           IF ESLIM-INSURANCE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESLIM-INSURANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ESLIM-EXPIRY-DATE > 0 AND ESLIM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESLIM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAX YTD.
      *----------------------------------------------------------------*
       2150-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESLIM-TAX-YTD ROUNDED =
                   ESLIM-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESLIM-TAX-YTD
           END-IF
           ADD ESLIM-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2160-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ESLIM-PAYMENT-AMT NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESLIM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SURPLUS AMT BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2180-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESLIM-PAYEE-TYPE-CD
                   ADD ESLIM-SURPLUS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESLIM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESLIM-SURPLUS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2190-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN EXPIRY DATE IS PAST
      * F -> K WHEN EXPIRY DATE IS PAST
      * K -> L WHEN EXPIRY DATE IS PAST
      * L -> S WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2200-SET-REASON.
           EVALUATE ESLIM-REASON-CD
               WHEN 'C'
                   IF ESLIM-EXPIRY-DATE > 0 AND ESLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO ESLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESLIM-EXPIRY-DATE > 0 AND ESLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO ESLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESLIM-EXPIRY-DATE > 0 AND ESLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO ESLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESLIM-EXPIRY-DATE > 0 AND ESLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ESLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN EXPIRY DATE IS PAST
      * REP -> SMX WHEN EXPIRY DATE IS PAST
      * SMX -> VJL WHEN EXPIRY DATE IS PAST
      * VJL -> WKN WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-TYPE.
           EVALUATE ESLIM-TYPE-CD
               WHEN 'AKX'
                   IF ESLIM-EXPIRY-DATE > 0 AND ESLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESLIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESLIM-EXPIRY-DATE > 0 AND ESLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESLIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESLIM-EXPIRY-DATE > 0 AND ESLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESLIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESLIM-EXPIRY-DATE > 0 AND ESLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESLIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2220-COUNT-EXCEPT.
           IF ESLIM-EXCEPT-CNT < 9999999
               ADD 1 TO ESLIM-EXCEPT-CNT
           END-IF
           IF ESLIM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO ESLIM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2230-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ESLIM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESLIM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ESLIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2240-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ESLIM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESLIM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2250-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ESLIM-INSURANCE-AMT NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE YTD.
      *----------------------------------------------------------------*
       2260-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESLIM-SHORTAGE-YTD ROUNDED =
                   ESLIM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESLIM-SHORTAGE-YTD
           END-IF
           ADD ESLIM-SHORTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2270-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ESLIM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESLIM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ESLIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       2280-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESLIM-PAYMENT-AMT ROUNDED =
                   ESLIM-SHORTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESLIM-PAYMENT-AMT
           END-IF
           ADD ESLIM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ESLIM-LAST-ACT-DATE > 0 AND ESLIM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESLIM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESLIM-SURPLUS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESLIM-SURPLUS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ESLIM-SURPLUS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESLIM-SURPLUS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESLIM-SURPLUS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ESLIM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESLIM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ESLIM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2310-FLAG-PAYMENT.
           IF ESLIM-PAYMENT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESLIM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2320-LIMIT-SHORTAGE.
           IF ESLIM-SHORTAGE-YTD < 25.00
               MOVE 25.00 TO ESLIM-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESLIM-SHORTAGE-YTD > 1000.00
               MOVE 1000.00 TO ESLIM-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2330-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ESLIM-INSURANCE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE AMT BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESLIM-PAYEE-TYPE-CD
                   ADD ESLIM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESLIM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESLIM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INSURANCE AMT.
      *----------------------------------------------------------------*
       2350-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESLIM-INSURANCE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESLIM-INSURANCE-AMT
           END-IF
           ADD ESLIM-INSURANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2360-ACCRUE-SURPLUS.
           IF ESLIM-SURPLUS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESLIM-SURPLUS-AMT * ESLIM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESLIM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2370-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ESLIM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2380-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ESLIM-TAX-YTD NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURPLUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       2390-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESLIM-PAYMENT-AMT ROUNDED =
                   ESLIM-SURPLUS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESLIM-PAYMENT-AMT
           END-IF
           ADD ESLIM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN LAST ACT DATE IS PAST
      * E -> R WHEN LAST ACT DATE IS PAST
      * R -> U WHEN LAST ACT DATE IS PAST
      * U -> Z WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2400-SET-STATUS.
           EVALUATE ESLIM-STATUS-CD
               WHEN 'A'
                   IF ESLIM-LAST-ACT-DATE > 0 AND ESLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO ESLIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF ESLIM-LAST-ACT-DATE > 0 AND ESLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ESLIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ESLIM-LAST-ACT-DATE > 0 AND ESLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO ESLIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF ESLIM-LAST-ACT-DATE > 0 AND ESLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO ESLIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2410-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESLIM-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESLIM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2430-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESLIM-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2440-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ESLIM-LAST-ACT-DATE > 0 AND ESLIM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESLIM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESLIM-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESLIM-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESLIM-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ESLIM-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESLIM-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2450-LIMIT-SHORTAGE.
           IF ESLIM-SHORTAGE-AMT < 25.00
               MOVE 25.00 TO ESLIM-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESLIM-SHORTAGE-AMT > 2500.00
               MOVE 2500.00 TO ESLIM-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN LAST PAY DATE IS PAST
      * UE -> UP WHEN LAST PAY DATE IS PAST
      * UP -> AM WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2460-SET-PAYEE-TYPE.
           EVALUATE ESLIM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESLIM-LAST-PAY-DATE > 0 AND ESLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESLIM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESLIM-LAST-PAY-DATE > 0 AND ESLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESLIM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESLIM-LAST-PAY-DATE > 0 AND ESLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESLIM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE SURPLUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ESLIM-SURPLUS-AMT NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESLIM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESLIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ESLIM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESLIM-STATUS-CD
                   ADD ESLIM-INSURANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESLIM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESLIM-INSURANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2510-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2520-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESLIM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN NOTICE DATE IS PAST
      * UE -> UP WHEN NOTICE DATE IS PAST
      * UP -> AM WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-PAYEE-TYPE.
           EVALUATE ESLIM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESLIM-NOTICE-DATE > 0 AND ESLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESLIM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESLIM-NOTICE-DATE > 0 AND ESLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESLIM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESLIM-NOTICE-DATE > 0 AND ESLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESLIM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESLIM-TYPE-CD
                   ADD ESLIM-INSURANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESLIM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESLIM-INSURANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2550-LIMIT-PAYMENT.
           IF ESLIM-PAYMENT-AMT < 25.00
               MOVE 25.00 TO ESLIM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESLIM-PAYMENT-AMT > 500.00
               MOVE 500.00 TO ESLIM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2560-COUNT-EXCEPT.
           IF ESLIM-EXCEPT-CNT < 9999999
               ADD 1 TO ESLIM-EXCEPT-CNT
           END-IF
           IF ESLIM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ESLIM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2570-ACCRUE-PAYMENT.
           IF ESLIM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESLIM-PAYMENT-AMT * ESLIM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESLIM-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ESLIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ESLIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ESLIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ESLIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
