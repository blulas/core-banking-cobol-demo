      *================================================================*
      * PROGRAM  : SLCAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : STUDENT LOANS RULE - DATE RULES. CALLED BY THE      *
      *            STUDENT LOANS BATCH AND ONLINE PROGRAMS.            *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-02-05 CRP  ORIGINAL                                       *
      * 1989-08-12 GAF  PRINCIPAL CUTOFF MOVED CHG2951                 *
      * 1990-07-06 DPS  CAPINT NOTICE TEXT PER LEGAL INC4796           *
      * 1992-10-04 PQR  DISBURSE ROUNDING - HALF UP CHG8538            *
      * 1994-07-08 GAF  AUDIT FINDING - DISBURSE CONTROL TOTALS PRB0637*
      * 2000-10-11 CRP  PRINCIPAL STATUS CODES ADDED REQ7288           *
      * 2000-05-21 PQR  PRINCIPAL FEE SCHEDULE UPDATE INC2886          *
      * 2003-07-28 PQR  NEW DEFERINT REPORT COLUMNS PRB5040            *
      * 2005-11-26 CRP  CAPINT EXCEPTIONS TO OPS QUEUE CHG0189         *
      * 2006-12-28 EWS  AUDIT FINDING - DISBURSE CONTROL TOTALS CHG4596*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLCAL.
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
      * WS-PAYMENT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000329000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000548000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000520000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000859000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000113000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011350000000389000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011450000000335500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000849500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000696500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000252500'.
       01  WS-PAYMENT-BAND-TBL REDEFINES WS-PAYMENT-BAND-VALUES.
           05  WS-PAYMENT-BAND-ROW OCCURS 10 TIMES.
               10  WS-PAYMENT-BAND-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-BAND-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SLCALP.

       PROCEDURE DIVISION USING SLCAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SET-CYCLE
           PERFORM 2010-AGE-LAST-ACT
           PERFORM 2020-ACCUM-SOURCE
           PERFORM 2030-SET-CYCLE
           PERFORM 2040-DESCRIBE-DEFER-CODE
           PERFORM 2050-PRORATE-SUBSIDY
           PERFORM 2060-SPLIT-PAYMENT
           PERFORM 2070-CHECK-LAST-PAY
           PERFORM 2080-ACCUM-SOURCE
           PERFORM 2090-EDIT-NAME
           PERFORM 2100-ACCUM-CYCLE
           PERFORM 2110-SPLIT-DISBURSE
           PERFORM 2120-SELECT-PENALTY-PCT
           PERFORM 2130-ACCUM-CYCLE
           PERFORM 2140-ACCUM-DEFER-CODE
           PERFORM 2150-ACCRUE-DISBURSE
           PERFORM 2160-ACCRUE-PAYMENT
           PERFORM 2170-ACCUM-CYCLE
           PERFORM 2180-SPLIT-DEFERINT
           PERFORM 2190-EDIT-ADDR
           PERFORM 2200-ACCRUE-SUBSIDY
           PERFORM 2210-DESCRIBE-REGION
           PERFORM 2220-SPLIT-ITEM
           PERFORM 2230-SET-CYCLE
           PERFORM 2240-AGE-LAST-ACT
           PERFORM 2250-CHECK-EXPIRY
           PERFORM 2260-FLAG-DEFERINT
           PERFORM 2270-ACCUM-DEFER-CODE
           PERFORM 2280-COUNT-PAYMENT
           PERFORM 2290-ACCUM-SOURCE
           PERFORM 2300-COUNT-EXCEPT
           PERFORM 2310-LIMIT-PRINCIPAL
           PERFORM 2320-ASSESS-NOTICE-SENT
           PERFORM 2330-DESCRIBE-SOURCE
           PERFORM 2340-PRORATE-SUBSIDY
           PERFORM 2350-ACCRUE-CAPINT
           PERFORM 2360-ACCRUE-DISBURSE
           PERFORM 2370-ACCUM-CYCLE
           PERFORM 2380-CHECK-OPEN
           PERFORM 2390-SELECT-FEE-PCT
           PERFORM 2400-FLAG-DEFERINT
           PERFORM 2410-SET-CYCLE
           PERFORM 2420-EDIT-NAME
           PERFORM 2430-SET-DEFER-CODE
           PERFORM 2440-DESCRIBE-REGION
           PERFORM 2450-AGE-EXPIRY
           PERFORM 2460-SET-SOURCE
           PERFORM 2470-EDIT-ADDR
           PERFORM 2480-ASSESS-REVIEW
           PERFORM 2490-PRORATE-SUBSIDY
           PERFORM 2500-FLAG-DISBURSE
           PERFORM 2510-ACCRUE-FEE-PCT
           PERFORM 2520-LIMIT-PAYMENT
           PERFORM 2530-EDIT-ADDR
           PERFORM 2540-AGE-LAST-PAY
           PERFORM 2550-ACCUM-REGION
           PERFORM 2560-SELECT-FEE-PCT
           PERFORM 2570-ACCRUE-PENALTY-PCT
           PERFORM 2580-LIMIT-ITEM
           PERFORM 2590-ACCRUE-PENALTY-PCT
           PERFORM 2600-DESCRIBE-SOURCE
           PERFORM 2610-CHECK-LAST-PAY
           PERFORM 2620-COUNT-PAYMENT
           PERFORM 2630-COUNT-PAYMENT
           PERFORM 2640-SPLIT-DEFERINT
           PERFORM 2650-AGE-LAST-ACT
           PERFORM 2660-PRORATE-DISBURSE
           PERFORM 2670-SET-DEFER-CODE
           PERFORM 2680-ASSESS-OVERRIDE
           PERFORM 2690-EDIT-NAME
           PERFORM 2700-COUNT-EXCEPT
           PERFORM 2710-ACCUM-SOURCE
           PERFORM 2720-CHECK-LAST-PAY
           PERFORM 2730-FLAG-DISBURSE
           PERFORM 2740-SPLIT-DISBURSE
           PERFORM 2750-PRORATE-PAYMENT
           PERFORM 2760-COUNT-PAYMENT
           PERFORM 2770-AGE-LAST-ACT
           PERFORM 2780-SET-SOURCE
           PERFORM 2790-SELECT-PENALTY-PCT
           PERFORM 2800-SET-CYCLE
           PERFORM 2810-COUNT-EXCEPT
           PERFORM 2820-CHECK-LAST-ACT
           PERFORM 2830-SET-SOURCE
           PERFORM 2840-SET-DEFER-CODE
           PERFORM 2850-ASSESS-NOTICE-SENT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SLCAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN LAST ACT DATE IS PAST
      * KHC -> SEU WHEN LAST ACT DATE IS PAST
      * SEU -> XKA WHEN LAST ACT DATE IS PAST
      * XKA -> CFJ WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2000-SET-CYCLE.
           EVALUATE SLCAL-CYCLE-CD
               WHEN 'CFJ'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHC' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SEU' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XKA' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLCAL-DEFERINT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SLCAL-DEFERINT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SLCAL-DEFERINT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SLCAL-DEFERINT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLCAL-DEFERINT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2020-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLCAL-SOURCE-CD
                   ADD SLCAL-CAPINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLCAL-CAPINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN LAST ACT DATE IS PAST
      * KHC -> SEU WHEN LAST ACT DATE IS PAST
      * SEU -> XKA WHEN LAST ACT DATE IS PAST
      * XKA -> CFJ WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2030-SET-CYCLE.
           EVALUATE SLCAL-CYCLE-CD
               WHEN 'CFJ'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHC' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SEU' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XKA' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2040-DESCRIBE-DEFER-CODE.
           EVALUATE SLCAL-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SUBSIDY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2050-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SLCAL-SUBSIDY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = SLCAL-PAYMENT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SLCAL-PAYMENT-AMT - (WS-WORK-AMT * 2)
           COMPUTE SLCAL-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2070-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SLCAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2080-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLCAL-SOURCE-CD
                   ADD SLCAL-CAPINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLCAL-CAPINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E592 NAME REQUIRED
      * E749 CAPINT AMT NEGATIVE
      * E390 CAPINT AMT OVER 10000
      * E480 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2090-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLCAL-NAME = SPACES
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN SLCAL-CAPINT-AMT < 0
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN SLCAL-CAPINT-AMT > 10000.00
                   MOVE 'E390' TO WS-REJECT-CODE
               WHEN SLCAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E480' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2100-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLCAL-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLCAL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2110-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = SLCAL-DISBURSE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SLCAL-DISBURSE-YTD - (WS-WORK-AMT *
               4)
           COMPUTE SLCAL-SUBSIDY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2120-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO SLCAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SLCAL-PRINCIPAL-AMT NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO SLCAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLCAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLCAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2130-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLCAL-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLCAL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DEFER CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2140-ACCUM-DEFER-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLCAL-DEFER-CODE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLCAL-DEFER-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2150-ACCRUE-DISBURSE.
           IF SLCAL-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLCAL-DISBURSE-YTD * SLCAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2160-ACCRUE-PAYMENT.
           IF SLCAL-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLCAL-PAYMENT-AMT * SLCAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SLCAL-SUBSIDY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2170-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLCAL-CYCLE-CD
                   ADD SLCAL-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLCAL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLCAL-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEFERINT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2180-SPLIT-DEFERINT.
           COMPUTE WS-WORK-AMT = SLCAL-DEFERINT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SLCAL-DEFERINT-YTD - (WS-WORK-AMT *
               4)
           COMPUTE SLCAL-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E960 ADDR REQUIRED
      * E105 PAYMENT AMT NEGATIVE
      * E531 PAYMENT AMT OVER 50000
      * E264 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2190-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLCAL-ADDR = SPACES
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN SLCAL-PAYMENT-AMT < 0
                   MOVE 'E105' TO WS-REJECT-CODE
               WHEN SLCAL-PAYMENT-AMT > 50000.00
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN SLCAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SUBSIDY YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2200-ACCRUE-SUBSIDY.
           IF SLCAL-SUBSIDY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLCAL-SUBSIDY-YTD * SLCAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SLCAL-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2210-DESCRIBE-REGION.
           EVALUATE SLCAL-REGION-CD
               WHEN 'C'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2220-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN LAST ACT DATE IS PAST
      * KHC -> SEU WHEN LAST ACT DATE IS PAST
      * SEU -> XKA WHEN LAST ACT DATE IS PAST
      * XKA -> CFJ WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2230-SET-CYCLE.
           EVALUATE SLCAL-CYCLE-CD
               WHEN 'CFJ'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHC' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SEU' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XKA' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2240-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLCAL-DISBURSE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD SLCAL-DISBURSE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SLCAL-DISBURSE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SLCAL-DISBURSE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLCAL-DISBURSE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2250-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SLCAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEFERINT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2260-FLAG-DEFERINT.
           IF SLCAL-DEFERINT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SLCAL-DEFERINT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SUBSIDY YTD BY DEFER CODE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2270-ACCUM-DEFER-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLCAL-DEFER-CODE-CD
                   ADD SLCAL-SUBSIDY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLCAL-DEFER-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLCAL-SUBSIDY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-PAYMENT.
           IF SLCAL-PAYMENT-CNT < 9999999
               ADD 1 TO SLCAL-PAYMENT-CNT
           END-IF
           IF SLCAL-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO SLCAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEFERINT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLCAL-SOURCE-CD
                   ADD SLCAL-DEFERINT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLCAL-DEFERINT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2300-COUNT-EXCEPT.
           IF SLCAL-EXCEPT-CNT < 9999999
               ADD 1 TO SLCAL-EXCEPT-CNT
           END-IF
           IF SLCAL-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO SLCAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2310-LIMIT-PRINCIPAL.
           IF SLCAL-PRINCIPAL-AMT < 25.00
               MOVE 25.00 TO SLCAL-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SLCAL-PRINCIPAL-AMT > 2500.00
               MOVE 2500.00 TO SLCAL-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2320-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SLCAL-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLCAL-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SLCAL-PRINCIPAL-AMT *
                       SLCAL-PENALTY-PCT / 200
               WHEN SLCAL-REGION-CD = 'C'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN SLCAL-REGION-CD = 'D'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SLCAL-PRINCIPAL-AMT *
                       SLCAL-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2330-DESCRIBE-SOURCE.
           EVALUATE SLCAL-SOURCE-CD
               WHEN 'AST'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BCK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CJG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DSJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JVY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VEF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SUBSIDY YTD.
      *----------------------------------------------------------------*
       2340-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLCAL-SUBSIDY-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SLCAL-SUBSIDY-YTD
           END-IF
           ADD SLCAL-SUBSIDY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CAPINT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2350-ACCRUE-CAPINT.
           IF SLCAL-CAPINT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLCAL-CAPINT-AMT * SLCAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SLCAL-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2360-ACCRUE-DISBURSE.
           IF SLCAL-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLCAL-DISBURSE-YTD * SLCAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SLCAL-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SUBSIDY YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2370-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLCAL-CYCLE-CD
                   ADD SLCAL-SUBSIDY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLCAL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLCAL-SUBSIDY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2380-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SLCAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2390-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO SLCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO SLCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEFERINT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2400-FLAG-DEFERINT.
           IF SLCAL-DEFERINT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SLCAL-DEFERINT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN LAST PAY DATE IS PAST
      * KHC -> SEU WHEN LAST PAY DATE IS PAST
      * SEU -> XKA WHEN LAST PAY DATE IS PAST
      * XKA -> CFJ WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2410-SET-CYCLE.
           EVALUATE SLCAL-CYCLE-CD
               WHEN 'CFJ'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHC' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SEU' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XKA' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E406 NAME REQUIRED
      * E323 DEFERINT YTD NEGATIVE
      * E675 DEFERINT YTD OVER 250000
      * E654 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2420-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLCAL-NAME = SPACES
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN SLCAL-DEFERINT-YTD < 0
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN SLCAL-DEFERINT-YTD > 250000.00
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN SLCAL-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEFER CODE CD:
      * AH -> CZ WHEN EXPIRY DATE IS PAST
      * CZ -> GM WHEN EXPIRY DATE IS PAST
      * GM -> ND WHEN EXPIRY DATE IS PAST
      * ND -> UC WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2430-SET-DEFER-CODE.
           EVALUATE SLCAL-DEFER-CODE-CD
               WHEN 'AH'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZ' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZ'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2440-DESCRIBE-REGION.
           EVALUATE SLCAL-REGION-CD
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2450-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN EXPIRY DATE IS PAST
      * BCK -> CJG WHEN EXPIRY DATE IS PAST
      * CJG -> DSJ WHEN EXPIRY DATE IS PAST
      * DSJ -> JVY WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2460-SET-SOURCE.
           EVALUATE SLCAL-SOURCE-CD
               WHEN 'AST'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BCK' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BCK'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJG' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJG'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DSJ' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DSJ'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JVY' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E796 ADDR REQUIRED
      * E043 PRINCIPAL AMT NEGATIVE
      * E606 PRINCIPAL AMT OVER 50000
      * E955 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2470-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLCAL-ADDR = SPACES
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN SLCAL-PRINCIPAL-AMT < 0
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN SLCAL-PRINCIPAL-AMT > 50000.00
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN SLCAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E955' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE YTD FEE ON THE DEFERINT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2480-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SLCAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SLCAL-DISBURSE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLCAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE SLCAL-DISBURSE-YTD ROUNDED =
                       SLCAL-DEFERINT-YTD * SLCAL-PENALTY-PCT / 200
               WHEN SLCAL-CYCLE-CD = 'CFJ'
                   MOVE 20.00 TO SLCAL-DISBURSE-YTD
               WHEN SLCAL-CYCLE-CD = 'KHC'
                   MOVE 5.00 TO SLCAL-DISBURSE-YTD
               WHEN OTHER
                   COMPUTE SLCAL-DISBURSE-YTD ROUNDED =
                       SLCAL-DEFERINT-YTD * SLCAL-PENALTY-PCT / 100
           END-EVALUATE
           IF SLCAL-DISBURSE-YTD > 0 AND SLCAL-DISBURSE-YTD < 10.00
               MOVE 10.00 TO SLCAL-DISBURSE-YTD
           END-IF
           IF SLCAL-DISBURSE-YTD > 35.00
               MOVE 35.00 TO SLCAL-DISBURSE-YTD
           END-IF
           ADD SLCAL-DISBURSE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SUBSIDY YTD.
      *----------------------------------------------------------------*
       2490-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLCAL-SUBSIDY-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SLCAL-SUBSIDY-YTD
           END-IF
           ADD SLCAL-SUBSIDY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2500-FLAG-DISBURSE.
           IF SLCAL-DISBURSE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SLCAL-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2510-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SLCAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SLCAL-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2520-LIMIT-PAYMENT.
           IF SLCAL-PAYMENT-AMT < 100.00
               MOVE 100.00 TO SLCAL-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SLCAL-PAYMENT-AMT > 1000.00
               MOVE 1000.00 TO SLCAL-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E110 ADDR REQUIRED
      * E880 DISBURSE YTD NEGATIVE
      * E566 DISBURSE YTD OVER 1000000
      * E507 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2530-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLCAL-ADDR = SPACES
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN SLCAL-DISBURSE-YTD < 0
                   MOVE 'E880' TO WS-REJECT-CODE
               WHEN SLCAL-DISBURSE-YTD > 1000000.00
                   MOVE 'E566' TO WS-REJECT-CODE
               WHEN SLCAL-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLCAL-CAPINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD SLCAL-CAPINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SLCAL-CAPINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SLCAL-CAPINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLCAL-CAPINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SUBSIDY YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2550-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLCAL-REGION-CD
                   ADD SLCAL-SUBSIDY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLCAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLCAL-SUBSIDY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE DISBURSE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2560-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO SLCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SLCAL-DISBURSE-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO SLCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2570-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SLCAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SLCAL-CAPINT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2580-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SLCAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SLCAL-SUBSIDY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2600-DESCRIBE-SOURCE.
           EVALUATE SLCAL-SOURCE-CD
               WHEN 'AST'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BCK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CJG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DSJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JVY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VEF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2610-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SLCAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2620-COUNT-PAYMENT.
           IF SLCAL-PAYMENT-CNT < 9999999
               ADD 1 TO SLCAL-PAYMENT-CNT
           END-IF
           IF SLCAL-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO SLCAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2630-COUNT-PAYMENT.
           IF SLCAL-PAYMENT-CNT < 9999999
               ADD 1 TO SLCAL-PAYMENT-CNT
           END-IF
           IF SLCAL-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO SLCAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2640-SPLIT-DEFERINT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE SLCAL-DEFERINT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2650-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLCAL-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SLCAL-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SLCAL-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SLCAL-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLCAL-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       2660-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLCAL-DISBURSE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SLCAL-DISBURSE-YTD
           END-IF
           ADD SLCAL-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEFER CODE CD:
      * AH -> CZ WHEN LAST PAY DATE IS PAST
      * CZ -> GM WHEN LAST PAY DATE IS PAST
      * GM -> ND WHEN LAST PAY DATE IS PAST
      * ND -> UC WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2670-SET-DEFER-CODE.
           EVALUATE SLCAL-DEFER-CODE-CD
               WHEN 'AH'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZ' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZ'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE SUBSIDY YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2680-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SLCAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SLCAL-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLCAL-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE SLCAL-PAYMENT-AMT ROUNDED = SLCAL-SUBSIDY-YTD
                       * SLCAL-PENALTY-PCT / 200
               WHEN SLCAL-SOURCE-CD = 'AST'
                   MOVE 10.00 TO SLCAL-PAYMENT-AMT
               WHEN SLCAL-SOURCE-CD = 'BCK'
                   MOVE 20.00 TO SLCAL-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE SLCAL-PAYMENT-AMT ROUNDED = SLCAL-SUBSIDY-YTD
                       * SLCAL-PENALTY-PCT / 100
           END-EVALUATE
           IF SLCAL-PAYMENT-AMT > 0 AND SLCAL-PAYMENT-AMT < 10.00
               MOVE 10.00 TO SLCAL-PAYMENT-AMT
           END-IF
           IF SLCAL-PAYMENT-AMT > 50.00
               MOVE 50.00 TO SLCAL-PAYMENT-AMT
           END-IF
           ADD SLCAL-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E064 NAME REQUIRED
      * E789 CAPINT AMT NEGATIVE
      * E543 CAPINT AMT OVER 250000
      * E144 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2690-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLCAL-NAME = SPACES
                   MOVE 'E064' TO WS-REJECT-CODE
               WHEN SLCAL-CAPINT-AMT < 0
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN SLCAL-CAPINT-AMT > 250000.00
                   MOVE 'E543' TO WS-REJECT-CODE
               WHEN SLCAL-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2700-COUNT-EXCEPT.
           IF SLCAL-EXCEPT-CNT < 9999999
               ADD 1 TO SLCAL-EXCEPT-CNT
           END-IF
           IF SLCAL-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO SLCAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2710-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLCAL-SOURCE-CD
                   ADD SLCAL-CAPINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLCAL-CAPINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2720-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SLCAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2730-FLAG-DISBURSE.
           IF SLCAL-DISBURSE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SLCAL-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2740-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = SLCAL-DISBURSE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SLCAL-DISBURSE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE SLCAL-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CAPINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       2750-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLCAL-PAYMENT-AMT ROUNDED =
                   SLCAL-CAPINT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SLCAL-PAYMENT-AMT
           END-IF
           ADD SLCAL-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2760-COUNT-PAYMENT.
           IF SLCAL-PAYMENT-CNT < 9999999
               ADD 1 TO SLCAL-PAYMENT-CNT
           END-IF
           IF SLCAL-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO SLCAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2770-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-LAST-ACT-DATE > 0 AND SLCAL-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN EXPIRY DATE IS PAST
      * BCK -> CJG WHEN EXPIRY DATE IS PAST
      * CJG -> DSJ WHEN EXPIRY DATE IS PAST
      * DSJ -> JVY WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2780-SET-SOURCE.
           EVALUATE SLCAL-SOURCE-CD
               WHEN 'AST'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BCK' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BCK'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJG' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJG'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DSJ' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DSJ'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JVY' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2790-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO SLCAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SLCAL-PAYMENT-AMT NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO SLCAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLCAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLCAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN EXPIRY DATE IS PAST
      * KHC -> SEU WHEN EXPIRY DATE IS PAST
      * SEU -> XKA WHEN EXPIRY DATE IS PAST
      * XKA -> CFJ WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2800-SET-CYCLE.
           EVALUATE SLCAL-CYCLE-CD
               WHEN 'CFJ'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHC' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SEU' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XKA' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO SLCAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2810-COUNT-EXCEPT.
           IF SLCAL-EXCEPT-CNT < 9999999
               ADD 1 TO SLCAL-EXCEPT-CNT
           END-IF
           IF SLCAL-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO SLCAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2820-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SLCAL-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SLCAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN EXPIRY DATE IS PAST
      * BCK -> CJG WHEN EXPIRY DATE IS PAST
      * CJG -> DSJ WHEN EXPIRY DATE IS PAST
      * DSJ -> JVY WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2830-SET-SOURCE.
           EVALUATE SLCAL-SOURCE-CD
               WHEN 'AST'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BCK' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BCK'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJG' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJG'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DSJ' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DSJ'
                   IF SLCAL-EXPIRY-DATE > 0 AND SLCAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JVY' TO SLCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEFER CODE CD:
      * AH -> CZ WHEN LAST PAY DATE IS PAST
      * CZ -> GM WHEN LAST PAY DATE IS PAST
      * GM -> ND WHEN LAST PAY DATE IS PAST
      * ND -> UC WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2840-SET-DEFER-CODE.
           EVALUATE SLCAL-DEFER-CODE-CD
               WHEN 'AH'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZ' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZ'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF SLCAL-LAST-PAY-DATE > 0 AND SLCAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO SLCAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE DEFERINT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2850-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SLCAL-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLCAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SLCAL-DEFERINT-YTD *
                       SLCAL-PENALTY-PCT / 200
               WHEN SLCAL-REGION-CD = 'C'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN SLCAL-REGION-CD = 'D'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SLCAL-DEFERINT-YTD *
                       SLCAL-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SLCAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SLCAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SLCAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SLCAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
