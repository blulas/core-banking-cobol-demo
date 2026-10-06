      *================================================================*
      * PROGRAM  : PYFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PAYROLL SERVICES RULE - VALIDATION. CALLED BY THE   *
      *            PAYROLL SERVICES BATCH AND ONLINE PROGRAMS.         *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-05-02 BHS  ORIGINAL                                       *
      * 1990-11-07 SKN  GROSS STATUS CODES ADDED CHG4850               *
      * 1990-10-23 TWH  NET TABLE EXPANDED REQ7810                     *
      * 1990-06-12 SKN  AUDIT FINDING - NET CONTROL TOTALS REQ5272     *
      * 1991-05-13 GAF  FICA STATUS CODES ADDED PRB2727                *
      * 1993-12-01 SKN  FED-TAX FEE SCHEDULE UPDATE INC9210            *
      * 1994-01-17 JLM  FED-TAX FEE SCHEDULE UPDATE PRB1371            *
      * 1997-08-13 PQR  FED-TAX STATUS CODES ADDED CHG6815             *
      * 1999-11-14 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-02-03 RJT  GROSS LIMIT REVISED CHG8691                    *
      * 2009-02-14 SKN  PERFORMANCE - DEDUCT READ AHEAD REQ5558        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYFEE.
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
      * WS-STATE-TA-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STATE-TA-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000463500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000793500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000681500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010200000000357000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000178000'.
       01  WS-STATE-TA-TIER-TBL REDEFINES WS-STATE-TA-TIER-VALUES.
           05  WS-STATE-TA-TIER-ROW OCCURS 5 TIMES.
               10  WS-STATE-TA-TIER-LOW    PIC 9(09)V99.
               10  WS-STATE-TA-TIER-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PYFEEP.

       PROCEDURE DIVISION USING PYFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-DEDUCT
           PERFORM 2010-EDIT-NAME
           PERFORM 2020-SELECT-MARGIN
           PERFORM 2030-ACCUM-PAY-FREQ
           PERFORM 2040-DESCRIBE-REGION
           PERFORM 2050-EDIT-NAME
           PERFORM 2060-SPLIT-GROSS
           PERFORM 2070-SELECT-MARGIN
           PERFORM 2080-PRORATE-NET
           PERFORM 2090-LIMIT-DEDUCT
           PERFORM 2100-ACCRUE-GROSS
           PERFORM 2110-CHECK-CLOSE
           PERFORM 2120-SET-TAX-STATE
           PERFORM 2130-ACCRUE-DEDUCT
           PERFORM 2140-ACCRUE-DEDUCT
           PERFORM 2150-SELECT-RATE
           PERFORM 2160-SPLIT-DEDUCT
           PERFORM 2170-COUNT-CYCLE
           PERFORM 2180-EDIT-NAME
           PERFORM 2190-ACCUM-PAY-FREQ
           PERFORM 2200-EDIT-NAME
           PERFORM 2210-FLAG-DEDUCT
           PERFORM 2220-EDIT-CONTACT
           PERFORM 2230-AGE-NOTICE
           PERFORM 2240-DESCRIBE-SOURCE
           PERFORM 2250-COUNT-NOTICE
           PERFORM 2260-LIMIT-DEDUCT
           PERFORM 2270-COUNT-NOTICE
           PERFORM 2280-AGE-NOTICE
           PERFORM 2290-LIMIT-ITEM
           PERFORM 2300-COUNT-CYCLE
           PERFORM 2310-EDIT-CONTACT
           PERFORM 2320-EDIT-CONTACT
           PERFORM 2330-PRORATE-STATE-TAX
           PERFORM 2340-AGE-NOTICE
           PERFORM 2350-CHECK-NEXT-DUE
           PERFORM 2360-COUNT-CYCLE
           PERFORM 2370-SELECT-MARGIN
           PERFORM 2380-FLAG-DEDUCT
           PERFORM 2390-DESCRIBE-SOURCE
           PERFORM 2400-DESCRIBE-SOURCE
           PERFORM 2410-FLAG-GROSS
           PERFORM 2420-ACCRUE-DEDUCT
           PERFORM 2430-ACCUM-REGION
           PERFORM 2440-PRORATE-STATE-TAX
           PERFORM 2450-SELECT-MARGIN
           PERFORM 2460-PRORATE-STATE-TAX
           PERFORM 2470-AGE-CLOSE
           PERFORM 2480-ACCUM-PAY-FREQ
           PERFORM 2490-COUNT-NOTICE
           PERFORM 2500-DESCRIBE-REGION
           PERFORM 2510-SELECT-RATE
           PERFORM 2520-ACCRUE-DEDUCT
           PERFORM 2530-AGE-CLOSE
           PERFORM 2540-ASSESS-HOLD
           PERFORM 2550-SET-SOURCE
           PERFORM 2560-CHECK-NEXT-DUE
           PERFORM 2570-FLAG-STATE-TAX
           PERFORM 2580-SELECT-MARGIN
           PERFORM 2590-SELECT-MARGIN
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PYFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE PYFEE-DEDUCT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E105 NAME REQUIRED
      * E671 STATE TAX AMT NEGATIVE
      * E426 STATE TAX AMT OVER 25000
      * E373 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2010-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYFEE-NAME = SPACES
                   MOVE 'E105' TO WS-REJECT-CODE
               WHEN PYFEE-STATE-TAX-AMT < 0
                   MOVE 'E671' TO WS-REJECT-CODE
               WHEN PYFEE-STATE-TAX-AMT > 25000.00
                   MOVE 'E426' TO WS-REJECT-CODE
               WHEN PYFEE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E373' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA TIER TIER FOR THE GROSS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2020-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-TIER-RATE(1) TO PYFEE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PYFEE-GROSS-YTD NOT < WS-STATE-TA-TIER-LOW(WS-TX)
                   MOVE WS-STATE-TA-TIER-RATE(WS-TX) TO PYFEE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYFEE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYFEE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2030-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYFEE-PAY-FREQ-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYFEE-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2040-DESCRIBE-REGION.
           EVALUATE PYFEE-REGION-CD
               WHEN 'DP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E762 NAME REQUIRED
      * E787 NET AMT NEGATIVE
      * E596 NET AMT OVER 250000
      * E340 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYFEE-NAME = SPACES
                   MOVE 'E762' TO WS-REJECT-CODE
               WHEN PYFEE-NET-AMT < 0
                   MOVE 'E787' TO WS-REJECT-CODE
               WHEN PYFEE-NET-AMT > 250000.00
                   MOVE 'E596' TO WS-REJECT-CODE
               WHEN PYFEE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GROSS YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-GROSS.
           COMPUTE WS-WORK-AMT = PYFEE-GROSS-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PYFEE-GROSS-YTD - (WS-WORK-AMT * 2)
           COMPUTE PYFEE-STATE-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA TIER TIER FOR THE STATE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2070-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-TIER-RATE(1) TO PYFEE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PYFEE-STATE-TAX-YTD NOT < WS-STATE-TA-TIER-LOW(WS-TX)
                   MOVE WS-STATE-TA-TIER-RATE(WS-TX) TO PYFEE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYFEE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYFEE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET AMT.
      *----------------------------------------------------------------*
       2080-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PYFEE-NET-AMT ROUNDED =
                   PYFEE-STATE-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PYFEE-NET-AMT
           END-IF
           ADD PYFEE-NET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DEDUCT YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2090-LIMIT-DEDUCT.
           IF PYFEE-DEDUCT-YTD < 100.00
               MOVE 100.00 TO PYFEE-DEDUCT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYFEE-DEDUCT-YTD > 2500.00
               MOVE 2500.00 TO PYFEE-DEDUCT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GROSS YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2100-ACCRUE-GROSS.
           IF PYFEE-GROSS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYFEE-GROSS-YTD * PYFEE-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PYFEE-NET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2110-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYFEE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PYFEE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN LAST ACT DATE IS PAST
      * LZU -> TUF WHEN LAST ACT DATE IS PAST
      * TUF -> WKE WHEN LAST ACT DATE IS PAST
      * WKE -> CAZ WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2120-SET-TAX-STATE.
           EVALUATE PYFEE-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PYFEE-LAST-ACT-DATE > 0 AND PYFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PYFEE-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PYFEE-LAST-ACT-DATE > 0 AND PYFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PYFEE-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PYFEE-LAST-ACT-DATE > 0 AND PYFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PYFEE-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PYFEE-LAST-ACT-DATE > 0 AND PYFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PYFEE-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2130-ACCRUE-DEDUCT.
           IF PYFEE-DEDUCT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYFEE-DEDUCT-YTD * PYFEE-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2140-ACCRUE-DEDUCT.
           IF PYFEE-DEDUCT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYFEE-DEDUCT-YTD * PYFEE-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-TIER-RATE(1) TO PYFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-STATE-TA-TIER-LOW(WS-TX)
                   MOVE WS-STATE-TA-TIER-RATE(WS-TX) TO PYFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PYFEE-DEDUCT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PYFEE-DEDUCT-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2170-COUNT-CYCLE.
           IF PYFEE-CYCLE-CNT < 9999999
               ADD 1 TO PYFEE-CYCLE-CNT
           END-IF
           IF PYFEE-CYCLE-CNT NOT < 12
               MOVE 'Y' TO PYFEE-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E431 NAME REQUIRED
      * E230 STATE TAX AMT NEGATIVE
      * E568 STATE TAX AMT OVER 99999
      * E181 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2180-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYFEE-NAME = SPACES
                   MOVE 'E431' TO WS-REJECT-CODE
               WHEN PYFEE-STATE-TAX-AMT < 0
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN PYFEE-STATE-TAX-AMT > 99999.00
                   MOVE 'E568' TO WS-REJECT-CODE
               WHEN PYFEE-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX YTD BY PAY FREQ CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2190-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYFEE-PAY-FREQ-CD
                   ADD PYFEE-STATE-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYFEE-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYFEE-STATE-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E878 NAME REQUIRED
      * E048 WORK AMT 2 NEGATIVE
      * E954 WORK AMT 2 OVER 50000
      * E706 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2200-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYFEE-NAME = SPACES
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E954' TO WS-REJECT-CODE
               WHEN PYFEE-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2210-FLAG-DEDUCT.
           IF PYFEE-DEDUCT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PYFEE-DEDUCT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E080 CONTACT REQUIRED
      * E079 STATE TAX AMT NEGATIVE
      * E384 STATE TAX AMT OVER 10000
      * E886 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2220-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYFEE-CONTACT = SPACES
                   MOVE 'E080' TO WS-REJECT-CODE
               WHEN PYFEE-STATE-TAX-AMT < 0
                   MOVE 'E079' TO WS-REJECT-CODE
               WHEN PYFEE-STATE-TAX-AMT > 10000.00
                   MOVE 'E384' TO WS-REJECT-CODE
               WHEN PYFEE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E886' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2230-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF PYFEE-NOTICE-DATE > 0 AND PYFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYFEE-DEDUCT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PYFEE-DEDUCT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PYFEE-DEDUCT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PYFEE-DEDUCT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYFEE-DEDUCT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2240-DESCRIBE-SOURCE.
           EVALUATE PYFEE-SOURCE-CD
               WHEN 'NY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2250-COUNT-NOTICE.
           IF PYFEE-NOTICE-CNT < 9999999
               ADD 1 TO PYFEE-NOTICE-CNT
           END-IF
           IF PYFEE-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PYFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2260-LIMIT-DEDUCT.
           IF PYFEE-DEDUCT-YTD < 50.00
               MOVE 50.00 TO PYFEE-DEDUCT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYFEE-DEDUCT-YTD > 5000.00
               MOVE 5000.00 TO PYFEE-DEDUCT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2270-COUNT-NOTICE.
           IF PYFEE-NOTICE-CNT < 9999999
               ADD 1 TO PYFEE-NOTICE-CNT
           END-IF
           IF PYFEE-NOTICE-CNT NOT < 3
               MOVE 'Y' TO PYFEE-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2280-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF PYFEE-NOTICE-DATE > 0 AND PYFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYFEE-DEDUCT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PYFEE-DEDUCT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PYFEE-DEDUCT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PYFEE-DEDUCT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYFEE-DEDUCT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2290-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2300-COUNT-CYCLE.
           IF PYFEE-CYCLE-CNT < 9999999
               ADD 1 TO PYFEE-CYCLE-CNT
           END-IF
           IF PYFEE-CYCLE-CNT NOT < 3
               MOVE 'Y' TO PYFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E932 CONTACT REQUIRED
      * E731 DEDUCT AMT NEGATIVE
      * E022 DEDUCT AMT OVER 5000
      * E870 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2310-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYFEE-CONTACT = SPACES
                   MOVE 'E932' TO WS-REJECT-CODE
               WHEN PYFEE-DEDUCT-AMT < 0
                   MOVE 'E731' TO WS-REJECT-CODE
               WHEN PYFEE-DEDUCT-AMT > 5000.00
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN PYFEE-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E082 CONTACT REQUIRED
      * E725 STATE TAX YTD NEGATIVE
      * E606 STATE TAX YTD OVER 10000
      * E912 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2320-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYFEE-CONTACT = SPACES
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN PYFEE-STATE-TAX-YTD < 0
                   MOVE 'E725' TO WS-REJECT-CODE
               WHEN PYFEE-STATE-TAX-YTD > 10000.00
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN PYFEE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STATE TAX YTD.
      *----------------------------------------------------------------*
       2330-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PYFEE-STATE-TAX-YTD ROUNDED =
                   PYFEE-STATE-TAX-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PYFEE-STATE-TAX-YTD
           END-IF
           ADD PYFEE-STATE-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2340-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF PYFEE-NOTICE-DATE > 0 AND PYFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PYFEE-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PYFEE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-CYCLE.
           IF PYFEE-CYCLE-CNT < 9999999
               ADD 1 TO PYFEE-CYCLE-CNT
           END-IF
           IF PYFEE-CYCLE-CNT NOT < 3
               MOVE 'Y' TO PYFEE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA TIER TIER FOR THE STATE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2370-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-TIER-RATE(1) TO PYFEE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PYFEE-STATE-TAX-YTD NOT < WS-STATE-TA-TIER-LOW(WS-TX)
                   MOVE WS-STATE-TA-TIER-RATE(WS-TX) TO PYFEE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYFEE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYFEE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2380-FLAG-DEDUCT.
           IF PYFEE-DEDUCT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PYFEE-DEDUCT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2390-DESCRIBE-SOURCE.
           EVALUATE PYFEE-SOURCE-CD
               WHEN 'NY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2400-DESCRIBE-SOURCE.
           EVALUATE PYFEE-SOURCE-CD
               WHEN 'NY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GROSS YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2410-FLAG-GROSS.
           IF PYFEE-GROSS-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PYFEE-GROSS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2420-ACCRUE-DEDUCT.
           IF PYFEE-DEDUCT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYFEE-DEDUCT-YTD * PYFEE-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2430-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYFEE-REGION-CD
                   ADD PYFEE-GROSS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYFEE-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYFEE-GROSS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STATE TAX YTD.
      *----------------------------------------------------------------*
       2440-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PYFEE-STATE-TAX-YTD ROUNDED =
                   PYFEE-NET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PYFEE-STATE-TAX-YTD
           END-IF
           ADD PYFEE-STATE-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA TIER TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2450-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-TIER-RATE(1) TO PYFEE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PYFEE-STATE-TAX-AMT NOT < WS-STATE-TA-TIER-LOW(WS-TX)
                   MOVE WS-STATE-TA-TIER-RATE(WS-TX) TO PYFEE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYFEE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYFEE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       2460-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PYFEE-STATE-TAX-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PYFEE-STATE-TAX-AMT
           END-IF
           ADD PYFEE-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2470-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYFEE-CLOSE-DATE > 0 AND PYFEE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT YTD BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2480-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYFEE-PAY-FREQ-CD
                   ADD PYFEE-DEDUCT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYFEE-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYFEE-DEDUCT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2490-COUNT-NOTICE.
           IF PYFEE-NOTICE-CNT < 9999999
               ADD 1 TO PYFEE-NOTICE-CNT
           END-IF
           IF PYFEE-NOTICE-CNT NOT < 5
               MOVE 'Y' TO PYFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2500-DESCRIBE-REGION.
           EVALUATE PYFEE-REGION-CD
               WHEN 'DP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA TIER TIER FOR THE NET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2510-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-TIER-RATE(1) TO PYFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PYFEE-NET-AMT NOT < WS-STATE-TA-TIER-LOW(WS-TX)
                   MOVE WS-STATE-TA-TIER-RATE(WS-TX) TO PYFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-DEDUCT.
           IF PYFEE-DEDUCT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYFEE-DEDUCT-AMT * PYFEE-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2530-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYFEE-CLOSE-DATE > 0 AND PYFEE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYFEE-DEDUCT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PYFEE-DEDUCT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PYFEE-DEDUCT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PYFEE-DEDUCT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYFEE-DEDUCT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STATE TAX FEE ON THE DEDUCT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2540-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PYFEE-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PYFEE-STATE-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYFEE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PYFEE-STATE-TAX-AMT ROUNDED =
                       PYFEE-DEDUCT-AMT * PYFEE-MARGIN / 200
               WHEN PYFEE-REGION-CD = 'DP'
                   MOVE 5.00 TO PYFEE-STATE-TAX-AMT
               WHEN PYFEE-REGION-CD = 'TU'
                   MOVE 3.00 TO PYFEE-STATE-TAX-AMT
               WHEN OTHER
                   COMPUTE PYFEE-STATE-TAX-AMT ROUNDED =
                       PYFEE-DEDUCT-AMT * PYFEE-MARGIN / 100
           END-EVALUATE
           IF PYFEE-STATE-TAX-AMT > 0 AND PYFEE-STATE-TAX-AMT < 2.00
               MOVE 2.00 TO PYFEE-STATE-TAX-AMT
           END-IF
           IF PYFEE-STATE-TAX-AMT > 150.00
               MOVE 150.00 TO PYFEE-STATE-TAX-AMT
           END-IF
           ADD PYFEE-STATE-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN LAST ACT DATE IS PAST
      * PL -> VK WHEN LAST ACT DATE IS PAST
      * VK -> WB WHEN LAST ACT DATE IS PAST
      * WB -> XL WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2550-SET-SOURCE.
           EVALUATE PYFEE-SOURCE-CD
               WHEN 'NY'
                   IF PYFEE-LAST-ACT-DATE > 0 AND PYFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO PYFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF PYFEE-LAST-ACT-DATE > 0 AND PYFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO PYFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF PYFEE-LAST-ACT-DATE > 0 AND PYFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO PYFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF PYFEE-LAST-ACT-DATE > 0 AND PYFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO PYFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PYFEE-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PYFEE-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2570-FLAG-STATE-TAX.
           IF PYFEE-STATE-TAX-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PYFEE-STATE-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA TIER TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2580-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-TIER-RATE(1) TO PYFEE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PYFEE-STATE-TAX-AMT NOT < WS-STATE-TA-TIER-LOW(WS-TX)
                   MOVE WS-STATE-TA-TIER-RATE(WS-TX) TO PYFEE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYFEE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYFEE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2590-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-TIER-RATE(1) TO PYFEE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-STATE-TA-TIER-LOW(WS-TX)
                   MOVE WS-STATE-TA-TIER-RATE(WS-TX) TO PYFEE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYFEE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYFEE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PYFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PYFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PYFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PYFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
