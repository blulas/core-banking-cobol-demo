      *================================================================*
      * PROGRAM  : ESRISK                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MORTGAGE ESCROW RULE - FEES. CALLED BY THE MORTGAGE *
      *            ESCROW BATCH AND ONLINE PROGRAMS.                   *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-07-28 LMB  ORIGINAL                                       *
      * 1992-12-21 TWH  INSURANCE CALCULATION CORRECTED CHG9981        *
      * 1996-07-13 BHS  PAYMENT CALCULATION CORRECTED REQ0532          *
      * 1996-11-15 PQR  SURPLUS FEE SCHEDULE UPDATE PRB9432            *
      * 2000-02-02 RJT  PAYMENT ROUNDING - HALF UP PRB2506             *
      * 2000-07-15 EWS  SURPLUS ROUNDING - HALF UP CHG0834             *
      * 2002-08-10 TWH  SHORTAGE NOTICE TEXT PER LEGAL CHG9551         *
      * 2006-04-06 MKL  CUSHION CUTOFF MOVED INC7554                   *
      * 2006-05-07 BHS  CUSHION STATUS CODES ADDED PRB7932             *
      * 2007-04-03 GAF  TAX CALCULATION CORRECTED CHG6867              *
      * 2008-04-08 GAF  INSURANCE STATUS CODES ADDED CHG2881           *
      * 2010-04-21 LMB  AUDIT FINDING - CUSHION CONTROL TOTALS PRB5995 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESRISK.
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
      * WS-CUSHION-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CUSHION-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000207000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000197000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000253500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000683500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002900000000681500'.
       01  WS-CUSHION-TIER-TBL REDEFINES WS-CUSHION-TIER-VALUES.
           05  WS-CUSHION-TIER-ROW OCCURS 5 TIMES.
               10  WS-CUSHION-TIER-LOW     PIC 9(09)V99.
               10  WS-CUSHION-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SURPLUS-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURPLUS-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000647000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000457500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000209500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020050000000113000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025050000000536000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027550000000315000'.
       01  WS-SURPLUS-TIER-TBL REDEFINES WS-SURPLUS-TIER-VALUES.
           05  WS-SURPLUS-TIER-ROW OCCURS 6 TIMES.
               10  WS-SURPLUS-TIER-LOW     PIC 9(09)V99.
               10  WS-SURPLUS-TIER-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ESRISKP.

       PROCEDURE DIVISION USING ESRISK-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-INSURANCE
           PERFORM 2010-COUNT-CYCLE
           PERFORM 2020-COUNT-CYCLE
           PERFORM 2030-SPLIT-INSURANCE
           PERFORM 2040-ACCRUE-FEE-PCT
           PERFORM 2050-SELECT-RATE
           PERFORM 2060-ASSESS-PREFERRED
           PERFORM 2070-LIMIT-ITEM
           PERFORM 2080-LIMIT-SHORTAGE
           PERFORM 2090-FLAG-TAX
           PERFORM 2100-FLAG-SHORTAGE
           PERFORM 2110-SELECT-RATE
           PERFORM 2120-SELECT-RATE
           PERFORM 2130-ACCRUE-SHORTAGE
           PERFORM 2140-ASSESS-PREFERRED
           PERFORM 2150-CHECK-LAST-ACT
           PERFORM 2160-COUNT-EXCEPT
           PERFORM 2170-COUNT-CYCLE
           PERFORM 2180-EDIT-NAME
           PERFORM 2190-COUNT-EXCEPT
           PERFORM 2200-SET-REASON
           PERFORM 2210-COUNT-EXCEPT
           PERFORM 2220-COUNT-CYCLE
           PERFORM 2230-ACCUM-STATUS
           PERFORM 2240-FLAG-INSURANCE
           PERFORM 2250-EDIT-NAME
           PERFORM 2260-ASSESS-HOLD
           PERFORM 2270-LIMIT-ITEM
           PERFORM 2280-ACCRUE-INSURANCE
           PERFORM 2290-ACCRUE-SHORTAGE
           PERFORM 2300-ASSESS-STOP
           PERFORM 2310-SET-STATUS
           PERFORM 2320-SPLIT-TAX
           PERFORM 2330-FLAG-ITEM
           PERFORM 2340-ACCRUE-INSURANCE
           PERFORM 2350-ASSESS-STOP
           PERFORM 2360-FLAG-ITEM
           PERFORM 2370-EDIT-NAME
           PERFORM 2380-AGE-LAST-PAY
           PERFORM 2390-ASSESS-PREFERRED
           PERFORM 2400-AGE-NEXT-REVIEW
           PERFORM 2410-SET-REGION
           PERFORM 2420-PRORATE-SHORTAGE
           PERFORM 2430-SET-STATUS
           PERFORM 2440-FLAG-INSURANCE
           PERFORM 2450-FLAG-INSURANCE
           PERFORM 2460-CHECK-NOTICE
           PERFORM 2470-AGE-LAST-PAY
           PERFORM 2480-AGE-LAST-PAY
           PERFORM 2490-COUNT-CYCLE
           PERFORM 2500-COUNT-CYCLE
           PERFORM 2510-COUNT-EXCEPT
           PERFORM 2520-SELECT-RATE
           PERFORM 2530-DESCRIBE-STATUS
           PERFORM 2540-FLAG-SHORTAGE
           PERFORM 2550-CHECK-NEXT-REVIEW
           PERFORM 2560-AGE-LAST-PAY
           PERFORM 2570-AGE-NOTICE
           PERFORM 2580-ASSESS-HOLD
           PERFORM 2590-FLAG-ITEM
           PERFORM 2600-ASSESS-STOP
           PERFORM 2610-FLAG-INSURANCE
           PERFORM 2620-FLAG-ITEM
           PERFORM 2630-DESCRIBE-TYPE
           PERFORM 2640-COUNT-CYCLE
           PERFORM 2650-SELECT-RATE
           PERFORM 2660-PRORATE-SHORTAGE
           PERFORM 2670-SPLIT-INSURANCE
           PERFORM 2680-ACCUM-REASON
           PERFORM 2690-ASSESS-HOLD
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ESRISK-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESRISK-INSURANCE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ESRISK-INSURANCE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE ESRISK-INSURANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-CYCLE.
           IF ESRISK-CYCLE-CNT < 9999999
               ADD 1 TO ESRISK-CYCLE-CNT
           END-IF
           IF ESRISK-CYCLE-CNT NOT < 5
               MOVE 'Y' TO ESRISK-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2020-COUNT-CYCLE.
           IF ESRISK-CYCLE-CNT < 9999999
               ADD 1 TO ESRISK-CYCLE-CNT
           END-IF
           IF ESRISK-CYCLE-CNT NOT < 10
               MOVE 'Y' TO ESRISK-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2030-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESRISK-INSURANCE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ESRISK-INSURANCE-YTD - (WS-WORK-AMT *
               4)
           COMPUTE ESRISK-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2040-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ESRISK-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESRISK-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CUSHION TIER TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2050-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CUSHION-TIER-RATE(1) TO ESRISK-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ESRISK-TAX-YTD NOT < WS-CUSHION-TIER-LOW(WS-TX)
                   MOVE WS-CUSHION-TIER-RATE(WS-TX) TO ESRISK-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESRISK-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESRISK-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2060-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESRISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESRISK-SHORTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESRISK-STOP-FLAG = 'Y'
                   COMPUTE ESRISK-SHORTAGE-YTD ROUNDED = WS-WORK-AMT-2 *
                       ESRISK-RATE / 200
               WHEN ESRISK-REASON-CD = 'C'
                   MOVE 7.00 TO ESRISK-SHORTAGE-YTD
               WHEN ESRISK-REASON-CD = 'F'
                   MOVE 3.00 TO ESRISK-SHORTAGE-YTD
               WHEN OTHER
                   COMPUTE ESRISK-SHORTAGE-YTD ROUNDED = WS-WORK-AMT-2 *
                       ESRISK-RATE / 100
           END-EVALUATE
           IF ESRISK-SHORTAGE-YTD > 0 AND ESRISK-SHORTAGE-YTD < 2.00
               MOVE 2.00 TO ESRISK-SHORTAGE-YTD
           END-IF
           IF ESRISK-SHORTAGE-YTD > 25.00
               MOVE 25.00 TO ESRISK-SHORTAGE-YTD
           END-IF
           ADD ESRISK-SHORTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2070-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2080-LIMIT-SHORTAGE.
           IF ESRISK-SHORTAGE-AMT < 10.00
               MOVE 10.00 TO ESRISK-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESRISK-SHORTAGE-AMT > 5000.00
               MOVE 5000.00 TO ESRISK-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2090-FLAG-TAX.
           IF ESRISK-TAX-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESRISK-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2100-FLAG-SHORTAGE.
           IF ESRISK-SHORTAGE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESRISK-SHORTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2110-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-TIER-RATE(1) TO ESRISK-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-SURPLUS-TIER-LOW(WS-TX)
                   MOVE WS-SURPLUS-TIER-RATE(WS-TX) TO ESRISK-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESRISK-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESRISK-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CUSHION TIER TIER FOR THE DISBURSE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CUSHION-TIER-RATE(1) TO ESRISK-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ESRISK-DISBURSE-YTD NOT < WS-CUSHION-TIER-LOW(WS-TX)
                   MOVE WS-CUSHION-TIER-RATE(WS-TX) TO ESRISK-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESRISK-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESRISK-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2130-ACCRUE-SHORTAGE.
           IF ESRISK-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESRISK-SHORTAGE-AMT * ESRISK-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE INSURANCE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2140-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESRISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESRISK-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESRISK-INSURANCE-AMT
                       * ESRISK-FEE-PCT / 200
               WHEN ESRISK-STATUS-CD = 'A'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN ESRISK-STATUS-CD = 'E'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESRISK-INSURANCE-AMT
                       * ESRISK-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2150-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ESRISK-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESRISK-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ESRISK-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-EXCEPT.
           IF ESRISK-EXCEPT-CNT < 9999999
               ADD 1 TO ESRISK-EXCEPT-CNT
           END-IF
           IF ESRISK-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO ESRISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2170-COUNT-CYCLE.
           IF ESRISK-CYCLE-CNT < 9999999
               ADD 1 TO ESRISK-CYCLE-CNT
           END-IF
           IF ESRISK-CYCLE-CNT NOT < 10
               MOVE 'Y' TO ESRISK-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E035 NAME REQUIRED
      * E757 SHORTAGE AMT NEGATIVE
      * E653 SHORTAGE AMT OVER 250000
      * E308 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2180-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESRISK-NAME = SPACES
                   MOVE 'E035' TO WS-REJECT-CODE
               WHEN ESRISK-SHORTAGE-AMT < 0
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN ESRISK-SHORTAGE-AMT > 250000.00
                   MOVE 'E653' TO WS-REJECT-CODE
               WHEN ESRISK-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E308' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2190-COUNT-EXCEPT.
           IF ESRISK-EXCEPT-CNT < 9999999
               ADD 1 TO ESRISK-EXCEPT-CNT
           END-IF
           IF ESRISK-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO ESRISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN LAST PAY DATE IS PAST
      * F -> K WHEN LAST PAY DATE IS PAST
      * K -> L WHEN LAST PAY DATE IS PAST
      * L -> S WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2200-SET-REASON.
           EVALUATE ESRISK-REASON-CD
               WHEN 'C'
                   IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'F' TO ESRISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO ESRISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO ESRISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO ESRISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2210-COUNT-EXCEPT.
           IF ESRISK-EXCEPT-CNT < 9999999
               ADD 1 TO ESRISK-EXCEPT-CNT
           END-IF
           IF ESRISK-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO ESRISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2220-COUNT-CYCLE.
           IF ESRISK-CYCLE-CNT < 9999999
               ADD 1 TO ESRISK-CYCLE-CNT
           END-IF
           IF ESRISK-CYCLE-CNT NOT < 10
               MOVE 'Y' TO ESRISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2230-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESRISK-STATUS-CD
                   ADD ESRISK-INSURANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESRISK-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESRISK-INSURANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2240-FLAG-INSURANCE.
           IF ESRISK-INSURANCE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESRISK-INSURANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E958 NAME REQUIRED
      * E576 SHORTAGE AMT NEGATIVE
      * E216 SHORTAGE AMT OVER 1000000
      * E858 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2250-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESRISK-NAME = SPACES
                   MOVE 'E958' TO WS-REJECT-CODE
               WHEN ESRISK-SHORTAGE-AMT < 0
                   MOVE 'E576' TO WS-REJECT-CODE
               WHEN ESRISK-SHORTAGE-AMT > 1000000.00
                   MOVE 'E216' TO WS-REJECT-CODE
               WHEN ESRISK-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE YTD FEE ON THE TAX YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2260-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESRISK-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ESRISK-SHORTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESRISK-PREFERRED-FLAG = 'Y'
                   COMPUTE ESRISK-SHORTAGE-YTD ROUNDED = ESRISK-TAX-YTD
                       * ESRISK-FEE-PCT / 200
               WHEN ESRISK-STATUS-CD = 'A'
                   MOVE 3.00 TO ESRISK-SHORTAGE-YTD
               WHEN ESRISK-STATUS-CD = 'E'
                   MOVE 5.00 TO ESRISK-SHORTAGE-YTD
               WHEN OTHER
                   COMPUTE ESRISK-SHORTAGE-YTD ROUNDED = ESRISK-TAX-YTD
                       * ESRISK-FEE-PCT / 100
           END-EVALUATE
           IF ESRISK-SHORTAGE-YTD > 0 AND ESRISK-SHORTAGE-YTD < 2.00
               MOVE 2.00 TO ESRISK-SHORTAGE-YTD
           END-IF
           IF ESRISK-SHORTAGE-YTD > 25.00
               MOVE 25.00 TO ESRISK-SHORTAGE-YTD
           END-IF
           ADD ESRISK-SHORTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2270-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INSURANCE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-INSURANCE.
           IF ESRISK-INSURANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESRISK-INSURANCE-YTD * ESRISK-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESRISK-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2290-ACCRUE-SHORTAGE.
           IF ESRISK-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESRISK-SHORTAGE-AMT * ESRISK-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESRISK-INSURANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX YTD FEE ON THE INSURANCE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2300-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESRISK-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESRISK-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESRISK-PREFERRED-FLAG = 'Y'
                   COMPUTE ESRISK-TAX-YTD ROUNDED = ESRISK-INSURANCE-YTD
                       * ESRISK-RATE / 200
               WHEN ESRISK-TYPE-CD = 'AKX'
                   MOVE 20.00 TO ESRISK-TAX-YTD
               WHEN ESRISK-TYPE-CD = 'REP'
                   MOVE 3.00 TO ESRISK-TAX-YTD
               WHEN OTHER
                   COMPUTE ESRISK-TAX-YTD ROUNDED = ESRISK-INSURANCE-YTD
                       * ESRISK-RATE / 100
           END-EVALUATE
           IF ESRISK-TAX-YTD > 0 AND ESRISK-TAX-YTD < 2.00
               MOVE 2.00 TO ESRISK-TAX-YTD
           END-IF
           IF ESRISK-TAX-YTD > 35.00
               MOVE 35.00 TO ESRISK-TAX-YTD
           END-IF
           ADD ESRISK-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN LAST ACT DATE IS PAST
      * E -> R WHEN LAST ACT DATE IS PAST
      * R -> U WHEN LAST ACT DATE IS PAST
      * U -> Z WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-STATUS.
           EVALUATE ESRISK-STATUS-CD
               WHEN 'A'
                   IF ESRISK-LAST-ACT-DATE > 0 AND ESRISK-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'E' TO ESRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF ESRISK-LAST-ACT-DATE > 0 AND ESRISK-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO ESRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ESRISK-LAST-ACT-DATE > 0 AND ESRISK-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'U' TO ESRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF ESRISK-LAST-ACT-DATE > 0 AND ESRISK-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'Z' TO ESRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESRISK-TAX-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ESRISK-TAX-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2330-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INSURANCE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2340-ACCRUE-INSURANCE.
           IF ESRISK-INSURANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESRISK-INSURANCE-AMT * ESRISK-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX YTD FEE ON THE INSURANCE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2350-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESRISK-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESRISK-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESRISK-HOLD-FLAG = 'Y'
                   COMPUTE ESRISK-TAX-YTD ROUNDED = ESRISK-INSURANCE-YTD
                       * ESRISK-FEE-PCT / 200
               WHEN ESRISK-TYPE-CD = 'AKX'
                   MOVE 12.00 TO ESRISK-TAX-YTD
               WHEN ESRISK-TYPE-CD = 'REP'
                   MOVE 7.00 TO ESRISK-TAX-YTD
               WHEN OTHER
                   COMPUTE ESRISK-TAX-YTD ROUNDED = ESRISK-INSURANCE-YTD
                       * ESRISK-FEE-PCT / 100
           END-EVALUATE
           IF ESRISK-TAX-YTD > 0 AND ESRISK-TAX-YTD < 5.00
               MOVE 5.00 TO ESRISK-TAX-YTD
           END-IF
           IF ESRISK-TAX-YTD > 25.00
               MOVE 25.00 TO ESRISK-TAX-YTD
           END-IF
           ADD ESRISK-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2360-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E397 NAME REQUIRED
      * E869 WORK AMT 2 NEGATIVE
      * E516 WORK AMT 2 OVER 10000
      * E154 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2370-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESRISK-NAME = SPACES
                   MOVE 'E397' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN ESRISK-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2380-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESRISK-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE DISBURSE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2390-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESRISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESRISK-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESRISK-DISBURSE-YTD *
                       ESRISK-RATE / 200
               WHEN ESRISK-TYPE-CD = 'AKX'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN ESRISK-TYPE-CD = 'REP'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESRISK-DISBURSE-YTD *
                       ESRISK-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2400-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ESRISK-NEXT-REVIEW-DATE > 0 AND ESRISK-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESRISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * B -> G WHEN NEXT REVIEW DATE IS PAST
      * G -> Y WHEN NEXT REVIEW DATE IS PAST
      * Y -> B WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2410-SET-REGION.
           EVALUATE ESRISK-REGION-CD
               WHEN 'B'
                   IF ESRISK-NEXT-REVIEW-DATE > 0 AND
                       ESRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'G' TO ESRISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ESRISK-NEXT-REVIEW-DATE > 0 AND
                       ESRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'Y' TO ESRISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF ESRISK-NEXT-REVIEW-DATE > 0 AND
                       ESRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'B' TO ESRISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2420-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ESRISK-SHORTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN LAST PAY DATE IS PAST
      * E -> R WHEN LAST PAY DATE IS PAST
      * R -> U WHEN LAST PAY DATE IS PAST
      * U -> Z WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2430-SET-STATUS.
           EVALUATE ESRISK-STATUS-CD
               WHEN 'A'
                   IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'E' TO ESRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO ESRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'U' TO ESRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'Z' TO ESRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INSURANCE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2440-FLAG-INSURANCE.
           IF ESRISK-INSURANCE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESRISK-INSURANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2450-FLAG-INSURANCE.
           IF ESRISK-INSURANCE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESRISK-INSURANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2460-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESRISK-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESRISK-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ESRISK-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2470-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESRISK-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2480-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESRISK-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESRISK-DISBURSE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESRISK-DISBURSE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESRISK-DISBURSE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ESRISK-DISBURSE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESRISK-DISBURSE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2490-COUNT-CYCLE.
           IF ESRISK-CYCLE-CNT < 9999999
               ADD 1 TO ESRISK-CYCLE-CNT
           END-IF
           IF ESRISK-CYCLE-CNT NOT < 5
               MOVE 'Y' TO ESRISK-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2500-COUNT-CYCLE.
           IF ESRISK-CYCLE-CNT < 9999999
               ADD 1 TO ESRISK-CYCLE-CNT
           END-IF
           IF ESRISK-CYCLE-CNT NOT < 12
               MOVE 'Y' TO ESRISK-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2510-COUNT-EXCEPT.
           IF ESRISK-EXCEPT-CNT < 9999999
               ADD 1 TO ESRISK-EXCEPT-CNT
           END-IF
           IF ESRISK-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO ESRISK-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CUSHION TIER TIER FOR THE INSURANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2520-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CUSHION-TIER-RATE(1) TO ESRISK-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ESRISK-INSURANCE-YTD NOT < WS-CUSHION-TIER-LOW(WS-TX)
                   MOVE WS-CUSHION-TIER-RATE(WS-TX) TO ESRISK-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESRISK-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESRISK-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2530-DESCRIBE-STATUS.
           EVALUATE ESRISK-STATUS-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'E'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHORTAGE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2540-FLAG-SHORTAGE.
           IF ESRISK-SHORTAGE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESRISK-SHORTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2550-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ESRISK-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESRISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ESRISK-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2560-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESRISK-LAST-PAY-DATE > 0 AND ESRISK-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESRISK-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESRISK-SHORTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESRISK-SHORTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESRISK-SHORTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ESRISK-SHORTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESRISK-SHORTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2570-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESRISK-NOTICE-DATE > 0 AND ESRISK-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESRISK-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESRISK-SHORTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESRISK-SHORTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESRISK-SHORTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESRISK-SHORTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESRISK-SHORTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE YTD FEE ON THE DISBURSE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2580-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESRISK-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ESRISK-SHORTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESRISK-PREFERRED-FLAG = 'Y'
                   COMPUTE ESRISK-SHORTAGE-YTD ROUNDED =
                       ESRISK-DISBURSE-YTD * ESRISK-RATE / 200
               WHEN ESRISK-TYPE-CD = 'AKX'
                   MOVE 20.00 TO ESRISK-SHORTAGE-YTD
               WHEN ESRISK-TYPE-CD = 'REP'
                   MOVE 3.00 TO ESRISK-SHORTAGE-YTD
               WHEN OTHER
                   COMPUTE ESRISK-SHORTAGE-YTD ROUNDED =
                       ESRISK-DISBURSE-YTD * ESRISK-RATE / 100
           END-EVALUATE
           IF ESRISK-SHORTAGE-YTD > 0 AND ESRISK-SHORTAGE-YTD < 10.00
               MOVE 10.00 TO ESRISK-SHORTAGE-YTD
           END-IF
           IF ESRISK-SHORTAGE-YTD > 100.00
               MOVE 100.00 TO ESRISK-SHORTAGE-YTD
           END-IF
           ADD ESRISK-SHORTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2590-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INSURANCE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2600-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESRISK-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESRISK-INSURANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESRISK-PREFERRED-FLAG = 'Y'
                   COMPUTE ESRISK-INSURANCE-YTD ROUNDED = WS-WORK-AMT-2
                       * ESRISK-FEE-PCT / 200
               WHEN ESRISK-REASON-CD = 'C'
                   MOVE 7.00 TO ESRISK-INSURANCE-YTD
               WHEN ESRISK-REASON-CD = 'F'
                   MOVE 5.00 TO ESRISK-INSURANCE-YTD
               WHEN OTHER
                   COMPUTE ESRISK-INSURANCE-YTD ROUNDED = WS-WORK-AMT-2
                       * ESRISK-FEE-PCT / 100
           END-EVALUATE
           IF ESRISK-INSURANCE-YTD > 0 AND ESRISK-INSURANCE-YTD < 2.00
               MOVE 2.00 TO ESRISK-INSURANCE-YTD
           END-IF
           IF ESRISK-INSURANCE-YTD > 150.00
               MOVE 150.00 TO ESRISK-INSURANCE-YTD
           END-IF
           ADD ESRISK-INSURANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * INSURANCE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2610-FLAG-INSURANCE.
           IF ESRISK-INSURANCE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESRISK-INSURANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2620-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2630-DESCRIBE-TYPE.
           EVALUATE ESRISK-TYPE-CD
               WHEN 'AKX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'REP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SMX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VJL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WKN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XKU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XTF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2640-COUNT-CYCLE.
           IF ESRISK-CYCLE-CNT < 9999999
               ADD 1 TO ESRISK-CYCLE-CNT
           END-IF
           IF ESRISK-CYCLE-CNT NOT < 5
               MOVE 'Y' TO ESRISK-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS TIER TIER FOR THE DISBURSE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2650-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-TIER-RATE(1) TO ESRISK-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ESRISK-DISBURSE-YTD NOT < WS-SURPLUS-TIER-LOW(WS-TX)
                   MOVE WS-SURPLUS-TIER-RATE(WS-TX) TO ESRISK-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESRISK-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESRISK-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHORTAGE YTD.
      *----------------------------------------------------------------*
       2660-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESRISK-SHORTAGE-YTD ROUNDED =
                   ESRISK-DISBURSE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESRISK-SHORTAGE-YTD
           END-IF
           ADD ESRISK-SHORTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2670-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESRISK-INSURANCE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ESRISK-INSURANCE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE ESRISK-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2680-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESRISK-REASON-CD
                   ADD ESRISK-INSURANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESRISK-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESRISK-INSURANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE INSURANCE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2690-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESRISK-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESRISK-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESRISK-INSURANCE-YTD
                       * ESRISK-RATE / 200
               WHEN ESRISK-REGION-CD = 'B'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN ESRISK-REGION-CD = 'G'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESRISK-INSURANCE-YTD
                       * ESRISK-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ESRISK-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ESRISK-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ESRISK-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ESRISK EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
