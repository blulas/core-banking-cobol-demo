      *================================================================*
      * PROGRAM  : CRVAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PROPRIETARY CREDIT CARD RULE - DATE RULES. CALLED BY*
      *            THE PROPRIETARY CREDIT CARD BATCH AND ONLINE        *
      *            PROGRAMS.                                           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-11 RJT  ORIGINAL                                       *
      * 1997-07-02 LMB  PERFORMANCE - CASH-ADV READ AHEAD INC1670      *
      * 1999-10-11 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-01-05 TWH  PERFORMANCE - LIMIT READ AHEAD PRB4944         *
      * 2000-12-20 RJT  MIN-DUE EXCEPTIONS TO OPS QUEUE PRB8863        *
      * 2003-11-15 GAF  AUDIT FINDING - CASH-ADV CONTROL TOTALS INC4427*
      * 2005-11-07 GAF  CASH-ADV FEE SCHEDULE UPDATE PRB7118           *
      * 2005-06-13 MKL  PAYMENT CUTOFF MOVED CHG7895                   *
      * 2007-09-28 MKL  PURCHASE STATUS CODES ADDED REQ6398            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRVAL.
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
                                           VALUE 20.000000.
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
      * WS-LIMIT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LIMIT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000056500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000746000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000338000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000812500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000526000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003200000000311000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005700000000011000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008200000000683500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008300000000569000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013300000000264000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013800000000715500'.
       01  WS-LIMIT-GRID-TBL REDEFINES WS-LIMIT-GRID-VALUES.
           05  WS-LIMIT-GRID-ROW OCCURS 11 TIMES.
               10  WS-LIMIT-GRID-LOW       PIC 9(09)V99.
               10  WS-LIMIT-GRID-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-ADV-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-ADV-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000264500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000309000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000900000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000848500'.
       01  WS-CASH-ADV-GRID-TBL REDEFINES WS-CASH-ADV-GRID-VALUES.
           05  WS-CASH-ADV-GRID-ROW OCCURS 4 TIMES.
               10  WS-CASH-ADV-GRID-LOW    PIC 9(09)V99.
               10  WS-CASH-ADV-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CRVALP.

       PROCEDURE DIVISION USING CRVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-COUNT-NOTICE
           PERFORM 2010-ASSESS-OVERRIDE
           PERFORM 2020-SELECT-DISCOUNT
           PERFORM 2030-AGE-EXPIRY
           PERFORM 2040-ASSESS-STOP
           PERFORM 2050-SELECT-PENALTY-PCT
           PERFORM 2060-AGE-EFFECTIVE
           PERFORM 2070-SPLIT-LIMIT
           PERFORM 2080-FLAG-FINANCE
           PERFORM 2090-PRORATE-MIN-DUE
           PERFORM 2100-ACCUM-BILL-CYCLE
           PERFORM 2110-ACCUM-TYPE
           PERFORM 2120-EDIT-DESC
           PERFORM 2130-AGE-OPEN
           PERFORM 2140-ASSESS-OVERRIDE
           PERFORM 2150-ACCRUE-LATE-FEE
           PERFORM 2160-SET-TYPE
           PERFORM 2170-AGE-EXPIRY
           PERFORM 2180-FLAG-LIMIT
           PERFORM 2190-COUNT-CYCLE
           PERFORM 2200-FLAG-ITEM
           PERFORM 2210-SET-TYPE
           PERFORM 2220-LIMIT-ITEM
           PERFORM 2230-AGE-EFFECTIVE
           PERFORM 2240-ACCRUE-LIMIT
           PERFORM 2250-ASSESS-OVERRIDE
           PERFORM 2260-SPLIT-PAYMENT
           PERFORM 2270-PRORATE-PAYMENT
           PERFORM 2280-DESCRIBE-BILL-CYCLE
           PERFORM 2290-SET-BRANCH
           PERFORM 2300-SET-TYPE
           PERFORM 2310-COUNT-NOTICE
           PERFORM 2320-ACCRUE-PAYMENT
           PERFORM 2330-ACCRUE-PENALTY-PCT
           PERFORM 2340-LIMIT-LIMIT
           PERFORM 2350-SET-BILL-CYCLE
           PERFORM 2360-FLAG-FINANCE
           PERFORM 2370-DESCRIBE-BILL-CYCLE
           PERFORM 2380-ACCRUE-MIN-DUE
           PERFORM 2390-ASSESS-STOP
           PERFORM 2400-ASSESS-OVERRIDE
           PERFORM 2410-ACCRUE-MIN-DUE
           PERFORM 2420-PRORATE-ITEM
           PERFORM 2430-PRORATE-MIN-DUE
           PERFORM 2440-LIMIT-LATE-FEE
           PERFORM 2450-PRORATE-LATE-FEE
           PERFORM 2460-SELECT-DISCOUNT
           PERFORM 2470-COUNT-CYCLE
           PERFORM 2480-PRORATE-MIN-DUE
           PERFORM 2490-ASSESS-STOP
           PERFORM 2500-ACCRUE-MIN-DUE
           PERFORM 2510-ACCRUE-MIN-DUE
           PERFORM 2520-LIMIT-LIMIT
           PERFORM 2530-CHECK-EXPIRY
           PERFORM 2540-SET-BILL-CYCLE
           PERFORM 2550-SELECT-DISCOUNT
           PERFORM 2560-LIMIT-FINANCE
           PERFORM 2570-DESCRIBE-TYPE
           PERFORM 2580-ACCUM-STATUS
           PERFORM 2590-SET-BRANCH
           PERFORM 2600-COUNT-CYCLE
           PERFORM 2610-EDIT-DESC
           PERFORM 2620-ACCUM-TYPE
           PERFORM 2630-SPLIT-MIN-DUE
           PERFORM 2640-FLAG-MIN-DUE
           PERFORM 2650-COUNT-NOTICE
           PERFORM 2660-EDIT-CONTACT
           PERFORM 2670-ACCRUE-MIN-DUE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CRVAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2000-COUNT-NOTICE.
           IF CRVAL-NOTICE-CNT < 9999999
               ADD 1 TO CRVAL-NOTICE-CNT
           END-IF
           IF CRVAL-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CRVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2010-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRVAL-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRVAL-STOP-FLAG = 'Y'
                   COMPUTE CRVAL-MIN-DUE-AMT ROUNDED = WS-WORK-AMT *
                       CRVAL-DISCOUNT / 200
               WHEN CRVAL-BILL-CYCLE-CD = 'B'
                   MOVE 12.00 TO CRVAL-MIN-DUE-AMT
               WHEN CRVAL-BILL-CYCLE-CD = 'D'
                   MOVE 5.00 TO CRVAL-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRVAL-MIN-DUE-AMT ROUNDED = WS-WORK-AMT *
                       CRVAL-DISCOUNT / 100
           END-EVALUATE
           IF CRVAL-MIN-DUE-AMT > 0 AND CRVAL-MIN-DUE-AMT < 2.00
               MOVE 2.00 TO CRVAL-MIN-DUE-AMT
           END-IF
           IF CRVAL-MIN-DUE-AMT > 25.00
               MOVE 25.00 TO CRVAL-MIN-DUE-AMT
           END-IF
           ADD CRVAL-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV GRID TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2020-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-GRID-RATE(1) TO CRVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRVAL-LIMIT-AMT NOT < WS-CASH-ADV-GRID-LOW(WS-TX)
                   MOVE WS-CASH-ADV-GRID-RATE(WS-TX) TO CRVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRVAL-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRVAL-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRVAL-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRVAL-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRVAL-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE MIN DUE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2040-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CRVAL-STOP-FLAG = 'Y'
                   MOVE ZERO TO CRVAL-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRVAL-PREFERRED-FLAG = 'Y'
                   COMPUTE CRVAL-MIN-DUE-AMT ROUNDED = CRVAL-MIN-DUE-YTD
                       * CRVAL-DISCOUNT / 200
               WHEN CRVAL-TYPE-CD = 'GP'
                   MOVE 5.00 TO CRVAL-MIN-DUE-AMT
               WHEN CRVAL-TYPE-CD = 'UV'
                   MOVE 20.00 TO CRVAL-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRVAL-MIN-DUE-AMT ROUNDED = CRVAL-MIN-DUE-YTD
                       * CRVAL-DISCOUNT / 100
           END-EVALUATE
           IF CRVAL-MIN-DUE-AMT > 0 AND CRVAL-MIN-DUE-AMT < 1.00
               MOVE 1.00 TO CRVAL-MIN-DUE-AMT
           END-IF
           IF CRVAL-MIN-DUE-AMT > 25.00
               MOVE 25.00 TO CRVAL-MIN-DUE-AMT
           END-IF
           ADD CRVAL-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE MIN DUE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2050-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO CRVAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CRVAL-MIN-DUE-YTD NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO CRVAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRVAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRVAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRVAL-EFFECTIVE-DATE > 0 AND CRVAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRVAL-EFFECTIVE-DATE TO DTU-DATE-1
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
      * SPLIT LIMIT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRVAL-LIMIT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CRVAL-LIMIT-AMT - (WS-WORK-AMT * 3)
           COMPUTE CRVAL-FINANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FINANCE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2080-FLAG-FINANCE.
           IF CRVAL-FINANCE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRVAL-FINANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       2090-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRVAL-MIN-DUE-YTD ROUNDED =
                   CRVAL-LATE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRVAL-MIN-DUE-YTD
           END-IF
           ADD CRVAL-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE YTD BY BILL CYCLE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2100-ACCUM-BILL-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRVAL-BILL-CYCLE-CD
                   ADD CRVAL-MIN-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRVAL-BILL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRVAL-MIN-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRVAL-TYPE-CD
                   ADD CRVAL-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRVAL-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRVAL-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E154 DESC REQUIRED
      * E541 WORK AMT NEGATIVE
      * E775 WORK AMT OVER 250000
      * E075 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2120-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRVAL-DESC = SPACES
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E541' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E775' TO WS-REJECT-CODE
               WHEN CRVAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2130-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRVAL-OPEN-DATE > 0 AND CRVAL-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRVAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2140-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRVAL-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRVAL-PREFERRED-FLAG = 'Y'
                   COMPUTE CRVAL-PAYMENT-AMT ROUNDED = WS-WORK-AMT *
                       CRVAL-PENALTY-PCT / 200
               WHEN CRVAL-BRANCH-CD = 'CTB'
                   MOVE 20.00 TO CRVAL-PAYMENT-AMT
               WHEN CRVAL-BRANCH-CD = 'LNX'
                   MOVE 20.00 TO CRVAL-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRVAL-PAYMENT-AMT ROUNDED = WS-WORK-AMT *
                       CRVAL-PENALTY-PCT / 100
           END-EVALUATE
           IF CRVAL-PAYMENT-AMT > 0 AND CRVAL-PAYMENT-AMT < 10.00
               MOVE 10.00 TO CRVAL-PAYMENT-AMT
           END-IF
           IF CRVAL-PAYMENT-AMT > 250.00
               MOVE 250.00 TO CRVAL-PAYMENT-AMT
           END-IF
           ADD CRVAL-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2150-ACCRUE-LATE-FEE.
           IF CRVAL-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRVAL-LATE-FEE-YTD * CRVAL-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRVAL-MIN-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN OPEN DATE IS PAST
      * UV -> YK WHEN OPEN DATE IS PAST
      * YK -> GP WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-TYPE.
           EVALUATE CRVAL-TYPE-CD
               WHEN 'GP'
                   IF CRVAL-OPEN-DATE > 0 AND CRVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRVAL-OPEN-DATE > 0 AND CRVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRVAL-OPEN-DATE > 0 AND CRVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2170-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRVAL-MIN-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRVAL-MIN-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRVAL-MIN-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CRVAL-MIN-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRVAL-MIN-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2180-FLAG-LIMIT.
           IF CRVAL-LIMIT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRVAL-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2190-COUNT-CYCLE.
           IF CRVAL-CYCLE-CNT < 9999999
               ADD 1 TO CRVAL-CYCLE-CNT
           END-IF
           IF CRVAL-CYCLE-CNT NOT < 3
               MOVE 'Y' TO CRVAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2200-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN EXPIRY DATE IS PAST
      * UV -> YK WHEN EXPIRY DATE IS PAST
      * YK -> GP WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-TYPE.
           EVALUATE CRVAL-TYPE-CD
               WHEN 'GP'
                   IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2220-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2230-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRVAL-EFFECTIVE-DATE > 0 AND CRVAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2240-ACCRUE-LIMIT.
           IF CRVAL-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRVAL-LIMIT-AMT * CRVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE YTD FEE ON THE LATE FEE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2250-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRVAL-MIN-DUE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRVAL-STOP-FLAG = 'Y'
                   COMPUTE CRVAL-MIN-DUE-YTD ROUNDED =
                       CRVAL-LATE-FEE-YTD * CRVAL-DISCOUNT / 200
               WHEN CRVAL-BILL-CYCLE-CD = 'B'
                   MOVE 5.00 TO CRVAL-MIN-DUE-YTD
               WHEN CRVAL-BILL-CYCLE-CD = 'D'
                   MOVE 5.00 TO CRVAL-MIN-DUE-YTD
               WHEN OTHER
                   COMPUTE CRVAL-MIN-DUE-YTD ROUNDED =
                       CRVAL-LATE-FEE-YTD * CRVAL-DISCOUNT / 100
           END-EVALUATE
           IF CRVAL-MIN-DUE-YTD > 0 AND CRVAL-MIN-DUE-YTD < 5.00
               MOVE 5.00 TO CRVAL-MIN-DUE-YTD
           END-IF
           IF CRVAL-MIN-DUE-YTD > 100.00
               MOVE 100.00 TO CRVAL-MIN-DUE-YTD
           END-IF
           ADD CRVAL-MIN-DUE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2260-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = CRVAL-PAYMENT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRVAL-PAYMENT-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRVAL-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       2270-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRVAL-PAYMENT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRVAL-PAYMENT-AMT
           END-IF
           ADD CRVAL-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2280-DESCRIBE-BILL-CYCLE.
           EVALUATE CRVAL-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'P'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN MATURITY DATE IS PAST
      * LNX -> MGS WHEN MATURITY DATE IS PAST
      * MGS -> YNJ WHEN MATURITY DATE IS PAST
      * YNJ -> CTB WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2290-SET-BRANCH.
           EVALUATE CRVAL-BRANCH-CD
               WHEN 'CTB'
                   IF CRVAL-MATURITY-DATE > 0 AND CRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRVAL-MATURITY-DATE > 0 AND CRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRVAL-MATURITY-DATE > 0 AND CRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRVAL-MATURITY-DATE > 0 AND CRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN EFFECTIVE DATE IS PAST
      * UV -> YK WHEN EFFECTIVE DATE IS PAST
      * YK -> GP WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2300-SET-TYPE.
           EVALUATE CRVAL-TYPE-CD
               WHEN 'GP'
                   IF CRVAL-EFFECTIVE-DATE > 0 AND CRVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRVAL-EFFECTIVE-DATE > 0 AND CRVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRVAL-EFFECTIVE-DATE > 0 AND CRVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2310-COUNT-NOTICE.
           IF CRVAL-NOTICE-CNT < 9999999
               ADD 1 TO CRVAL-NOTICE-CNT
           END-IF
           IF CRVAL-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CRVAL-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2320-ACCRUE-PAYMENT.
           IF CRVAL-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRVAL-PAYMENT-AMT * CRVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2330-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CRVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRVAL-FINANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2340-LIMIT-LIMIT.
           IF CRVAL-LIMIT-AMT < 10.00
               MOVE 10.00 TO CRVAL-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRVAL-LIMIT-AMT > 1000.00
               MOVE 1000.00 TO CRVAL-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN EXPIRY DATE IS PAST
      * D -> E WHEN EXPIRY DATE IS PAST
      * E -> F WHEN EXPIRY DATE IS PAST
      * F -> H WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2350-SET-BILL-CYCLE.
           EVALUATE CRVAL-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRVAL-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRVAL-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRVAL-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRVAL-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FINANCE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2360-FLAG-FINANCE.
           IF CRVAL-FINANCE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRVAL-FINANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-BILL-CYCLE.
           EVALUATE CRVAL-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-MIN-DUE.
           IF CRVAL-MIN-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRVAL-MIN-DUE-YTD * CRVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRVAL-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FINANCE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2390-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CRVAL-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRVAL-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRVAL-FINANCE-YTD *
                       CRVAL-DISCOUNT / 200
               WHEN CRVAL-TYPE-CD = 'GP'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN CRVAL-TYPE-CD = 'UV'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRVAL-FINANCE-YTD *
                       CRVAL-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FINANCE YTD FEE ON THE LIMIT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2400-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRVAL-FINANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRVAL-STOP-FLAG = 'Y'
                   COMPUTE CRVAL-FINANCE-YTD ROUNDED = CRVAL-LIMIT-AMT *
                       CRVAL-DISCOUNT / 200
               WHEN CRVAL-STATUS-CD = 'BAL'
                   MOVE 5.00 TO CRVAL-FINANCE-YTD
               WHEN CRVAL-STATUS-CD = 'DDY'
                   MOVE 7.00 TO CRVAL-FINANCE-YTD
               WHEN OTHER
                   COMPUTE CRVAL-FINANCE-YTD ROUNDED = CRVAL-LIMIT-AMT *
                       CRVAL-DISCOUNT / 100
           END-EVALUATE
           IF CRVAL-FINANCE-YTD > 0 AND CRVAL-FINANCE-YTD < 1.00
               MOVE 1.00 TO CRVAL-FINANCE-YTD
           END-IF
           IF CRVAL-FINANCE-YTD > 100.00
               MOVE 100.00 TO CRVAL-FINANCE-YTD
           END-IF
           ADD CRVAL-FINANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2410-ACCRUE-MIN-DUE.
           IF CRVAL-MIN-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRVAL-MIN-DUE-AMT * CRVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRVAL-FINANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2420-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2430-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRVAL-MIN-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LATE FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2440-LIMIT-LATE-FEE.
           IF CRVAL-LATE-FEE-YTD < 25.00
               MOVE 25.00 TO CRVAL-LATE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRVAL-LATE-FEE-YTD > 2500.00
               MOVE 2500.00 TO CRVAL-LATE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LATE FEE YTD.
      *----------------------------------------------------------------*
       2450-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRVAL-LATE-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRVAL-LATE-FEE-YTD
           END-IF
           ADD CRVAL-LATE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO CRVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CRVAL-PAYMENT-AMT NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO CRVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2470-COUNT-CYCLE.
           IF CRVAL-CYCLE-CNT < 9999999
               ADD 1 TO CRVAL-CYCLE-CNT
           END-IF
           IF CRVAL-CYCLE-CNT NOT < 5
               MOVE 'Y' TO CRVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2480-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRVAL-MIN-DUE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE MIN DUE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2490-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CRVAL-STOP-FLAG = 'Y'
                   MOVE ZERO TO CRVAL-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRVAL-PREFERRED-FLAG = 'Y'
                   COMPUTE CRVAL-MIN-DUE-AMT ROUNDED = CRVAL-MIN-DUE-YTD
                       * CRVAL-DISCOUNT / 200
               WHEN CRVAL-BILL-CYCLE-CD = 'B'
                   MOVE 20.00 TO CRVAL-MIN-DUE-AMT
               WHEN CRVAL-BILL-CYCLE-CD = 'D'
                   MOVE 7.00 TO CRVAL-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRVAL-MIN-DUE-AMT ROUNDED = CRVAL-MIN-DUE-YTD
                       * CRVAL-DISCOUNT / 100
           END-EVALUATE
           IF CRVAL-MIN-DUE-AMT > 0 AND CRVAL-MIN-DUE-AMT < 5.00
               MOVE 5.00 TO CRVAL-MIN-DUE-AMT
           END-IF
           IF CRVAL-MIN-DUE-AMT > 35.00
               MOVE 35.00 TO CRVAL-MIN-DUE-AMT
           END-IF
           ADD CRVAL-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-MIN-DUE.
           IF CRVAL-MIN-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRVAL-MIN-DUE-AMT * CRVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRVAL-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2510-ACCRUE-MIN-DUE.
           IF CRVAL-MIN-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRVAL-MIN-DUE-AMT * CRVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRVAL-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2520-LIMIT-LIMIT.
           IF CRVAL-LIMIT-AMT < 10.00
               MOVE 10.00 TO CRVAL-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRVAL-LIMIT-AMT > 500.00
               MOVE 500.00 TO CRVAL-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2530-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRVAL-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CRVAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN OPEN DATE IS PAST
      * D -> E WHEN OPEN DATE IS PAST
      * E -> F WHEN OPEN DATE IS PAST
      * F -> H WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2540-SET-BILL-CYCLE.
           EVALUATE CRVAL-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRVAL-OPEN-DATE > 0 AND CRVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRVAL-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRVAL-OPEN-DATE > 0 AND CRVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRVAL-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRVAL-OPEN-DATE > 0 AND CRVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRVAL-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRVAL-OPEN-DATE > 0 AND CRVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRVAL-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2550-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-GRID-RATE(1) TO CRVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-CASH-ADV-GRID-LOW(WS-TX)
                   MOVE WS-CASH-ADV-GRID-RATE(WS-TX) TO CRVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FINANCE YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2560-LIMIT-FINANCE.
           IF CRVAL-FINANCE-YTD < 10.00
               MOVE 10.00 TO CRVAL-FINANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRVAL-FINANCE-YTD > 2500.00
               MOVE 2500.00 TO CRVAL-FINANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2570-DESCRIBE-TYPE.
           EVALUATE CRVAL-TYPE-CD
               WHEN 'GP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FINANCE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRVAL-STATUS-CD
                   ADD CRVAL-FINANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRVAL-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRVAL-FINANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN EXPIRY DATE IS PAST
      * LNX -> MGS WHEN EXPIRY DATE IS PAST
      * MGS -> YNJ WHEN EXPIRY DATE IS PAST
      * YNJ -> CTB WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2590-SET-BRANCH.
           EVALUATE CRVAL-BRANCH-CD
               WHEN 'CTB'
                   IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRVAL-EXPIRY-DATE > 0 AND CRVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2600-COUNT-CYCLE.
           IF CRVAL-CYCLE-CNT < 9999999
               ADD 1 TO CRVAL-CYCLE-CNT
           END-IF
           IF CRVAL-CYCLE-CNT NOT < 3
               MOVE 'Y' TO CRVAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E555 DESC REQUIRED
      * E118 LATE FEE YTD NEGATIVE
      * E721 LATE FEE YTD OVER 99999
      * E131 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRVAL-DESC = SPACES
                   MOVE 'E555' TO WS-REJECT-CODE
               WHEN CRVAL-LATE-FEE-YTD < 0
                   MOVE 'E118' TO WS-REJECT-CODE
               WHEN CRVAL-LATE-FEE-YTD > 99999.00
                   MOVE 'E721' TO WS-REJECT-CODE
               WHEN CRVAL-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E131' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2620-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRVAL-TYPE-CD
                   ADD CRVAL-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRVAL-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRVAL-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MIN DUE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2630-SPLIT-MIN-DUE.
           COMPUTE WS-WORK-AMT = CRVAL-MIN-DUE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CRVAL-MIN-DUE-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MIN DUE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2640-FLAG-MIN-DUE.
           IF CRVAL-MIN-DUE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRVAL-MIN-DUE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2650-COUNT-NOTICE.
           IF CRVAL-NOTICE-CNT < 9999999
               ADD 1 TO CRVAL-NOTICE-CNT
           END-IF
           IF CRVAL-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CRVAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E944 CONTACT REQUIRED
      * E851 WORK AMT NEGATIVE
      * E998 WORK AMT OVER 99999
      * E583 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2660-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRVAL-CONTACT = SPACES
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E851' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN CRVAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2670-ACCRUE-MIN-DUE.
           IF CRVAL-MIN-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRVAL-MIN-DUE-YTD * CRVAL-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CRVAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CRVAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CRVAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CRVAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
