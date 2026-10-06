      *================================================================*
      * PROGRAM  : CRDTE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PROPRIETARY CREDIT CARD RULE - TIERING. CALLED BY   *
      *            THE PROPRIETARY CREDIT CARD BATCH AND ONLINE        *
      *            PROGRAMS.                                           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-22 PQR  ORIGINAL                                       *
      * 2000-11-22 TWH  AUDIT FINDING - LATE-FEE CONTROL TOTALS CHG9562*
      * 2000-02-08 DPS  AUDIT FINDING - LATE-FEE CONTROL TOTALS CHG2379*
      * 2001-04-18 GAF  AUDIT FINDING - LATE-FEE CONTROL TOTALS INC6565*
      * 2003-03-21 PQR  NEW LIMIT REPORT COLUMNS CHG3807               *
      * 2005-05-13 GAF  LIMIT EXCEPTIONS TO OPS QUEUE INC9669          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRDTE.
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
                                           VALUE 22.000000.
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
      * WS-PURCHASE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PURCHASE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000791000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000340000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000662000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000395500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000532000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000460500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014600000000179000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014850000000055000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015350000000761500'.
       01  WS-PURCHASE-GRID-TBL REDEFINES WS-PURCHASE-GRID-VALUES.
           05  WS-PURCHASE-GRID-ROW OCCURS 9 TIMES.
               10  WS-PURCHASE-GRID-LOW    PIC 9(09)V99.
               10  WS-PURCHASE-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-ADV-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-ADV-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000401000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000133000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000839500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000782000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000900000000400500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001400000000067000'.
       01  WS-CASH-ADV-BAND-TBL REDEFINES WS-CASH-ADV-BAND-VALUES.
           05  WS-CASH-ADV-BAND-ROW OCCURS 6 TIMES.
               10  WS-CASH-ADV-BAND-LOW    PIC 9(09)V99.
               10  WS-CASH-ADV-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CRDTEP.

       PROCEDURE DIVISION USING CRDTE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-LIMIT
           PERFORM 2010-COUNT-NOTICE
           PERFORM 2020-ACCUM-SOURCE
           PERFORM 2030-ACCUM-TYPE
           PERFORM 2040-PRORATE-LATE-FEE
           PERFORM 2050-EDIT-CUST-ID
           PERFORM 2060-CHECK-LAST-PAY
           PERFORM 2070-SET-TYPE
           PERFORM 2080-PRORATE-FINANCE
           PERFORM 2090-SELECT-PENALTY-PCT
           PERFORM 2100-SET-BRANCH
           PERFORM 2110-SET-CARD-TYPE
           PERFORM 2120-ASSESS-PREFERRED
           PERFORM 2130-AGE-CLOSE
           PERFORM 2140-PRORATE-LIMIT
           PERFORM 2150-LIMIT-PURCHASE
           PERFORM 2160-LIMIT-LIMIT
           PERFORM 2170-SPLIT-LATE-FEE
           PERFORM 2180-CHECK-LAST-PAY
           PERFORM 2190-AGE-LAST-PAY
           PERFORM 2200-LIMIT-ITEM
           PERFORM 2210-DESCRIBE-SOURCE
           PERFORM 2220-FLAG-PURCHASE
           PERFORM 2230-AGE-OPEN
           PERFORM 2240-FLAG-LIMIT
           PERFORM 2250-ASSESS-PREFERRED
           PERFORM 2260-LIMIT-LIMIT
           PERFORM 2270-DESCRIBE-TYPE
           PERFORM 2280-DESCRIBE-BRANCH
           PERFORM 2290-SPLIT-PURCHASE
           PERFORM 2300-DESCRIBE-SOURCE
           PERFORM 2310-ASSESS-EXEMPT
           PERFORM 2320-LIMIT-ITEM
           PERFORM 2330-SET-TYPE
           PERFORM 2340-DESCRIBE-SOURCE
           PERFORM 2350-PRORATE-PURCHASE
           PERFORM 2360-ACCRUE-LATE-FEE
           PERFORM 2370-ACCUM-BRANCH
           PERFORM 2380-CHECK-EXPIRY
           PERFORM 2390-ACCRUE-PENALTY-PCT
           PERFORM 2400-ACCRUE-DISCOUNT
           PERFORM 2410-FLAG-LIMIT
           PERFORM 2420-ACCRUE-FINANCE
           PERFORM 2430-DESCRIBE-CARD-TYPE
           PERFORM 2440-CHECK-EXPIRY
           PERFORM 2450-COUNT-CYCLE
           PERFORM 2460-PRORATE-LIMIT
           PERFORM 2470-ACCRUE-DISCOUNT
           PERFORM 2480-AGE-OPEN
           PERFORM 2490-SET-SOURCE
           PERFORM 2500-ACCRUE-DISCOUNT
           PERFORM 2510-SPLIT-LATE-FEE
           PERFORM 2520-SET-BRANCH
           PERFORM 2530-LIMIT-ITEM
           PERFORM 2540-PRORATE-LIMIT
           PERFORM 2550-SPLIT-LATE-FEE
           PERFORM 2560-AGE-LAST-PAY
           PERFORM 2570-SPLIT-LATE-FEE
           PERFORM 2580-SET-BRANCH
           PERFORM 2590-ASSESS-PREFERRED
           PERFORM 2600-ASSESS-EXEMPT
           PERFORM 2610-SET-BRANCH
           PERFORM 2620-EDIT-NAME
           PERFORM 2630-AGE-LAST-PAY
           PERFORM 2640-PRORATE-LIMIT
           PERFORM 2650-ACCRUE-FINANCE
           PERFORM 2660-DESCRIBE-TYPE
           PERFORM 2670-EDIT-NAME
           PERFORM 2680-DESCRIBE-SOURCE
           PERFORM 2690-CHECK-CLOSE
           PERFORM 2700-SPLIT-LIMIT
           PERFORM 2710-FLAG-PURCHASE
           PERFORM 2720-COUNT-NOTICE
           PERFORM 2730-FLAG-PURCHASE
           PERFORM 2740-EDIT-NAME
           PERFORM 2750-ACCRUE-PENALTY-PCT
           PERFORM 2760-PRORATE-PURCHASE
           PERFORM 2770-SPLIT-LIMIT
           PERFORM 2780-ACCRUE-PENALTY-PCT
           PERFORM 2790-DESCRIBE-BRANCH
           PERFORM 2800-SELECT-PENALTY-PCT
           PERFORM 2810-COUNT-NOTICE
           PERFORM 2820-PRORATE-LIMIT
           PERFORM 2830-SELECT-DISCOUNT
           PERFORM 2840-CHECK-LAST-PAY
           PERFORM 2850-COUNT-NOTICE
           PERFORM 2860-EDIT-NAME
           PERFORM 2870-COUNT-CYCLE
           PERFORM 2880-EDIT-NAME
           PERFORM 2890-SET-SOURCE
           PERFORM 2900-EDIT-NAME
           PERFORM 2910-LIMIT-PURCHASE
           PERFORM 2920-ASSESS-STOP
           PERFORM 2930-ACCUM-CARD-TYPE
           PERFORM 2940-SPLIT-PURCHASE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CRDTE-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-LIMIT.
           IF CRDTE-LIMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRDTE-LIMIT-YTD * CRDTE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRDTE-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-NOTICE.
           IF CRDTE-NOTICE-CNT < 9999999
               ADD 1 TO CRDTE-NOTICE-CNT
           END-IF
           IF CRDTE-NOTICE-CNT NOT < 3
               MOVE 'Y' TO CRDTE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2020-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRDTE-SOURCE-CD
                   ADD CRDTE-LIMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRDTE-LIMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2030-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRDTE-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRDTE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2040-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRDTE-LATE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E323 CUST ID REQUIRED
      * E877 FINANCE AMT NEGATIVE
      * E928 FINANCE AMT OVER 99999
      * E721 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRDTE-CUST-ID = SPACES
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN CRDTE-FINANCE-AMT < 0
                   MOVE 'E877' TO WS-REJECT-CODE
               WHEN CRDTE-FINANCE-AMT > 99999.00
                   MOVE 'E928' TO WS-REJECT-CODE
               WHEN CRDTE-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E721' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2060-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CRDTE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN LAST PAY DATE IS PAST
      * UV -> YK WHEN LAST PAY DATE IS PAST
      * YK -> GP WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2070-SET-TYPE.
           EVALUATE CRDTE-TYPE-CD
               WHEN 'GP'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FINANCE AMT.
      *----------------------------------------------------------------*
       2080-PRORATE-FINANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRDTE-FINANCE-AMT ROUNDED =
                   CRDTE-PURCHASE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRDTE-FINANCE-AMT
           END-IF
           ADD CRDTE-FINANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-GRID-RATE(1) TO CRDTE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-PURCHASE-GRID-LOW(WS-TX)
                   MOVE WS-PURCHASE-GRID-RATE(WS-TX) TO
                       CRDTE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRDTE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRDTE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN CLOSE DATE IS PAST
      * LNX -> MGS WHEN CLOSE DATE IS PAST
      * MGS -> YNJ WHEN CLOSE DATE IS PAST
      * YNJ -> CTB WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2100-SET-BRANCH.
           EVALUATE CRDTE-BRANCH-CD
               WHEN 'CTB'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARD TYPE CD:
      * PGU -> TRN WHEN LAST PAY DATE IS PAST
      * TRN -> WHR WHEN LAST PAY DATE IS PAST
      * WHR -> ZWB WHEN LAST PAY DATE IS PAST
      * ZWB -> PGU WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2110-SET-CARD-TYPE.
           EVALUATE CRDTE-CARD-TYPE-CD
               WHEN 'PGU'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TRN' TO CRDTE-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TRN'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WHR' TO CRDTE-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WHR'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWB' TO CRDTE-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZWB'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PGU' TO CRDTE-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE LIMIT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2120-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRDTE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRDTE-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRDTE-EXEMPT-FLAG = 'Y'
                   COMPUTE CRDTE-PURCHASE-AMT ROUNDED = CRDTE-LIMIT-YTD
                       * CRDTE-PENALTY-PCT / 200
               WHEN CRDTE-SOURCE-CD = 'C'
                   MOVE 7.00 TO CRDTE-PURCHASE-AMT
               WHEN CRDTE-SOURCE-CD = 'H'
                   MOVE 10.00 TO CRDTE-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE CRDTE-PURCHASE-AMT ROUNDED = CRDTE-LIMIT-YTD
                       * CRDTE-PENALTY-PCT / 100
           END-EVALUATE
           IF CRDTE-PURCHASE-AMT > 0 AND CRDTE-PURCHASE-AMT < 1.00
               MOVE 1.00 TO CRDTE-PURCHASE-AMT
           END-IF
           IF CRDTE-PURCHASE-AMT > 25.00
               MOVE 25.00 TO CRDTE-PURCHASE-AMT
           END-IF
           ADD CRDTE-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2130-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRDTE-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRDTE-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CRDTE-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRDTE-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRDTE-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       2140-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRDTE-LIMIT-AMT ROUNDED =
                   CRDTE-PURCHASE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRDTE-LIMIT-AMT
           END-IF
           ADD CRDTE-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2150-LIMIT-PURCHASE.
           IF CRDTE-PURCHASE-AMT < 50.00
               MOVE 50.00 TO CRDTE-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRDTE-PURCHASE-AMT > 10000.00
               MOVE 10000.00 TO CRDTE-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2160-LIMIT-LIMIT.
           IF CRDTE-LIMIT-AMT < 50.00
               MOVE 50.00 TO CRDTE-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRDTE-LIMIT-AMT > 2500.00
               MOVE 2500.00 TO CRDTE-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2170-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRDTE-LATE-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CRDTE-LATE-FEE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE CRDTE-PURCHASE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CRDTE-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2190-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRDTE-LIMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRDTE-LIMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRDTE-LIMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRDTE-LIMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRDTE-LIMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2200-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2210-DESCRIBE-SOURCE.
           EVALUATE CRDTE-SOURCE-CD
               WHEN 'C'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PURCHASE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2220-FLAG-PURCHASE.
           IF CRDTE-PURCHASE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRDTE-PURCHASE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2230-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-OPEN-DATE > 0 AND CRDTE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRDTE-FINANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CRDTE-FINANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRDTE-FINANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRDTE-FINANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRDTE-FINANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LIMIT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2240-FLAG-LIMIT.
           IF CRDTE-LIMIT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRDTE-LIMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2250-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRDTE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRDTE-LIMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRDTE-STOP-FLAG = 'Y'
                   COMPUTE CRDTE-LIMIT-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRDTE-PENALTY-PCT / 200
               WHEN CRDTE-CARD-TYPE-CD = 'PGU'
                   MOVE 10.00 TO CRDTE-LIMIT-AMT
               WHEN CRDTE-CARD-TYPE-CD = 'TRN'
                   MOVE 10.00 TO CRDTE-LIMIT-AMT
               WHEN OTHER
                   COMPUTE CRDTE-LIMIT-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRDTE-PENALTY-PCT / 100
           END-EVALUATE
           IF CRDTE-LIMIT-AMT > 0 AND CRDTE-LIMIT-AMT < 10.00
               MOVE 10.00 TO CRDTE-LIMIT-AMT
           END-IF
           IF CRDTE-LIMIT-AMT > 50.00
               MOVE 50.00 TO CRDTE-LIMIT-AMT
           END-IF
           ADD CRDTE-LIMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2260-LIMIT-LIMIT.
           IF CRDTE-LIMIT-AMT < 0.00
               MOVE 0.00 TO CRDTE-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRDTE-LIMIT-AMT > 50000.00
               MOVE 50000.00 TO CRDTE-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2270-DESCRIBE-TYPE.
           EVALUATE CRDTE-TYPE-CD
               WHEN 'GP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2280-DESCRIBE-BRANCH.
           EVALUATE CRDTE-BRANCH-CD
               WHEN 'CTB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LNX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MGS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YNJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2290-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE CRDTE-PURCHASE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-SOURCE.
           EVALUATE CRDTE-SOURCE-CD
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT FEE ON THE PURCHASE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2310-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRDTE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRDTE-LIMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRDTE-PREFERRED-FLAG = 'Y'
                   COMPUTE CRDTE-LIMIT-AMT ROUNDED = CRDTE-PURCHASE-YTD
                       * CRDTE-DISCOUNT / 200
               WHEN CRDTE-BRANCH-CD = 'CTB'
                   MOVE 7.00 TO CRDTE-LIMIT-AMT
               WHEN CRDTE-BRANCH-CD = 'LNX'
                   MOVE 5.00 TO CRDTE-LIMIT-AMT
               WHEN OTHER
                   COMPUTE CRDTE-LIMIT-AMT ROUNDED = CRDTE-PURCHASE-YTD
                       * CRDTE-DISCOUNT / 100
           END-EVALUATE
           IF CRDTE-LIMIT-AMT > 0 AND CRDTE-LIMIT-AMT < 2.00
               MOVE 2.00 TO CRDTE-LIMIT-AMT
           END-IF
           IF CRDTE-LIMIT-AMT > 75.00
               MOVE 75.00 TO CRDTE-LIMIT-AMT
           END-IF
           ADD CRDTE-LIMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2320-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN LAST PAY DATE IS PAST
      * UV -> YK WHEN LAST PAY DATE IS PAST
      * YK -> GP WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2330-SET-TYPE.
           EVALUATE CRDTE-TYPE-CD
               WHEN 'GP'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2340-DESCRIBE-SOURCE.
           EVALUATE CRDTE-SOURCE-CD
               WHEN 'C'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PURCHASE AMT.
      *----------------------------------------------------------------*
       2350-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRDTE-PURCHASE-AMT ROUNDED =
                   CRDTE-LIMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRDTE-PURCHASE-AMT
           END-IF
           ADD CRDTE-PURCHASE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2360-ACCRUE-LATE-FEE.
           IF CRDTE-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRDTE-LATE-FEE-YTD * CRDTE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2370-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRDTE-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRDTE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2380-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CRDTE-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2390-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CRDTE-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2400-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CRDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRDTE-LIMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2410-FLAG-LIMIT.
           IF CRDTE-LIMIT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRDTE-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2420-ACCRUE-FINANCE.
           IF CRDTE-FINANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRDTE-FINANCE-AMT * CRDTE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2430-DESCRIBE-CARD-TYPE.
           EVALUATE CRDTE-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CRDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2450-COUNT-CYCLE.
           IF CRDTE-CYCLE-CNT < 9999999
               ADD 1 TO CRDTE-CYCLE-CNT
           END-IF
           IF CRDTE-CYCLE-CNT NOT < 3
               MOVE 'Y' TO CRDTE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2460-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRDTE-LIMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2470-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CRDTE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRDTE-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2480-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-OPEN-DATE > 0 AND CRDTE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRDTE-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRDTE-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CRDTE-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD CRDTE-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRDTE-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN EXPIRY DATE IS PAST
      * H -> M WHEN EXPIRY DATE IS PAST
      * M -> R WHEN EXPIRY DATE IS PAST
      * R -> S WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-SOURCE.
           EVALUATE CRDTE-SOURCE-CD
               WHEN 'C'
                   IF CRDTE-EXPIRY-DATE > 0 AND CRDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF CRDTE-EXPIRY-DATE > 0 AND CRDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CRDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CRDTE-EXPIRY-DATE > 0 AND CRDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO CRDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF CRDTE-EXPIRY-DATE > 0 AND CRDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CRDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CRDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRDTE-LIMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRDTE-LATE-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CRDTE-LATE-FEE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CRDTE-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN EXPIRY DATE IS PAST
      * LNX -> MGS WHEN EXPIRY DATE IS PAST
      * MGS -> YNJ WHEN EXPIRY DATE IS PAST
      * YNJ -> CTB WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2520-SET-BRANCH.
           EVALUATE CRDTE-BRANCH-CD
               WHEN 'CTB'
                   IF CRDTE-EXPIRY-DATE > 0 AND CRDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRDTE-EXPIRY-DATE > 0 AND CRDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRDTE-EXPIRY-DATE > 0 AND CRDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRDTE-EXPIRY-DATE > 0 AND CRDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2530-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT YTD.
      *----------------------------------------------------------------*
       2540-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRDTE-LIMIT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRDTE-LIMIT-YTD
           END-IF
           ADD CRDTE-LIMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE CRDTE-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2560-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRDTE-FINANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRDTE-FINANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRDTE-FINANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRDTE-FINANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRDTE-FINANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2570-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE CRDTE-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN CLOSE DATE IS PAST
      * LNX -> MGS WHEN CLOSE DATE IS PAST
      * MGS -> YNJ WHEN CLOSE DATE IS PAST
      * YNJ -> CTB WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2580-SET-BRANCH.
           EVALUATE CRDTE-BRANCH-CD
               WHEN 'CTB'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE LIMIT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2590-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRDTE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRDTE-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRDTE-EXEMPT-FLAG = 'Y'
                   COMPUTE CRDTE-PURCHASE-AMT ROUNDED = CRDTE-LIMIT-YTD
                       * CRDTE-PENALTY-PCT / 200
               WHEN CRDTE-BRANCH-CD = 'CTB'
                   MOVE 20.00 TO CRDTE-PURCHASE-AMT
               WHEN CRDTE-BRANCH-CD = 'LNX'
                   MOVE 10.00 TO CRDTE-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE CRDTE-PURCHASE-AMT ROUNDED = CRDTE-LIMIT-YTD
                       * CRDTE-PENALTY-PCT / 100
           END-EVALUATE
           IF CRDTE-PURCHASE-AMT > 0 AND CRDTE-PURCHASE-AMT < 10.00
               MOVE 10.00 TO CRDTE-PURCHASE-AMT
           END-IF
           IF CRDTE-PURCHASE-AMT > 100.00
               MOVE 100.00 TO CRDTE-PURCHASE-AMT
           END-IF
           ADD CRDTE-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT YTD FEE ON THE PURCHASE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2600-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRDTE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRDTE-LIMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRDTE-PREFERRED-FLAG = 'Y'
                   COMPUTE CRDTE-LIMIT-YTD ROUNDED = CRDTE-PURCHASE-YTD
                       * CRDTE-DISCOUNT / 200
               WHEN CRDTE-CARD-TYPE-CD = 'PGU'
                   MOVE 5.00 TO CRDTE-LIMIT-YTD
               WHEN CRDTE-CARD-TYPE-CD = 'TRN'
                   MOVE 12.00 TO CRDTE-LIMIT-YTD
               WHEN OTHER
                   COMPUTE CRDTE-LIMIT-YTD ROUNDED = CRDTE-PURCHASE-YTD
                       * CRDTE-DISCOUNT / 100
           END-EVALUATE
           IF CRDTE-LIMIT-YTD > 0 AND CRDTE-LIMIT-YTD < 2.00
               MOVE 2.00 TO CRDTE-LIMIT-YTD
           END-IF
           IF CRDTE-LIMIT-YTD > 35.00
               MOVE 35.00 TO CRDTE-LIMIT-YTD
           END-IF
           ADD CRDTE-LIMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN CLOSE DATE IS PAST
      * LNX -> MGS WHEN CLOSE DATE IS PAST
      * MGS -> YNJ WHEN CLOSE DATE IS PAST
      * YNJ -> CTB WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2610-SET-BRANCH.
           EVALUATE CRDTE-BRANCH-CD
               WHEN 'CTB'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRDTE-CLOSE-DATE > 0 AND CRDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E669 NAME REQUIRED
      * E442 WORK AMT NEGATIVE
      * E757 WORK AMT OVER 1000000
      * E747 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2620-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRDTE-NAME = SPACES
                   MOVE 'E669' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN CRDTE-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E747' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2630-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRDTE-LIMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRDTE-LIMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRDTE-LIMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRDTE-LIMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRDTE-LIMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       2640-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRDTE-LIMIT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRDTE-LIMIT-AMT
           END-IF
           ADD CRDTE-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2650-ACCRUE-FINANCE.
           IF CRDTE-FINANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRDTE-FINANCE-AMT * CRDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRDTE-LIMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2660-DESCRIBE-TYPE.
           EVALUATE CRDTE-TYPE-CD
               WHEN 'GP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E670 NAME REQUIRED
      * E714 PURCHASE AMT NEGATIVE
      * E416 PURCHASE AMT OVER 250000
      * E631 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2670-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRDTE-NAME = SPACES
                   MOVE 'E670' TO WS-REJECT-CODE
               WHEN CRDTE-PURCHASE-AMT < 0
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN CRDTE-PURCHASE-AMT > 250000.00
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN CRDTE-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2680-DESCRIBE-SOURCE.
           EVALUATE CRDTE-SOURCE-CD
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2690-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CRDTE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2700-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRDTE-LIMIT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CRDTE-LIMIT-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2710-FLAG-PURCHASE.
           IF CRDTE-PURCHASE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRDTE-PURCHASE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2720-COUNT-NOTICE.
           IF CRDTE-NOTICE-CNT < 9999999
               ADD 1 TO CRDTE-NOTICE-CNT
           END-IF
           IF CRDTE-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CRDTE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2730-FLAG-PURCHASE.
           IF CRDTE-PURCHASE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRDTE-PURCHASE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E702 NAME REQUIRED
      * E546 PURCHASE YTD NEGATIVE
      * E056 PURCHASE YTD OVER 25000
      * E190 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2740-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRDTE-NAME = SPACES
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN CRDTE-PURCHASE-YTD < 0
                   MOVE 'E546' TO WS-REJECT-CODE
               WHEN CRDTE-PURCHASE-YTD > 25000.00
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN CRDTE-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E190' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2750-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CRDTE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRDTE-PURCHASE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2760-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRDTE-PURCHASE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT LIMIT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2770-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRDTE-LIMIT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRDTE-LIMIT-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2780-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CRDTE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2790-DESCRIBE-BRANCH.
           EVALUATE CRDTE-BRANCH-CD
               WHEN 'CTB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LNX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MGS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YNJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2800-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-BAND-RATE(1) TO CRDTE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-CASH-ADV-BAND-LOW(WS-TX)
                   MOVE WS-CASH-ADV-BAND-RATE(WS-TX) TO
                       CRDTE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRDTE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRDTE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2810-COUNT-NOTICE.
           IF CRDTE-NOTICE-CNT < 9999999
               ADD 1 TO CRDTE-NOTICE-CNT
           END-IF
           IF CRDTE-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CRDTE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2820-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRDTE-LIMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE GRID TIER FOR THE LIMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2830-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-GRID-RATE(1) TO CRDTE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CRDTE-LIMIT-YTD NOT < WS-PURCHASE-GRID-LOW(WS-TX)
                   MOVE WS-PURCHASE-GRID-RATE(WS-TX) TO CRDTE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRDTE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRDTE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2840-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRDTE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CRDTE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2850-COUNT-NOTICE.
           IF CRDTE-NOTICE-CNT < 9999999
               ADD 1 TO CRDTE-NOTICE-CNT
           END-IF
           IF CRDTE-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CRDTE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E863 NAME REQUIRED
      * E959 LIMIT AMT NEGATIVE
      * E429 LIMIT AMT OVER 50000
      * E060 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2860-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRDTE-NAME = SPACES
                   MOVE 'E863' TO WS-REJECT-CODE
               WHEN CRDTE-LIMIT-AMT < 0
                   MOVE 'E959' TO WS-REJECT-CODE
               WHEN CRDTE-LIMIT-AMT > 50000.00
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN CRDTE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2870-COUNT-CYCLE.
           IF CRDTE-CYCLE-CNT < 9999999
               ADD 1 TO CRDTE-CYCLE-CNT
           END-IF
           IF CRDTE-CYCLE-CNT NOT < 10
               MOVE 'Y' TO CRDTE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E622 NAME REQUIRED
      * E600 FINANCE AMT NEGATIVE
      * E300 FINANCE AMT OVER 25000
      * E348 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2880-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRDTE-NAME = SPACES
                   MOVE 'E622' TO WS-REJECT-CODE
               WHEN CRDTE-FINANCE-AMT < 0
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN CRDTE-FINANCE-AMT > 25000.00
                   MOVE 'E300' TO WS-REJECT-CODE
               WHEN CRDTE-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E348' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN LAST PAY DATE IS PAST
      * H -> M WHEN LAST PAY DATE IS PAST
      * M -> R WHEN LAST PAY DATE IS PAST
      * R -> S WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2890-SET-SOURCE.
           EVALUATE CRDTE-SOURCE-CD
               WHEN 'C'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CRDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO CRDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF CRDTE-LAST-PAY-DATE > 0 AND CRDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CRDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E952 NAME REQUIRED
      * E944 LIMIT YTD NEGATIVE
      * E036 LIMIT YTD OVER 250000
      * E866 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2900-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRDTE-NAME = SPACES
                   MOVE 'E952' TO WS-REJECT-CODE
               WHEN CRDTE-LIMIT-YTD < 0
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN CRDTE-LIMIT-YTD > 250000.00
                   MOVE 'E036' TO WS-REJECT-CODE
               WHEN CRDTE-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E866' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2910-LIMIT-PURCHASE.
           IF CRDTE-PURCHASE-AMT < 50.00
               MOVE 50.00 TO CRDTE-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRDTE-PURCHASE-AMT > 5000.00
               MOVE 5000.00 TO CRDTE-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2920-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CRDTE-STOP-FLAG = 'Y'
                   MOVE ZERO TO CRDTE-LIMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRDTE-EXEMPT-FLAG = 'Y'
                   COMPUTE CRDTE-LIMIT-YTD ROUNDED = WS-WORK-AMT-2 *
                       CRDTE-PENALTY-PCT / 200
               WHEN CRDTE-SOURCE-CD = 'C'
                   MOVE 10.00 TO CRDTE-LIMIT-YTD
               WHEN CRDTE-SOURCE-CD = 'H'
                   MOVE 20.00 TO CRDTE-LIMIT-YTD
               WHEN OTHER
                   COMPUTE CRDTE-LIMIT-YTD ROUNDED = WS-WORK-AMT-2 *
                       CRDTE-PENALTY-PCT / 100
           END-EVALUATE
           IF CRDTE-LIMIT-YTD > 0 AND CRDTE-LIMIT-YTD < 5.00
               MOVE 5.00 TO CRDTE-LIMIT-YTD
           END-IF
           IF CRDTE-LIMIT-YTD > 150.00
               MOVE 150.00 TO CRDTE-LIMIT-YTD
           END-IF
           ADD CRDTE-LIMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE AMT BY CARD TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2930-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRDTE-CARD-TYPE-CD
                   ADD CRDTE-PURCHASE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRDTE-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRDTE-PURCHASE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2940-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = CRDTE-PURCHASE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRDTE-PURCHASE-AMT - (WS-WORK-AMT *
               2)
           COMPUTE CRDTE-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CRDTE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CRDTE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CRDTE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CRDTE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
