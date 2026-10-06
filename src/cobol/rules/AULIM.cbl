      *================================================================*
      * PROGRAM  : AULIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DEALER AUTO FINANCE RULE - LIMITS. CALLED BY THE    *
      *            DEALER AUTO FINANCE BATCH AND ONLINE PROGRAMS.      *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-09-21 EWS  ORIGINAL                                       *
      * 1997-07-21 LMB  AUDIT FINDING - GAP CONTROL TOTALS CHG1712     *
      * 2001-03-20 SKN  GAP ROUNDING - HALF UP PRB0879                 *
      * 2003-07-01 TWH  NEW REBATE REPORT COLUMNS INC0967              *
      * 2003-06-12 GAF  REBATE TABLE EXPANDED INC1553                  *
      * 2005-06-15 LMB  REBATE NOTICE TEXT PER LEGAL INC2472           *
      * 2006-09-23 LMB  AUDIT FINDING - AMOUNT-FIN CONTROL TOTALS CHG92*
      * 2007-03-23 MKL  PAYMENT FEE SCHEDULE UPDATE REQ6580            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULIM.
       AUTHOR. B H SANDERS.

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
                                           VALUE 15.000000.
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
      * WS-PAYMENT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000008000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000862500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000027500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000071000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000871000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000249000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000300000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000183000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016050000000310000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016550000000626000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017050000000376500'.
       01  WS-PAYMENT-BAND-TBL REDEFINES WS-PAYMENT-BAND-VALUES.
           05  WS-PAYMENT-BAND-ROW OCCURS 11 TIMES.
               10  WS-PAYMENT-BAND-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-BAND-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY AULIMP.

       PROCEDURE DIVISION USING AULIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-EDIT-NAME
           PERFORM 2010-COUNT-PAYMENT
           PERFORM 2020-FLAG-ITEM
           PERFORM 2030-LIMIT-ITEM
           PERFORM 2040-COUNT-PAYMENT
           PERFORM 2050-ASSESS-STOP
           PERFORM 2060-SELECT-FEE-PCT
           PERFORM 2070-EDIT-ADDR
           PERFORM 2080-ACCUM-DEALER
           PERFORM 2090-COUNT-CYCLE
           PERFORM 2100-SPLIT-REBATE
           PERFORM 2110-SELECT-MARGIN
           PERFORM 2120-SELECT-MARGIN
           PERFORM 2130-PRORATE-CHARGEBK
           PERFORM 2140-SET-TIER
           PERFORM 2150-CHECK-LAST-STMT
           PERFORM 2160-SPLIT-CHARGEBK
           PERFORM 2170-SPLIT-REBATE
           PERFORM 2180-SELECT-FEE-PCT
           PERFORM 2190-CHECK-LAST-STMT
           PERFORM 2200-LIMIT-REBATE
           PERFORM 2210-LIMIT-REBATE
           PERFORM 2220-PRORATE-REBATE
           PERFORM 2230-LIMIT-REBATE
           PERFORM 2240-SELECT-MARGIN
           PERFORM 2250-SET-TIER
           PERFORM 2260-DESCRIBE-STATUS
           PERFORM 2270-AGE-LAST-STMT
           PERFORM 2280-ACCRUE-MARGIN
           PERFORM 2290-SELECT-MARGIN
           PERFORM 2300-FLAG-AMOUNT-FIN
           PERFORM 2310-SET-TYPE
           PERFORM 2320-LIMIT-PAYMENT
           PERFORM 2330-CHECK-NEXT-DUE
           PERFORM 2340-SPLIT-CHARGEBK
           PERFORM 2350-SET-TYPE
           PERFORM 2360-SELECT-MARGIN
           PERFORM 2370-AGE-NEXT-DUE
           PERFORM 2380-LIMIT-CHARGEBK
           PERFORM 2390-FLAG-REBATE
           PERFORM 2400-CHECK-LAST-STMT
           PERFORM 2410-EDIT-ADDR
           PERFORM 2420-ASSESS-OVERRIDE
           PERFORM 2430-COUNT-PAYMENT
           PERFORM 2440-SET-TYPE
           PERFORM 2450-AGE-LAST-STMT
           PERFORM 2460-ACCUM-TIER
           PERFORM 2470-ACCUM-STATUS
           PERFORM 2480-PRORATE-REBATE
           PERFORM 2490-SET-TIER
           PERFORM 2500-SPLIT-AMOUNT-FIN
           PERFORM 2510-AGE-LAST-STMT
           PERFORM 2520-ACCRUE-FEE-PCT
           PERFORM 2530-ACCRUE-FEE-PCT
           PERFORM 2540-DESCRIBE-TIER
           PERFORM 2550-AGE-LAST-MAINT
           PERFORM 2560-SET-DEALER
           PERFORM 2570-COUNT-CYCLE
           PERFORM 2580-ACCRUE-CHARGEBK
           PERFORM 2590-SELECT-FEE-PCT
           PERFORM 2600-COUNT-PAYMENT
           PERFORM 2610-CHECK-LAST-MAINT
           PERFORM 2620-SELECT-FEE-PCT
           PERFORM 2630-SPLIT-CHARGEBK
           PERFORM 2640-FLAG-CHARGEBK
           PERFORM 2650-ACCUM-TYPE
           PERFORM 2660-LIMIT-CHARGEBK
           PERFORM 2670-SELECT-MARGIN
           PERFORM 2680-DESCRIBE-STATUS
           PERFORM 2690-COUNT-CYCLE
           PERFORM 2700-EDIT-NAME
           PERFORM 2710-SET-DEALER
           PERFORM 2720-LIMIT-CHARGEBK
           PERFORM 2730-AGE-LAST-STMT
           PERFORM 2740-COUNT-CYCLE
           PERFORM 2750-SET-STATUS
           PERFORM 2760-EDIT-ADDR
           PERFORM 2770-LIMIT-CHARGEBK
           PERFORM 2780-DESCRIBE-TYPE
           PERFORM 2790-SET-STATUS
           PERFORM 2800-AGE-LAST-MAINT
           PERFORM 2810-ACCRUE-CHARGEBK
           PERFORM 2820-LIMIT-ITEM
           PERFORM 2830-LIMIT-CHARGEBK
           PERFORM 2840-PRORATE-CHARGEBK
           PERFORM 2850-COUNT-PAYMENT
           PERFORM 2860-CHECK-LAST-PAY
           PERFORM 2870-CHECK-NEXT-DUE
           PERFORM 2880-SELECT-FEE-PCT
           PERFORM 2890-COUNT-PAYMENT
           PERFORM 2900-ASSESS-OVERRIDE
           PERFORM 2910-DESCRIBE-DEALER
           PERFORM 2920-SET-DEALER
           PERFORM 2930-CHECK-NEXT-DUE
           PERFORM 2940-CHECK-LAST-STMT
           PERFORM 2950-SPLIT-PAYMENT
           PERFORM 2960-CHECK-LAST-MAINT
           PERFORM 2970-EDIT-NAME
           PERFORM 2980-DESCRIBE-TYPE
           PERFORM 2990-LIMIT-ITEM
           PERFORM 3000-SELECT-MARGIN
           PERFORM 3010-AGE-LAST-MAINT
           PERFORM 3020-DESCRIBE-TYPE
           PERFORM 3030-SPLIT-AMOUNT-FIN
           PERFORM 3040-LIMIT-AMOUNT-FIN
           PERFORM 3050-SELECT-FEE-PCT
           PERFORM 3060-ACCRUE-FEE-PCT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO AULIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E978 NAME REQUIRED
      * E048 WORK AMT 2 NEGATIVE
      * E658 WORK AMT 2 OVER 1000000
      * E168 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2000-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AULIM-NAME = SPACES
                   MOVE 'E978' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E658' TO WS-REJECT-CODE
               WHEN AULIM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-PAYMENT.
           IF AULIM-PAYMENT-CNT < 9999999
               ADD 1 TO AULIM-PAYMENT-CNT
           END-IF
           IF AULIM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO AULIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2020-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2030-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-PAYMENT.
           IF AULIM-PAYMENT-CNT < 9999999
               ADD 1 TO AULIM-PAYMENT-CNT
           END-IF
           IF AULIM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO AULIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGEBK YTD FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2050-ASSESS-STOP.
           EVALUATE TRUE
               WHEN AULIM-STOP-FLAG = 'Y'
                   MOVE ZERO TO AULIM-CHARGEBK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AULIM-EXEMPT-FLAG = 'Y'
                   COMPUTE AULIM-CHARGEBK-YTD ROUNDED = WS-WORK-AMT *
                       AULIM-FEE-PCT / 200
               WHEN AULIM-TIER-CD = 'EW'
                   MOVE 15.00 TO AULIM-CHARGEBK-YTD
               WHEN AULIM-TIER-CD = 'JF'
                   MOVE 10.00 TO AULIM-CHARGEBK-YTD
               WHEN OTHER
                   COMPUTE AULIM-CHARGEBK-YTD ROUNDED = WS-WORK-AMT *
                       AULIM-FEE-PCT / 100
           END-EVALUATE
           IF AULIM-CHARGEBK-YTD > 0 AND AULIM-CHARGEBK-YTD < 2.00
               MOVE 2.00 TO AULIM-CHARGEBK-YTD
           END-IF
           IF AULIM-CHARGEBK-YTD > 250.00
               MOVE 250.00 TO AULIM-CHARGEBK-YTD
           END-IF
           ADD AULIM-CHARGEBK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE AMOUNT FIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2060-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AULIM-AMOUNT-FIN-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E957 ADDR REQUIRED
      * E885 AMOUNT FIN YTD NEGATIVE
      * E288 AMOUNT FIN YTD OVER 99999
      * E514 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2070-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AULIM-ADDR = SPACES
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN AULIM-AMOUNT-FIN-YTD < 0
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN AULIM-AMOUNT-FIN-YTD > 99999.00
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN AULIM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E514' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REBATE YTD BY DEALER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2080-ACCUM-DEALER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AULIM-DEALER-CD
                   ADD AULIM-REBATE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AULIM-DEALER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AULIM-REBATE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2090-COUNT-CYCLE.
           IF AULIM-CYCLE-CNT < 9999999
               ADD 1 TO AULIM-CYCLE-CNT
           END-IF
           IF AULIM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO AULIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REBATE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2100-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = AULIM-REBATE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = AULIM-REBATE-AMT - (WS-WORK-AMT * 3)
           COMPUTE AULIM-REBATE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2110-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE CHARGEBK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2120-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AULIM-CHARGEBK-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGEBK YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2130-PRORATE-CHARGEBK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   AULIM-CHARGEBK-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN LAST STMT DATE IS PAST
      * JF -> KD WHEN LAST STMT DATE IS PAST
      * KD -> UG WHEN LAST STMT DATE IS PAST
      * UG -> WY WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2140-SET-TIER.
           EVALUATE AULIM-TIER-CD
               WHEN 'EW'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JF' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KD' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UG' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WY' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2150-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO AULIM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGEBK YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-CHARGEBK.
           COMPUTE WS-WORK-AMT = AULIM-CHARGEBK-YTD / 12
           COMPUTE WS-WORK-AMT-2 = AULIM-CHARGEBK-YTD - (WS-WORK-AMT *
               12)
           COMPUTE AULIM-AMOUNT-FIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REBATE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2170-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = AULIM-REBATE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = AULIM-REBATE-AMT - (WS-WORK-AMT * 2)
           COMPUTE AULIM-AMOUNT-FIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE REBATE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2180-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AULIM-REBATE-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2190-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO AULIM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REBATE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2200-LIMIT-REBATE.
           IF AULIM-REBATE-YTD < 0.00
               MOVE 0.00 TO AULIM-REBATE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AULIM-REBATE-YTD > 50000.00
               MOVE 50000.00 TO AULIM-REBATE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REBATE YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2210-LIMIT-REBATE.
           IF AULIM-REBATE-YTD < 100.00
               MOVE 100.00 TO AULIM-REBATE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AULIM-REBATE-YTD > 10000.00
               MOVE 10000.00 TO AULIM-REBATE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REBATE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2220-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AULIM-REBATE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * REBATE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2230-LIMIT-REBATE.
           IF AULIM-REBATE-AMT < 100.00
               MOVE 100.00 TO AULIM-REBATE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AULIM-REBATE-AMT > 500.00
               MOVE 500.00 TO AULIM-REBATE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE CHARGEBK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2240-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AULIM-CHARGEBK-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN LAST MAINT DATE IS PAST
      * JF -> KD WHEN LAST MAINT DATE IS PAST
      * KD -> UG WHEN LAST MAINT DATE IS PAST
      * UG -> WY WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2250-SET-TIER.
           EVALUATE AULIM-TIER-CD
               WHEN 'EW'
                   IF AULIM-LAST-MAINT-DATE > 0 AND
                       AULIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JF' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF AULIM-LAST-MAINT-DATE > 0 AND
                       AULIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KD' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF AULIM-LAST-MAINT-DATE > 0 AND
                       AULIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UG' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF AULIM-LAST-MAINT-DATE > 0 AND
                       AULIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'WY' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2260-DESCRIBE-STATUS.
           EVALUATE AULIM-STATUS-CD
               WHEN 'EUA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HMW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RCB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AULIM-REBATE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD AULIM-REBATE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD AULIM-REBATE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD AULIM-REBATE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AULIM-REBATE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * AULIM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2290-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AULIM-REBATE-AMT NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2300-FLAG-AMOUNT-FIN.
           IF AULIM-AMOUNT-FIN-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE AULIM-AMOUNT-FIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN LAST PAY DATE IS PAST
      * H -> S WHEN LAST PAY DATE IS PAST
      * S -> V WHEN LAST PAY DATE IS PAST
      * V -> W WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-TYPE.
           EVALUATE AULIM-TYPE-CD
               WHEN 'B'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2320-LIMIT-PAYMENT.
           IF AULIM-PAYMENT-AMT < 0.00
               MOVE 0.00 TO AULIM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AULIM-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO AULIM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2330-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF AULIM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO AULIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2340-SPLIT-CHARGEBK.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE AULIM-CHARGEBK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN LAST PAY DATE IS PAST
      * H -> S WHEN LAST PAY DATE IS PAST
      * S -> V WHEN LAST PAY DATE IS PAST
      * V -> W WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2350-SET-TYPE.
           EVALUATE AULIM-TYPE-CD
               WHEN 'B'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE CHARGEBK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2360-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AULIM-CHARGEBK-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2370-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF AULIM-NEXT-DUE-DATE > 0 AND AULIM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2380-LIMIT-CHARGEBK.
           IF AULIM-CHARGEBK-AMT < 10.00
               MOVE 10.00 TO AULIM-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AULIM-CHARGEBK-AMT > 5000.00
               MOVE 5000.00 TO AULIM-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REBATE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2390-FLAG-REBATE.
           IF AULIM-REBATE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE AULIM-REBATE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2400-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO AULIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E778 ADDR REQUIRED
      * E129 REBATE YTD NEGATIVE
      * E531 REBATE YTD OVER 50000
      * E865 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AULIM-ADDR = SPACES
                   MOVE 'E778' TO WS-REJECT-CODE
               WHEN AULIM-REBATE-YTD < 0
                   MOVE 'E129' TO WS-REJECT-CODE
               WHEN AULIM-REBATE-YTD > 50000.00
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN AULIM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E865' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGEBK YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2420-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN AULIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO AULIM-CHARGEBK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AULIM-EXEMPT-FLAG = 'Y'
                   COMPUTE AULIM-CHARGEBK-YTD ROUNDED = WS-WORK-AMT-2 *
                       AULIM-MARGIN / 200
               WHEN AULIM-TIER-CD = 'EW'
                   MOVE 3.00 TO AULIM-CHARGEBK-YTD
               WHEN AULIM-TIER-CD = 'JF'
                   MOVE 20.00 TO AULIM-CHARGEBK-YTD
               WHEN OTHER
                   COMPUTE AULIM-CHARGEBK-YTD ROUNDED = WS-WORK-AMT-2 *
                       AULIM-MARGIN / 100
           END-EVALUATE
           IF AULIM-CHARGEBK-YTD > 0 AND AULIM-CHARGEBK-YTD < 10.00
               MOVE 10.00 TO AULIM-CHARGEBK-YTD
           END-IF
           IF AULIM-CHARGEBK-YTD > 50.00
               MOVE 50.00 TO AULIM-CHARGEBK-YTD
           END-IF
           ADD AULIM-CHARGEBK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-PAYMENT.
           IF AULIM-PAYMENT-CNT < 9999999
               ADD 1 TO AULIM-PAYMENT-CNT
           END-IF
           IF AULIM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO AULIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN NEXT DUE DATE IS PAST
      * H -> S WHEN NEXT DUE DATE IS PAST
      * S -> V WHEN NEXT DUE DATE IS PAST
      * V -> W WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2440-SET-TYPE.
           EVALUATE AULIM-TYPE-CD
               WHEN 'B'
                   IF AULIM-NEXT-DUE-DATE > 0 AND AULIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF AULIM-NEXT-DUE-DATE > 0 AND AULIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF AULIM-NEXT-DUE-DATE > 0 AND AULIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF AULIM-NEXT-DUE-DATE > 0 AND AULIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO AULIM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2450-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-STMT-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2460-ACCUM-TIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AULIM-TIER-CD
                   ADD AULIM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AULIM-TIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AULIM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2470-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AULIM-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AULIM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REBATE YTD.
      *----------------------------------------------------------------*
       2480-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AULIM-REBATE-YTD ROUNDED =
                   AULIM-AMOUNT-FIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO AULIM-REBATE-YTD
           END-IF
           ADD AULIM-REBATE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN NEXT DUE DATE IS PAST
      * JF -> KD WHEN NEXT DUE DATE IS PAST
      * KD -> UG WHEN NEXT DUE DATE IS PAST
      * UG -> WY WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-TIER.
           EVALUATE AULIM-TIER-CD
               WHEN 'EW'
                   IF AULIM-NEXT-DUE-DATE > 0 AND AULIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF AULIM-NEXT-DUE-DATE > 0 AND AULIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF AULIM-NEXT-DUE-DATE > 0 AND AULIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF AULIM-NEXT-DUE-DATE > 0 AND AULIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO AULIM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2500-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = AULIM-AMOUNT-FIN-YTD / 4
           COMPUTE WS-WORK-AMT-2 = AULIM-AMOUNT-FIN-YTD - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2510-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AULIM-CHARGEBK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD AULIM-CHARGEBK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD AULIM-CHARGEBK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD AULIM-CHARGEBK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AULIM-CHARGEBK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * AULIM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO AULIM-CHARGEBK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2530-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * AULIM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2540-DESCRIBE-TIER.
           EVALUATE AULIM-TIER-CD
               WHEN 'EW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2550-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-MAINT-DATE > 0 AND AULIM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN LAST PAY DATE IS PAST
      * FNF -> GBX WHEN LAST PAY DATE IS PAST
      * GBX -> GTU WHEN LAST PAY DATE IS PAST
      * GTU -> JNE WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2560-SET-DEALER.
           EVALUATE AULIM-DEALER-CD
               WHEN 'DRU'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNF' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GBX' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GTU' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF AULIM-LAST-PAY-DATE > 0 AND AULIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNE' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2570-COUNT-CYCLE.
           IF AULIM-CYCLE-CNT < 9999999
               ADD 1 TO AULIM-CYCLE-CNT
           END-IF
           IF AULIM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO AULIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGEBK YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-CHARGEBK.
           IF AULIM-CHARGEBK-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AULIM-CHARGEBK-YTD * AULIM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE AMOUNT FIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2590-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AULIM-AMOUNT-FIN-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2600-COUNT-PAYMENT.
           IF AULIM-PAYMENT-CNT < 9999999
               ADD 1 TO AULIM-PAYMENT-CNT
           END-IF
           IF AULIM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO AULIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2610-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO AULIM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2620-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGEBK YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2630-SPLIT-CHARGEBK.
           COMPUTE WS-WORK-AMT = AULIM-CHARGEBK-YTD / 2
           COMPUTE WS-WORK-AMT-2 = AULIM-CHARGEBK-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2640-FLAG-CHARGEBK.
           IF AULIM-CHARGEBK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE AULIM-CHARGEBK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGEBK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2650-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AULIM-TYPE-CD
                   ADD AULIM-CHARGEBK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AULIM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AULIM-CHARGEBK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2660-LIMIT-CHARGEBK.
           IF AULIM-CHARGEBK-YTD < 10.00
               MOVE 10.00 TO AULIM-CHARGEBK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AULIM-CHARGEBK-YTD > 500.00
               MOVE 500.00 TO AULIM-CHARGEBK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE CHARGEBK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2670-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AULIM-CHARGEBK-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2680-DESCRIBE-STATUS.
           EVALUATE AULIM-STATUS-CD
               WHEN 'EUA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HMW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RCB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2690-COUNT-CYCLE.
           IF AULIM-CYCLE-CNT < 9999999
               ADD 1 TO AULIM-CYCLE-CNT
           END-IF
           IF AULIM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO AULIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E588 NAME REQUIRED
      * E251 WORK AMT NEGATIVE
      * E747 WORK AMT OVER 25000
      * E778 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2700-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AULIM-NAME = SPACES
                   MOVE 'E588' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E747' TO WS-REJECT-CODE
               WHEN AULIM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E778' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN LAST STMT DATE IS PAST
      * FNF -> GBX WHEN LAST STMT DATE IS PAST
      * GBX -> GTU WHEN LAST STMT DATE IS PAST
      * GTU -> JNE WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2710-SET-DEALER.
           EVALUATE AULIM-DEALER-CD
               WHEN 'DRU'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FNF' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GBX' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GTU' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JNE' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2720-LIMIT-CHARGEBK.
           IF AULIM-CHARGEBK-AMT < 0.00
               MOVE 0.00 TO AULIM-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AULIM-CHARGEBK-AMT > 1000.00
               MOVE 1000.00 TO AULIM-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2730-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AULIM-REBATE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD AULIM-REBATE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD AULIM-REBATE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD AULIM-REBATE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AULIM-REBATE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2740-COUNT-CYCLE.
           IF AULIM-CYCLE-CNT < 9999999
               ADD 1 TO AULIM-CYCLE-CNT
           END-IF
           IF AULIM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO AULIM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN LAST MAINT DATE IS PAST
      * HMW -> RCB WHEN LAST MAINT DATE IS PAST
      * RCB -> EUA WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2750-SET-STATUS.
           EVALUATE AULIM-STATUS-CD
               WHEN 'EUA'
                   IF AULIM-LAST-MAINT-DATE > 0 AND
                       AULIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HMW' TO AULIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF AULIM-LAST-MAINT-DATE > 0 AND
                       AULIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'RCB' TO AULIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF AULIM-LAST-MAINT-DATE > 0 AND
                       AULIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'EUA' TO AULIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E299 ADDR REQUIRED
      * E057 CHARGEBK YTD NEGATIVE
      * E646 CHARGEBK YTD OVER 10000
      * E858 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2760-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AULIM-ADDR = SPACES
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN AULIM-CHARGEBK-YTD < 0
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN AULIM-CHARGEBK-YTD > 10000.00
                   MOVE 'E646' TO WS-REJECT-CODE
               WHEN AULIM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2770-LIMIT-CHARGEBK.
           IF AULIM-CHARGEBK-YTD < 100.00
               MOVE 100.00 TO AULIM-CHARGEBK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AULIM-CHARGEBK-YTD > 10000.00
               MOVE 10000.00 TO AULIM-CHARGEBK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2780-DESCRIBE-TYPE.
           EVALUATE AULIM-TYPE-CD
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Z'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN LAST STMT DATE IS PAST
      * HMW -> RCB WHEN LAST STMT DATE IS PAST
      * RCB -> EUA WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2790-SET-STATUS.
           EVALUATE AULIM-STATUS-CD
               WHEN 'EUA'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HMW' TO AULIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RCB' TO AULIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EUA' TO AULIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2800-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-MAINT-DATE > 0 AND AULIM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AULIM-REBATE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD AULIM-REBATE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD AULIM-REBATE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD AULIM-REBATE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AULIM-REBATE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGEBK YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2810-ACCRUE-CHARGEBK.
           IF AULIM-CHARGEBK-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AULIM-CHARGEBK-YTD * AULIM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO AULIM-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2820-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2830-LIMIT-CHARGEBK.
           IF AULIM-CHARGEBK-AMT < 25.00
               MOVE 25.00 TO AULIM-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AULIM-CHARGEBK-AMT > 5000.00
               MOVE 5000.00 TO AULIM-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGEBK YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2840-PRORATE-CHARGEBK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   AULIM-CHARGEBK-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2850-COUNT-PAYMENT.
           IF AULIM-PAYMENT-CNT < 9999999
               ADD 1 TO AULIM-PAYMENT-CNT
           END-IF
           IF AULIM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO AULIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2860-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO AULIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2870-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF AULIM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO AULIM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2880-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AULIM-REBATE-AMT NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2890-COUNT-PAYMENT.
           IF AULIM-PAYMENT-CNT < 9999999
               ADD 1 TO AULIM-PAYMENT-CNT
           END-IF
           IF AULIM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO AULIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGEBK YTD FEE ON THE CHARGEBK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2900-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN AULIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO AULIM-CHARGEBK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AULIM-STOP-FLAG = 'Y'
                   COMPUTE AULIM-CHARGEBK-YTD ROUNDED =
                       AULIM-CHARGEBK-AMT * AULIM-FEE-PCT / 200
               WHEN AULIM-DEALER-CD = 'DRU'
                   MOVE 12.00 TO AULIM-CHARGEBK-YTD
               WHEN AULIM-DEALER-CD = 'FNF'
                   MOVE 15.00 TO AULIM-CHARGEBK-YTD
               WHEN OTHER
                   COMPUTE AULIM-CHARGEBK-YTD ROUNDED =
                       AULIM-CHARGEBK-AMT * AULIM-FEE-PCT / 100
           END-EVALUATE
           IF AULIM-CHARGEBK-YTD > 0 AND AULIM-CHARGEBK-YTD < 1.00
               MOVE 1.00 TO AULIM-CHARGEBK-YTD
           END-IF
           IF AULIM-CHARGEBK-YTD > 50.00
               MOVE 50.00 TO AULIM-CHARGEBK-YTD
           END-IF
           ADD AULIM-CHARGEBK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE DEALER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2910-DESCRIBE-DEALER.
           EVALUATE AULIM-DEALER-CD
               WHEN 'DRU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FNF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GBX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GTU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JNE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KTP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN LAST STMT DATE IS PAST
      * FNF -> GBX WHEN LAST STMT DATE IS PAST
      * GBX -> GTU WHEN LAST STMT DATE IS PAST
      * GTU -> JNE WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2920-SET-DEALER.
           EVALUATE AULIM-DEALER-CD
               WHEN 'DRU'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FNF' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GBX' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GTU' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF AULIM-LAST-STMT-DATE > 0 AND AULIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JNE' TO AULIM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2930-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF AULIM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO AULIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2940-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO AULIM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2950-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE AULIM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2960-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO AULIM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E523 NAME REQUIRED
      * E354 PAYMENT AMT NEGATIVE
      * E935 PAYMENT AMT OVER 10000
      * E705 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2970-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AULIM-NAME = SPACES
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN AULIM-PAYMENT-AMT < 0
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN AULIM-PAYMENT-AMT > 10000.00
                   MOVE 'E935' TO WS-REJECT-CODE
               WHEN AULIM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E705' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2980-DESCRIBE-TYPE.
           EVALUATE AULIM-TYPE-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'V'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2990-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE AMOUNT FIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AULIM-AMOUNT-FIN-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AULIM-LAST-MAINT-DATE > 0 AND AULIM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AULIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AULIM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD AULIM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD AULIM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AULIM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AULIM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-TYPE.
           EVALUATE AULIM-TYPE-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = AULIM-AMOUNT-FIN-YTD / 4
           COMPUTE WS-WORK-AMT-2 = AULIM-AMOUNT-FIN-YTD - (WS-WORK-AMT *
               4)
           COMPUTE AULIM-CHARGEBK-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3040-LIMIT-AMOUNT-FIN.
           IF AULIM-AMOUNT-FIN-YTD < 25.00
               MOVE 25.00 TO AULIM-AMOUNT-FIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AULIM-AMOUNT-FIN-YTD > 500.00
               MOVE 500.00 TO AULIM-AMOUNT-FIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE CHARGEBK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO AULIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AULIM-CHARGEBK-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO AULIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AULIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AULIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * AULIM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO AULIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO AULIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO AULIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'AULIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
