      *================================================================*
      * PROGRAM  : AUDTE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DEALER AUTO FINANCE RULE - ELIGIBILITY. CALLED BY   *
      *            THE DEALER AUTO FINANCE BATCH AND ONLINE PROGRAMS.  *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-07-10 TWH  ORIGINAL                                       *
      * 1995-09-07 SKN  GAP ROUNDING - HALF UP CHG7227                 *
      * 1995-03-26 PQR  GAP RERUN PROTECTION REQ7881                   *
      * 1998-12-20 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-03-21 TWH  PAYMENT RERUN PROTECTION REQ5612               *
      * 2000-02-15 LMB  CHARGEBK NOTICE TEXT PER LEGAL REQ7317         *
      * 2001-04-15 TWH  GAP NOTICE TEXT PER LEGAL CHG2150              *
      * 2005-12-26 PQR  AMOUNT-FIN EXCEPTIONS TO OPS QUEUE INC3516     *
      * 2007-09-14 BHS  NEW AMOUNT-FIN REPORT COLUMNS CHG9578          *
      * 2008-07-01 DPS  PERFORMANCE - CHARGEBK READ AHEAD REQ1749      *
      * 2008-10-17 BHS  GAP STATUS CODES ADDED INC6108                 *
      * 2010-12-10 SKN  PAYMENT CUTOFF MOVED INC2459                   *
      * 2010-01-24 TWH  RESERVE EXCEPTIONS TO OPS QUEUE INC6074        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUDTE.
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
                                           VALUE 24.000000.
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
      * WS-PAYMENT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000052000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000236000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000082000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004500000000322500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000227500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000155500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000670000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000658500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000476500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000230000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000519000'.
       01  WS-PAYMENT-GRID-TBL REDEFINES WS-PAYMENT-GRID-VALUES.
           05  WS-PAYMENT-GRID-ROW OCCURS 11 TIMES.
               10  WS-PAYMENT-GRID-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-GRID-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY AUDTEP.

       PROCEDURE DIVISION USING AUDTE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-COUNT-EXCEPT
           PERFORM 2010-AGE-LAST-MAINT
           PERFORM 2020-SET-CYCLE
           PERFORM 2030-CHECK-LAST-MAINT
           PERFORM 2040-AGE-EFFECTIVE
           PERFORM 2050-SELECT-FEE-PCT
           PERFORM 2060-AGE-NOTICE
           PERFORM 2070-SET-DEALER
           PERFORM 2080-LIMIT-PAYMENT
           PERFORM 2090-SPLIT-PAYMENT
           PERFORM 2100-SPLIT-PAYMENT
           PERFORM 2110-ASSESS-NOTICE-SENT
           PERFORM 2120-ASSESS-REVIEW
           PERFORM 2130-PRORATE-AMOUNT-FIN
           PERFORM 2140-SPLIT-REBATE
           PERFORM 2150-FLAG-ITEM
           PERFORM 2160-SPLIT-PAYMENT
           PERFORM 2170-FLAG-REBATE
           PERFORM 2180-LIMIT-ITEM
           PERFORM 2190-SELECT-MARGIN
           PERFORM 2200-AGE-LAST-MAINT
           PERFORM 2210-SET-TIER
           PERFORM 2220-ACCUM-CYCLE
           PERFORM 2230-CHECK-NOTICE
           PERFORM 2240-DESCRIBE-DEALER
           PERFORM 2250-FLAG-PAYMENT
           PERFORM 2260-SELECT-FEE-PCT
           PERFORM 2270-DESCRIBE-DEALER
           PERFORM 2280-COUNT-EXCEPT
           PERFORM 2290-ASSESS-REVIEW
           PERFORM 2300-ASSESS-NOTICE-SENT
           PERFORM 2310-ACCRUE-MARGIN
           PERFORM 2320-ACCUM-TYPE
           PERFORM 2330-SET-DEALER
           PERFORM 2340-DESCRIBE-CYCLE
           PERFORM 2350-AGE-EXPIRY
           PERFORM 2360-LIMIT-PAYMENT
           PERFORM 2370-COUNT-RETRY
           PERFORM 2380-PRORATE-PAYMENT
           PERFORM 2390-SET-TIER
           PERFORM 2400-ACCUM-DEALER
           PERFORM 2410-EDIT-NAME
           PERFORM 2420-SELECT-FEE-PCT
           PERFORM 2430-COUNT-RETRY
           PERFORM 2440-PRORATE-PAYMENT
           PERFORM 2450-FLAG-REBATE
           PERFORM 2460-AGE-NOTICE
           PERFORM 2470-SELECT-FEE-PCT
           PERFORM 2480-CHECK-EXPIRY
           PERFORM 2490-SET-TYPE
           PERFORM 2500-COUNT-RETRY
           PERFORM 2510-SELECT-FEE-PCT
           PERFORM 2520-SET-TIER
           PERFORM 2530-AGE-LAST-MAINT
           PERFORM 2540-COUNT-EXCEPT
           PERFORM 2550-SET-CYCLE
           PERFORM 2560-EDIT-NAME
           PERFORM 2570-CHECK-EXPIRY
           PERFORM 2580-ACCUM-CYCLE
           PERFORM 2590-ACCUM-CYCLE
           PERFORM 2600-COUNT-EXCEPT
           PERFORM 2610-ASSESS-OVERRIDE
           PERFORM 2620-LIMIT-GAP
           PERFORM 2630-AGE-EXPIRY
           PERFORM 2640-ASSESS-NOTICE-SENT
           PERFORM 2650-COUNT-RETRY
           PERFORM 2660-EDIT-DESC
           PERFORM 2670-AGE-NOTICE
           PERFORM 2680-COUNT-RETRY
           PERFORM 2690-CHECK-NOTICE
           PERFORM 2700-SELECT-FEE-PCT
           PERFORM 2710-SET-DEALER
           PERFORM 2720-ACCUM-DEALER
           PERFORM 2730-CHECK-EXPIRY
           PERFORM 2740-AGE-LAST-MAINT
           PERFORM 2750-SET-TYPE
           PERFORM 2760-COUNT-RETRY
           PERFORM 2770-SPLIT-REBATE
           PERFORM 2780-ACCUM-CYCLE
           PERFORM 2790-SPLIT-REBATE
           PERFORM 2800-ACCRUE-PAYMENT
           PERFORM 2810-LIMIT-PAYMENT
           PERFORM 2820-ASSESS-REVIEW
           PERFORM 2830-DESCRIBE-TYPE
           PERFORM 2840-FLAG-GAP
           PERFORM 2850-CHECK-LAST-MAINT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO AUDTE-RETURN-CODE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2000-COUNT-EXCEPT.
           IF AUDTE-EXCEPT-CNT < 9999999
               ADD 1 TO AUDTE-EXCEPT-CNT
           END-IF
           IF AUDTE-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO AUDTE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-LAST-MAINT-DATE > 0 AND AUDTE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUDTE-REBATE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD AUDTE-REBATE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD AUDTE-REBATE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD AUDTE-REBATE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUDTE-REBATE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> H WHEN EXPIRY DATE IS PAST
      * H -> J WHEN EXPIRY DATE IS PAST
      * J -> T WHEN EXPIRY DATE IS PAST
      * T -> W WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2020-SET-CYCLE.
           EVALUATE AUDTE-CYCLE-CD
               WHEN 'A'
                   IF AUDTE-EXPIRY-DATE > 0 AND AUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO AUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF AUDTE-EXPIRY-DATE > 0 AND AUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO AUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF AUDTE-EXPIRY-DATE > 0 AND AUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO AUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF AUDTE-EXPIRY-DATE > 0 AND AUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO AUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2030-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO AUDTE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2040-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2050-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO AUDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO AUDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-NOTICE-DATE > 0 AND AUDTE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUDTE-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD AUDTE-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD AUDTE-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD AUDTE-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUDTE-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN EFFECTIVE DATE IS PAST
      * FNF -> GBX WHEN EFFECTIVE DATE IS PAST
      * GBX -> GTU WHEN EFFECTIVE DATE IS PAST
      * GTU -> JNE WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2070-SET-DEALER.
           EVALUATE AUDTE-DEALER-CD
               WHEN 'DRU'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FNF' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GBX' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GTU' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JNE' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2080-LIMIT-PAYMENT.
           IF AUDTE-PAYMENT-AMT < 10.00
               MOVE 10.00 TO AUDTE-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AUDTE-PAYMENT-AMT > 500.00
               MOVE 500.00 TO AUDTE-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2090-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = AUDTE-PAYMENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = AUDTE-PAYMENT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2100-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = AUDTE-PAYMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = AUDTE-PAYMENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE AUDTE-REBATE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REBATE YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2110-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN AUDTE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO AUDTE-REBATE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUDTE-REVIEW-FLAG = 'Y'
                   COMPUTE AUDTE-REBATE-YTD ROUNDED = AUDTE-PAYMENT-AMT
                       * AUDTE-FEE-PCT / 200
               WHEN AUDTE-TYPE-CD = 'B'
                   MOVE 5.00 TO AUDTE-REBATE-YTD
               WHEN AUDTE-TYPE-CD = 'H'
                   MOVE 10.00 TO AUDTE-REBATE-YTD
               WHEN OTHER
                   COMPUTE AUDTE-REBATE-YTD ROUNDED = AUDTE-PAYMENT-AMT
                       * AUDTE-FEE-PCT / 100
           END-EVALUATE
           IF AUDTE-REBATE-YTD > 0 AND AUDTE-REBATE-YTD < 10.00
               MOVE 10.00 TO AUDTE-REBATE-YTD
           END-IF
           IF AUDTE-REBATE-YTD > 250.00
               MOVE 250.00 TO AUDTE-REBATE-YTD
           END-IF
           ADD AUDTE-REBATE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PAYMENT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2120-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN AUDTE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUDTE-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AUDTE-PAYMENT-YTD *
                       AUDTE-FEE-PCT / 200
               WHEN AUDTE-CYCLE-CD = 'A'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN AUDTE-CYCLE-CD = 'H'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AUDTE-PAYMENT-YTD *
                       AUDTE-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE GAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT FIN AMT.
      *----------------------------------------------------------------*
       2130-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AUDTE-AMOUNT-FIN-AMT ROUNDED =
                   AUDTE-GAP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO AUDTE-AMOUNT-FIN-AMT
           END-IF
           ADD AUDTE-AMOUNT-FIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REBATE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2140-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = AUDTE-REBATE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = AUDTE-REBATE-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2150-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE AUDTE-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REBATE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2170-FLAG-REBATE.
           IF AUDTE-REBATE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE AUDTE-REBATE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2180-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2190-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO AUDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AUDTE-PAYMENT-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO AUDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2200-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-LAST-MAINT-DATE > 0 AND AUDTE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUDTE-REBATE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD AUDTE-REBATE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD AUDTE-REBATE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD AUDTE-REBATE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUDTE-REBATE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN EFFECTIVE DATE IS PAST
      * JF -> KD WHEN EFFECTIVE DATE IS PAST
      * KD -> UG WHEN EFFECTIVE DATE IS PAST
      * UG -> WY WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-TIER.
           EVALUATE AUDTE-TIER-CD
               WHEN 'EW'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JF' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KD' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UG' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WY' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2220-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUDTE-CYCLE-CD
                   ADD AUDTE-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AUDTE-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2230-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO AUDTE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEALER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2240-DESCRIBE-DEALER.
           EVALUATE AUDTE-DEALER-CD
               WHEN 'DRU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FNF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GBX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GTU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JNE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KTP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2250-FLAG-PAYMENT.
           IF AUDTE-PAYMENT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE AUDTE-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2260-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO AUDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AUDTE-GAP-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO AUDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEALER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2270-DESCRIBE-DEALER.
           EVALUATE AUDTE-DEALER-CD
               WHEN 'DRU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FNF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GBX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GTU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JNE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KTP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-EXCEPT.
           IF AUDTE-EXCEPT-CNT < 9999999
               ADD 1 TO AUDTE-EXCEPT-CNT
           END-IF
           IF AUDTE-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO AUDTE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE REBATE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2290-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN AUDTE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUDTE-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AUDTE-REBATE-YTD *
                       AUDTE-MARGIN / 200
               WHEN AUDTE-CYCLE-CD = 'A'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN AUDTE-CYCLE-CD = 'H'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AUDTE-REBATE-YTD *
                       AUDTE-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2300-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN AUDTE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO AUDTE-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUDTE-OVERRIDE-FLAG = 'Y'
                   COMPUTE AUDTE-PAYMENT-YTD ROUNDED = WS-WORK-AMT *
                       AUDTE-FEE-PCT / 200
               WHEN AUDTE-CYCLE-CD = 'A'
                   MOVE 7.00 TO AUDTE-PAYMENT-YTD
               WHEN AUDTE-CYCLE-CD = 'H'
                   MOVE 3.00 TO AUDTE-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE AUDTE-PAYMENT-YTD ROUNDED = WS-WORK-AMT *
                       AUDTE-FEE-PCT / 100
           END-EVALUATE
           IF AUDTE-PAYMENT-YTD > 0 AND AUDTE-PAYMENT-YTD < 2.00
               MOVE 2.00 TO AUDTE-PAYMENT-YTD
           END-IF
           IF AUDTE-PAYMENT-YTD > 50.00
               MOVE 50.00 TO AUDTE-PAYMENT-YTD
           END-IF
           ADD AUDTE-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2310-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * AUDTE-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO AUDTE-GAP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUDTE-TYPE-CD
                   ADD AUDTE-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUDTE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AUDTE-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN EFFECTIVE DATE IS PAST
      * FNF -> GBX WHEN EFFECTIVE DATE IS PAST
      * GBX -> GTU WHEN EFFECTIVE DATE IS PAST
      * GTU -> JNE WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2330-SET-DEALER.
           EVALUATE AUDTE-DEALER-CD
               WHEN 'DRU'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FNF' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GBX' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GTU' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JNE' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2340-DESCRIBE-CYCLE.
           EVALUATE AUDTE-CYCLE-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'J'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2350-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-EXPIRY-DATE > 0 AND AUDTE-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-EXPIRY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2360-LIMIT-PAYMENT.
           IF AUDTE-PAYMENT-YTD < 50.00
               MOVE 50.00 TO AUDTE-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AUDTE-PAYMENT-YTD > 2500.00
               MOVE 2500.00 TO AUDTE-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2370-COUNT-RETRY.
           IF AUDTE-RETRY-CNT < 9999999
               ADD 1 TO AUDTE-RETRY-CNT
           END-IF
           IF AUDTE-RETRY-CNT NOT < 10
               MOVE 'Y' TO AUDTE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REBATE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       2380-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AUDTE-PAYMENT-AMT ROUNDED =
                   AUDTE-REBATE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO AUDTE-PAYMENT-AMT
           END-IF
           ADD AUDTE-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN NOTICE DATE IS PAST
      * JF -> KD WHEN NOTICE DATE IS PAST
      * KD -> UG WHEN NOTICE DATE IS PAST
      * UG -> WY WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2390-SET-TIER.
           EVALUATE AUDTE-TIER-CD
               WHEN 'EW'
                   IF AUDTE-NOTICE-DATE > 0 AND AUDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF AUDTE-NOTICE-DATE > 0 AND AUDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF AUDTE-NOTICE-DATE > 0 AND AUDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF AUDTE-NOTICE-DATE > 0 AND AUDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REBATE YTD BY DEALER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2400-ACCUM-DEALER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUDTE-DEALER-CD
                   ADD AUDTE-REBATE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUDTE-DEALER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AUDTE-REBATE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E676 NAME REQUIRED
      * E100 PAYMENT YTD NEGATIVE
      * E713 PAYMENT YTD OVER 250000
      * E893 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AUDTE-NAME = SPACES
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN AUDTE-PAYMENT-YTD < 0
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN AUDTE-PAYMENT-YTD > 250000.00
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN AUDTE-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE REBATE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO AUDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AUDTE-REBATE-YTD NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO AUDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-RETRY.
           IF AUDTE-RETRY-CNT < 9999999
               ADD 1 TO AUDTE-RETRY-CNT
           END-IF
           IF AUDTE-RETRY-CNT NOT < 10
               MOVE 'Y' TO AUDTE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       2440-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AUDTE-PAYMENT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO AUDTE-PAYMENT-YTD
           END-IF
           ADD AUDTE-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * REBATE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2450-FLAG-REBATE.
           IF AUDTE-REBATE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE AUDTE-REBATE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2460-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-NOTICE-DATE > 0 AND AUDTE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUDTE-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD AUDTE-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD AUDTE-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AUDTE-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUDTE-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO AUDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO AUDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2480-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO AUDTE-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN EFFECTIVE DATE IS PAST
      * H -> S WHEN EFFECTIVE DATE IS PAST
      * S -> V WHEN EFFECTIVE DATE IS PAST
      * V -> W WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-TYPE.
           EVALUATE AUDTE-TYPE-CD
               WHEN 'B'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO AUDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO AUDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO AUDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'W' TO AUDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2500-COUNT-RETRY.
           IF AUDTE-RETRY-CNT < 9999999
               ADD 1 TO AUDTE-RETRY-CNT
           END-IF
           IF AUDTE-RETRY-CNT NOT < 12
               MOVE 'Y' TO AUDTE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2510-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO AUDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO AUDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN EXPIRY DATE IS PAST
      * JF -> KD WHEN EXPIRY DATE IS PAST
      * KD -> UG WHEN EXPIRY DATE IS PAST
      * UG -> WY WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2520-SET-TIER.
           EVALUATE AUDTE-TIER-CD
               WHEN 'EW'
                   IF AUDTE-EXPIRY-DATE > 0 AND AUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF AUDTE-EXPIRY-DATE > 0 AND AUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF AUDTE-EXPIRY-DATE > 0 AND AUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF AUDTE-EXPIRY-DATE > 0 AND AUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO AUDTE-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2530-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-LAST-MAINT-DATE > 0 AND AUDTE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUDTE-GAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD AUDTE-GAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD AUDTE-GAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AUDTE-GAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUDTE-GAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-EXCEPT.
           IF AUDTE-EXCEPT-CNT < 9999999
               ADD 1 TO AUDTE-EXCEPT-CNT
           END-IF
           IF AUDTE-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO AUDTE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> H WHEN EFFECTIVE DATE IS PAST
      * H -> J WHEN EFFECTIVE DATE IS PAST
      * J -> T WHEN EFFECTIVE DATE IS PAST
      * T -> W WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2550-SET-CYCLE.
           EVALUATE AUDTE-CYCLE-CD
               WHEN 'A'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO AUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'J' TO AUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO AUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF AUDTE-EFFECTIVE-DATE > 0 AND AUDTE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'W' TO AUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E104 NAME REQUIRED
      * E735 WORK AMT NEGATIVE
      * E020 WORK AMT OVER 25000
      * E414 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2560-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AUDTE-NAME = SPACES
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E735' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E020' TO WS-REJECT-CODE
               WHEN AUDTE-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E414' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO AUDTE-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUDTE-CYCLE-CD
                   ADD AUDTE-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AUDTE-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REBATE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2590-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUDTE-CYCLE-CD
                   ADD AUDTE-REBATE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AUDTE-REBATE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2600-COUNT-EXCEPT.
           IF AUDTE-EXCEPT-CNT < 9999999
               ADD 1 TO AUDTE-EXCEPT-CNT
           END-IF
           IF AUDTE-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO AUDTE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT FIN FEE ON THE REBATE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2610-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN AUDTE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO AUDTE-AMOUNT-FIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUDTE-REVIEW-FLAG = 'Y'
                   COMPUTE AUDTE-AMOUNT-FIN-AMT ROUNDED =
                       AUDTE-REBATE-YTD * AUDTE-FEE-PCT / 200
               WHEN AUDTE-TYPE-CD = 'B'
                   MOVE 20.00 TO AUDTE-AMOUNT-FIN-AMT
               WHEN AUDTE-TYPE-CD = 'H'
                   MOVE 7.00 TO AUDTE-AMOUNT-FIN-AMT
               WHEN OTHER
                   COMPUTE AUDTE-AMOUNT-FIN-AMT ROUNDED =
                       AUDTE-REBATE-YTD * AUDTE-FEE-PCT / 100
           END-EVALUATE
           IF AUDTE-AMOUNT-FIN-AMT > 0 AND AUDTE-AMOUNT-FIN-AMT < 1.00
               MOVE 1.00 TO AUDTE-AMOUNT-FIN-AMT
           END-IF
           IF AUDTE-AMOUNT-FIN-AMT > 50.00
               MOVE 50.00 TO AUDTE-AMOUNT-FIN-AMT
           END-IF
           ADD AUDTE-AMOUNT-FIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * GAP AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2620-LIMIT-GAP.
           IF AUDTE-GAP-AMT < 0.00
               MOVE 0.00 TO AUDTE-GAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AUDTE-GAP-AMT > 50000.00
               MOVE 50000.00 TO AUDTE-GAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2630-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-EXPIRY-DATE > 0 AND AUDTE-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUDTE-GAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD AUDTE-GAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD AUDTE-GAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AUDTE-GAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUDTE-GAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REBATE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2640-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN AUDTE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO AUDTE-REBATE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUDTE-OVERRIDE-FLAG = 'Y'
                   COMPUTE AUDTE-REBATE-YTD ROUNDED = WS-WORK-AMT *
                       AUDTE-MARGIN / 200
               WHEN AUDTE-CYCLE-CD = 'A'
                   MOVE 20.00 TO AUDTE-REBATE-YTD
               WHEN AUDTE-CYCLE-CD = 'H'
                   MOVE 3.00 TO AUDTE-REBATE-YTD
               WHEN OTHER
                   COMPUTE AUDTE-REBATE-YTD ROUNDED = WS-WORK-AMT *
                       AUDTE-MARGIN / 100
           END-EVALUATE
           IF AUDTE-REBATE-YTD > 0 AND AUDTE-REBATE-YTD < 10.00
               MOVE 10.00 TO AUDTE-REBATE-YTD
           END-IF
           IF AUDTE-REBATE-YTD > 25.00
               MOVE 25.00 TO AUDTE-REBATE-YTD
           END-IF
           ADD AUDTE-REBATE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2650-COUNT-RETRY.
           IF AUDTE-RETRY-CNT < 9999999
               ADD 1 TO AUDTE-RETRY-CNT
           END-IF
           IF AUDTE-RETRY-CNT NOT < 12
               MOVE 'Y' TO AUDTE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E308 DESC REQUIRED
      * E045 REBATE YTD NEGATIVE
      * E199 REBATE YTD OVER 250000
      * E038 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2660-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AUDTE-DESC = SPACES
                   MOVE 'E308' TO WS-REJECT-CODE
               WHEN AUDTE-REBATE-YTD < 0
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN AUDTE-REBATE-YTD > 250000.00
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN AUDTE-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2670-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-NOTICE-DATE > 0 AND AUDTE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2680-COUNT-RETRY.
           IF AUDTE-RETRY-CNT < 9999999
               ADD 1 TO AUDTE-RETRY-CNT
           END-IF
           IF AUDTE-RETRY-CNT NOT < 5
               MOVE 'Y' TO AUDTE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2690-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO AUDTE-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2700-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO AUDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AUDTE-GAP-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO AUDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AUDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AUDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN LAST MAINT DATE IS PAST
      * FNF -> GBX WHEN LAST MAINT DATE IS PAST
      * GBX -> GTU WHEN LAST MAINT DATE IS PAST
      * GTU -> JNE WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2710-SET-DEALER.
           EVALUATE AUDTE-DEALER-CD
               WHEN 'DRU'
                   IF AUDTE-LAST-MAINT-DATE > 0 AND
                       AUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FNF' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF AUDTE-LAST-MAINT-DATE > 0 AND
                       AUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GBX' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF AUDTE-LAST-MAINT-DATE > 0 AND
                       AUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GTU' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF AUDTE-LAST-MAINT-DATE > 0 AND
                       AUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JNE' TO AUDTE-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DEALER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2720-ACCUM-DEALER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUDTE-DEALER-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUDTE-DEALER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2730-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO AUDTE-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2740-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-LAST-MAINT-DATE > 0 AND AUDTE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AUDTE-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD AUDTE-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD AUDTE-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD AUDTE-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AUDTE-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN NOTICE DATE IS PAST
      * H -> S WHEN NOTICE DATE IS PAST
      * S -> V WHEN NOTICE DATE IS PAST
      * V -> W WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2750-SET-TYPE.
           EVALUATE AUDTE-TYPE-CD
               WHEN 'B'
                   IF AUDTE-NOTICE-DATE > 0 AND AUDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO AUDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF AUDTE-NOTICE-DATE > 0 AND AUDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO AUDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF AUDTE-NOTICE-DATE > 0 AND AUDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO AUDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF AUDTE-NOTICE-DATE > 0 AND AUDTE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO AUDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2760-COUNT-RETRY.
           IF AUDTE-RETRY-CNT < 9999999
               ADD 1 TO AUDTE-RETRY-CNT
           END-IF
           IF AUDTE-RETRY-CNT NOT < 6
               MOVE 'Y' TO AUDTE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2770-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE AUDTE-REBATE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2780-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AUDTE-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AUDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REBATE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2790-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = AUDTE-REBATE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = AUDTE-REBATE-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2800-ACCRUE-PAYMENT.
           IF AUDTE-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AUDTE-PAYMENT-YTD * AUDTE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO AUDTE-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2810-LIMIT-PAYMENT.
           IF AUDTE-PAYMENT-AMT < 100.00
               MOVE 100.00 TO AUDTE-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AUDTE-PAYMENT-AMT > 500.00
               MOVE 500.00 TO AUDTE-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE REBATE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2820-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN AUDTE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AUDTE-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = AUDTE-REBATE-YTD *
                       AUDTE-FEE-PCT / 200
               WHEN AUDTE-TIER-CD = 'EW'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN AUDTE-TIER-CD = 'JF'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = AUDTE-REBATE-YTD *
                       AUDTE-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2830-DESCRIBE-TYPE.
           EVALUATE AUDTE-TYPE-CD
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GAP AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2840-FLAG-GAP.
           IF AUDTE-GAP-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE AUDTE-GAP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2850-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF AUDTE-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AUDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO AUDTE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO AUDTE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO AUDTE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO AUDTE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'AUDTE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
