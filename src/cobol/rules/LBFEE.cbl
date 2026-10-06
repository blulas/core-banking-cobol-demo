      *================================================================*
      * PROGRAM  : LBFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : LOCKBOX REMITTANCE RULE - TIERING. CALLED BY THE    *
      *            LOCKBOX REMITTANCE BATCH AND ONLINE PROGRAMS.       *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-02-08 CRP  ORIGINAL                                       *
      * 1998-11-18 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2007-01-14 DPS  CHECK STATUS CODES ADDED REQ0707               *
      * 2010-09-13 DPS  FLOAT ROUNDING - HALF UP CHG5448               *
      * 2013-08-13 BHS  AUDIT FINDING - DEPOSIT CONTROL TOTALS PRB7837 *
      * 2014-09-13 PQR  FEE ROUNDING - HALF UP PRB2908                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBFEE.
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
                                           VALUE 18.000000.
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
      * WS-CASH-ON-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-ON-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000696500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000833500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000714000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000447000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005800000000221500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005900000000176500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006400000000517000'.
       01  WS-CASH-ON-BAND-TBL REDEFINES WS-CASH-ON-BAND-VALUES.
           05  WS-CASH-ON-BAND-ROW OCCURS 7 TIMES.
               10  WS-CASH-ON-BAND-LOW     PIC 9(09)V99.
               10  WS-CASH-ON-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CHECK-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CHECK-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000891000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000697500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000588500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000107000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000119500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000294000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018050000000879500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019050000000601000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021550000000375500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022550000000487000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023050000000742000'.
       01  WS-CHECK-TIER-TBL REDEFINES WS-CHECK-TIER-VALUES.
           05  WS-CHECK-TIER-ROW OCCURS 11 TIMES.
               10  WS-CHECK-TIER-LOW       PIC 9(09)V99.
               10  WS-CHECK-TIER-RATE      PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY LBFEEP.

       PROCEDURE DIVISION USING LBFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SELECT-DISCOUNT
           PERFORM 2010-ACCUM-CHANNEL
           PERFORM 2020-DESCRIBE-BATCH-TYPE
           PERFORM 2030-EDIT-ADDR
           PERFORM 2040-FLAG-EXCEPT
           PERFORM 2050-SELECT-DISCOUNT
           PERFORM 2060-SELECT-PENALTY-PCT
           PERFORM 2070-CHECK-CLOSE
           PERFORM 2080-COUNT-ITEM
           PERFORM 2090-COUNT-ITEM
           PERFORM 2100-PRORATE-EXCEPT
           PERFORM 2110-CHECK-LAST-PAY
           PERFORM 2120-COUNT-PAYMENT
           PERFORM 2130-SET-CHANNEL
           PERFORM 2140-PRORATE-DEPOSIT
           PERFORM 2150-ACCRUE-EXCEPT
           PERFORM 2160-FLAG-FLOAT
           PERFORM 2170-ASSESS-NOTICE-SENT
           PERFORM 2180-PRORATE-FLOAT
           PERFORM 2190-LIMIT-DEPOSIT
           PERFORM 2200-SPLIT-FLOAT
           PERFORM 2210-SET-REASON
           PERFORM 2220-ACCUM-REASON
           PERFORM 2230-EDIT-ADDR
           PERFORM 2240-CHECK-OPEN
           PERFORM 2250-PRORATE-FLOAT
           PERFORM 2260-AGE-OPEN
           PERFORM 2270-ASSESS-NOTICE-SENT
           PERFORM 2280-ACCRUE-DISCOUNT
           PERFORM 2290-AGE-LAST-PAY
           PERFORM 2300-EDIT-NAME
           PERFORM 2310-CHECK-NEXT-DUE
           PERFORM 2320-CHECK-CLOSE
           PERFORM 2330-PRORATE-FLOAT
           PERFORM 2340-DESCRIBE-BATCH-TYPE
           PERFORM 2350-LIMIT-DEPOSIT
           PERFORM 2360-ACCUM-LOCKBOX
           PERFORM 2370-SELECT-DISCOUNT
           PERFORM 2380-DESCRIBE-REASON
           PERFORM 2390-EDIT-ADDR
           PERFORM 2400-SET-REASON
           PERFORM 2410-COUNT-ITEM
           PERFORM 2420-SPLIT-REMIT
           PERFORM 2430-LIMIT-FLOAT
           PERFORM 2440-SELECT-PENALTY-PCT
           PERFORM 2450-LIMIT-DEPOSIT
           PERFORM 2460-ACCRUE-DEPOSIT
           PERFORM 2470-COUNT-ITEM
           PERFORM 2480-FLAG-ITEM
           PERFORM 2490-ACCUM-LOCKBOX
           PERFORM 2500-CHECK-OPEN
           PERFORM 2510-ACCUM-LOCKBOX
           PERFORM 2520-LIMIT-FLOAT
           PERFORM 2530-SET-LOCKBOX
           PERFORM 2540-PRORATE-FLOAT
           PERFORM 2550-COUNT-ITEM
           PERFORM 2560-SPLIT-REMIT
           PERFORM 2570-CHECK-NEXT-DUE
           PERFORM 2580-ACCUM-CHANNEL
           PERFORM 2590-EDIT-ADDR
           PERFORM 2600-SPLIT-EXCEPT
           PERFORM 2610-SELECT-DISCOUNT
           PERFORM 2620-FLAG-DEPOSIT
           PERFORM 2630-ACCUM-LOCKBOX
           PERFORM 2640-ASSESS-NOTICE-SENT
           PERFORM 2650-SELECT-DISCOUNT
           PERFORM 2660-ASSESS-NOTICE-SENT
           PERFORM 2670-CHECK-LAST-PAY
           PERFORM 2680-ACCRUE-PENALTY-PCT
           PERFORM 2690-SET-LOCKBOX
           PERFORM 2700-COUNT-PAYMENT
           PERFORM 2710-ACCUM-REASON
           PERFORM 2720-SELECT-DISCOUNT
           PERFORM 2730-ACCRUE-DEPOSIT
           PERFORM 2740-SELECT-DISCOUNT
           PERFORM 2750-ASSESS-NOTICE-SENT
           PERFORM 2760-FLAG-REMIT
           PERFORM 2770-EDIT-ADDR
           PERFORM 2780-ASSESS-EXEMPT
           PERFORM 2790-CHECK-CLOSE
           PERFORM 2800-COUNT-PAYMENT
           PERFORM 2810-FLAG-REMIT
           PERFORM 2820-SELECT-DISCOUNT
           PERFORM 2830-FLAG-FLOAT
           PERFORM 2840-FLAG-DEPOSIT
           PERFORM 2850-SELECT-DISCOUNT
           PERFORM 2860-PRORATE-EXCEPT
           PERFORM 2870-CHECK-OPEN
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO LBFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON BAND TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2000-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-BAND-RATE(1) TO LBFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBFEE-DEPOSIT-YTD NOT < WS-CASH-ON-BAND-LOW(WS-TX)
                   MOVE WS-CASH-ON-BAND-RATE(WS-TX) TO LBFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBFEE-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBFEE-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BATCH TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2020-DESCRIBE-BATCH-TYPE.
           EVALUATE LBFEE-BATCH-TYPE-CD
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E413 ADDR REQUIRED
      * E954 DEPOSIT YTD NEGATIVE
      * E006 DEPOSIT YTD OVER 99999
      * E767 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2030-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBFEE-ADDR = SPACES
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN LBFEE-DEPOSIT-YTD < 0
                   MOVE 'E954' TO WS-REJECT-CODE
               WHEN LBFEE-DEPOSIT-YTD > 99999.00
                   MOVE 'E006' TO WS-REJECT-CODE
               WHEN LBFEE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2040-FLAG-EXCEPT.
           IF LBFEE-EXCEPT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBFEE-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2050-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-BAND-RATE(1) TO LBFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-CASH-ON-BAND-LOW(WS-TX)
                   MOVE WS-CASH-ON-BAND-RATE(WS-TX) TO LBFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHECK TIER TIER FOR THE EXCEPT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2060-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CHECK-TIER-RATE(1) TO LBFEE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF LBFEE-EXCEPT-YTD NOT < WS-CHECK-TIER-LOW(WS-TX)
                   MOVE WS-CHECK-TIER-RATE(WS-TX) TO LBFEE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBFEE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBFEE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2070-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-ITEM.
           IF LBFEE-ITEM-CNT < 9999999
               ADD 1 TO LBFEE-ITEM-CNT
           END-IF
           IF LBFEE-ITEM-CNT NOT < 6
               MOVE 'Y' TO LBFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2090-COUNT-ITEM.
           IF LBFEE-ITEM-CNT < 9999999
               ADD 1 TO LBFEE-ITEM-CNT
           END-IF
           IF LBFEE-ITEM-CNT NOT < 12
               MOVE 'Y' TO LBFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2100-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBFEE-EXCEPT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2110-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBFEE-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-PAYMENT.
           IF LBFEE-PAYMENT-CNT < 9999999
               ADD 1 TO LBFEE-PAYMENT-CNT
           END-IF
           IF LBFEE-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO LBFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> H WHEN OPEN DATE IS PAST
      * H -> Z WHEN OPEN DATE IS PAST
      * Z -> B WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2130-SET-CHANNEL.
           EVALUATE LBFEE-CHANNEL-CD
               WHEN 'B'
                   IF LBFEE-OPEN-DATE > 0 AND LBFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO LBFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF LBFEE-OPEN-DATE > 0 AND LBFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO LBFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF LBFEE-OPEN-DATE > 0 AND LBFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO LBFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       2140-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBFEE-DEPOSIT-YTD ROUNDED =
                   LBFEE-EXCEPT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBFEE-DEPOSIT-YTD
           END-IF
           ADD LBFEE-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2150-ACCRUE-EXCEPT.
           IF LBFEE-EXCEPT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBFEE-EXCEPT-YTD * LBFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2160-FLAG-FLOAT.
           IF LBFEE-FLOAT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBFEE-FLOAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE EXCEPT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2170-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN LBFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO LBFEE-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE LBFEE-EXCEPT-YTD ROUNDED = LBFEE-EXCEPT-AMT *
                       LBFEE-DISCOUNT / 200
               WHEN LBFEE-LOCKBOX-CD = 'DX'
                   MOVE 12.00 TO LBFEE-EXCEPT-YTD
               WHEN LBFEE-LOCKBOX-CD = 'JM'
                   MOVE 3.00 TO LBFEE-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE LBFEE-EXCEPT-YTD ROUNDED = LBFEE-EXCEPT-AMT *
                       LBFEE-DISCOUNT / 100
           END-EVALUATE
           IF LBFEE-EXCEPT-YTD > 0 AND LBFEE-EXCEPT-YTD < 5.00
               MOVE 5.00 TO LBFEE-EXCEPT-YTD
           END-IF
           IF LBFEE-EXCEPT-YTD > 35.00
               MOVE 35.00 TO LBFEE-EXCEPT-YTD
           END-IF
           ADD LBFEE-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FLOAT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2180-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBFEE-FLOAT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2190-LIMIT-DEPOSIT.
           IF LBFEE-DEPOSIT-YTD < 50.00
               MOVE 50.00 TO LBFEE-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBFEE-DEPOSIT-YTD > 500.00
               MOVE 500.00 TO LBFEE-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2200-SPLIT-FLOAT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE LBFEE-FLOAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN NEXT DUE DATE IS PAST
      * UK -> VR WHEN NEXT DUE DATE IS PAST
      * VR -> XG WHEN NEXT DUE DATE IS PAST
      * XG -> GF WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-REASON.
           EVALUATE LBFEE-REASON-CD
               WHEN 'GF'
                   IF LBFEE-NEXT-DUE-DATE > 0 AND LBFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBFEE-NEXT-DUE-DATE > 0 AND LBFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBFEE-NEXT-DUE-DATE > 0 AND LBFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBFEE-NEXT-DUE-DATE > 0 AND LBFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2220-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBFEE-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E047 ADDR REQUIRED
      * E181 WORK AMT 2 NEGATIVE
      * E711 WORK AMT 2 OVER 25000
      * E543 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBFEE-ADDR = SPACES
                   MOVE 'E047' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN LBFEE-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E543' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2240-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FLOAT AMT.
      *----------------------------------------------------------------*
       2250-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBFEE-FLOAT-AMT ROUNDED =
                   LBFEE-DEPOSIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBFEE-FLOAT-AMT
           END-IF
           ADD LBFEE-FLOAT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-OPEN-DATE > 0 AND LBFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBFEE-REMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD LBFEE-REMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBFEE-REMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBFEE-REMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBFEE-REMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT YTD FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2270-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN LBFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO LBFEE-FLOAT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBFEE-STOP-FLAG = 'Y'
                   COMPUTE LBFEE-FLOAT-YTD ROUNDED = LBFEE-DEPOSIT-YTD *
                       LBFEE-DISCOUNT / 200
               WHEN LBFEE-CHANNEL-CD = 'B'
                   MOVE 5.00 TO LBFEE-FLOAT-YTD
               WHEN LBFEE-CHANNEL-CD = 'H'
                   MOVE 20.00 TO LBFEE-FLOAT-YTD
               WHEN OTHER
                   COMPUTE LBFEE-FLOAT-YTD ROUNDED = LBFEE-DEPOSIT-YTD *
                       LBFEE-DISCOUNT / 100
           END-EVALUATE
           IF LBFEE-FLOAT-YTD > 0 AND LBFEE-FLOAT-YTD < 5.00
               MOVE 5.00 TO LBFEE-FLOAT-YTD
           END-IF
           IF LBFEE-FLOAT-YTD > 75.00
               MOVE 75.00 TO LBFEE-FLOAT-YTD
           END-IF
           ADD LBFEE-FLOAT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * LBFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBFEE-FLOAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-LAST-PAY-DATE > 0 AND LBFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBFEE-REMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBFEE-REMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD LBFEE-REMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBFEE-REMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBFEE-REMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E250 NAME REQUIRED
      * E188 FLOAT AMT NEGATIVE
      * E796 FLOAT AMT OVER 50000
      * E257 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2300-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBFEE-NAME = SPACES
                   MOVE 'E250' TO WS-REJECT-CODE
               WHEN LBFEE-FLOAT-AMT < 0
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN LBFEE-FLOAT-AMT > 50000.00
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN LBFEE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO LBFEE-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2320-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FLOAT YTD.
      *----------------------------------------------------------------*
       2330-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBFEE-FLOAT-YTD ROUNDED =
                   LBFEE-EXCEPT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBFEE-FLOAT-YTD
           END-IF
           ADD LBFEE-FLOAT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE BATCH TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2340-DESCRIBE-BATCH-TYPE.
           EVALUATE LBFEE-BATCH-TYPE-CD
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2350-LIMIT-DEPOSIT.
           IF LBFEE-DEPOSIT-YTD < 25.00
               MOVE 25.00 TO LBFEE-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBFEE-DEPOSIT-YTD > 10000.00
               MOVE 10000.00 TO LBFEE-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT YTD BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2360-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBFEE-LOCKBOX-CD
                   ADD LBFEE-DEPOSIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBFEE-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBFEE-DEPOSIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON BAND TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2370-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-BAND-RATE(1) TO LBFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBFEE-DEPOSIT-YTD NOT < WS-CASH-ON-BAND-LOW(WS-TX)
                   MOVE WS-CASH-ON-BAND-RATE(WS-TX) TO LBFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-REASON.
           EVALUATE LBFEE-REASON-CD
               WHEN 'GF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E560 ADDR REQUIRED
      * E212 WORK AMT 2 NEGATIVE
      * E412 WORK AMT 2 OVER 25000
      * E406 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2390-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBFEE-ADDR = SPACES
                   MOVE 'E560' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E212' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E412' TO WS-REJECT-CODE
               WHEN LBFEE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN CLOSE DATE IS PAST
      * UK -> VR WHEN CLOSE DATE IS PAST
      * VR -> XG WHEN CLOSE DATE IS PAST
      * XG -> GF WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2400-SET-REASON.
           EVALUATE LBFEE-REASON-CD
               WHEN 'GF'
                   IF LBFEE-CLOSE-DATE > 0 AND LBFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBFEE-CLOSE-DATE > 0 AND LBFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBFEE-CLOSE-DATE > 0 AND LBFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBFEE-CLOSE-DATE > 0 AND LBFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-ITEM.
           IF LBFEE-ITEM-CNT < 9999999
               ADD 1 TO LBFEE-ITEM-CNT
           END-IF
           IF LBFEE-ITEM-CNT NOT < 5
               MOVE 'Y' TO LBFEE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE LBFEE-REMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2430-LIMIT-FLOAT.
           IF LBFEE-FLOAT-AMT < 50.00
               MOVE 50.00 TO LBFEE-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBFEE-FLOAT-AMT > 50000.00
               MOVE 50000.00 TO LBFEE-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON BAND TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-BAND-RATE(1) TO LBFEE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBFEE-DEPOSIT-YTD NOT < WS-CASH-ON-BAND-LOW(WS-TX)
                   MOVE WS-CASH-ON-BAND-RATE(WS-TX) TO LBFEE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBFEE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBFEE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2450-LIMIT-DEPOSIT.
           IF LBFEE-DEPOSIT-YTD < 25.00
               MOVE 25.00 TO LBFEE-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBFEE-DEPOSIT-YTD > 2500.00
               MOVE 2500.00 TO LBFEE-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-DEPOSIT.
           IF LBFEE-DEPOSIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBFEE-DEPOSIT-YTD * LBFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBFEE-REMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2470-COUNT-ITEM.
           IF LBFEE-ITEM-CNT < 9999999
               ADD 1 TO LBFEE-ITEM-CNT
           END-IF
           IF LBFEE-ITEM-CNT NOT < 3
               MOVE 'Y' TO LBFEE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2480-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT YTD BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBFEE-LOCKBOX-CD
                   ADD LBFEE-REMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBFEE-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBFEE-REMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2500-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO LBFEE-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT YTD BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBFEE-LOCKBOX-CD
                   ADD LBFEE-EXCEPT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBFEE-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBFEE-EXCEPT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2520-LIMIT-FLOAT.
           IF LBFEE-FLOAT-YTD < 50.00
               MOVE 50.00 TO LBFEE-FLOAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBFEE-FLOAT-YTD > 5000.00
               MOVE 5000.00 TO LBFEE-FLOAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN OPEN DATE IS PAST
      * JM -> WW WHEN OPEN DATE IS PAST
      * WW -> XE WHEN OPEN DATE IS PAST
      * XE -> DX WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-LOCKBOX.
           EVALUATE LBFEE-LOCKBOX-CD
               WHEN 'DX'
                   IF LBFEE-OPEN-DATE > 0 AND LBFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBFEE-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBFEE-OPEN-DATE > 0 AND LBFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBFEE-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBFEE-OPEN-DATE > 0 AND LBFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBFEE-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBFEE-OPEN-DATE > 0 AND LBFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBFEE-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FLOAT YTD.
      *----------------------------------------------------------------*
       2540-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBFEE-FLOAT-YTD ROUNDED =
                   LBFEE-DEPOSIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBFEE-FLOAT-YTD
           END-IF
           ADD LBFEE-FLOAT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2550-COUNT-ITEM.
           IF LBFEE-ITEM-CNT < 9999999
               ADD 1 TO LBFEE-ITEM-CNT
           END-IF
           IF LBFEE-ITEM-CNT NOT < 10
               MOVE 'Y' TO LBFEE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2560-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBFEE-REMIT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBFEE-REMIT-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO LBFEE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBFEE-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBFEE-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E365 ADDR REQUIRED
      * E456 FLOAT AMT NEGATIVE
      * E099 FLOAT AMT OVER 50000
      * E882 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2590-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBFEE-ADDR = SPACES
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN LBFEE-FLOAT-AMT < 0
                   MOVE 'E456' TO WS-REJECT-CODE
               WHEN LBFEE-FLOAT-AMT > 50000.00
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN LBFEE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E882' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2600-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = LBFEE-EXCEPT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = LBFEE-EXCEPT-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHECK TIER TIER FOR THE EXCEPT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2610-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CHECK-TIER-RATE(1) TO LBFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF LBFEE-EXCEPT-AMT NOT < WS-CHECK-TIER-LOW(WS-TX)
                   MOVE WS-CHECK-TIER-RATE(WS-TX) TO LBFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2620-FLAG-DEPOSIT.
           IF LBFEE-DEPOSIT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBFEE-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT YTD BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2630-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBFEE-LOCKBOX-CD
                   ADD LBFEE-EXCEPT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBFEE-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBFEE-EXCEPT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2640-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN LBFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO LBFEE-DEPOSIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBFEE-STOP-FLAG = 'Y'
                   COMPUTE LBFEE-DEPOSIT-YTD ROUNDED = WS-WORK-AMT *
                       LBFEE-PENALTY-PCT / 200
               WHEN LBFEE-BATCH-TYPE-CD = 'C'
                   MOVE 15.00 TO LBFEE-DEPOSIT-YTD
               WHEN LBFEE-BATCH-TYPE-CD = 'D'
                   MOVE 3.00 TO LBFEE-DEPOSIT-YTD
               WHEN OTHER
                   COMPUTE LBFEE-DEPOSIT-YTD ROUNDED = WS-WORK-AMT *
                       LBFEE-PENALTY-PCT / 100
           END-EVALUATE
           IF LBFEE-DEPOSIT-YTD > 0 AND LBFEE-DEPOSIT-YTD < 5.00
               MOVE 5.00 TO LBFEE-DEPOSIT-YTD
           END-IF
           IF LBFEE-DEPOSIT-YTD > 150.00
               MOVE 150.00 TO LBFEE-DEPOSIT-YTD
           END-IF
           ADD LBFEE-DEPOSIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CHECK TIER TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2650-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CHECK-TIER-RATE(1) TO LBFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF LBFEE-REMIT-YTD NOT < WS-CHECK-TIER-LOW(WS-TX)
                   MOVE WS-CHECK-TIER-RATE(WS-TX) TO LBFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT YTD FEE ON THE FLOAT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2660-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN LBFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO LBFEE-FLOAT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE LBFEE-FLOAT-YTD ROUNDED = LBFEE-FLOAT-AMT *
                       LBFEE-PENALTY-PCT / 200
               WHEN LBFEE-REASON-CD = 'GF'
                   MOVE 12.00 TO LBFEE-FLOAT-YTD
               WHEN LBFEE-REASON-CD = 'UK'
                   MOVE 12.00 TO LBFEE-FLOAT-YTD
               WHEN OTHER
                   COMPUTE LBFEE-FLOAT-YTD ROUNDED = LBFEE-FLOAT-AMT *
                       LBFEE-PENALTY-PCT / 100
           END-EVALUATE
           IF LBFEE-FLOAT-YTD > 0 AND LBFEE-FLOAT-YTD < 1.00
               MOVE 1.00 TO LBFEE-FLOAT-YTD
           END-IF
           IF LBFEE-FLOAT-YTD > 250.00
               MOVE 250.00 TO LBFEE-FLOAT-YTD
           END-IF
           ADD LBFEE-FLOAT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2670-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO LBFEE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2680-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * LBFEE-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN CLOSE DATE IS PAST
      * JM -> WW WHEN CLOSE DATE IS PAST
      * WW -> XE WHEN CLOSE DATE IS PAST
      * XE -> DX WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2690-SET-LOCKBOX.
           EVALUATE LBFEE-LOCKBOX-CD
               WHEN 'DX'
                   IF LBFEE-CLOSE-DATE > 0 AND LBFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBFEE-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBFEE-CLOSE-DATE > 0 AND LBFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBFEE-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBFEE-CLOSE-DATE > 0 AND LBFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBFEE-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBFEE-CLOSE-DATE > 0 AND LBFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBFEE-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2700-COUNT-PAYMENT.
           IF LBFEE-PAYMENT-CNT < 9999999
               ADD 1 TO LBFEE-PAYMENT-CNT
           END-IF
           IF LBFEE-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO LBFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2710-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBFEE-REASON-CD
                   ADD LBFEE-EXCEPT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBFEE-EXCEPT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHECK TIER TIER FOR THE FLOAT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2720-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CHECK-TIER-RATE(1) TO LBFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF LBFEE-FLOAT-YTD NOT < WS-CHECK-TIER-LOW(WS-TX)
                   MOVE WS-CHECK-TIER-RATE(WS-TX) TO LBFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2730-ACCRUE-DEPOSIT.
           IF LBFEE-DEPOSIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBFEE-DEPOSIT-YTD * LBFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBFEE-REMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON BAND TIER FOR THE FLOAT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2740-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-BAND-RATE(1) TO LBFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBFEE-FLOAT-YTD NOT < WS-CASH-ON-BAND-LOW(WS-TX)
                   MOVE WS-CASH-ON-BAND-RATE(WS-TX) TO LBFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FLOAT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2750-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN LBFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = LBFEE-FLOAT-YTD *
                       LBFEE-DISCOUNT / 200
               WHEN LBFEE-BATCH-TYPE-CD = 'C'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN LBFEE-BATCH-TYPE-CD = 'D'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = LBFEE-FLOAT-YTD *
                       LBFEE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REMIT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2760-FLAG-REMIT.
           IF LBFEE-REMIT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBFEE-REMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E888 ADDR REQUIRED
      * E872 WORK AMT NEGATIVE
      * E260 WORK AMT OVER 250000
      * E485 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2770-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBFEE-ADDR = SPACES
                   MOVE 'E888' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN LBFEE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E485' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2780-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBFEE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBFEE-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = LBFEE-DEPOSIT-YTD *
                       LBFEE-DISCOUNT / 200
               WHEN LBFEE-BATCH-TYPE-CD = 'C'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN LBFEE-BATCH-TYPE-CD = 'D'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = LBFEE-DEPOSIT-YTD *
                       LBFEE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2790-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO LBFEE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2800-COUNT-PAYMENT.
           IF LBFEE-PAYMENT-CNT < 9999999
               ADD 1 TO LBFEE-PAYMENT-CNT
           END-IF
           IF LBFEE-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO LBFEE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REMIT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2810-FLAG-REMIT.
           IF LBFEE-REMIT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBFEE-REMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON BAND TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2820-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-BAND-RATE(1) TO LBFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBFEE-REMIT-YTD NOT < WS-CASH-ON-BAND-LOW(WS-TX)
                   MOVE WS-CASH-ON-BAND-RATE(WS-TX) TO LBFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2830-FLAG-FLOAT.
           IF LBFEE-FLOAT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBFEE-FLOAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2840-FLAG-DEPOSIT.
           IF LBFEE-DEPOSIT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBFEE-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2850-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-BAND-RATE(1) TO LBFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-CASH-ON-BAND-LOW(WS-TX)
                   MOVE WS-CASH-ON-BAND-RATE(WS-TX) TO LBFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCEPT AMT.
      *----------------------------------------------------------------*
       2860-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBFEE-EXCEPT-AMT ROUNDED =
                   LBFEE-FLOAT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBFEE-EXCEPT-AMT
           END-IF
           ADD LBFEE-EXCEPT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2870-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBFEE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO LBFEE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO LBFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO LBFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO LBFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'LBFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
