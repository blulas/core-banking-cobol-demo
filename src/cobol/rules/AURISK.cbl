      *================================================================*
      * PROGRAM  : AURISK                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DEALER AUTO FINANCE RULE - ELIGIBILITY. CALLED BY   *
      *            THE DEALER AUTO FINANCE BATCH AND ONLINE PROGRAMS.  *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-01-18 CRP  ORIGINAL                                       *
      * 1994-05-21 MKL  AUDIT FINDING - REBATE CONTROL TOTALS INC5703  *
      * 1999-06-18 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-04-27 RJT  GAP LIMIT REVISED CHG1675                      *
      * 2000-05-11 SKN  AMOUNT-FIN CUTOFF MOVED INC1337                *
      * 2000-09-21 GAF  REBATE LIMIT REVISED CHG8753                   *
      * 2001-05-12 BHS  AMOUNT-FIN FEE SCHEDULE UPDATE REQ3060         *
      * 2006-12-20 LMB  CHARGEBK LIMIT REVISED PRB8023                 *
      * 2006-10-01 DPS  AUDIT FINDING - AMOUNT-FIN CONTROL TOTALS REQ90*
      * 2008-08-28 TWH  GAP NOTICE TEXT PER LEGAL INC5955              *
      * 2010-04-01 GAF  RESERVE EXCEPTIONS TO OPS QUEUE PRB5402        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AURISK.
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
                                           VALUE 24.000000.
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
      * WS-CHARGEBK-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CHARGEBK-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000328500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000284000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000595000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000513000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000455500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000381500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000068000'.
       01  WS-CHARGEBK-SCHED-TBL REDEFINES WS-CHARGEBK-SCHED-VALUES.
           05  WS-CHARGEBK-SCHED-ROW OCCURS 7 TIMES.
               10  WS-CHARGEBK-SCHED-LOW   PIC 9(09)V99.
               10  WS-CHARGEBK-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000356000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000110000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000217000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005150000000404500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005200000000636000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005700000000768000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015700000000837000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016700000000788000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021700000000307500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024200000000561000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024250000000077000'.
       01  WS-PAYMENT-GRID-TBL REDEFINES WS-PAYMENT-GRID-VALUES.
           05  WS-PAYMENT-GRID-ROW OCCURS 11 TIMES.
               10  WS-PAYMENT-GRID-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REBATE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REBATE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000341000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000312000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000188500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000593500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000497000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018100000000153000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023100000000142000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023350000000147500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024350000000242000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034350000000435000'.
       01  WS-REBATE-GRID-TBL REDEFINES WS-REBATE-GRID-VALUES.
           05  WS-REBATE-GRID-ROW OCCURS 10 TIMES.
               10  WS-REBATE-GRID-LOW      PIC 9(09)V99.
               10  WS-REBATE-GRID-RATE     PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY AURISKP.

       PROCEDURE DIVISION USING AURISK-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-LAST-STMT
           PERFORM 2010-DESCRIBE-STATUS
           PERFORM 2020-ACCRUE-RESERVE
           PERFORM 2030-EDIT-REF-NO
           PERFORM 2040-ACCRUE-RESERVE
           PERFORM 2050-EDIT-REF-NO
           PERFORM 2060-PRORATE-RESERVE
           PERFORM 2070-COUNT-PAYMENT
           PERFORM 2080-FLAG-PAYMENT
           PERFORM 2090-EDIT-CONTACT
           PERFORM 2100-EDIT-CONTACT
           PERFORM 2110-SELECT-RATE
           PERFORM 2120-EDIT-REF-NO
           PERFORM 2130-EDIT-CONTACT
           PERFORM 2140-AGE-LAST-ACT
           PERFORM 2150-COUNT-PAYMENT
           PERFORM 2160-DESCRIBE-STATUS
           PERFORM 2170-PRORATE-GAP
           PERFORM 2180-FLAG-RESERVE
           PERFORM 2190-FLAG-GAP
           PERFORM 2200-SELECT-RATE
           PERFORM 2210-EDIT-CONTACT
           PERFORM 2220-PRORATE-GAP
           PERFORM 2230-LIMIT-AMOUNT-FIN
           PERFORM 2240-DESCRIBE-CYCLE
           PERFORM 2250-FLAG-CHARGEBK
           PERFORM 2260-ACCUM-CYCLE
           PERFORM 2270-AGE-EFFECTIVE
           PERFORM 2280-DESCRIBE-CYCLE
           PERFORM 2290-EDIT-CONTACT
           PERFORM 2300-ACCUM-TIER
           PERFORM 2310-PRORATE-REBATE
           PERFORM 2320-AGE-EFFECTIVE
           PERFORM 2330-ASSESS-EXEMPT
           PERFORM 2340-SELECT-MARGIN
           PERFORM 2350-CHECK-LAST-STMT
           PERFORM 2360-DESCRIBE-DEALER
           PERFORM 2370-ASSESS-EXEMPT
           PERFORM 2380-LIMIT-CHARGEBK
           PERFORM 2390-ASSESS-REVIEW
           PERFORM 2400-PRORATE-AMOUNT-FIN
           PERFORM 2410-ASSESS-PREFERRED
           PERFORM 2420-ACCRUE-PAYMENT
           PERFORM 2430-AGE-EFFECTIVE
           PERFORM 2440-PRORATE-CHARGEBK
           PERFORM 2450-COUNT-PAYMENT
           PERFORM 2460-ASSESS-REVIEW
           PERFORM 2470-COUNT-PAYMENT
           PERFORM 2480-SET-DEALER
           PERFORM 2490-DESCRIBE-CYCLE
           PERFORM 2500-ACCRUE-RESERVE
           PERFORM 2510-SPLIT-REBATE
           PERFORM 2520-AGE-LAST-ACT
           PERFORM 2530-SET-TIER
           PERFORM 2540-COUNT-RETRY
           PERFORM 2550-EDIT-REF-NO
           PERFORM 2560-FLAG-ITEM
           PERFORM 2570-ASSESS-EXEMPT
           PERFORM 2580-LIMIT-AMOUNT-FIN
           PERFORM 2590-ASSESS-EXEMPT
           PERFORM 2600-FLAG-ITEM
           PERFORM 2610-ACCUM-DEALER
           PERFORM 2620-EDIT-REF-NO
           PERFORM 2630-ACCRUE-RATE
           PERFORM 2640-CHECK-NEXT-REVIEW
           PERFORM 2650-ASSESS-PREFERRED
           PERFORM 2660-CHECK-NEXT-REVIEW
           PERFORM 2670-AGE-LAST-STMT
           PERFORM 2680-COUNT-RETRY
           PERFORM 2690-PRORATE-ITEM
           PERFORM 2700-ACCUM-DEALER
           PERFORM 2710-SELECT-MARGIN
           PERFORM 2720-LIMIT-ITEM
           PERFORM 2730-SELECT-RATE
           PERFORM 2740-COUNT-RETRY
           PERFORM 2750-LIMIT-ITEM
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO AURISK-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF AURISK-LAST-STMT-DATE > 0 AND AURISK-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AURISK-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2010-DESCRIBE-STATUS.
           EVALUATE AURISK-STATUS-CD
               WHEN 'EUA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HMW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RCB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-RESERVE.
           IF AURISK-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AURISK-RESERVE-AMT * AURISK-RATE / 100 / 360
               ADD WS-WORK-AMT TO AURISK-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E317 REF NO REQUIRED
      * E227 GAP AMT NEGATIVE
      * E842 GAP AMT OVER 1000000
      * E073 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2030-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AURISK-REF-NO = SPACES
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN AURISK-GAP-AMT < 0
                   MOVE 'E227' TO WS-REJECT-CODE
               WHEN AURISK-GAP-AMT > 1000000.00
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN AURISK-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2040-ACCRUE-RESERVE.
           IF AURISK-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AURISK-RESERVE-AMT * AURISK-RATE / 100 / 360
               ADD WS-WORK-AMT TO AURISK-AMOUNT-FIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E061 REF NO REQUIRED
      * E842 WORK AMT NEGATIVE
      * E547 WORK AMT OVER 5000
      * E828 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AURISK-REF-NO = SPACES
                   MOVE 'E061' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E547' TO WS-REJECT-CODE
               WHEN AURISK-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E828' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REBATE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESERVE AMT.
      *----------------------------------------------------------------*
       2060-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AURISK-RESERVE-AMT ROUNDED =
                   AURISK-REBATE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO AURISK-RESERVE-AMT
           END-IF
           ADD AURISK-RESERVE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2070-COUNT-PAYMENT.
           IF AURISK-PAYMENT-CNT < 9999999
               ADD 1 TO AURISK-PAYMENT-CNT
           END-IF
           IF AURISK-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO AURISK-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2080-FLAG-PAYMENT.
           IF AURISK-PAYMENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE AURISK-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E353 CONTACT REQUIRED
      * E094 GAP AMT NEGATIVE
      * E421 GAP AMT OVER 25000
      * E237 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2090-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AURISK-CONTACT = SPACES
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN AURISK-GAP-AMT < 0
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN AURISK-GAP-AMT > 25000.00
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN AURISK-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E237' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E826 CONTACT REQUIRED
      * E350 AMOUNT FIN AMT NEGATIVE
      * E514 AMOUNT FIN AMT OVER 50000
      * E498 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AURISK-CONTACT = SPACES
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN AURISK-AMOUNT-FIN-AMT < 0
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN AURISK-AMOUNT-FIN-AMT > 50000.00
                   MOVE 'E514' TO WS-REJECT-CODE
               WHEN AURISK-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E498' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2110-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO AURISK-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AURISK-REBATE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO AURISK-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AURISK-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AURISK-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E387 REF NO REQUIRED
      * E019 GAP AMT NEGATIVE
      * E793 GAP AMT OVER 5000
      * E081 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2120-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AURISK-REF-NO = SPACES
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN AURISK-GAP-AMT < 0
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN AURISK-GAP-AMT > 5000.00
                   MOVE 'E793' TO WS-REJECT-CODE
               WHEN AURISK-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E944 CONTACT REQUIRED
      * E557 WORK AMT 2 NEGATIVE
      * E962 WORK AMT 2 OVER 5000
      * E869 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AURISK-CONTACT = SPACES
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E557' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN AURISK-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF AURISK-LAST-ACT-DATE > 0 AND AURISK-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AURISK-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AURISK-CHARGEBK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD AURISK-CHARGEBK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD AURISK-CHARGEBK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD AURISK-CHARGEBK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AURISK-CHARGEBK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2150-COUNT-PAYMENT.
           IF AURISK-PAYMENT-CNT < 9999999
               ADD 1 TO AURISK-PAYMENT-CNT
           END-IF
           IF AURISK-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO AURISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2160-DESCRIBE-STATUS.
           EVALUATE AURISK-STATUS-CD
               WHEN 'EUA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HMW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RCB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE GAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2170-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AURISK-GAP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESERVE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2180-FLAG-RESERVE.
           IF AURISK-RESERVE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE AURISK-RESERVE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * GAP AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2190-FLAG-GAP.
           IF AURISK-GAP-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE AURISK-GAP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REBATE GRID TIER FOR THE CHARGEBK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-GRID-RATE(1) TO AURISK-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF AURISK-CHARGEBK-AMT NOT < WS-REBATE-GRID-LOW(WS-TX)
                   MOVE WS-REBATE-GRID-RATE(WS-TX) TO AURISK-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AURISK-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AURISK-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E136 CONTACT REQUIRED
      * E767 WORK AMT NEGATIVE
      * E333 WORK AMT OVER 99999
      * E381 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2210-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AURISK-CONTACT = SPACES
                   MOVE 'E136' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E333' TO WS-REJECT-CODE
               WHEN AURISK-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E381' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REBATE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GAP AMT.
      *----------------------------------------------------------------*
       2220-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AURISK-GAP-AMT ROUNDED =
                   AURISK-REBATE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO AURISK-GAP-AMT
           END-IF
           ADD AURISK-GAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2230-LIMIT-AMOUNT-FIN.
           IF AURISK-AMOUNT-FIN-AMT < 50.00
               MOVE 50.00 TO AURISK-AMOUNT-FIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AURISK-AMOUNT-FIN-AMT > 2500.00
               MOVE 2500.00 TO AURISK-AMOUNT-FIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2240-DESCRIBE-CYCLE.
           EVALUATE AURISK-CYCLE-CD
               WHEN 'A'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'J'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHARGEBK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2250-FLAG-CHARGEBK.
           IF AURISK-CHARGEBK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE AURISK-CHARGEBK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2260-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AURISK-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AURISK-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF AURISK-EFFECTIVE-DATE > 0 AND AURISK-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AURISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2280-DESCRIBE-CYCLE.
           EVALUATE AURISK-CYCLE-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'J'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E059 CONTACT REQUIRED
      * E481 RESERVE AMT NEGATIVE
      * E011 RESERVE AMT OVER 25000
      * E659 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2290-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AURISK-CONTACT = SPACES
                   MOVE 'E059' TO WS-REJECT-CODE
               WHEN AURISK-RESERVE-AMT < 0
                   MOVE 'E481' TO WS-REJECT-CODE
               WHEN AURISK-RESERVE-AMT > 25000.00
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN AURISK-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E659' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2300-ACCUM-TIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AURISK-TIER-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AURISK-TIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REBATE AMT.
      *----------------------------------------------------------------*
       2310-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AURISK-REBATE-AMT ROUNDED =
                   AURISK-AMOUNT-FIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO AURISK-REBATE-AMT
           END-IF
           ADD AURISK-REBATE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2320-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF AURISK-EFFECTIVE-DATE > 0 AND AURISK-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AURISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE AMOUNT FIN AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2330-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN AURISK-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AURISK-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = AURISK-AMOUNT-FIN-AMT *
                       AURISK-RATE / 200
               WHEN AURISK-DEALER-CD = 'DRU'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN AURISK-DEALER-CD = 'FNF'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = AURISK-AMOUNT-FIN-AMT *
                       AURISK-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE RESERVE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2340-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO AURISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF AURISK-RESERVE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO AURISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AURISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AURISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF AURISK-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AURISK-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO AURISK-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEALER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2360-DESCRIBE-DEALER.
           EVALUATE AURISK-DEALER-CD
               WHEN 'DRU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FNF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GBX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GTU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JNE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KTP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE GAP AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2370-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN AURISK-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AURISK-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AURISK-GAP-AMT *
                       AURISK-RATE / 200
               WHEN AURISK-STATUS-CD = 'EUA'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN AURISK-STATUS-CD = 'HMW'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AURISK-GAP-AMT *
                       AURISK-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2380-LIMIT-CHARGEBK.
           IF AURISK-CHARGEBK-AMT < 25.00
               MOVE 25.00 TO AURISK-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AURISK-CHARGEBK-AMT > 50000.00
               MOVE 50000.00 TO AURISK-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE GAP AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2390-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN AURISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AURISK-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AURISK-GAP-AMT *
                       AURISK-RATE / 200
               WHEN AURISK-STATUS-CD = 'EUA'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN AURISK-STATUS-CD = 'HMW'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = AURISK-GAP-AMT *
                       AURISK-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT FIN AMT.
      *----------------------------------------------------------------*
       2400-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AURISK-AMOUNT-FIN-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO AURISK-AMOUNT-FIN-AMT
           END-IF
           ADD AURISK-AMOUNT-FIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2410-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN AURISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO AURISK-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AURISK-REVIEW-FLAG = 'Y'
                   COMPUTE AURISK-PAYMENT-AMT ROUNDED = WS-WORK-AMT *
                       AURISK-RATE / 200
               WHEN AURISK-STATUS-CD = 'EUA'
                   MOVE 20.00 TO AURISK-PAYMENT-AMT
               WHEN AURISK-STATUS-CD = 'HMW'
                   MOVE 10.00 TO AURISK-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE AURISK-PAYMENT-AMT ROUNDED = WS-WORK-AMT *
                       AURISK-RATE / 100
           END-EVALUATE
           IF AURISK-PAYMENT-AMT > 0 AND AURISK-PAYMENT-AMT < 10.00
               MOVE 10.00 TO AURISK-PAYMENT-AMT
           END-IF
           IF AURISK-PAYMENT-AMT > 100.00
               MOVE 100.00 TO AURISK-PAYMENT-AMT
           END-IF
           ADD AURISK-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2420-ACCRUE-PAYMENT.
           IF AURISK-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AURISK-PAYMENT-AMT * AURISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO AURISK-GAP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2430-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF AURISK-EFFECTIVE-DATE > 0 AND AURISK-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AURISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE GAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHARGEBK AMT.
      *----------------------------------------------------------------*
       2440-PRORATE-CHARGEBK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE AURISK-CHARGEBK-AMT ROUNDED =
                   AURISK-GAP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO AURISK-CHARGEBK-AMT
           END-IF
           ADD AURISK-CHARGEBK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2450-COUNT-PAYMENT.
           IF AURISK-PAYMENT-CNT < 9999999
               ADD 1 TO AURISK-PAYMENT-CNT
           END-IF
           IF AURISK-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO AURISK-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REBATE FEE ON THE RESERVE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2460-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN AURISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO AURISK-REBATE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AURISK-PREFERRED-FLAG = 'Y'
                   COMPUTE AURISK-REBATE-AMT ROUNDED =
                       AURISK-RESERVE-AMT * AURISK-RATE / 200
               WHEN AURISK-TIER-CD = 'EW'
                   MOVE 20.00 TO AURISK-REBATE-AMT
               WHEN AURISK-TIER-CD = 'JF'
                   MOVE 20.00 TO AURISK-REBATE-AMT
               WHEN OTHER
                   COMPUTE AURISK-REBATE-AMT ROUNDED =
                       AURISK-RESERVE-AMT * AURISK-RATE / 100
           END-EVALUATE
           IF AURISK-REBATE-AMT > 0 AND AURISK-REBATE-AMT < 5.00
               MOVE 5.00 TO AURISK-REBATE-AMT
           END-IF
           IF AURISK-REBATE-AMT > 35.00
               MOVE 35.00 TO AURISK-REBATE-AMT
           END-IF
           ADD AURISK-REBATE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2470-COUNT-PAYMENT.
           IF AURISK-PAYMENT-CNT < 9999999
               ADD 1 TO AURISK-PAYMENT-CNT
           END-IF
           IF AURISK-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO AURISK-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN LAST STMT DATE IS PAST
      * FNF -> GBX WHEN LAST STMT DATE IS PAST
      * GBX -> GTU WHEN LAST STMT DATE IS PAST
      * GTU -> JNE WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2480-SET-DEALER.
           EVALUATE AURISK-DEALER-CD
               WHEN 'DRU'
                   IF AURISK-LAST-STMT-DATE > 0 AND
                       AURISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FNF' TO AURISK-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF AURISK-LAST-STMT-DATE > 0 AND
                       AURISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GBX' TO AURISK-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF AURISK-LAST-STMT-DATE > 0 AND
                       AURISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GTU' TO AURISK-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF AURISK-LAST-STMT-DATE > 0 AND
                       AURISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JNE' TO AURISK-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2490-DESCRIBE-CYCLE.
           EVALUATE AURISK-CYCLE-CD
               WHEN 'A'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'J'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-RESERVE.
           IF AURISK-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   AURISK-RESERVE-AMT * AURISK-RATE / 100 / 365
               ADD WS-WORK-AMT TO AURISK-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REBATE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = AURISK-REBATE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = AURISK-REBATE-AMT - (WS-WORK-AMT * 4)
           COMPUTE AURISK-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2520-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF AURISK-LAST-ACT-DATE > 0 AND AURISK-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AURISK-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AURISK-REBATE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN EFFECTIVE DATE IS PAST
      * JF -> KD WHEN EFFECTIVE DATE IS PAST
      * KD -> UG WHEN EFFECTIVE DATE IS PAST
      * UG -> WY WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-TIER.
           EVALUATE AURISK-TIER-CD
               WHEN 'EW'
                   IF AURISK-EFFECTIVE-DATE > 0 AND
                       AURISK-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'JF' TO AURISK-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF AURISK-EFFECTIVE-DATE > 0 AND
                       AURISK-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'KD' TO AURISK-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF AURISK-EFFECTIVE-DATE > 0 AND
                       AURISK-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'UG' TO AURISK-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF AURISK-EFFECTIVE-DATE > 0 AND
                       AURISK-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'WY' TO AURISK-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-RETRY.
           IF AURISK-RETRY-CNT < 9999999
               ADD 1 TO AURISK-RETRY-CNT
           END-IF
           IF AURISK-RETRY-CNT NOT < 6
               MOVE 'Y' TO AURISK-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E593 REF NO REQUIRED
      * E727 REBATE AMT NEGATIVE
      * E888 REBATE AMT OVER 5000
      * E737 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2550-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AURISK-REF-NO = SPACES
                   MOVE 'E593' TO WS-REJECT-CODE
               WHEN AURISK-REBATE-AMT < 0
                   MOVE 'E727' TO WS-REJECT-CODE
               WHEN AURISK-REBATE-AMT > 5000.00
                   MOVE 'E888' TO WS-REJECT-CODE
               WHEN AURISK-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E737' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2560-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT FIN FEE ON THE REBATE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2570-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN AURISK-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO AURISK-AMOUNT-FIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AURISK-PREFERRED-FLAG = 'Y'
                   COMPUTE AURISK-AMOUNT-FIN-AMT ROUNDED =
                       AURISK-REBATE-AMT * AURISK-MARGIN / 200
               WHEN AURISK-STATUS-CD = 'EUA'
                   MOVE 3.00 TO AURISK-AMOUNT-FIN-AMT
               WHEN AURISK-STATUS-CD = 'HMW'
                   MOVE 7.00 TO AURISK-AMOUNT-FIN-AMT
               WHEN OTHER
                   COMPUTE AURISK-AMOUNT-FIN-AMT ROUNDED =
                       AURISK-REBATE-AMT * AURISK-MARGIN / 100
           END-EVALUATE
           IF AURISK-AMOUNT-FIN-AMT > 0 AND AURISK-AMOUNT-FIN-AMT < 2.00
               MOVE 2.00 TO AURISK-AMOUNT-FIN-AMT
           END-IF
           IF AURISK-AMOUNT-FIN-AMT > 250.00
               MOVE 250.00 TO AURISK-AMOUNT-FIN-AMT
           END-IF
           ADD AURISK-AMOUNT-FIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2580-LIMIT-AMOUNT-FIN.
           IF AURISK-AMOUNT-FIN-AMT < 25.00
               MOVE 25.00 TO AURISK-AMOUNT-FIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF AURISK-AMOUNT-FIN-AMT > 500.00
               MOVE 500.00 TO AURISK-AMOUNT-FIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE AMOUNT FIN AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2590-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN AURISK-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO AURISK-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AURISK-PREFERRED-FLAG = 'Y'
                   COMPUTE AURISK-PAYMENT-AMT ROUNDED =
                       AURISK-AMOUNT-FIN-AMT * AURISK-RATE / 200
               WHEN AURISK-STATUS-CD = 'EUA'
                   MOVE 3.00 TO AURISK-PAYMENT-AMT
               WHEN AURISK-STATUS-CD = 'HMW'
                   MOVE 7.00 TO AURISK-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE AURISK-PAYMENT-AMT ROUNDED =
                       AURISK-AMOUNT-FIN-AMT * AURISK-RATE / 100
           END-EVALUATE
           IF AURISK-PAYMENT-AMT > 0 AND AURISK-PAYMENT-AMT < 10.00
               MOVE 10.00 TO AURISK-PAYMENT-AMT
           END-IF
           IF AURISK-PAYMENT-AMT > 150.00
               MOVE 150.00 TO AURISK-PAYMENT-AMT
           END-IF
           ADD AURISK-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2600-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGEBK AMT BY DEALER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2610-ACCUM-DEALER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AURISK-DEALER-CD
                   ADD AURISK-CHARGEBK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AURISK-DEALER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AURISK-CHARGEBK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E727 REF NO REQUIRED
      * E883 GAP AMT NEGATIVE
      * E412 GAP AMT OVER 25000
      * E518 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2620-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN AURISK-REF-NO = SPACES
                   MOVE 'E727' TO WS-REJECT-CODE
               WHEN AURISK-GAP-AMT < 0
                   MOVE 'E883' TO WS-REJECT-CODE
               WHEN AURISK-GAP-AMT > 25000.00
                   MOVE 'E412' TO WS-REJECT-CODE
               WHEN AURISK-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2630-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * AURISK-RATE / 100 / 360
               ADD WS-WORK-AMT TO AURISK-CHARGEBK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2640-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF AURISK-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AURISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO AURISK-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2650-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN AURISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN AURISK-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       AURISK-MARGIN / 200
               WHEN AURISK-TIER-CD = 'EW'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN AURISK-TIER-CD = 'JF'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       AURISK-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2660-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF AURISK-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AURISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO AURISK-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2670-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF AURISK-LAST-STMT-DATE > 0 AND AURISK-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE AURISK-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD AURISK-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2680-COUNT-RETRY.
           IF AURISK-RETRY-CNT < 9999999
               ADD 1 TO AURISK-RETRY-CNT
           END-IF
           IF AURISK-RETRY-CNT NOT < 6
               MOVE 'Y' TO AURISK-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2690-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE GAP AMT BY DEALER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2700-ACCUM-DEALER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = AURISK-DEALER-CD
                   ADD AURISK-GAP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE AURISK-DEALER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE AURISK-GAP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REBATE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2710-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-GRID-RATE(1) TO AURISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-REBATE-GRID-LOW(WS-TX)
                   MOVE WS-REBATE-GRID-RATE(WS-TX) TO AURISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AURISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AURISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2720-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2730-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO AURISK-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO AURISK-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF AURISK-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO AURISK-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2740-COUNT-RETRY.
           IF AURISK-RETRY-CNT < 9999999
               ADD 1 TO AURISK-RETRY-CNT
           END-IF
           IF AURISK-RETRY-CNT NOT < 6
               MOVE 'Y' TO AURISK-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2750-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO AURISK-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO AURISK-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO AURISK-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'AURISK EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
