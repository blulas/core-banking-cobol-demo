      *================================================================*
      * PROGRAM  : MFLIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MICROFICHE STATEMENTS RULE - ELIGIBILITY. CALLED BY *
      *            THE MICROFICHE STATEMENTS BATCH AND ONLINE PROGRAMS.*
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-04-10 LMB  ORIGINAL                                       *
      * 1998-07-23 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-16 LMB  AUDIT FINDING - RESEARCH CONTROL TOTALS CHG9496*
      * 2001-03-19 MKL  COPY-FEE NOTICE TEXT PER LEGAL INC1705         *
      * 2002-05-07 SKN  RESEARCH EXCEPTIONS TO OPS QUEUE CHG2446       *
      * 2004-02-19 TWH  STORAGE CALCULATION CORRECTED PRB8666          *
      * 2005-01-14 DPS  RESEARCH TABLE EXPANDED PRB6039                *
      * 2008-05-16 GAF  COPY-FEE FEE SCHEDULE UPDATE CHG8876           *
      * 2008-04-12 BHS  PERFORMANCE - COPY-FEE READ AHEAD CHG0173      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFLIM.
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
                                           VALUE 14.000000.
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
      * WS-RESEARCH-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESEARCH-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000462000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000556500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000496500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000894000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000466500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014250000000632000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014750000000468000'.
       01  WS-RESEARCH-TIER-TBL REDEFINES WS-RESEARCH-TIER-VALUES.
           05  WS-RESEARCH-TIER-ROW OCCURS 7 TIMES.
               10  WS-RESEARCH-TIER-LOW    PIC 9(09)V99.
               10  WS-RESEARCH-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COPY-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COPY-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000684000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000439500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000105500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000029500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003800000000799500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006300000000266000'.
       01  WS-COPY-FEE-SCHED-TBL REDEFINES WS-COPY-FEE-SCHED-VALUES.
           05  WS-COPY-FEE-SCHED-ROW OCCURS 6 TIMES.
               10  WS-COPY-FEE-SCHED-LOW   PIC 9(09)V99.
               10  WS-COPY-FEE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESEARCH-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESEARCH-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000218000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000077000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000175500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000429500'.
       01  WS-RESEARCH-GRID-TBL REDEFINES WS-RESEARCH-GRID-VALUES.
           05  WS-RESEARCH-GRID-ROW OCCURS 4 TIMES.
               10  WS-RESEARCH-GRID-LOW    PIC 9(09)V99.
               10  WS-RESEARCH-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY MFLIMP.

       PROCEDURE DIVISION USING MFLIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PRORATE-POSTAGE
           PERFORM 2010-SET-RETENTION
           PERFORM 2020-COUNT-PAYMENT
           PERFORM 2030-SELECT-MARGIN
           PERFORM 2040-SELECT-DISCOUNT
           PERFORM 2050-SPLIT-POSTAGE
           PERFORM 2060-LIMIT-POSTAGE
           PERFORM 2070-ACCRUE-MARGIN
           PERFORM 2080-ACCRUE-STORAGE
           PERFORM 2090-SPLIT-STORAGE
           PERFORM 2100-ACCRUE-POSTAGE
           PERFORM 2110-CHECK-LAST-PAY
           PERFORM 2120-FLAG-POSTAGE
           PERFORM 2130-SPLIT-COPY-FEE
           PERFORM 2140-COUNT-CYCLE
           PERFORM 2150-SPLIT-POSTAGE
           PERFORM 2160-SET-CHANNEL
           PERFORM 2170-AGE-CLOSE
           PERFORM 2180-COUNT-PAYMENT
           PERFORM 2190-LIMIT-POSTAGE
           PERFORM 2200-ACCRUE-MARGIN
           PERFORM 2210-SELECT-MARGIN
           PERFORM 2220-DESCRIBE-TYPE
           PERFORM 2230-FLAG-COPY-FEE
           PERFORM 2240-SPLIT-RESEARCH
           PERFORM 2250-EDIT-CONTACT
           PERFORM 2260-CHECK-EXPIRY
           PERFORM 2270-AGE-CLOSE
           PERFORM 2280-DESCRIBE-CHANNEL
           PERFORM 2290-CHECK-LAST-PAY
           PERFORM 2300-SELECT-DISCOUNT
           PERFORM 2310-COUNT-CYCLE
           PERFORM 2320-SPLIT-POSTAGE
           PERFORM 2330-ACCUM-CHANNEL
           PERFORM 2340-SPLIT-STORAGE
           PERFORM 2350-AGE-CLOSE
           PERFORM 2360-PRORATE-POSTAGE
           PERFORM 2370-CHECK-OPEN
           PERFORM 2380-LIMIT-COPY-FEE
           PERFORM 2390-CHECK-CLOSE
           PERFORM 2400-ACCUM-BRANCH
           PERFORM 2410-EDIT-ADDR
           PERFORM 2420-DESCRIBE-TYPE
           PERFORM 2430-SET-CHANNEL
           PERFORM 2440-CHECK-LAST-PAY
           PERFORM 2450-ASSESS-STOP
           PERFORM 2460-ACCRUE-STORAGE
           PERFORM 2470-EDIT-ADDR
           PERFORM 2480-DESCRIBE-CHANNEL
           PERFORM 2490-DESCRIBE-BRANCH
           PERFORM 2500-ACCRUE-STORAGE
           PERFORM 2510-PRORATE-STORAGE
           PERFORM 2520-ACCUM-RETENTION
           PERFORM 2530-ACCRUE-POSTAGE
           PERFORM 2540-CHECK-OPEN
           PERFORM 2550-SELECT-MARGIN
           PERFORM 2560-SET-CHANNEL
           PERFORM 2570-ACCUM-BRANCH
           PERFORM 2580-ASSESS-NOTICE-SENT
           PERFORM 2590-COUNT-PAYMENT
           PERFORM 2600-SELECT-MARGIN
           PERFORM 2610-COUNT-PAYMENT
           PERFORM 2620-DESCRIBE-TYPE
           PERFORM 2630-SET-RETENTION
           PERFORM 2640-EDIT-ADDR
           PERFORM 2650-LIMIT-COPY-FEE
           PERFORM 2660-SPLIT-RESEARCH
           PERFORM 2670-SET-CHANNEL
           PERFORM 2680-SET-CHANNEL
           PERFORM 2690-SPLIT-RESEARCH
           PERFORM 2700-ACCUM-RETENTION
           PERFORM 2710-CHECK-OPEN
           PERFORM 2720-ACCRUE-STORAGE
           PERFORM 2730-SPLIT-STORAGE
           PERFORM 2740-ASSESS-REVIEW
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO MFLIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE YTD.
      *----------------------------------------------------------------*
       2000-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFLIM-POSTAGE-YTD ROUNDED =
                   MFLIM-POSTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFLIM-POSTAGE-YTD
           END-IF
           ADD MFLIM-POSTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN OPEN DATE IS PAST
      * SJ -> WV WHEN OPEN DATE IS PAST
      * WV -> KT WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2010-SET-RETENTION.
           EVALUATE MFLIM-RETENTION-CD
               WHEN 'KT'
                   IF MFLIM-OPEN-DATE > 0 AND MFLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFLIM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFLIM-OPEN-DATE > 0 AND MFLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFLIM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFLIM-OPEN-DATE > 0 AND MFLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFLIM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2020-COUNT-PAYMENT.
           IF MFLIM-PAYMENT-CNT < 9999999
               ADD 1 TO MFLIM-PAYMENT-CNT
           END-IF
           IF MFLIM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO MFLIM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2030-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-GRID-RATE(1) TO MFLIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-RESEARCH-GRID-LOW(WS-TX)
                   MOVE WS-RESEARCH-GRID-RATE(WS-TX) TO MFLIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFLIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFLIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH GRID TIER FOR THE STORAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-GRID-RATE(1) TO MFLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFLIM-STORAGE-YTD NOT < WS-RESEARCH-GRID-LOW(WS-TX)
                   MOVE WS-RESEARCH-GRID-RATE(WS-TX) TO MFLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2050-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE MFLIM-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2060-LIMIT-POSTAGE.
           IF MFLIM-POSTAGE-YTD < 25.00
               MOVE 25.00 TO MFLIM-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFLIM-POSTAGE-YTD > 5000.00
               MOVE 5000.00 TO MFLIM-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2070-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * MFLIM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2080-ACCRUE-STORAGE.
           IF MFLIM-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFLIM-STORAGE-AMT * MFLIM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFLIM-POSTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2090-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFLIM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2100-ACCRUE-POSTAGE.
           IF MFLIM-POSTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFLIM-POSTAGE-YTD * MFLIM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MFLIM-RESEARCH-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2110-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MFLIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MFLIM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2120-FLAG-POSTAGE.
           IF MFLIM-POSTAGE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFLIM-POSTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE MFLIM-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-CYCLE.
           IF MFLIM-CYCLE-CNT < 9999999
               ADD 1 TO MFLIM-CYCLE-CNT
           END-IF
           IF MFLIM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO MFLIM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2150-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFLIM-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN EXPIRY DATE IS PAST
      * GL -> HH WHEN EXPIRY DATE IS PAST
      * HH -> LM WHEN EXPIRY DATE IS PAST
      * LM -> MD WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-CHANNEL.
           EVALUATE MFLIM-CHANNEL-CD
               WHEN 'FV'
                   IF MFLIM-EXPIRY-DATE > 0 AND MFLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFLIM-EXPIRY-DATE > 0 AND MFLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFLIM-EXPIRY-DATE > 0 AND MFLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFLIM-EXPIRY-DATE > 0 AND MFLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2170-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFLIM-CLOSE-DATE > 0 AND MFLIM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFLIM-POSTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFLIM-POSTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFLIM-POSTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFLIM-POSTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFLIM-POSTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2180-COUNT-PAYMENT.
           IF MFLIM-PAYMENT-CNT < 9999999
               ADD 1 TO MFLIM-PAYMENT-CNT
           END-IF
           IF MFLIM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO MFLIM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2190-LIMIT-POSTAGE.
           IF MFLIM-POSTAGE-AMT < 100.00
               MOVE 100.00 TO MFLIM-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFLIM-POSTAGE-AMT > 5000.00
               MOVE 5000.00 TO MFLIM-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2200-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * MFLIM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFLIM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE SCHED TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2210-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-SCHED-RATE(1) TO MFLIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MFLIM-COPY-FEE-YTD NOT < WS-COPY-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-COPY-FEE-SCHED-RATE(WS-TX) TO MFLIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFLIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFLIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2220-DESCRIBE-TYPE.
           EVALUATE MFLIM-TYPE-CD
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COPY FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2230-FLAG-COPY-FEE.
           IF MFLIM-COPY-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFLIM-COPY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2240-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFLIM-RESEARCH-YTD / 12
           COMPUTE WS-WORK-AMT-2 = MFLIM-RESEARCH-YTD - (WS-WORK-AMT *
               12)
           COMPUTE MFLIM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E919 CONTACT REQUIRED
      * E577 POSTAGE AMT NEGATIVE
      * E347 POSTAGE AMT OVER 99999
      * E168 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2250-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFLIM-CONTACT = SPACES
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN MFLIM-POSTAGE-AMT < 0
                   MOVE 'E577' TO WS-REJECT-CODE
               WHEN MFLIM-POSTAGE-AMT > 99999.00
                   MOVE 'E347' TO WS-REJECT-CODE
               WHEN MFLIM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2260-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MFLIM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFLIM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MFLIM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFLIM-CLOSE-DATE > 0 AND MFLIM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFLIM-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFLIM-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFLIM-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFLIM-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFLIM-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2280-DESCRIBE-CHANNEL.
           EVALUATE MFLIM-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MFLIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO MFLIM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2300-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-GRID-RATE(1) TO MFLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-RESEARCH-GRID-LOW(WS-TX)
                   MOVE WS-RESEARCH-GRID-RATE(WS-TX) TO MFLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2310-COUNT-CYCLE.
           IF MFLIM-CYCLE-CNT < 9999999
               ADD 1 TO MFLIM-CYCLE-CNT
           END-IF
           IF MFLIM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO MFLIM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFLIM-POSTAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MFLIM-POSTAGE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE MFLIM-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2330-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFLIM-CHANNEL-CD
                   ADD MFLIM-POSTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFLIM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFLIM-POSTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2340-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFLIM-STORAGE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = MFLIM-STORAGE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE MFLIM-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2350-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFLIM-CLOSE-DATE > 0 AND MFLIM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFLIM-POSTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MFLIM-POSTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFLIM-POSTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFLIM-POSTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFLIM-POSTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE YTD.
      *----------------------------------------------------------------*
       2360-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFLIM-POSTAGE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFLIM-POSTAGE-YTD
           END-IF
           ADD MFLIM-POSTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2370-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MFLIM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MFLIM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2380-LIMIT-COPY-FEE.
           IF MFLIM-COPY-FEE-YTD < 25.00
               MOVE 25.00 TO MFLIM-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFLIM-COPY-FEE-YTD > 10000.00
               MOVE 10000.00 TO MFLIM-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2390-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFLIM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO MFLIM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2400-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFLIM-BRANCH-CD
                   ADD MFLIM-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFLIM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFLIM-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E689 ADDR REQUIRED
      * E396 STORAGE AMT NEGATIVE
      * E523 STORAGE AMT OVER 50000
      * E214 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFLIM-ADDR = SPACES
                   MOVE 'E689' TO WS-REJECT-CODE
               WHEN MFLIM-STORAGE-AMT < 0
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN MFLIM-STORAGE-AMT > 50000.00
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN MFLIM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E214' TO WS-REJECT-CODE
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
       2420-DESCRIBE-TYPE.
           EVALUATE MFLIM-TYPE-CD
               WHEN 'G'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST PAY DATE IS PAST
      * GL -> HH WHEN LAST PAY DATE IS PAST
      * HH -> LM WHEN LAST PAY DATE IS PAST
      * LM -> MD WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2430-SET-CHANNEL.
           EVALUATE MFLIM-CHANNEL-CD
               WHEN 'FV'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MFLIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO MFLIM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE POSTAGE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2450-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MFLIM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MFLIM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFLIM-REVIEW-FLAG = 'Y'
                   COMPUTE MFLIM-STORAGE-AMT ROUNDED = MFLIM-POSTAGE-AMT
                       * MFLIM-DISCOUNT / 200
               WHEN MFLIM-CHANNEL-CD = 'FV'
                   MOVE 15.00 TO MFLIM-STORAGE-AMT
               WHEN MFLIM-CHANNEL-CD = 'GL'
                   MOVE 10.00 TO MFLIM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE MFLIM-STORAGE-AMT ROUNDED = MFLIM-POSTAGE-AMT
                       * MFLIM-DISCOUNT / 100
           END-EVALUATE
           IF MFLIM-STORAGE-AMT > 0 AND MFLIM-STORAGE-AMT < 10.00
               MOVE 10.00 TO MFLIM-STORAGE-AMT
           END-IF
           IF MFLIM-STORAGE-AMT > 25.00
               MOVE 25.00 TO MFLIM-STORAGE-AMT
           END-IF
           ADD MFLIM-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-STORAGE.
           IF MFLIM-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFLIM-STORAGE-YTD * MFLIM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFLIM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E942 ADDR REQUIRED
      * E884 STORAGE YTD NEGATIVE
      * E769 STORAGE YTD OVER 1000000
      * E323 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2470-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFLIM-ADDR = SPACES
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN MFLIM-STORAGE-YTD < 0
                   MOVE 'E884' TO WS-REJECT-CODE
               WHEN MFLIM-STORAGE-YTD > 1000000.00
                   MOVE 'E769' TO WS-REJECT-CODE
               WHEN MFLIM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-CHANNEL.
           EVALUATE MFLIM-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2490-DESCRIBE-BRANCH.
           EVALUATE MFLIM-BRANCH-CD
               WHEN 'AT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'US'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-STORAGE.
           IF MFLIM-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFLIM-STORAGE-AMT * MFLIM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFLIM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE YTD.
      *----------------------------------------------------------------*
       2510-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFLIM-STORAGE-YTD ROUNDED =
                   MFLIM-POSTAGE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFLIM-STORAGE-YTD
           END-IF
           ADD MFLIM-STORAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY RETENTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2520-ACCUM-RETENTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFLIM-RETENTION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFLIM-RETENTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2530-ACCRUE-POSTAGE.
           IF MFLIM-POSTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFLIM-POSTAGE-YTD * MFLIM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MFLIM-RESEARCH-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2540-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MFLIM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO MFLIM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2550-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-GRID-RATE(1) TO MFLIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-RESEARCH-GRID-LOW(WS-TX)
                   MOVE WS-RESEARCH-GRID-RATE(WS-TX) TO MFLIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFLIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFLIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST PAY DATE IS PAST
      * GL -> HH WHEN LAST PAY DATE IS PAST
      * HH -> LM WHEN LAST PAY DATE IS PAST
      * LM -> MD WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2560-SET-CHANNEL.
           EVALUATE MFLIM-CHANNEL-CD
               WHEN 'FV'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2570-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFLIM-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFLIM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE POSTAGE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2580-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFLIM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFLIM-POSTAGE-YTD *
                       MFLIM-DISCOUNT / 200
               WHEN MFLIM-TYPE-CD = 'G'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN MFLIM-TYPE-CD = 'K'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFLIM-POSTAGE-YTD *
                       MFLIM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-PAYMENT.
           IF MFLIM-PAYMENT-CNT < 9999999
               ADD 1 TO MFLIM-PAYMENT-CNT
           END-IF
           IF MFLIM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO MFLIM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE SCHED TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2600-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-SCHED-RATE(1) TO MFLIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MFLIM-STORAGE-AMT NOT < WS-COPY-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-COPY-FEE-SCHED-RATE(WS-TX) TO MFLIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFLIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFLIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2610-COUNT-PAYMENT.
           IF MFLIM-PAYMENT-CNT < 9999999
               ADD 1 TO MFLIM-PAYMENT-CNT
           END-IF
           IF MFLIM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO MFLIM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2620-DESCRIBE-TYPE.
           EVALUATE MFLIM-TYPE-CD
               WHEN 'G'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN EXPIRY DATE IS PAST
      * SJ -> WV WHEN EXPIRY DATE IS PAST
      * WV -> KT WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2630-SET-RETENTION.
           EVALUATE MFLIM-RETENTION-CD
               WHEN 'KT'
                   IF MFLIM-EXPIRY-DATE > 0 AND MFLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFLIM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFLIM-EXPIRY-DATE > 0 AND MFLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFLIM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFLIM-EXPIRY-DATE > 0 AND MFLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFLIM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E519 ADDR REQUIRED
      * E012 RESEARCH YTD NEGATIVE
      * E726 RESEARCH YTD OVER 10000
      * E951 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2640-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFLIM-ADDR = SPACES
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN MFLIM-RESEARCH-YTD < 0
                   MOVE 'E012' TO WS-REJECT-CODE
               WHEN MFLIM-RESEARCH-YTD > 10000.00
                   MOVE 'E726' TO WS-REJECT-CODE
               WHEN MFLIM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E951' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2650-LIMIT-COPY-FEE.
           IF MFLIM-COPY-FEE-YTD < 25.00
               MOVE 25.00 TO MFLIM-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFLIM-COPY-FEE-YTD > 50000.00
               MOVE 50000.00 TO MFLIM-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2660-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE MFLIM-RESEARCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST PAY DATE IS PAST
      * GL -> HH WHEN LAST PAY DATE IS PAST
      * HH -> LM WHEN LAST PAY DATE IS PAST
      * LM -> MD WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2670-SET-CHANNEL.
           EVALUATE MFLIM-CHANNEL-CD
               WHEN 'FV'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFLIM-LAST-PAY-DATE > 0 AND MFLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN EXPIRY DATE IS PAST
      * GL -> HH WHEN EXPIRY DATE IS PAST
      * HH -> LM WHEN EXPIRY DATE IS PAST
      * LM -> MD WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2680-SET-CHANNEL.
           EVALUATE MFLIM-CHANNEL-CD
               WHEN 'FV'
                   IF MFLIM-EXPIRY-DATE > 0 AND MFLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFLIM-EXPIRY-DATE > 0 AND MFLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFLIM-EXPIRY-DATE > 0 AND MFLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFLIM-EXPIRY-DATE > 0 AND MFLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFLIM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2690-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE MFLIM-RESEARCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY RETENTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2700-ACCUM-RETENTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFLIM-RETENTION-CD
                   ADD MFLIM-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFLIM-RETENTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFLIM-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2710-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MFLIM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MFLIM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2720-ACCRUE-STORAGE.
           IF MFLIM-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFLIM-STORAGE-YTD * MFLIM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2730-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFLIM-STORAGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MFLIM-STORAGE-YTD - (WS-WORK-AMT * 4)
           COMPUTE MFLIM-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE STORAGE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2740-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MFLIM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MFLIM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFLIM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFLIM-STORAGE-AMT ROUNDED = MFLIM-STORAGE-YTD
                       * MFLIM-DISCOUNT / 200
               WHEN MFLIM-RETENTION-CD = 'KT'
                   MOVE 5.00 TO MFLIM-STORAGE-AMT
               WHEN MFLIM-RETENTION-CD = 'SJ'
                   MOVE 12.00 TO MFLIM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE MFLIM-STORAGE-AMT ROUNDED = MFLIM-STORAGE-YTD
                       * MFLIM-DISCOUNT / 100
           END-EVALUATE
           IF MFLIM-STORAGE-AMT > 0 AND MFLIM-STORAGE-AMT < 2.00
               MOVE 2.00 TO MFLIM-STORAGE-AMT
           END-IF
           IF MFLIM-STORAGE-AMT > 25.00
               MOVE 25.00 TO MFLIM-STORAGE-AMT
           END-IF
           ADD MFLIM-STORAGE-AMT TO WS-TOTAL-FEES.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO MFLIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO MFLIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO MFLIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'MFLIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
