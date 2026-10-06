      *================================================================*
      * PROGRAM  : TXPRC                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TELEX WIRE MESSAGING RULE - STATUS. CALLED BY THE   *
      *            TELEX WIRE MESSAGING BATCH AND ONLINE PROGRAMS.     *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-06-04 EWS  ORIGINAL                                       *
      * 1994-10-10 JLM  CORRESP STATUS CODES ADDED REQ7145             *
      * 1997-04-14 PQR  CHARGE NOTICE TEXT PER LEGAL PRB6636           *
      * 1998-02-11 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-11 SKN  SETTLE CUTOFF MOVED PRB1694                    *
      * 1999-10-07 EWS  WIRE CUTOFF MOVED PRB5143                      *
      * 2004-04-09 GAF  AUDIT FINDING - COVER CONTROL TOTALS CHG6354   *
      * 2006-10-16 RJT  QUEUED NOTICE TEXT PER LEGAL INC2433           *
      * 2007-06-16 CRP  AUDIT FINDING - QUEUED CONTROL TOTALS PRB1326  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXPRC.
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
      * WS-SETTLE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SETTLE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000783000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000586500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000225000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000089500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012250000000660000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012300000000586500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000767000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022550000000309000'.
       01  WS-SETTLE-TIER-TBL REDEFINES WS-SETTLE-TIER-VALUES.
           05  WS-SETTLE-TIER-ROW OCCURS 9 TIMES.
               10  WS-SETTLE-TIER-LOW      PIC 9(09)V99.
               10  WS-SETTLE-TIER-RATE     PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TXPRCP.

       PROCEDURE DIVISION USING TXPRC-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-LIMIT-QUEUED
           PERFORM 2010-COUNT-PAYMENT
           PERFORM 2020-SPLIT-QUEUED
           PERFORM 2030-SET-REASON
           PERFORM 2040-DESCRIBE-REGION
           PERFORM 2050-CHECK-NEXT-DUE
           PERFORM 2060-EDIT-NAME
           PERFORM 2070-PRORATE-COVER
           PERFORM 2080-LIMIT-ITEM
           PERFORM 2090-LIMIT-QUEUED
           PERFORM 2100-ACCRUE-COVER
           PERFORM 2110-CHECK-NEXT-DUE
           PERFORM 2120-AGE-NOTICE
           PERFORM 2130-SPLIT-QUEUED
           PERFORM 2140-CHECK-NEXT-DUE
           PERFORM 2150-DESCRIBE-TEST-KEY
           PERFORM 2160-EDIT-NAME
           PERFORM 2170-AGE-OPEN
           PERFORM 2180-SELECT-DISCOUNT
           PERFORM 2190-ACCUM-TEST-KEY
           PERFORM 2200-COUNT-EXCEPT
           PERFORM 2210-DESCRIBE-REASON
           PERFORM 2220-PRORATE-SETTLE
           PERFORM 2230-ACCUM-BRANCH
           PERFORM 2240-ACCRUE-QUEUED
           PERFORM 2250-SPLIT-COVER
           PERFORM 2260-EDIT-ADDR
           PERFORM 2270-SET-REGION
           PERFORM 2280-SPLIT-SETTLE
           PERFORM 2290-ACCUM-BRANCH
           PERFORM 2300-PRORATE-SETTLE
           PERFORM 2310-FLAG-SETTLE
           PERFORM 2320-SPLIT-SETTLE
           PERFORM 2330-ACCUM-REASON
           PERFORM 2340-SELECT-RATE
           PERFORM 2350-COUNT-EXCEPT
           PERFORM 2360-LIMIT-SETTLE
           PERFORM 2370-PRORATE-QUEUED
           PERFORM 2380-DESCRIBE-REASON
           PERFORM 2390-SELECT-RATE
           PERFORM 2400-PRORATE-ITEM
           PERFORM 2410-ASSESS-REVIEW
           PERFORM 2420-SELECT-DISCOUNT
           PERFORM 2430-ASSESS-NOTICE-SENT
           PERFORM 2440-LIMIT-ITEM
           PERFORM 2450-ACCRUE-RATE
           PERFORM 2460-DESCRIBE-BRANCH
           PERFORM 2470-COUNT-PAYMENT
           PERFORM 2480-SET-REGION
           PERFORM 2490-ACCRUE-SETTLE
           PERFORM 2500-FLAG-QUEUED
           PERFORM 2510-ASSESS-REVIEW
           PERFORM 2520-SPLIT-SETTLE
           PERFORM 2530-COUNT-PAYMENT
           PERFORM 2540-ACCUM-TEST-KEY
           PERFORM 2550-ASSESS-STOP
           PERFORM 2560-ACCUM-TEST-KEY
           PERFORM 2570-AGE-EXPIRY
           PERFORM 2580-COUNT-EXCEPT
           PERFORM 2590-PRORATE-CORRESP
           PERFORM 2600-COUNT-PAYMENT
           PERFORM 2610-PRORATE-QUEUED
           PERFORM 2620-ASSESS-STOP
           PERFORM 2630-LIMIT-QUEUED
           PERFORM 2640-CHECK-NEXT-DUE
           PERFORM 2650-ACCRUE-DISCOUNT
           PERFORM 2660-EDIT-NAME
           PERFORM 2670-FLAG-SETTLE
           PERFORM 2680-CHECK-EXPIRY
           PERFORM 2690-COUNT-EXCEPT
           PERFORM 2700-ACCRUE-QUEUED
           PERFORM 2710-FLAG-ITEM
           PERFORM 2720-LIMIT-COVER
           PERFORM 2730-EDIT-ADDR
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TXPRC-RETURN-CODE.

      *----------------------------------------------------------------*
      * QUEUED YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2000-LIMIT-QUEUED.
           IF TXPRC-QUEUED-YTD < 0.00
               MOVE 0.00 TO TXPRC-QUEUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXPRC-QUEUED-YTD > 500.00
               MOVE 500.00 TO TXPRC-QUEUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-PAYMENT.
           IF TXPRC-PAYMENT-CNT < 9999999
               ADD 1 TO TXPRC-PAYMENT-CNT
           END-IF
           IF TXPRC-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO TXPRC-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2020-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TXPRC-QUEUED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TXPRC-QUEUED-YTD - (WS-WORK-AMT * 4)
           COMPUTE TXPRC-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DGN -> JBT WHEN OPEN DATE IS PAST
      * JBT -> LSS WHEN OPEN DATE IS PAST
      * LSS -> RDP WHEN OPEN DATE IS PAST
      * RDP -> RPL WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2030-SET-REASON.
           EVALUATE TXPRC-REASON-CD
               WHEN 'DGN'
                   IF TXPRC-OPEN-DATE > 0 AND TXPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBT' TO TXPRC-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBT'
                   IF TXPRC-OPEN-DATE > 0 AND TXPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LSS' TO TXPRC-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LSS'
                   IF TXPRC-OPEN-DATE > 0 AND TXPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RDP' TO TXPRC-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RDP'
                   IF TXPRC-OPEN-DATE > 0 AND TXPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RPL' TO TXPRC-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2040-DESCRIBE-REGION.
           EVALUATE TXPRC-REGION-CD
               WHEN 'G'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2050-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXPRC-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TXPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E367 NAME REQUIRED
      * E873 WORK AMT NEGATIVE
      * E263 WORK AMT OVER 1000000
      * E963 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXPRC-NAME = SPACES
                   MOVE 'E367' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E873' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN TXPRC-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COVER YTD.
      *----------------------------------------------------------------*
       2070-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXPRC-COVER-YTD ROUNDED =
                   TXPRC-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TXPRC-COVER-YTD
           END-IF
           ADD TXPRC-COVER-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2080-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2090-LIMIT-QUEUED.
           IF TXPRC-QUEUED-AMT < 50.00
               MOVE 50.00 TO TXPRC-QUEUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXPRC-QUEUED-AMT > 50000.00
               MOVE 50000.00 TO TXPRC-QUEUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COVER YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2100-ACCRUE-COVER.
           IF TXPRC-COVER-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXPRC-COVER-YTD * TXPRC-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TXPRC-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2110-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXPRC-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TXPRC-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2120-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TXPRC-NOTICE-DATE > 0 AND TXPRC-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXPRC-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXPRC-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD TXPRC-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TXPRC-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TXPRC-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXPRC-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT QUEUED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TXPRC-QUEUED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TXPRC-QUEUED-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXPRC-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TXPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TEST KEY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2150-DESCRIBE-TEST-KEY.
           EVALUATE TXPRC-TEST-KEY-CD
               WHEN 'EA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E959 NAME REQUIRED
      * E362 SETTLE AMT NEGATIVE
      * E134 SETTLE AMT OVER 50000
      * E677 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2160-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXPRC-NAME = SPACES
                   MOVE 'E959' TO WS-REJECT-CODE
               WHEN TXPRC-SETTLE-AMT < 0
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN TXPRC-SETTLE-AMT > 50000.00
                   MOVE 'E134' TO WS-REJECT-CODE
               WHEN TXPRC-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2170-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TXPRC-OPEN-DATE > 0 AND TXPRC-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXPRC-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXPRC-CORRESP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TXPRC-CORRESP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TXPRC-CORRESP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TXPRC-CORRESP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXPRC-CORRESP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE TIER TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2180-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-TIER-RATE(1) TO TXPRC-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TXPRC-QUEUED-AMT NOT < WS-SETTLE-TIER-LOW(WS-TX)
                   MOVE WS-SETTLE-TIER-RATE(WS-TX) TO TXPRC-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXPRC-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXPRC-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED YTD BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2190-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXPRC-TEST-KEY-CD
                   ADD TXPRC-QUEUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXPRC-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXPRC-QUEUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-EXCEPT.
           IF TXPRC-EXCEPT-CNT < 9999999
               ADD 1 TO TXPRC-EXCEPT-CNT
           END-IF
           IF TXPRC-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TXPRC-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2210-DESCRIBE-REASON.
           EVALUATE TXPRC-REASON-CD
               WHEN 'DGN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JBT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LSS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RDP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RPL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SNE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UJS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE COVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       2220-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXPRC-SETTLE-AMT ROUNDED =
                   TXPRC-COVER-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TXPRC-SETTLE-AMT
           END-IF
           ADD TXPRC-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2230-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXPRC-BRANCH-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXPRC-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2240-ACCRUE-QUEUED.
           IF TXPRC-QUEUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXPRC-QUEUED-YTD * TXPRC-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COVER YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2250-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TXPRC-COVER-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TXPRC-COVER-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E690 ADDR REQUIRED
      * E835 SETTLE YTD NEGATIVE
      * E469 SETTLE YTD OVER 250000
      * E360 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2260-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXPRC-ADDR = SPACES
                   MOVE 'E690' TO WS-REJECT-CODE
               WHEN TXPRC-SETTLE-YTD < 0
                   MOVE 'E835' TO WS-REJECT-CODE
               WHEN TXPRC-SETTLE-YTD > 250000.00
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN TXPRC-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E360' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * G -> M WHEN EXPIRY DATE IS PAST
      * M -> T WHEN EXPIRY DATE IS PAST
      * T -> G WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2270-SET-REGION.
           EVALUATE TXPRC-REGION-CD
               WHEN 'G'
                   IF TXPRC-EXPIRY-DATE > 0 AND TXPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TXPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TXPRC-EXPIRY-DATE > 0 AND TXPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TXPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TXPRC-EXPIRY-DATE > 0 AND TXPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO TXPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SETTLE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TXPRC-SETTLE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TXPRC-SETTLE-YTD - (WS-WORK-AMT * 4)
           COMPUTE TXPRC-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXPRC-BRANCH-CD
                   ADD TXPRC-SETTLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXPRC-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXPRC-SETTLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE QUEUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE YTD.
      *----------------------------------------------------------------*
       2300-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXPRC-SETTLE-YTD ROUNDED =
                   TXPRC-QUEUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TXPRC-SETTLE-YTD
           END-IF
           ADD TXPRC-SETTLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SETTLE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2310-FLAG-SETTLE.
           IF TXPRC-SETTLE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TXPRC-SETTLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE TXPRC-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2330-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXPRC-REASON-CD
                   ADD TXPRC-QUEUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXPRC-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXPRC-QUEUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2340-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-TIER-RATE(1) TO TXPRC-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-SETTLE-TIER-LOW(WS-TX)
                   MOVE WS-SETTLE-TIER-RATE(WS-TX) TO TXPRC-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXPRC-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXPRC-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2350-COUNT-EXCEPT.
           IF TXPRC-EXCEPT-CNT < 9999999
               ADD 1 TO TXPRC-EXCEPT-CNT
           END-IF
           IF TXPRC-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TXPRC-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2360-LIMIT-SETTLE.
           IF TXPRC-SETTLE-YTD < 100.00
               MOVE 100.00 TO TXPRC-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXPRC-SETTLE-YTD > 2500.00
               MOVE 2500.00 TO TXPRC-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE QUEUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO QUEUED YTD.
      *----------------------------------------------------------------*
       2370-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXPRC-QUEUED-YTD ROUNDED =
                   TXPRC-QUEUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TXPRC-QUEUED-YTD
           END-IF
           ADD TXPRC-QUEUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-REASON.
           EVALUATE TXPRC-REASON-CD
               WHEN 'DGN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JBT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LSS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RDP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RPL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SNE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UJS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE TIER TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2390-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-TIER-RATE(1) TO TXPRC-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TXPRC-SETTLE-YTD NOT < WS-SETTLE-TIER-LOW(WS-TX)
                   MOVE WS-SETTLE-TIER-RATE(WS-TX) TO TXPRC-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXPRC-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXPRC-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2400-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE COVER YTD FEE ON THE QUEUED YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2410-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TXPRC-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TXPRC-COVER-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXPRC-STOP-FLAG = 'Y'
                   COMPUTE TXPRC-COVER-YTD ROUNDED = TXPRC-QUEUED-YTD *
                       TXPRC-RATE / 200
               WHEN TXPRC-REASON-CD = 'DGN'
                   MOVE 10.00 TO TXPRC-COVER-YTD
               WHEN TXPRC-REASON-CD = 'JBT'
                   MOVE 5.00 TO TXPRC-COVER-YTD
               WHEN OTHER
                   COMPUTE TXPRC-COVER-YTD ROUNDED = TXPRC-QUEUED-YTD *
                       TXPRC-RATE / 100
           END-EVALUATE
           IF TXPRC-COVER-YTD > 0 AND TXPRC-COVER-YTD < 5.00
               MOVE 5.00 TO TXPRC-COVER-YTD
           END-IF
           IF TXPRC-COVER-YTD > 150.00
               MOVE 150.00 TO TXPRC-COVER-YTD
           END-IF
           ADD TXPRC-COVER-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE TIER TIER FOR THE COVER YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-TIER-RATE(1) TO TXPRC-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TXPRC-COVER-YTD NOT < WS-SETTLE-TIER-LOW(WS-TX)
                   MOVE WS-SETTLE-TIER-RATE(WS-TX) TO TXPRC-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXPRC-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXPRC-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CORRESP YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2430-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TXPRC-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TXPRC-CORRESP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXPRC-STOP-FLAG = 'Y'
                   COMPUTE TXPRC-CORRESP-YTD ROUNDED = WS-WORK-AMT-2 *
                       TXPRC-DISCOUNT / 200
               WHEN TXPRC-REASON-CD = 'DGN'
                   MOVE 15.00 TO TXPRC-CORRESP-YTD
               WHEN TXPRC-REASON-CD = 'JBT'
                   MOVE 7.00 TO TXPRC-CORRESP-YTD
               WHEN OTHER
                   COMPUTE TXPRC-CORRESP-YTD ROUNDED = WS-WORK-AMT-2 *
                       TXPRC-DISCOUNT / 100
           END-EVALUATE
           IF TXPRC-CORRESP-YTD > 0 AND TXPRC-CORRESP-YTD < 10.00
               MOVE 10.00 TO TXPRC-CORRESP-YTD
           END-IF
           IF TXPRC-CORRESP-YTD > 35.00
               MOVE 35.00 TO TXPRC-CORRESP-YTD
           END-IF
           ADD TXPRC-CORRESP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2440-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2450-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TXPRC-RATE / 100 / 365
               ADD WS-WORK-AMT TO TXPRC-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2460-DESCRIBE-BRANCH.
           EVALUATE TXPRC-BRANCH-CD
               WHEN 'CC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2470-COUNT-PAYMENT.
           IF TXPRC-PAYMENT-CNT < 9999999
               ADD 1 TO TXPRC-PAYMENT-CNT
           END-IF
           IF TXPRC-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO TXPRC-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * G -> M WHEN NEXT DUE DATE IS PAST
      * M -> T WHEN NEXT DUE DATE IS PAST
      * T -> G WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2480-SET-REGION.
           EVALUATE TXPRC-REGION-CD
               WHEN 'G'
                   IF TXPRC-NEXT-DUE-DATE > 0 AND TXPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TXPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TXPRC-NEXT-DUE-DATE > 0 AND TXPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TXPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TXPRC-NEXT-DUE-DATE > 0 AND TXPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO TXPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2490-ACCRUE-SETTLE.
           IF TXPRC-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXPRC-SETTLE-AMT * TXPRC-RATE / 100 / 360
               ADD WS-WORK-AMT TO TXPRC-CORRESP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2500-FLAG-QUEUED.
           IF TXPRC-QUEUED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TXPRC-QUEUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CORRESP YTD FEE ON THE SETTLE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2510-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TXPRC-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TXPRC-CORRESP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXPRC-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TXPRC-CORRESP-YTD ROUNDED = TXPRC-SETTLE-AMT
                       * TXPRC-DISCOUNT / 200
               WHEN TXPRC-BRANCH-CD = 'CC'
                   MOVE 3.00 TO TXPRC-CORRESP-YTD
               WHEN TXPRC-BRANCH-CD = 'KR'
                   MOVE 5.00 TO TXPRC-CORRESP-YTD
               WHEN OTHER
                   COMPUTE TXPRC-CORRESP-YTD ROUNDED = TXPRC-SETTLE-AMT
                       * TXPRC-DISCOUNT / 100
           END-EVALUATE
           IF TXPRC-CORRESP-YTD > 0 AND TXPRC-CORRESP-YTD < 5.00
               MOVE 5.00 TO TXPRC-CORRESP-YTD
           END-IF
           IF TXPRC-CORRESP-YTD > 75.00
               MOVE 75.00 TO TXPRC-CORRESP-YTD
           END-IF
           ADD TXPRC-CORRESP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2520-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE TXPRC-SETTLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2530-COUNT-PAYMENT.
           IF TXPRC-PAYMENT-CNT < 9999999
               ADD 1 TO TXPRC-PAYMENT-CNT
           END-IF
           IF TXPRC-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO TXPRC-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED YTD BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXPRC-TEST-KEY-CD
                   ADD TXPRC-QUEUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXPRC-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXPRC-QUEUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2550-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TXPRC-STOP-FLAG = 'Y'
                   MOVE ZERO TO TXPRC-SETTLE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXPRC-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TXPRC-SETTLE-YTD ROUNDED = WS-WORK-AMT *
                       TXPRC-DISCOUNT / 200
               WHEN TXPRC-REGION-CD = 'G'
                   MOVE 15.00 TO TXPRC-SETTLE-YTD
               WHEN TXPRC-REGION-CD = 'M'
                   MOVE 7.00 TO TXPRC-SETTLE-YTD
               WHEN OTHER
                   COMPUTE TXPRC-SETTLE-YTD ROUNDED = WS-WORK-AMT *
                       TXPRC-DISCOUNT / 100
           END-EVALUATE
           IF TXPRC-SETTLE-YTD > 0 AND TXPRC-SETTLE-YTD < 1.00
               MOVE 1.00 TO TXPRC-SETTLE-YTD
           END-IF
           IF TXPRC-SETTLE-YTD > 50.00
               MOVE 50.00 TO TXPRC-SETTLE-YTD
           END-IF
           ADD TXPRC-SETTLE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXPRC-TEST-KEY-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXPRC-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2570-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TXPRC-EXPIRY-DATE > 0 AND TXPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXPRC-SETTLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TXPRC-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TXPRC-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TXPRC-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXPRC-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2580-COUNT-EXCEPT.
           IF TXPRC-EXCEPT-CNT < 9999999
               ADD 1 TO TXPRC-EXCEPT-CNT
           END-IF
           IF TXPRC-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TXPRC-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE QUEUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CORRESP YTD.
      *----------------------------------------------------------------*
       2590-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXPRC-CORRESP-YTD ROUNDED =
                   TXPRC-QUEUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TXPRC-CORRESP-YTD
           END-IF
           ADD TXPRC-CORRESP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2600-COUNT-PAYMENT.
           IF TXPRC-PAYMENT-CNT < 9999999
               ADD 1 TO TXPRC-PAYMENT-CNT
           END-IF
           IF TXPRC-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO TXPRC-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CORRESP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO QUEUED YTD.
      *----------------------------------------------------------------*
       2610-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXPRC-QUEUED-YTD ROUNDED =
                   TXPRC-CORRESP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TXPRC-QUEUED-YTD
           END-IF
           ADD TXPRC-QUEUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE SETTLE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2620-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TXPRC-STOP-FLAG = 'Y'
                   MOVE ZERO TO TXPRC-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXPRC-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TXPRC-SETTLE-AMT ROUNDED = TXPRC-SETTLE-YTD *
                       TXPRC-RATE / 200
               WHEN TXPRC-REASON-CD = 'DGN'
                   MOVE 7.00 TO TXPRC-SETTLE-AMT
               WHEN TXPRC-REASON-CD = 'JBT'
                   MOVE 3.00 TO TXPRC-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TXPRC-SETTLE-AMT ROUNDED = TXPRC-SETTLE-YTD *
                       TXPRC-RATE / 100
           END-EVALUATE
           IF TXPRC-SETTLE-AMT > 0 AND TXPRC-SETTLE-AMT < 2.00
               MOVE 2.00 TO TXPRC-SETTLE-AMT
           END-IF
           IF TXPRC-SETTLE-AMT > 150.00
               MOVE 150.00 TO TXPRC-SETTLE-AMT
           END-IF
           ADD TXPRC-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * QUEUED YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2630-LIMIT-QUEUED.
           IF TXPRC-QUEUED-YTD < 25.00
               MOVE 25.00 TO TXPRC-QUEUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXPRC-QUEUED-YTD > 1000.00
               MOVE 1000.00 TO TXPRC-QUEUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2640-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXPRC-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TXPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2650-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TXPRC-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TXPRC-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E707 NAME REQUIRED
      * E559 SETTLE YTD NEGATIVE
      * E248 SETTLE YTD OVER 25000
      * E191 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2660-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXPRC-NAME = SPACES
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN TXPRC-SETTLE-YTD < 0
                   MOVE 'E559' TO WS-REJECT-CODE
               WHEN TXPRC-SETTLE-YTD > 25000.00
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN TXPRC-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E191' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2670-FLAG-SETTLE.
           IF TXPRC-SETTLE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TXPRC-SETTLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2680-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TXPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TXPRC-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2690-COUNT-EXCEPT.
           IF TXPRC-EXCEPT-CNT < 9999999
               ADD 1 TO TXPRC-EXCEPT-CNT
           END-IF
           IF TXPRC-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TXPRC-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2700-ACCRUE-QUEUED.
           IF TXPRC-QUEUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXPRC-QUEUED-YTD * TXPRC-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2710-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COVER YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2720-LIMIT-COVER.
           IF TXPRC-COVER-YTD < 10.00
               MOVE 10.00 TO TXPRC-COVER-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXPRC-COVER-YTD > 2500.00
               MOVE 2500.00 TO TXPRC-COVER-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E207 ADDR REQUIRED
      * E560 COVER YTD NEGATIVE
      * E104 COVER YTD OVER 99999
      * E106 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2730-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXPRC-ADDR = SPACES
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN TXPRC-COVER-YTD < 0
                   MOVE 'E560' TO WS-REJECT-CODE
               WHEN TXPRC-COVER-YTD > 99999.00
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN TXPRC-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TXPRC-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TXPRC-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TXPRC-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TXPRC EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
