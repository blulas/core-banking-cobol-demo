      *================================================================*
      * PROGRAM  : TXCAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TELEX WIRE MESSAGING RULE - PRICING. CALLED BY THE  *
      *            TELEX WIRE MESSAGING BATCH AND ONLINE PROGRAMS.     *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-07-08 TWH  ORIGINAL                                       *
      * 1996-12-20 CRP  CORRESP TABLE EXPANDED CHG0370                 *
      * 2001-11-13 EWS  WIRE LIMIT REVISED PRB6088                     *
      * 2005-09-11 BHS  QUEUED FEE SCHEDULE UPDATE CHG3425             *
      * 2005-08-23 CRP  CHARGE CALCULATION CORRECTED REQ6823           *
      * 2008-06-16 BHS  COVER RERUN PROTECTION REQ9933                 *
      * 2009-10-28 RJT  COVER FEE SCHEDULE UPDATE PRB1037              *
      * 2009-01-08 EWS  CORRESP STATUS CODES ADDED PRB2463             *
      * 2009-02-08 PQR  QUEUED CALCULATION CORRECTED PRB5094           *
      * 2010-07-02 EWS  WIRE CALCULATION CORRECTED CHG1623             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXCAL.
       AUTHOR. D P SHAW.

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
      * WS-SETTLE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SETTLE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000541000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000316000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000475000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000218500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020350000000582000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030350000000205000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040350000000161500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00042850000000571500'.
       01  WS-SETTLE-GRID-TBL REDEFINES WS-SETTLE-GRID-VALUES.
           05  WS-SETTLE-GRID-ROW OCCURS 8 TIMES.
               10  WS-SETTLE-GRID-LOW      PIC 9(09)V99.
               10  WS-SETTLE-GRID-RATE     PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TXCALP.

       PROCEDURE DIVISION USING TXCAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-CHARGE
           PERFORM 2010-AGE-NEXT-DUE
           PERFORM 2020-SPLIT-SETTLE
           PERFORM 2030-LIMIT-SETTLE
           PERFORM 2040-SELECT-RATE
           PERFORM 2050-ACCRUE-CHARGE
           PERFORM 2060-SPLIT-SETTLE
           PERFORM 2070-SPLIT-COVER
           PERFORM 2080-PRORATE-SETTLE
           PERFORM 2090-FLAG-SETTLE
           PERFORM 2100-COUNT-EXCEPT
           PERFORM 2110-AGE-NEXT-DUE
           PERFORM 2120-COUNT-PAYMENT
           PERFORM 2130-SELECT-FEE-PCT
           PERFORM 2140-LIMIT-COVER
           PERFORM 2150-DESCRIBE-MSG-TYPE
           PERFORM 2160-PRORATE-QUEUED
           PERFORM 2170-CHECK-NEXT-DUE
           PERFORM 2180-SELECT-FEE-PCT
           PERFORM 2190-PRORATE-QUEUED
           PERFORM 2200-FLAG-QUEUED
           PERFORM 2210-SET-MSG-TYPE
           PERFORM 2220-EDIT-DESC
           PERFORM 2230-EDIT-DESC
           PERFORM 2240-ACCRUE-QUEUED
           PERFORM 2250-CHECK-NEXT-REVIEW
           PERFORM 2260-CHECK-NEXT-REVIEW
           PERFORM 2270-FLAG-QUEUED
           PERFORM 2280-FLAG-ITEM
           PERFORM 2290-ACCUM-MSG-TYPE
           PERFORM 2300-SPLIT-SETTLE
           PERFORM 2310-COUNT-EXCEPT
           PERFORM 2320-EDIT-DESC
           PERFORM 2330-PRORATE-SETTLE
           PERFORM 2340-PRORATE-QUEUED
           PERFORM 2350-SET-REASON
           PERFORM 2360-EDIT-DESC
           PERFORM 2370-LIMIT-ITEM
           PERFORM 2380-AGE-NEXT-DUE
           PERFORM 2390-ACCUM-MSG-TYPE
           PERFORM 2400-COUNT-PAYMENT
           PERFORM 2410-ASSESS-PREFERRED
           PERFORM 2420-PRORATE-CHARGE
           PERFORM 2430-FLAG-ITEM
           PERFORM 2440-PRORATE-QUEUED
           PERFORM 2450-SPLIT-CHARGE
           PERFORM 2460-LIMIT-QUEUED
           PERFORM 2470-SELECT-RATE
           PERFORM 2480-PRORATE-QUEUED
           PERFORM 2490-ACCUM-MSG-TYPE
           PERFORM 2500-AGE-LAST-ACT
           PERFORM 2510-PRORATE-CHARGE
           PERFORM 2520-FLAG-ITEM
           PERFORM 2530-FLAG-QUEUED
           PERFORM 2540-EDIT-DESC
           PERFORM 2550-FLAG-CHARGE
           PERFORM 2560-SPLIT-QUEUED
           PERFORM 2570-LIMIT-QUEUED
           PERFORM 2580-FLAG-QUEUED
           PERFORM 2590-ASSESS-SUSPEND
           PERFORM 2600-ASSESS-PREFERRED
           PERFORM 2610-EDIT-DESC
           PERFORM 2620-SPLIT-COVER
           PERFORM 2630-FLAG-SETTLE
           PERFORM 2640-SELECT-RATE
           PERFORM 2650-SELECT-FEE-PCT
           PERFORM 2660-FLAG-SETTLE
           PERFORM 2670-AGE-NEXT-REVIEW
           PERFORM 2680-FLAG-QUEUED
           PERFORM 2690-ACCRUE-CHARGE
           PERFORM 2700-SPLIT-QUEUED
           PERFORM 2710-SPLIT-CHARGE
           PERFORM 2720-SELECT-FEE-PCT
           PERFORM 2730-SELECT-RATE
           PERFORM 2740-CHECK-NEXT-DUE
           PERFORM 2750-ASSESS-REVIEW
           PERFORM 2760-SELECT-FEE-PCT
           PERFORM 2770-ACCRUE-COVER
           PERFORM 2780-EDIT-NAME
           PERFORM 2790-COUNT-EXCEPT
           PERFORM 2800-AGE-NEXT-DUE
           PERFORM 2810-COUNT-EXCEPT
           PERFORM 2820-EDIT-NAME
           PERFORM 2830-LIMIT-QUEUED
           PERFORM 2840-EDIT-DESC
           PERFORM 2850-AGE-LAST-ACT
           PERFORM 2860-CHECK-NEXT-REVIEW
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TXCAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT CHARGE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TXCAL-CHARGE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TXCAL-CHARGE-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-NEXT-DUE-DATE > 0 AND TXCAL-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXCAL-CHARGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TXCAL-CHARGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TXCAL-CHARGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TXCAL-CHARGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXCAL-CHARGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2020-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TXCAL-SETTLE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TXCAL-SETTLE-AMT - (WS-WORK-AMT * 4)
           COMPUTE TXCAL-QUEUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2030-LIMIT-SETTLE.
           IF TXCAL-SETTLE-AMT < 25.00
               MOVE 25.00 TO TXCAL-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXCAL-SETTLE-AMT > 2500.00
               MOVE 2500.00 TO TXCAL-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE CHARGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TXCAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TXCAL-CHARGE-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TXCAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXCAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXCAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2050-ACCRUE-CHARGE.
           IF TXCAL-CHARGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXCAL-CHARGE-AMT * TXCAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TXCAL-QUEUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE TXCAL-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COVER YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TXCAL-COVER-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TXCAL-COVER-YTD - (WS-WORK-AMT * 2)
           COMPUTE TXCAL-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE QUEUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       2080-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXCAL-SETTLE-AMT ROUNDED =
                   TXCAL-QUEUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TXCAL-SETTLE-AMT
           END-IF
           ADD TXCAL-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2090-FLAG-SETTLE.
           IF TXCAL-SETTLE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TXCAL-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2100-COUNT-EXCEPT.
           IF TXCAL-EXCEPT-CNT < 9999999
               ADD 1 TO TXCAL-EXCEPT-CNT
           END-IF
           IF TXCAL-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TXCAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-NEXT-DUE-DATE > 0 AND TXCAL-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXCAL-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TXCAL-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TXCAL-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TXCAL-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXCAL-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-PAYMENT.
           IF TXCAL-PAYMENT-CNT < 9999999
               ADD 1 TO TXCAL-PAYMENT-CNT
           END-IF
           IF TXCAL-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO TXCAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE QUEUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2130-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TXCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TXCAL-QUEUED-YTD NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TXCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COVER YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2140-LIMIT-COVER.
           IF TXCAL-COVER-YTD < 100.00
               MOVE 100.00 TO TXCAL-COVER-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXCAL-COVER-YTD > 500.00
               MOVE 500.00 TO TXCAL-COVER-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MSG TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2150-DESCRIBE-MSG-TYPE.
           EVALUATE TXCAL-MSG-TYPE-CD
               WHEN 'LG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ND'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHARGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO QUEUED AMT.
      *----------------------------------------------------------------*
       2160-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXCAL-QUEUED-AMT ROUNDED =
                   TXCAL-CHARGE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TXCAL-QUEUED-AMT
           END-IF
           ADD TXCAL-QUEUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2170-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TXCAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE CHARGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2180-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TXCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TXCAL-CHARGE-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TXCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO QUEUED AMT.
      *----------------------------------------------------------------*
       2190-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXCAL-QUEUED-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TXCAL-QUEUED-AMT
           END-IF
           ADD TXCAL-QUEUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * QUEUED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2200-FLAG-QUEUED.
           IF TXCAL-QUEUED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TXCAL-QUEUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN LAST ACT DATE IS PAST
      * LX -> ND WHEN LAST ACT DATE IS PAST
      * ND -> PT WHEN LAST ACT DATE IS PAST
      * PT -> LG WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-MSG-TYPE.
           EVALUATE TXCAL-MSG-TYPE-CD
               WHEN 'LG'
                   IF TXCAL-LAST-ACT-DATE > 0 AND TXCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LX' TO TXCAL-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TXCAL-LAST-ACT-DATE > 0 AND TXCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO TXCAL-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TXCAL-LAST-ACT-DATE > 0 AND TXCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TXCAL-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TXCAL-LAST-ACT-DATE > 0 AND TXCAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LG' TO TXCAL-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E161 DESC REQUIRED
      * E439 CHARGE YTD NEGATIVE
      * E715 CHARGE YTD OVER 25000
      * E963 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2220-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXCAL-DESC = SPACES
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-YTD < 0
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-YTD > 25000.00
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN TXCAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E169 DESC REQUIRED
      * E407 CHARGE AMT NEGATIVE
      * E545 CHARGE AMT OVER 10000
      * E101 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXCAL-DESC = SPACES
                   MOVE 'E169' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-AMT < 0
                   MOVE 'E407' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-AMT > 10000.00
                   MOVE 'E545' TO WS-REJECT-CODE
               WHEN TXCAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E101' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2240-ACCRUE-QUEUED.
           IF TXCAL-QUEUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXCAL-QUEUED-YTD * TXCAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2250-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TXCAL-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2260-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TXCAL-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2270-FLAG-QUEUED.
           IF TXCAL-QUEUED-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TXCAL-QUEUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2280-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED AMT BY MSG TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-MSG-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXCAL-MSG-TYPE-CD
                   ADD TXCAL-QUEUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXCAL-MSG-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXCAL-QUEUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2300-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TXCAL-SETTLE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TXCAL-SETTLE-AMT - (WS-WORK-AMT * 4)
           COMPUTE TXCAL-CHARGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2310-COUNT-EXCEPT.
           IF TXCAL-EXCEPT-CNT < 9999999
               ADD 1 TO TXCAL-EXCEPT-CNT
           END-IF
           IF TXCAL-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TXCAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E761 DESC REQUIRED
      * E576 QUEUED AMT NEGATIVE
      * E510 QUEUED AMT OVER 99999
      * E212 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2320-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXCAL-DESC = SPACES
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN TXCAL-QUEUED-AMT < 0
                   MOVE 'E576' TO WS-REJECT-CODE
               WHEN TXCAL-QUEUED-AMT > 99999.00
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN TXCAL-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E212' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2330-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TXCAL-SETTLE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE QUEUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2340-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXCAL-QUEUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DGN -> JBT WHEN NEXT DUE DATE IS PAST
      * JBT -> LSS WHEN NEXT DUE DATE IS PAST
      * LSS -> RDP WHEN NEXT DUE DATE IS PAST
      * RDP -> RPL WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2350-SET-REASON.
           EVALUATE TXCAL-REASON-CD
               WHEN 'DGN'
                   IF TXCAL-NEXT-DUE-DATE > 0 AND TXCAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBT' TO TXCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBT'
                   IF TXCAL-NEXT-DUE-DATE > 0 AND TXCAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LSS' TO TXCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LSS'
                   IF TXCAL-NEXT-DUE-DATE > 0 AND TXCAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RDP' TO TXCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RDP'
                   IF TXCAL-NEXT-DUE-DATE > 0 AND TXCAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RPL' TO TXCAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E924 DESC REQUIRED
      * E934 CHARGE AMT NEGATIVE
      * E970 CHARGE AMT OVER 250000
      * E482 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2360-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXCAL-DESC = SPACES
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-AMT < 0
                   MOVE 'E934' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-AMT > 250000.00
                   MOVE 'E970' TO WS-REJECT-CODE
               WHEN TXCAL-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2370-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2380-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-NEXT-DUE-DATE > 0 AND TXCAL-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXCAL-QUEUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TXCAL-QUEUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TXCAL-QUEUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TXCAL-QUEUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXCAL-QUEUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY MSG TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2390-ACCUM-MSG-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXCAL-MSG-TYPE-CD
                   ADD TXCAL-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXCAL-MSG-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXCAL-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2400-COUNT-PAYMENT.
           IF TXCAL-PAYMENT-CNT < 9999999
               ADD 1 TO TXCAL-PAYMENT-CNT
           END-IF
           IF TXCAL-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO TXCAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGE YTD FEE ON THE QUEUED AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2410-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TXCAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TXCAL-CHARGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXCAL-REVIEW-FLAG = 'Y'
                   COMPUTE TXCAL-CHARGE-YTD ROUNDED = TXCAL-QUEUED-AMT *
                       TXCAL-FEE-PCT / 200
               WHEN TXCAL-REASON-CD = 'DGN'
                   MOVE 3.00 TO TXCAL-CHARGE-YTD
               WHEN TXCAL-REASON-CD = 'JBT'
                   MOVE 3.00 TO TXCAL-CHARGE-YTD
               WHEN OTHER
                   COMPUTE TXCAL-CHARGE-YTD ROUNDED = TXCAL-QUEUED-AMT *
                       TXCAL-FEE-PCT / 100
           END-EVALUATE
           IF TXCAL-CHARGE-YTD > 0 AND TXCAL-CHARGE-YTD < 1.00
               MOVE 1.00 TO TXCAL-CHARGE-YTD
           END-IF
           IF TXCAL-CHARGE-YTD > 250.00
               MOVE 250.00 TO TXCAL-CHARGE-YTD
           END-IF
           ADD TXCAL-CHARGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHARGE AMT.
      *----------------------------------------------------------------*
       2420-PRORATE-CHARGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXCAL-CHARGE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TXCAL-CHARGE-AMT
           END-IF
           ADD TXCAL-CHARGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2430-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE QUEUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2440-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TXCAL-QUEUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CHARGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2450-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TXCAL-CHARGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TXCAL-CHARGE-YTD - (WS-WORK-AMT * 4)
           COMPUTE TXCAL-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2460-LIMIT-QUEUED.
           IF TXCAL-QUEUED-AMT < 25.00
               MOVE 25.00 TO TXCAL-QUEUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXCAL-QUEUED-AMT > 50000.00
               MOVE 50000.00 TO TXCAL-QUEUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE CHARGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TXCAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TXCAL-CHARGE-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TXCAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXCAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXCAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO QUEUED YTD.
      *----------------------------------------------------------------*
       2480-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXCAL-QUEUED-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TXCAL-QUEUED-YTD
           END-IF
           ADD TXCAL-QUEUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED AMT BY MSG TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-MSG-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXCAL-MSG-TYPE-CD
                   ADD TXCAL-QUEUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXCAL-MSG-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXCAL-QUEUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2500-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-LAST-ACT-DATE > 0 AND TXCAL-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXCAL-CHARGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TXCAL-CHARGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TXCAL-CHARGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TXCAL-CHARGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXCAL-CHARGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHARGE AMT.
      *----------------------------------------------------------------*
       2510-PRORATE-CHARGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXCAL-CHARGE-AMT ROUNDED =
                   TXCAL-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TXCAL-CHARGE-AMT
           END-IF
           ADD TXCAL-CHARGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2520-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2530-FLAG-QUEUED.
           IF TXCAL-QUEUED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TXCAL-QUEUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E355 DESC REQUIRED
      * E904 QUEUED AMT NEGATIVE
      * E542 QUEUED AMT OVER 99999
      * E077 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2540-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXCAL-DESC = SPACES
                   MOVE 'E355' TO WS-REJECT-CODE
               WHEN TXCAL-QUEUED-AMT < 0
                   MOVE 'E904' TO WS-REJECT-CODE
               WHEN TXCAL-QUEUED-AMT > 99999.00
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN TXCAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E077' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2550-FLAG-CHARGE.
           IF TXCAL-CHARGE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TXCAL-CHARGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2560-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TXCAL-QUEUED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TXCAL-QUEUED-AMT - (WS-WORK-AMT * 12)
           COMPUTE TXCAL-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2570-LIMIT-QUEUED.
           IF TXCAL-QUEUED-YTD < 25.00
               MOVE 25.00 TO TXCAL-QUEUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXCAL-QUEUED-YTD > 5000.00
               MOVE 5000.00 TO TXCAL-QUEUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2580-FLAG-QUEUED.
           IF TXCAL-QUEUED-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TXCAL-QUEUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COVER YTD FEE ON THE QUEUED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2590-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TXCAL-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TXCAL-COVER-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXCAL-REVIEW-FLAG = 'Y'
                   COMPUTE TXCAL-COVER-YTD ROUNDED = TXCAL-QUEUED-AMT *
                       TXCAL-FEE-PCT / 200
               WHEN TXCAL-REASON-CD = 'DGN'
                   MOVE 20.00 TO TXCAL-COVER-YTD
               WHEN TXCAL-REASON-CD = 'JBT'
                   MOVE 12.00 TO TXCAL-COVER-YTD
               WHEN OTHER
                   COMPUTE TXCAL-COVER-YTD ROUNDED = TXCAL-QUEUED-AMT *
                       TXCAL-FEE-PCT / 100
           END-EVALUATE
           IF TXCAL-COVER-YTD > 0 AND TXCAL-COVER-YTD < 1.00
               MOVE 1.00 TO TXCAL-COVER-YTD
           END-IF
           IF TXCAL-COVER-YTD > 25.00
               MOVE 25.00 TO TXCAL-COVER-YTD
           END-IF
           ADD TXCAL-COVER-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED FEE ON THE CHARGE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2600-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TXCAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TXCAL-QUEUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXCAL-REVIEW-FLAG = 'Y'
                   COMPUTE TXCAL-QUEUED-AMT ROUNDED = TXCAL-CHARGE-YTD *
                       TXCAL-FEE-PCT / 200
               WHEN TXCAL-TYPE-CD = 'K'
                   MOVE 20.00 TO TXCAL-QUEUED-AMT
               WHEN TXCAL-TYPE-CD = 'L'
                   MOVE 3.00 TO TXCAL-QUEUED-AMT
               WHEN OTHER
                   COMPUTE TXCAL-QUEUED-AMT ROUNDED = TXCAL-CHARGE-YTD *
                       TXCAL-FEE-PCT / 100
           END-EVALUATE
           IF TXCAL-QUEUED-AMT > 0 AND TXCAL-QUEUED-AMT < 1.00
               MOVE 1.00 TO TXCAL-QUEUED-AMT
           END-IF
           IF TXCAL-QUEUED-AMT > 100.00
               MOVE 100.00 TO TXCAL-QUEUED-AMT
           END-IF
           ADD TXCAL-QUEUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E073 DESC REQUIRED
      * E744 CHARGE AMT NEGATIVE
      * E619 CHARGE AMT OVER 50000
      * E263 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXCAL-DESC = SPACES
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-AMT < 0
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-AMT > 50000.00
                   MOVE 'E619' TO WS-REJECT-CODE
               WHEN TXCAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COVER YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2620-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TXCAL-COVER-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TXCAL-COVER-YTD - (WS-WORK-AMT * 4)
           COMPUTE TXCAL-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2630-FLAG-SETTLE.
           IF TXCAL-SETTLE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TXCAL-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2640-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TXCAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TXCAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXCAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXCAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2650-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TXCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TXCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2660-FLAG-SETTLE.
           IF TXCAL-SETTLE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TXCAL-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2670-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-NEXT-REVIEW-DATE > 0 AND TXCAL-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXCAL-COVER-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TXCAL-COVER-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TXCAL-COVER-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TXCAL-COVER-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXCAL-COVER-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * QUEUED YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2680-FLAG-QUEUED.
           IF TXCAL-QUEUED-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TXCAL-QUEUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2690-ACCRUE-CHARGE.
           IF TXCAL-CHARGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXCAL-CHARGE-AMT * TXCAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO TXCAL-CHARGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2700-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TXCAL-QUEUED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TXCAL-QUEUED-AMT - (WS-WORK-AMT * 2)
           COMPUTE TXCAL-QUEUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2710-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE TXCAL-CHARGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2720-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TXCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TXCAL-QUEUED-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TXCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2730-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TXCAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TXCAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXCAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXCAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2740-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TXCAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SETTLE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2750-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TXCAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXCAL-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TXCAL-SETTLE-AMT *
                       TXCAL-RATE / 200
               WHEN TXCAL-BRANCH-CD = 'CC'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN TXCAL-BRANCH-CD = 'KR'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TXCAL-SETTLE-AMT *
                       TXCAL-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2760-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TXCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TXCAL-QUEUED-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TXCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COVER YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2770-ACCRUE-COVER.
           IF TXCAL-COVER-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXCAL-COVER-YTD * TXCAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TXCAL-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E763 NAME REQUIRED
      * E379 CHARGE AMT NEGATIVE
      * E474 CHARGE AMT OVER 99999
      * E462 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2780-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXCAL-NAME = SPACES
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-AMT < 0
                   MOVE 'E379' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-AMT > 99999.00
                   MOVE 'E474' TO WS-REJECT-CODE
               WHEN TXCAL-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E462' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2790-COUNT-EXCEPT.
           IF TXCAL-EXCEPT-CNT < 9999999
               ADD 1 TO TXCAL-EXCEPT-CNT
           END-IF
           IF TXCAL-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TXCAL-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2800-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-NEXT-DUE-DATE > 0 AND TXCAL-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-NEXT-DUE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2810-COUNT-EXCEPT.
           IF TXCAL-EXCEPT-CNT < 9999999
               ADD 1 TO TXCAL-EXCEPT-CNT
           END-IF
           IF TXCAL-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TXCAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E955 NAME REQUIRED
      * E781 SETTLE AMT NEGATIVE
      * E649 SETTLE AMT OVER 1000000
      * E117 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2820-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXCAL-NAME = SPACES
                   MOVE 'E955' TO WS-REJECT-CODE
               WHEN TXCAL-SETTLE-AMT < 0
                   MOVE 'E781' TO WS-REJECT-CODE
               WHEN TXCAL-SETTLE-AMT > 1000000.00
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN TXCAL-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2830-LIMIT-QUEUED.
           IF TXCAL-QUEUED-YTD < 10.00
               MOVE 10.00 TO TXCAL-QUEUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXCAL-QUEUED-YTD > 5000.00
               MOVE 5000.00 TO TXCAL-QUEUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E994 DESC REQUIRED
      * E912 CHARGE YTD NEGATIVE
      * E787 CHARGE YTD OVER 1000000
      * E126 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2840-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXCAL-DESC = SPACES
                   MOVE 'E994' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-YTD < 0
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN TXCAL-CHARGE-YTD > 1000000.00
                   MOVE 'E787' TO WS-REJECT-CODE
               WHEN TXCAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2850-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-LAST-ACT-DATE > 0 AND TXCAL-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXCAL-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TXCAL-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TXCAL-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TXCAL-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXCAL-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2860-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TXCAL-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXCAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TXCAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TXCAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TXCAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TXCAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TXCAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
