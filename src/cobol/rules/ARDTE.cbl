      *================================================================*
      * PROGRAM  : ARDTE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : ACCOUNT RECONCILEMENT RULE - DATE RULES. CALLED BY  *
      *            THE ACCOUNT RECONCILEMENT BATCH AND ONLINE PROGRAMS.*
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-20 JLM  ORIGINAL                                       *
      * 1997-08-03 PQR  OUTSTAND FEE SCHEDULE UPDATE REQ2495           *
      * 1998-01-09 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-09-27 SKN  PAID FEE SCHEDULE UPDATE REQ8745               *
      * 2000-08-16 RJT  PAID STATUS CODES ADDED CHG0490                *
      * 2001-07-18 LMB  ISSUED RERUN PROTECTION PRB3136                *
      * 2002-11-28 RJT  FEE RERUN PROTECTION CHG3562                   *
      * 2004-02-25 LMB  VOID RERUN PROTECTION PRB4500                  *
      * 2010-10-10 RJT  VOID RERUN PROTECTION REQ4108                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARDTE.
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
                                           VALUE 23.000000.
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
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000316000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000330000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000746000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000560500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000041000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000389500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008100000000282000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008200000000687000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008700000000509000'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 9 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAID-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000288000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000177500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000848000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000506000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000467500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020550000000757000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021550000000244500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024050000000560000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034050000000395000'.
       01  WS-PAID-TIER-TBL REDEFINES WS-PAID-TIER-VALUES.
           05  WS-PAID-TIER-ROW OCCURS 9 TIMES.
               10  WS-PAID-TIER-LOW        PIC 9(09)V99.
               10  WS-PAID-TIER-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ARDTEP.

       PROCEDURE DIVISION USING ARDTE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CHECK-LAST-MAINT
           PERFORM 2010-LIMIT-VOID
           PERFORM 2020-SET-SOURCE
           PERFORM 2030-SET-REGION
           PERFORM 2040-ACCUM-RECON-TYPE
           PERFORM 2050-AGE-LAST-STMT
           PERFORM 2060-ASSESS-STOP
           PERFORM 2070-ACCRUE-VOID
           PERFORM 2080-PRORATE-ISSUED
           PERFORM 2090-ACCRUE-FEE
           PERFORM 2100-SPLIT-ITEM
           PERFORM 2110-ACCUM-SOURCE
           PERFORM 2120-SELECT-DISCOUNT
           PERFORM 2130-LIMIT-VOID
           PERFORM 2140-SPLIT-VOID
           PERFORM 2150-SPLIT-ISSUED
           PERFORM 2160-COUNT-ITEM
           PERFORM 2170-PRORATE-PAID
           PERFORM 2180-FLAG-ISSUED
           PERFORM 2190-ACCUM-RECON-TYPE
           PERFORM 2200-ACCUM-CYCLE
           PERFORM 2210-LIMIT-ISSUED
           PERFORM 2220-SELECT-PENALTY-PCT
           PERFORM 2230-SPLIT-PAID
           PERFORM 2240-EDIT-CUST-ID
           PERFORM 2250-ACCRUE-ISSUED
           PERFORM 2260-PRORATE-PAID
           PERFORM 2270-LIMIT-VOID
           PERFORM 2280-PRORATE-VOID
           PERFORM 2290-FLAG-ISSUED
           PERFORM 2300-DESCRIBE-RECON-TYPE
           PERFORM 2310-DESCRIBE-REGION
           PERFORM 2320-EDIT-CUST-ID
           PERFORM 2330-EDIT-CUST-ID
           PERFORM 2340-ACCUM-SOURCE
           PERFORM 2350-FLAG-VOID
           PERFORM 2360-SET-REGION
           PERFORM 2370-EDIT-NAME
           PERFORM 2380-SET-CYCLE
           PERFORM 2390-DESCRIBE-REGION
           PERFORM 2400-COUNT-CYCLE
           PERFORM 2410-SPLIT-VOID
           PERFORM 2420-SELECT-DISCOUNT
           PERFORM 2430-SPLIT-VOID
           PERFORM 2440-CHECK-LAST-MAINT
           PERFORM 2450-SPLIT-FEE
           PERFORM 2460-LIMIT-ITEM
           PERFORM 2470-LIMIT-ISSUED
           PERFORM 2480-ASSESS-STOP
           PERFORM 2490-ACCRUE-PENALTY-PCT
           PERFORM 2500-SPLIT-VOID
           PERFORM 2510-ACCRUE-PAID
           PERFORM 2520-CHECK-LAST-MAINT
           PERFORM 2530-ACCUM-SOURCE
           PERFORM 2540-DESCRIBE-CYCLE
           PERFORM 2550-FLAG-ITEM
           PERFORM 2560-CHECK-LAST-PAY
           PERFORM 2570-PRORATE-ISSUED
           PERFORM 2580-SPLIT-VOID
           PERFORM 2590-ACCUM-CYCLE
           PERFORM 2600-EDIT-CUST-ID
           PERFORM 2610-AGE-LAST-PAY
           PERFORM 2620-SELECT-PENALTY-PCT
           PERFORM 2630-SET-REGION
           PERFORM 2640-DESCRIBE-SOURCE
           PERFORM 2650-SET-SOURCE
           PERFORM 2660-LIMIT-FEE
           PERFORM 2670-ACCUM-CYCLE
           PERFORM 2680-FLAG-FEE
           PERFORM 2690-COUNT-ITEM
           PERFORM 2700-ACCUM-CYCLE
           PERFORM 2710-ACCUM-CYCLE
           PERFORM 2720-LIMIT-VOID
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ARDTE-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2000-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ARDTE-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ARDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2010-LIMIT-VOID.
           IF ARDTE-VOID-YTD < 100.00
               MOVE 100.00 TO ARDTE-VOID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARDTE-VOID-YTD > 500.00
               MOVE 500.00 TO ARDTE-VOID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN LAST PAY DATE IS PAST
      * MB -> MH WHEN LAST PAY DATE IS PAST
      * MH -> UG WHEN LAST PAY DATE IS PAST
      * UG -> UR WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2020-SET-SOURCE.
           EVALUATE ARDTE-SOURCE-CD
               WHEN 'GW'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AD -> BU WHEN LAST STMT DATE IS PAST
      * BU -> GM WHEN LAST STMT DATE IS PAST
      * GM -> SW WHEN LAST STMT DATE IS PAST
      * SW -> WD WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2030-SET-REGION.
           EVALUATE ARDTE-REGION-CD
               WHEN 'AD'
                   IF ARDTE-LAST-STMT-DATE > 0 AND ARDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BU' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BU'
                   IF ARDTE-LAST-STMT-DATE > 0 AND ARDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARDTE-LAST-STMT-DATE > 0 AND ARDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARDTE-LAST-STMT-DATE > 0 AND ARDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WD' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY RECON TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2040-ACCUM-RECON-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARDTE-RECON-TYPE-CD
                   ADD ARDTE-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARDTE-RECON-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARDTE-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARDTE-LAST-STMT-DATE > 0 AND ARDTE-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARDTE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE ISSUED YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2060-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ARDTE-STOP-FLAG = 'Y'
                   MOVE ZERO TO ARDTE-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARDTE-EXEMPT-FLAG = 'Y'
                   COMPUTE ARDTE-PAID-YTD ROUNDED = ARDTE-ISSUED-YTD *
                       ARDTE-DISCOUNT / 200
               WHEN ARDTE-RECON-TYPE-CD = 'EEH'
                   MOVE 7.00 TO ARDTE-PAID-YTD
               WHEN ARDTE-RECON-TYPE-CD = 'KBS'
                   MOVE 3.00 TO ARDTE-PAID-YTD
               WHEN OTHER
                   COMPUTE ARDTE-PAID-YTD ROUNDED = ARDTE-ISSUED-YTD *
                       ARDTE-DISCOUNT / 100
           END-EVALUATE
           IF ARDTE-PAID-YTD > 0 AND ARDTE-PAID-YTD < 10.00
               MOVE 10.00 TO ARDTE-PAID-YTD
           END-IF
           IF ARDTE-PAID-YTD > 250.00
               MOVE 250.00 TO ARDTE-PAID-YTD
           END-IF
           ADD ARDTE-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2070-ACCRUE-VOID.
           IF ARDTE-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARDTE-VOID-AMT * ARDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARDTE-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2080-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARDTE-ISSUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2090-ACCRUE-FEE.
           IF ARDTE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARDTE-FEE-AMT * ARDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2100-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARDTE-SOURCE-CD
                   ADD ARDTE-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARDTE-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2120-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARDTE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ARDTE-FEE-YTD NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARDTE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARDTE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARDTE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-VOID.
           IF ARDTE-VOID-AMT < 100.00
               MOVE 100.00 TO ARDTE-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARDTE-VOID-AMT > 10000.00
               MOVE 10000.00 TO ARDTE-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2140-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARDTE-VOID-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARDTE-VOID-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARDTE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2150-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARDTE-ISSUED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ARDTE-ISSUED-YTD - (WS-WORK-AMT * 12)
           COMPUTE ARDTE-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-ITEM.
           IF ARDTE-ITEM-CNT < 9999999
               ADD 1 TO ARDTE-ITEM-CNT
           END-IF
           IF ARDTE-ITEM-CNT NOT < 10
               MOVE 'Y' TO ARDTE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       2170-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARDTE-PAID-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARDTE-PAID-YTD
           END-IF
           ADD ARDTE-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ISSUED YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2180-FLAG-ISSUED.
           IF ARDTE-ISSUED-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARDTE-ISSUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY RECON TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2190-ACCUM-RECON-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARDTE-RECON-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARDTE-RECON-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2200-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARDTE-CYCLE-CD
                   ADD ARDTE-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARDTE-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2210-LIMIT-ISSUED.
           IF ARDTE-ISSUED-YTD < 10.00
               MOVE 10.00 TO ARDTE-ISSUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARDTE-ISSUED-YTD > 1000.00
               MOVE 1000.00 TO ARDTE-ISSUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE ISSUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARDTE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ARDTE-ISSUED-YTD NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARDTE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARDTE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARDTE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2230-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = ARDTE-PAID-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ARDTE-PAID-YTD - (WS-WORK-AMT * 3)
           COMPUTE ARDTE-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E963 CUST ID REQUIRED
      * E978 WORK AMT 2 NEGATIVE
      * E221 WORK AMT 2 OVER 99999
      * E861 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2240-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARDTE-CUST-ID = SPACES
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E978' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E221' TO WS-REJECT-CODE
               WHEN ARDTE-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E861' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2250-ACCRUE-ISSUED.
           IF ARDTE-ISSUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARDTE-ISSUED-YTD * ARDTE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARDTE-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       2260-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARDTE-PAID-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARDTE-PAID-YTD
           END-IF
           ADD ARDTE-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VOID YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2270-LIMIT-VOID.
           IF ARDTE-VOID-YTD < 25.00
               MOVE 25.00 TO ARDTE-VOID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARDTE-VOID-YTD > 1000.00
               MOVE 1000.00 TO ARDTE-VOID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       2280-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARDTE-VOID-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARDTE-VOID-AMT
           END-IF
           ADD ARDTE-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ISSUED YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2290-FLAG-ISSUED.
           IF ARDTE-ISSUED-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARDTE-ISSUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RECON TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-RECON-TYPE.
           EVALUATE ARDTE-RECON-TYPE-CD
               WHEN 'EEH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KBS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PAC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VKT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YMX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZVT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2310-DESCRIBE-REGION.
           EVALUATE ARDTE-REGION-CD
               WHEN 'AD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E972 CUST ID REQUIRED
      * E557 FEE YTD NEGATIVE
      * E161 FEE YTD OVER 99999
      * E015 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2320-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARDTE-CUST-ID = SPACES
                   MOVE 'E972' TO WS-REJECT-CODE
               WHEN ARDTE-FEE-YTD < 0
                   MOVE 'E557' TO WS-REJECT-CODE
               WHEN ARDTE-FEE-YTD > 99999.00
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN ARDTE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E015' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E953 CUST ID REQUIRED
      * E835 VOID YTD NEGATIVE
      * E280 VOID YTD OVER 50000
      * E247 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2330-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARDTE-CUST-ID = SPACES
                   MOVE 'E953' TO WS-REJECT-CODE
               WHEN ARDTE-VOID-YTD < 0
                   MOVE 'E835' TO WS-REJECT-CODE
               WHEN ARDTE-VOID-YTD > 50000.00
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN ARDTE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARDTE-SOURCE-CD
                   ADD ARDTE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARDTE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2350-FLAG-VOID.
           IF ARDTE-VOID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARDTE-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AD -> BU WHEN LAST PAY DATE IS PAST
      * BU -> GM WHEN LAST PAY DATE IS PAST
      * GM -> SW WHEN LAST PAY DATE IS PAST
      * SW -> WD WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2360-SET-REGION.
           EVALUATE ARDTE-REGION-CD
               WHEN 'AD'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BU'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E769 NAME REQUIRED
      * E048 ISSUED YTD NEGATIVE
      * E027 ISSUED YTD OVER 5000
      * E493 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2370-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARDTE-NAME = SPACES
                   MOVE 'E769' TO WS-REJECT-CODE
               WHEN ARDTE-ISSUED-YTD < 0
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN ARDTE-ISSUED-YTD > 5000.00
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN ARDTE-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CP -> MR WHEN LAST STMT DATE IS PAST
      * MR -> PY WHEN LAST STMT DATE IS PAST
      * PY -> SP WHEN LAST STMT DATE IS PAST
      * SP -> TP WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2380-SET-CYCLE.
           EVALUATE ARDTE-CYCLE-CD
               WHEN 'CP'
                   IF ARDTE-LAST-STMT-DATE > 0 AND ARDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MR' TO ARDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF ARDTE-LAST-STMT-DATE > 0 AND ARDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PY' TO ARDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ARDTE-LAST-STMT-DATE > 0 AND ARDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARDTE-LAST-STMT-DATE > 0 AND ARDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TP' TO ARDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2390-DESCRIBE-REGION.
           EVALUATE ARDTE-REGION-CD
               WHEN 'AD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2400-COUNT-CYCLE.
           IF ARDTE-CYCLE-CNT < 9999999
               ADD 1 TO ARDTE-CYCLE-CNT
           END-IF
           IF ARDTE-CYCLE-CNT NOT < 10
               MOVE 'Y' TO ARDTE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2410-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARDTE-VOID-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARDTE-VOID-AMT - (WS-WORK-AMT * 2)
           COMPUTE ARDTE-VOID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO ARDTE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ARDTE-FEE-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO ARDTE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARDTE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARDTE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2430-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARDTE-VOID-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ARDTE-VOID-YTD - (WS-WORK-AMT * 12)
           COMPUTE ARDTE-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ARDTE-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2450-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARDTE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARDTE-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE ARDTE-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2460-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2470-LIMIT-ISSUED.
           IF ARDTE-ISSUED-YTD < 10.00
               MOVE 10.00 TO ARDTE-ISSUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARDTE-ISSUED-YTD > 1000.00
               MOVE 1000.00 TO ARDTE-ISSUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE VOID YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2480-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ARDTE-STOP-FLAG = 'Y'
                   MOVE ZERO TO ARDTE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARDTE-EXEMPT-FLAG = 'Y'
                   COMPUTE ARDTE-FEE-YTD ROUNDED = ARDTE-VOID-YTD *
                       ARDTE-PENALTY-PCT / 200
               WHEN ARDTE-SOURCE-CD = 'GW'
                   MOVE 12.00 TO ARDTE-FEE-YTD
               WHEN ARDTE-SOURCE-CD = 'MB'
                   MOVE 3.00 TO ARDTE-FEE-YTD
               WHEN OTHER
                   COMPUTE ARDTE-FEE-YTD ROUNDED = ARDTE-VOID-YTD *
                       ARDTE-PENALTY-PCT / 100
           END-EVALUATE
           IF ARDTE-FEE-YTD > 0 AND ARDTE-FEE-YTD < 1.00
               MOVE 1.00 TO ARDTE-FEE-YTD
           END-IF
           IF ARDTE-FEE-YTD > 250.00
               MOVE 250.00 TO ARDTE-FEE-YTD
           END-IF
           ADD ARDTE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2490-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ARDTE-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARDTE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2500-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARDTE-VOID-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARDTE-VOID-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARDTE-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2510-ACCRUE-PAID.
           IF ARDTE-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARDTE-PAID-YTD * ARDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARDTE-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2520-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ARDTE-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ARDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2530-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARDTE-SOURCE-CD
                   ADD ARDTE-PAID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARDTE-PAID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2540-DESCRIBE-CYCLE.
           EVALUATE ARDTE-CYCLE-CD
               WHEN 'CP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2550-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ARDTE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ARDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED YTD.
      *----------------------------------------------------------------*
       2570-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARDTE-ISSUED-YTD ROUNDED =
                   ARDTE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARDTE-ISSUED-YTD
           END-IF
           ADD ARDTE-ISSUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2580-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE ARDTE-VOID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2590-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARDTE-CYCLE-CD
                   ADD ARDTE-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARDTE-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E105 CUST ID REQUIRED
      * E300 ISSUED YTD NEGATIVE
      * E842 ISSUED YTD OVER 250000
      * E826 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2600-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARDTE-CUST-ID = SPACES
                   MOVE 'E105' TO WS-REJECT-CODE
               WHEN ARDTE-ISSUED-YTD < 0
                   MOVE 'E300' TO WS-REJECT-CODE
               WHEN ARDTE-ISSUED-YTD > 250000.00
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN ARDTE-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2610-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARDTE-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD ARDTE-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ARDTE-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARDTE-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARDTE-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE ISSUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2620-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO ARDTE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ARDTE-ISSUED-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO ARDTE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARDTE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARDTE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AD -> BU WHEN LAST PAY DATE IS PAST
      * BU -> GM WHEN LAST PAY DATE IS PAST
      * GM -> SW WHEN LAST PAY DATE IS PAST
      * SW -> WD WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2630-SET-REGION.
           EVALUATE ARDTE-REGION-CD
               WHEN 'AD'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BU'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO ARDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2640-DESCRIBE-SOURCE.
           EVALUATE ARDTE-SOURCE-CD
               WHEN 'GW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN LAST PAY DATE IS PAST
      * MB -> MH WHEN LAST PAY DATE IS PAST
      * MH -> UG WHEN LAST PAY DATE IS PAST
      * UG -> UR WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2650-SET-SOURCE.
           EVALUATE ARDTE-SOURCE-CD
               WHEN 'GW'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARDTE-LAST-PAY-DATE > 0 AND ARDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2660-LIMIT-FEE.
           IF ARDTE-FEE-YTD < 10.00
               MOVE 10.00 TO ARDTE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARDTE-FEE-YTD > 2500.00
               MOVE 2500.00 TO ARDTE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2670-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARDTE-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2680-FLAG-FEE.
           IF ARDTE-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARDTE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2690-COUNT-ITEM.
           IF ARDTE-ITEM-CNT < 9999999
               ADD 1 TO ARDTE-ITEM-CNT
           END-IF
           IF ARDTE-ITEM-CNT NOT < 10
               MOVE 'Y' TO ARDTE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2700-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARDTE-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2710-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARDTE-CYCLE-CD
                   ADD ARDTE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARDTE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2720-LIMIT-VOID.
           IF ARDTE-VOID-AMT < 10.00
               MOVE 10.00 TO ARDTE-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARDTE-VOID-AMT > 10000.00
               MOVE 10000.00 TO ARDTE-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ARDTE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ARDTE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ARDTE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ARDTE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
