      *================================================================*
      * PROGRAM  : ARSTAT                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : ACCOUNT RECONCILEMENT RULE - TIERING. CALLED BY THE *
      *            ACCOUNT RECONCILEMENT BATCH AND ONLINE PROGRAMS.    *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-06-02 LMB  ORIGINAL                                       *
      * 2003-09-05 BHS  STOP CALCULATION CORRECTED PRB4242             *
      * 2004-12-25 GAF  FEE STATUS CODES ADDED PRB5201                 *
      * 2004-11-06 BHS  PAID NOTICE TEXT PER LEGAL INC3954             *
      * 2006-09-03 SKN  PERFORMANCE - VOID READ AHEAD CHG7128          *
      * 2006-03-07 MKL  STOP LIMIT REVISED CHG1256                     *
      * 2013-04-19 GAF  FEE STATUS CODES ADDED INC2721                 *
      * 2013-11-16 MKL  PAID STATUS CODES ADDED PRB0132                *
      * 2013-12-10 JLM  FEE LIMIT REVISED PRB0259                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARSTAT.
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
      * WS-OUTSTAND-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OUTSTAND-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000015000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000376000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000048000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000882000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000530000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011700000000302000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021700000000801000'.
       01  WS-OUTSTAND-BAND-TBL REDEFINES WS-OUTSTAND-BAND-VALUES.
           05  WS-OUTSTAND-BAND-ROW OCCURS 7 TIMES.
               10  WS-OUTSTAND-BAND-LOW    PIC 9(09)V99.
               10  WS-OUTSTAND-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ARSTATP.

       PROCEDURE DIVISION USING ARSTAT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SET-CHANNEL
           PERFORM 2010-DESCRIBE-CYCLE
           PERFORM 2020-CHECK-CLOSE
           PERFORM 2030-CHECK-NEXT-REVIEW
           PERFORM 2040-SET-SOURCE
           PERFORM 2050-SELECT-DISCOUNT
           PERFORM 2060-ACCUM-SOURCE
           PERFORM 2070-SPLIT-ISSUED
           PERFORM 2080-AGE-LAST-STMT
           PERFORM 2090-ACCUM-BRANCH
           PERFORM 2100-CHECK-LAST-STMT
           PERFORM 2110-AGE-LAST-STMT
           PERFORM 2120-LIMIT-ITEM
           PERFORM 2130-DESCRIBE-SOURCE
           PERFORM 2140-AGE-LAST-STMT
           PERFORM 2150-AGE-LAST-STMT
           PERFORM 2160-FLAG-FEE
           PERFORM 2170-LIMIT-OUTSTAND
           PERFORM 2180-COUNT-ITEM
           PERFORM 2190-LIMIT-PAID
           PERFORM 2200-DESCRIBE-SOURCE
           PERFORM 2210-AGE-NEXT-REVIEW
           PERFORM 2220-ACCUM-CHANNEL
           PERFORM 2230-PRORATE-OUTSTAND
           PERFORM 2240-EDIT-CUST-ID
           PERFORM 2250-LIMIT-FEE
           PERFORM 2260-ACCRUE-PENALTY-PCT
           PERFORM 2270-ACCRUE-OUTSTAND
           PERFORM 2280-COUNT-ITEM
           PERFORM 2290-CHECK-LAST-STMT
           PERFORM 2300-DESCRIBE-BRANCH
           PERFORM 2310-DESCRIBE-BRANCH
           PERFORM 2320-SPLIT-PAID
           PERFORM 2330-SELECT-DISCOUNT
           PERFORM 2340-FLAG-ISSUED
           PERFORM 2350-LIMIT-ITEM
           PERFORM 2360-AGE-EFFECTIVE
           PERFORM 2370-SET-BRANCH
           PERFORM 2380-SPLIT-FEE
           PERFORM 2390-SPLIT-ISSUED
           PERFORM 2400-AGE-LAST-STMT
           PERFORM 2410-ACCRUE-OUTSTAND
           PERFORM 2420-SPLIT-ISSUED
           PERFORM 2430-ASSESS-PREFERRED
           PERFORM 2440-CHECK-LAST-STMT
           PERFORM 2450-EDIT-CUST-ID
           PERFORM 2460-EDIT-CUST-ID
           PERFORM 2470-LIMIT-PAID
           PERFORM 2480-AGE-LAST-STMT
           PERFORM 2490-EDIT-CUST-ID
           PERFORM 2500-CHECK-CLOSE
           PERFORM 2510-ACCUM-BRANCH
           PERFORM 2520-COUNT-CYCLE
           PERFORM 2530-EDIT-CUST-ID
           PERFORM 2540-EDIT-CUST-ID
           PERFORM 2550-ACCUM-CHANNEL
           PERFORM 2560-PRORATE-FEE
           PERFORM 2570-ACCRUE-ISSUED
           PERFORM 2580-ACCRUE-FEE
           PERFORM 2590-DESCRIBE-BRANCH
           PERFORM 2600-DESCRIBE-SOURCE
           PERFORM 2610-AGE-LAST-STMT
           PERFORM 2620-ACCRUE-ISSUED
           PERFORM 2630-ACCUM-CHANNEL
           PERFORM 2640-LIMIT-OUTSTAND
           PERFORM 2650-EDIT-REF-NO
           PERFORM 2660-SPLIT-ITEM
           PERFORM 2670-DESCRIBE-CYCLE
           PERFORM 2680-SELECT-PENALTY-PCT
           PERFORM 2690-PRORATE-OUTSTAND
           PERFORM 2700-AGE-EFFECTIVE
           PERFORM 2710-SPLIT-OUTSTAND
           PERFORM 2720-FLAG-ITEM
           PERFORM 2730-ASSESS-NOTICE-SENT
           PERFORM 2740-PRORATE-ISSUED
           PERFORM 2750-AGE-CLOSE
           PERFORM 2760-FLAG-PAID
           PERFORM 2770-CHECK-NEXT-REVIEW
           PERFORM 2780-CHECK-NEXT-REVIEW
           PERFORM 2790-SET-CHANNEL
           PERFORM 2800-SELECT-DISCOUNT
           PERFORM 2810-PRORATE-PAID
           PERFORM 2820-EDIT-CUST-ID
           PERFORM 2830-FLAG-OUTSTAND
           PERFORM 2840-AGE-CLOSE
           PERFORM 2850-SPLIT-ISSUED
           PERFORM 2860-AGE-NEXT-REVIEW
           PERFORM 2870-SET-CHANNEL
           PERFORM 2880-PRORATE-ITEM
           PERFORM 2890-CHECK-NEXT-REVIEW
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ARSTAT-RETURN-CODE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN NEXT REVIEW DATE IS PAST
      * SW -> UA WHEN NEXT REVIEW DATE IS PAST
      * UA -> WB WHEN NEXT REVIEW DATE IS PAST
      * WB -> ME WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2000-SET-CHANNEL.
           EVALUATE ARSTAT-CHANNEL-CD
               WHEN 'ME'
                   IF ARSTAT-NEXT-REVIEW-DATE > 0 AND
                       ARSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARSTAT-NEXT-REVIEW-DATE > 0 AND
                       ARSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARSTAT-NEXT-REVIEW-DATE > 0 AND
                       ARSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARSTAT-NEXT-REVIEW-DATE > 0 AND
                       ARSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2010-DESCRIBE-CYCLE.
           EVALUATE ARSTAT-CYCLE-CD
               WHEN 'CP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2020-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ARSTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2030-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ARSTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN CLOSE DATE IS PAST
      * MB -> MH WHEN CLOSE DATE IS PAST
      * MH -> UG WHEN CLOSE DATE IS PAST
      * UG -> UR WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2040-SET-SOURCE.
           EVALUATE ARSTAT-SOURCE-CD
               WHEN 'GW'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2050-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARSTAT-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ARSTAT-FEE-AMT NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO ARSTAT-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARSTAT-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARSTAT-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2060-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARSTAT-SOURCE-CD
                   ADD ARSTAT-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARSTAT-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARSTAT-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARSTAT-ISSUED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARSTAT-ISSUED-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARSTAT-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2080-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-LAST-STMT-DATE > 0 AND ARSTAT-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2090-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARSTAT-BRANCH-CD
                   ADD ARSTAT-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARSTAT-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARSTAT-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2100-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ARSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-LAST-STMT-DATE > 0 AND ARSTAT-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2120-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2130-DESCRIBE-SOURCE.
           EVALUATE ARSTAT-SOURCE-CD
               WHEN 'GW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-LAST-STMT-DATE > 0 AND ARSTAT-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-LAST-STMT-DATE > 0 AND ARSTAT-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARSTAT-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ARSTAT-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARSTAT-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARSTAT-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARSTAT-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2160-FLAG-FEE.
           IF ARSTAT-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARSTAT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2170-LIMIT-OUTSTAND.
           IF ARSTAT-OUTSTAND-YTD < 10.00
               MOVE 10.00 TO ARSTAT-OUTSTAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARSTAT-OUTSTAND-YTD > 10000.00
               MOVE 10000.00 TO ARSTAT-OUTSTAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2180-COUNT-ITEM.
           IF ARSTAT-ITEM-CNT < 9999999
               ADD 1 TO ARSTAT-ITEM-CNT
           END-IF
           IF ARSTAT-ITEM-CNT NOT < 6
               MOVE 'Y' TO ARSTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2190-LIMIT-PAID.
           IF ARSTAT-PAID-AMT < 10.00
               MOVE 10.00 TO ARSTAT-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARSTAT-PAID-AMT > 2500.00
               MOVE 2500.00 TO ARSTAT-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2200-DESCRIBE-SOURCE.
           EVALUATE ARSTAT-SOURCE-CD
               WHEN 'GW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2210-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-NEXT-REVIEW-DATE > 0 AND ARSTAT-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2220-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARSTAT-CHANNEL-CD
                   ADD ARSTAT-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARSTAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARSTAT-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2230-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ARSTAT-OUTSTAND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E456 CUST ID REQUIRED
      * E907 OUTSTAND YTD NEGATIVE
      * E406 OUTSTAND YTD OVER 5000
      * E037 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2240-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARSTAT-CUST-ID = SPACES
                   MOVE 'E456' TO WS-REJECT-CODE
               WHEN ARSTAT-OUTSTAND-YTD < 0
                   MOVE 'E907' TO WS-REJECT-CODE
               WHEN ARSTAT-OUTSTAND-YTD > 5000.00
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN ARSTAT-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2250-LIMIT-FEE.
           IF ARSTAT-FEE-AMT < 10.00
               MOVE 10.00 TO ARSTAT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARSTAT-FEE-AMT > 10000.00
               MOVE 10000.00 TO ARSTAT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2260-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ARSTAT-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARSTAT-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-OUTSTAND.
           IF ARSTAT-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARSTAT-OUTSTAND-YTD * ARSTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARSTAT-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-ITEM.
           IF ARSTAT-ITEM-CNT < 9999999
               ADD 1 TO ARSTAT-ITEM-CNT
           END-IF
           IF ARSTAT-ITEM-CNT NOT < 6
               MOVE 'Y' TO ARSTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ARSTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-BRANCH.
           EVALUATE ARSTAT-BRANCH-CD
               WHEN 'FN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2310-DESCRIBE-BRANCH.
           EVALUATE ARSTAT-BRANCH-CD
               WHEN 'FN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE ARSTAT-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2330-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARSTAT-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO ARSTAT-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARSTAT-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARSTAT-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2340-FLAG-ISSUED.
           IF ARSTAT-ISSUED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARSTAT-ISSUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2350-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-EFFECTIVE-DATE > 0 AND ARSTAT-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN LAST STMT DATE IS PAST
      * GM -> GV WHEN LAST STMT DATE IS PAST
      * GV -> HK WHEN LAST STMT DATE IS PAST
      * HK -> VX WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2370-SET-BRANCH.
           EVALUATE ARSTAT-BRANCH-CD
               WHEN 'FN'
                   IF ARSTAT-LAST-STMT-DATE > 0 AND
                       ARSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARSTAT-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARSTAT-LAST-STMT-DATE > 0 AND
                       ARSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARSTAT-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARSTAT-LAST-STMT-DATE > 0 AND
                       ARSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARSTAT-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARSTAT-LAST-STMT-DATE > 0 AND
                       ARSTAT-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARSTAT-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2380-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARSTAT-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARSTAT-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE ARSTAT-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2390-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE ARSTAT-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2400-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-LAST-STMT-DATE > 0 AND ARSTAT-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2410-ACCRUE-OUTSTAND.
           IF ARSTAT-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARSTAT-OUTSTAND-YTD * ARSTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARSTAT-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE ARSTAT-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2430-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARSTAT-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARSTAT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARSTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE ARSTAT-FEE-AMT ROUNDED = ARSTAT-OUTSTAND-YTD
                       * ARSTAT-PENALTY-PCT / 200
               WHEN ARSTAT-CYCLE-CD = 'CP'
                   MOVE 12.00 TO ARSTAT-FEE-AMT
               WHEN ARSTAT-CYCLE-CD = 'MR'
                   MOVE 5.00 TO ARSTAT-FEE-AMT
               WHEN OTHER
                   COMPUTE ARSTAT-FEE-AMT ROUNDED = ARSTAT-OUTSTAND-YTD
                       * ARSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF ARSTAT-FEE-AMT > 0 AND ARSTAT-FEE-AMT < 5.00
               MOVE 5.00 TO ARSTAT-FEE-AMT
           END-IF
           IF ARSTAT-FEE-AMT > 100.00
               MOVE 100.00 TO ARSTAT-FEE-AMT
           END-IF
           ADD ARSTAT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ARSTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E828 CUST ID REQUIRED
      * E452 FEE AMT NEGATIVE
      * E562 FEE AMT OVER 10000
      * E773 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2450-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARSTAT-CUST-ID = SPACES
                   MOVE 'E828' TO WS-REJECT-CODE
               WHEN ARSTAT-FEE-AMT < 0
                   MOVE 'E452' TO WS-REJECT-CODE
               WHEN ARSTAT-FEE-AMT > 10000.00
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN ARSTAT-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E470 CUST ID REQUIRED
      * E306 OUTSTAND AMT NEGATIVE
      * E958 OUTSTAND AMT OVER 10000
      * E399 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2460-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARSTAT-CUST-ID = SPACES
                   MOVE 'E470' TO WS-REJECT-CODE
               WHEN ARSTAT-OUTSTAND-AMT < 0
                   MOVE 'E306' TO WS-REJECT-CODE
               WHEN ARSTAT-OUTSTAND-AMT > 10000.00
                   MOVE 'E958' TO WS-REJECT-CODE
               WHEN ARSTAT-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E399' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2470-LIMIT-PAID.
           IF ARSTAT-PAID-AMT < 50.00
               MOVE 50.00 TO ARSTAT-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARSTAT-PAID-AMT > 5000.00
               MOVE 5000.00 TO ARSTAT-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2480-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-LAST-STMT-DATE > 0 AND ARSTAT-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARSTAT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E581 CUST ID REQUIRED
      * E901 ISSUED YTD NEGATIVE
      * E186 ISSUED YTD OVER 250000
      * E873 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2490-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARSTAT-CUST-ID = SPACES
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN ARSTAT-ISSUED-YTD < 0
                   MOVE 'E901' TO WS-REJECT-CODE
               WHEN ARSTAT-ISSUED-YTD > 250000.00
                   MOVE 'E186' TO WS-REJECT-CODE
               WHEN ARSTAT-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E873' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2500-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ARSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARSTAT-BRANCH-CD
                   ADD ARSTAT-ISSUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARSTAT-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARSTAT-ISSUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2520-COUNT-CYCLE.
           IF ARSTAT-CYCLE-CNT < 9999999
               ADD 1 TO ARSTAT-CYCLE-CNT
           END-IF
           IF ARSTAT-CYCLE-CNT NOT < 10
               MOVE 'Y' TO ARSTAT-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E587 CUST ID REQUIRED
      * E785 ISSUED AMT NEGATIVE
      * E050 ISSUED AMT OVER 10000
      * E918 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2530-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARSTAT-CUST-ID = SPACES
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN ARSTAT-ISSUED-AMT < 0
                   MOVE 'E785' TO WS-REJECT-CODE
               WHEN ARSTAT-ISSUED-AMT > 10000.00
                   MOVE 'E050' TO WS-REJECT-CODE
               WHEN ARSTAT-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E960 CUST ID REQUIRED
      * E099 ISSUED YTD NEGATIVE
      * E421 ISSUED YTD OVER 5000
      * E020 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2540-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARSTAT-CUST-ID = SPACES
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN ARSTAT-ISSUED-YTD < 0
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN ARSTAT-ISSUED-YTD > 5000.00
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN ARSTAT-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E020' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2550-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARSTAT-CHANNEL-CD
                   ADD ARSTAT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARSTAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARSTAT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       2560-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARSTAT-FEE-AMT ROUNDED =
                   ARSTAT-OUTSTAND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARSTAT-FEE-AMT
           END-IF
           ADD ARSTAT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2570-ACCRUE-ISSUED.
           IF ARSTAT-ISSUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARSTAT-ISSUED-YTD * ARSTAT-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARSTAT-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-FEE.
           IF ARSTAT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARSTAT-FEE-AMT * ARSTAT-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARSTAT-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2590-DESCRIBE-BRANCH.
           EVALUATE ARSTAT-BRANCH-CD
               WHEN 'FN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2600-DESCRIBE-SOURCE.
           EVALUATE ARSTAT-SOURCE-CD
               WHEN 'GW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2610-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-LAST-STMT-DATE > 0 AND ARSTAT-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2620-ACCRUE-ISSUED.
           IF ARSTAT-ISSUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARSTAT-ISSUED-YTD * ARSTAT-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARSTAT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2630-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARSTAT-CHANNEL-CD
                   ADD ARSTAT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARSTAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARSTAT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2640-LIMIT-OUTSTAND.
           IF ARSTAT-OUTSTAND-AMT < 10.00
               MOVE 10.00 TO ARSTAT-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARSTAT-OUTSTAND-AMT > 50000.00
               MOVE 50000.00 TO ARSTAT-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E244 REF NO REQUIRED
      * E520 PAID AMT NEGATIVE
      * E222 PAID AMT OVER 99999
      * E440 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2650-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARSTAT-REF-NO = SPACES
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN ARSTAT-PAID-AMT < 0
                   MOVE 'E520' TO WS-REJECT-CODE
               WHEN ARSTAT-PAID-AMT > 99999.00
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN ARSTAT-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2660-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2670-DESCRIBE-CYCLE.
           EVALUATE ARSTAT-CYCLE-CD
               WHEN 'CP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2680-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO
                       ARSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       2690-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARSTAT-OUTSTAND-AMT ROUNDED =
                   ARSTAT-ISSUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARSTAT-OUTSTAND-AMT
           END-IF
           ADD ARSTAT-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2700-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-EFFECTIVE-DATE > 0 AND ARSTAT-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARSTAT-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ARSTAT-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ARSTAT-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARSTAT-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARSTAT-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2710-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARSTAT-OUTSTAND-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ARSTAT-OUTSTAND-YTD - (WS-WORK-AMT *
               2)
           COMPUTE ARSTAT-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2720-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2730-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARSTAT-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ARSTAT-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARSTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE ARSTAT-ISSUED-YTD ROUNDED =
                       ARSTAT-OUTSTAND-AMT * ARSTAT-DISCOUNT / 200
               WHEN ARSTAT-CYCLE-CD = 'CP'
                   MOVE 5.00 TO ARSTAT-ISSUED-YTD
               WHEN ARSTAT-CYCLE-CD = 'MR'
                   MOVE 10.00 TO ARSTAT-ISSUED-YTD
               WHEN OTHER
                   COMPUTE ARSTAT-ISSUED-YTD ROUNDED =
                       ARSTAT-OUTSTAND-AMT * ARSTAT-DISCOUNT / 100
           END-EVALUATE
           IF ARSTAT-ISSUED-YTD > 0 AND ARSTAT-ISSUED-YTD < 1.00
               MOVE 1.00 TO ARSTAT-ISSUED-YTD
           END-IF
           IF ARSTAT-ISSUED-YTD > 25.00
               MOVE 25.00 TO ARSTAT-ISSUED-YTD
           END-IF
           ADD ARSTAT-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2740-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ARSTAT-ISSUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2750-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-CLOSE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2760-FLAG-PAID.
           IF ARSTAT-PAID-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARSTAT-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2770-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ARSTAT-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2780-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ARSTAT-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN CLOSE DATE IS PAST
      * SW -> UA WHEN CLOSE DATE IS PAST
      * UA -> WB WHEN CLOSE DATE IS PAST
      * WB -> ME WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2790-SET-CHANNEL.
           EVALUATE ARSTAT-CHANNEL-CD
               WHEN 'ME'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2800-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO ARSTAT-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO ARSTAT-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARSTAT-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARSTAT-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2810-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ARSTAT-PAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E065 CUST ID REQUIRED
      * E579 ISSUED AMT NEGATIVE
      * E171 ISSUED AMT OVER 10000
      * E421 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2820-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARSTAT-CUST-ID = SPACES
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN ARSTAT-ISSUED-AMT < 0
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN ARSTAT-ISSUED-AMT > 10000.00
                   MOVE 'E171' TO WS-REJECT-CODE
               WHEN ARSTAT-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2830-FLAG-OUTSTAND.
           IF ARSTAT-OUTSTAND-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARSTAT-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2840-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARSTAT-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARSTAT-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARSTAT-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARSTAT-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARSTAT-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2850-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARSTAT-ISSUED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ARSTAT-ISSUED-YTD - (WS-WORK-AMT * 3)
           COMPUTE ARSTAT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2860-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-NEXT-REVIEW-DATE > 0 AND ARSTAT-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARSTAT-ISSUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN CLOSE DATE IS PAST
      * SW -> UA WHEN CLOSE DATE IS PAST
      * UA -> WB WHEN CLOSE DATE IS PAST
      * WB -> ME WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2870-SET-CHANNEL.
           EVALUATE ARSTAT-CHANNEL-CD
               WHEN 'ME'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARSTAT-CLOSE-DATE > 0 AND ARSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2880-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2890-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARSTAT-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARSTAT-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ARSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ARSTAT-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ARSTAT-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ARSTAT-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ARSTAT EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
