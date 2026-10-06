      *================================================================*
      * PROGRAM  : YKPRC                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DATE WINDOW BRIDGE RULE - LIMITS. CALLED BY THE DATE*
      *            WINDOW BRIDGE BATCH AND ONLINE PROGRAMS.            *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-23 MKL  ORIGINAL                                       *
      * 1995-10-09 RJT  AMOUNT-2 TABLE EXPANDED REQ0613                *
      * 1997-10-13 PQR  AUDIT FINDING - AMOUNT-2 CONTROL TOTALS INC7875*
      * 1997-02-12 TWH  AMOUNT-2 LIMIT REVISED INC2710                 *
      * 1998-12-23 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-02-12 DPS  CONTROL FEE SCHEDULE UPDATE PRB3122            *
      * 2002-06-15 BHS  PERFORMANCE - AMOUNT-2 READ AHEAD CHG0526      *
      * 2003-07-03 JLM  AMOUNT-2 RERUN PROTECTION PRB3067              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKPRC.
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
                                           VALUE 17.000000.
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
      * WS-AMOUNT-2-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-2-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000827000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000758000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000478000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000761000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000390000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018750000000333000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019750000000879500'.
       01  WS-AMOUNT-2-GRID-TBL REDEFINES WS-AMOUNT-2-GRID-VALUES.
           05  WS-AMOUNT-2-GRID-ROW OCCURS 7 TIMES.
               10  WS-AMOUNT-2-GRID-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-2-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-AMOUNT-1-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-1-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000354000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000005000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000598500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000246500'.
       01  WS-AMOUNT-1-TIER-TBL REDEFINES WS-AMOUNT-1-TIER-VALUES.
           05  WS-AMOUNT-1-TIER-ROW OCCURS 4 TIMES.
               10  WS-AMOUNT-1-TIER-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-1-TIER-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY YKPRCP.

       PROCEDURE DIVISION USING YKPRC-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CHECK-LAST-STMT
           PERFORM 2010-AGE-MATURITY
           PERFORM 2020-LIMIT-AMOUNT-2
           PERFORM 2030-SPLIT-AMOUNT-1
           PERFORM 2040-COUNT-ITEM
           PERFORM 2050-AGE-EXPIRY
           PERFORM 2060-COUNT-ITEM
           PERFORM 2070-AGE-EXPIRY
           PERFORM 2080-AGE-EXPIRY
           PERFORM 2090-PRORATE-AMOUNT-2
           PERFORM 2100-COUNT-PAYMENT
           PERFORM 2110-LIMIT-CONTROL
           PERFORM 2120-AGE-EXPIRY
           PERFORM 2130-AGE-MATURITY
           PERFORM 2140-LIMIT-ITEM
           PERFORM 2150-ACCUM-CYCLE
           PERFORM 2160-SET-SOURCE
           PERFORM 2170-ASSESS-NOTICE-SENT
           PERFORM 2180-ACCUM-PIVOT
           PERFORM 2190-LIMIT-AMOUNT-2
           PERFORM 2200-COUNT-PAYMENT
           PERFORM 2210-LIMIT-ITEM
           PERFORM 2220-SET-PIVOT
           PERFORM 2230-ACCUM-REGION
           PERFORM 2240-AGE-MATURITY
           PERFORM 2250-EDIT-CONTACT
           PERFORM 2260-LIMIT-AMOUNT-1
           PERFORM 2270-DESCRIBE-PIVOT
           PERFORM 2280-FLAG-AMOUNT-2
           PERFORM 2290-COUNT-PAYMENT
           PERFORM 2300-SELECT-DISCOUNT
           PERFORM 2310-FLAG-AMOUNT-2
           PERFORM 2320-ACCRUE-DISCOUNT
           PERFORM 2330-CHECK-LAST-STMT
           PERFORM 2340-PRORATE-CONTROL
           PERFORM 2350-SELECT-MARGIN
           PERFORM 2360-COUNT-ITEM
           PERFORM 2370-CHECK-LAST-ACT
           PERFORM 2380-ASSESS-PREFERRED
           PERFORM 2390-ACCRUE-AMOUNT-2
           PERFORM 2400-LIMIT-ITEM
           PERFORM 2410-COUNT-PAYMENT
           PERFORM 2420-COUNT-ITEM
           PERFORM 2430-FLAG-ITEM
           PERFORM 2440-COUNT-ITEM
           PERFORM 2450-ACCUM-CYCLE
           PERFORM 2460-AGE-EXPIRY
           PERFORM 2470-SPLIT-AMOUNT-1
           PERFORM 2480-COUNT-PAYMENT
           PERFORM 2490-EDIT-NAME
           PERFORM 2500-AGE-MATURITY
           PERFORM 2510-SELECT-MARGIN
           PERFORM 2520-CHECK-LAST-ACT
           PERFORM 2530-PRORATE-AMOUNT-2
           PERFORM 2540-SELECT-DISCOUNT
           PERFORM 2550-FLAG-CONTROL
           PERFORM 2560-CHECK-MATURITY
           PERFORM 2570-CHECK-LAST-STMT
           PERFORM 2580-ACCUM-REGION
           PERFORM 2590-ACCRUE-CONTROL
           PERFORM 2600-SPLIT-AMOUNT-1
           PERFORM 2610-DESCRIBE-REGION
           PERFORM 2620-ASSESS-NOTICE-SENT
           PERFORM 2630-ACCUM-REGION
           PERFORM 2640-FLAG-ITEM
           PERFORM 2650-SET-SOURCE
           PERFORM 2660-ASSESS-PREFERRED
           PERFORM 2670-AGE-EXPIRY
           PERFORM 2680-LIMIT-CONTROL
           PERFORM 2690-FLAG-AMOUNT-1
           PERFORM 2700-ACCUM-PIVOT
           PERFORM 2710-LIMIT-ITEM
           PERFORM 2720-FLAG-CONTROL
           PERFORM 2730-FLAG-CONTROL
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO YKPRC-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2000-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO YKPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-MATURITY-DATE > 0 AND YKPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKPRC-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YKPRC-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YKPRC-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD YKPRC-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKPRC-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2020-LIMIT-AMOUNT-2.
           IF YKPRC-AMOUNT-2-AMT < 10.00
               MOVE 10.00 TO YKPRC-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKPRC-AMOUNT-2-AMT > 1000.00
               MOVE 1000.00 TO YKPRC-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2030-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YKPRC-AMOUNT-1-AMT / 12
           COMPUTE WS-WORK-AMT-2 = YKPRC-AMOUNT-1-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-ITEM.
           IF YKPRC-ITEM-CNT < 9999999
               ADD 1 TO YKPRC-ITEM-CNT
           END-IF
           IF YKPRC-ITEM-CNT NOT < 6
               MOVE 'Y' TO YKPRC-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-EXPIRY-DATE > 0 AND YKPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2060-COUNT-ITEM.
           IF YKPRC-ITEM-CNT < 9999999
               ADD 1 TO YKPRC-ITEM-CNT
           END-IF
           IF YKPRC-ITEM-CNT NOT < 6
               MOVE 'Y' TO YKPRC-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-EXPIRY-DATE > 0 AND YKPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKPRC-CONTROL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YKPRC-CONTROL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YKPRC-CONTROL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YKPRC-CONTROL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKPRC-CONTROL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2080-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-EXPIRY-DATE > 0 AND YKPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKPRC-AMOUNT-1-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YKPRC-AMOUNT-1-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YKPRC-AMOUNT-1-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD YKPRC-AMOUNT-1-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKPRC-AMOUNT-1-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       2090-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKPRC-AMOUNT-2-AMT ROUNDED =
                   YKPRC-CONTROL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YKPRC-AMOUNT-2-AMT
           END-IF
           ADD YKPRC-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2100-COUNT-PAYMENT.
           IF YKPRC-PAYMENT-CNT < 9999999
               ADD 1 TO YKPRC-PAYMENT-CNT
           END-IF
           IF YKPRC-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO YKPRC-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2110-LIMIT-CONTROL.
           IF YKPRC-CONTROL-YTD < 25.00
               MOVE 25.00 TO YKPRC-CONTROL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKPRC-CONTROL-YTD > 1000.00
               MOVE 1000.00 TO YKPRC-CONTROL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2120-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-EXPIRY-DATE > 0 AND YKPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-EXPIRY-DATE TO DTU-DATE-1
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
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2130-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-MATURITY-DATE > 0 AND YKPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-MATURITY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2140-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2150-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKPRC-CYCLE-CD
                   ADD YKPRC-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKPRC-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKPRC-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN EXPIRY DATE IS PAST
      * FTB -> GAP WHEN EXPIRY DATE IS PAST
      * GAP -> PAP WHEN EXPIRY DATE IS PAST
      * PAP -> VGC WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-SOURCE.
           EVALUATE YKPRC-SOURCE-CD
               WHEN 'BNX'
                   IF YKPRC-EXPIRY-DATE > 0 AND YKPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YKPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YKPRC-EXPIRY-DATE > 0 AND YKPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YKPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YKPRC-EXPIRY-DATE > 0 AND YKPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YKPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YKPRC-EXPIRY-DATE > 0 AND YKPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YKPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2170-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN YKPRC-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO YKPRC-CONTROL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKPRC-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YKPRC-CONTROL-YTD ROUNDED = WS-WORK-AMT *
                       YKPRC-DISCOUNT / 200
               WHEN YKPRC-REGION-CD = 'C'
                   MOVE 20.00 TO YKPRC-CONTROL-YTD
               WHEN YKPRC-REGION-CD = 'H'
                   MOVE 12.00 TO YKPRC-CONTROL-YTD
               WHEN OTHER
                   COMPUTE YKPRC-CONTROL-YTD ROUNDED = WS-WORK-AMT *
                       YKPRC-DISCOUNT / 100
           END-EVALUATE
           IF YKPRC-CONTROL-YTD > 0 AND YKPRC-CONTROL-YTD < 2.00
               MOVE 2.00 TO YKPRC-CONTROL-YTD
           END-IF
           IF YKPRC-CONTROL-YTD > 25.00
               MOVE 25.00 TO YKPRC-CONTROL-YTD
           END-IF
           ADD YKPRC-CONTROL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2180-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKPRC-PIVOT-CD
                   ADD YKPRC-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKPRC-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKPRC-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2190-LIMIT-AMOUNT-2.
           IF YKPRC-AMOUNT-2-YTD < 0.00
               MOVE 0.00 TO YKPRC-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKPRC-AMOUNT-2-YTD > 50000.00
               MOVE 50000.00 TO YKPRC-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-PAYMENT.
           IF YKPRC-PAYMENT-CNT < 9999999
               ADD 1 TO YKPRC-PAYMENT-CNT
           END-IF
           IF YKPRC-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO YKPRC-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2210-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN LAST ACT DATE IS PAST
      * NNG -> PTC WHEN LAST ACT DATE IS PAST
      * PTC -> WZW WHEN LAST ACT DATE IS PAST
      * WZW -> KGT WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2220-SET-PIVOT.
           EVALUATE YKPRC-PIVOT-CD
               WHEN 'KGT'
                   IF YKPRC-LAST-ACT-DATE > 0 AND YKPRC-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YKPRC-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YKPRC-LAST-ACT-DATE > 0 AND YKPRC-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YKPRC-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YKPRC-LAST-ACT-DATE > 0 AND YKPRC-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YKPRC-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YKPRC-LAST-ACT-DATE > 0 AND YKPRC-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YKPRC-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2230-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKPRC-REGION-CD
                   ADD YKPRC-CONTROL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKPRC-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKPRC-CONTROL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2240-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-MATURITY-DATE > 0 AND YKPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKPRC-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YKPRC-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YKPRC-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD YKPRC-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKPRC-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E371 CONTACT REQUIRED
      * E234 AMOUNT 1 YTD NEGATIVE
      * E180 AMOUNT 1 YTD OVER 50000
      * E200 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2250-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKPRC-CONTACT = SPACES
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN YKPRC-AMOUNT-1-YTD < 0
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN YKPRC-AMOUNT-1-YTD > 50000.00
                   MOVE 'E180' TO WS-REJECT-CODE
               WHEN YKPRC-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2260-LIMIT-AMOUNT-1.
           IF YKPRC-AMOUNT-1-YTD < 100.00
               MOVE 100.00 TO YKPRC-AMOUNT-1-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKPRC-AMOUNT-1-YTD > 1000.00
               MOVE 1000.00 TO YKPRC-AMOUNT-1-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2270-DESCRIBE-PIVOT.
           EVALUATE YKPRC-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2280-FLAG-AMOUNT-2.
           IF YKPRC-AMOUNT-2-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YKPRC-AMOUNT-2-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2290-COUNT-PAYMENT.
           IF YKPRC-PAYMENT-CNT < 9999999
               ADD 1 TO YKPRC-PAYMENT-CNT
           END-IF
           IF YKPRC-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO YKPRC-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 TIER TIER FOR THE AMOUNT 2 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2300-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-TIER-RATE(1) TO YKPRC-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YKPRC-AMOUNT-2-YTD NOT < WS-AMOUNT-1-TIER-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-TIER-RATE(WS-TX) TO YKPRC-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKPRC-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKPRC-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2310-FLAG-AMOUNT-2.
           IF YKPRC-AMOUNT-2-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YKPRC-AMOUNT-2-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2320-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * YKPRC-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO YKPRC-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2330-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO YKPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       2340-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKPRC-CONTROL-AMT ROUNDED =
                   YKPRC-AMOUNT-1-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YKPRC-CONTROL-AMT
           END-IF
           ADD YKPRC-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 TIER TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2350-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-TIER-RATE(1) TO YKPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YKPRC-AMOUNT-2-AMT NOT < WS-AMOUNT-1-TIER-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-TIER-RATE(WS-TX) TO YKPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-ITEM.
           IF YKPRC-ITEM-CNT < 9999999
               ADD 1 TO YKPRC-ITEM-CNT
           END-IF
           IF YKPRC-ITEM-CNT NOT < 6
               MOVE 'Y' TO YKPRC-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2370-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO YKPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2380-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YKPRC-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKPRC-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       YKPRC-DISCOUNT / 200
               WHEN YKPRC-SOURCE-CD = 'BNX'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN YKPRC-SOURCE-CD = 'FTB'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       YKPRC-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2390-ACCRUE-AMOUNT-2.
           IF YKPRC-AMOUNT-2-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKPRC-AMOUNT-2-YTD * YKPRC-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2400-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-PAYMENT.
           IF YKPRC-PAYMENT-CNT < 9999999
               ADD 1 TO YKPRC-PAYMENT-CNT
           END-IF
           IF YKPRC-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO YKPRC-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2420-COUNT-ITEM.
           IF YKPRC-ITEM-CNT < 9999999
               ADD 1 TO YKPRC-ITEM-CNT
           END-IF
           IF YKPRC-ITEM-CNT NOT < 12
               MOVE 'Y' TO YKPRC-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2430-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2440-COUNT-ITEM.
           IF YKPRC-ITEM-CNT < 9999999
               ADD 1 TO YKPRC-ITEM-CNT
           END-IF
           IF YKPRC-ITEM-CNT NOT < 12
               MOVE 'Y' TO YKPRC-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2450-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKPRC-CYCLE-CD
                   ADD YKPRC-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKPRC-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKPRC-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2460-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-EXPIRY-DATE > 0 AND YKPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKPRC-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YKPRC-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD YKPRC-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD YKPRC-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKPRC-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2470-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YKPRC-AMOUNT-1-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YKPRC-AMOUNT-1-AMT - (WS-WORK-AMT *
               2)
           COMPUTE YKPRC-CONTROL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2480-COUNT-PAYMENT.
           IF YKPRC-PAYMENT-CNT < 9999999
               ADD 1 TO YKPRC-PAYMENT-CNT
           END-IF
           IF YKPRC-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO YKPRC-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E924 NAME REQUIRED
      * E407 AMOUNT 1 AMT NEGATIVE
      * E412 AMOUNT 1 AMT OVER 1000000
      * E638 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2490-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKPRC-NAME = SPACES
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN YKPRC-AMOUNT-1-AMT < 0
                   MOVE 'E407' TO WS-REJECT-CODE
               WHEN YKPRC-AMOUNT-1-AMT > 1000000.00
                   MOVE 'E412' TO WS-REJECT-CODE
               WHEN YKPRC-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E638' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2500-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-MATURITY-DATE > 0 AND YKPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 TIER TIER FOR THE AMOUNT 1 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2510-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-TIER-RATE(1) TO YKPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YKPRC-AMOUNT-1-YTD NOT < WS-AMOUNT-1-TIER-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-TIER-RATE(WS-TX) TO YKPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2520-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO YKPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       2530-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKPRC-AMOUNT-2-AMT ROUNDED =
                   YKPRC-AMOUNT-2-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YKPRC-AMOUNT-2-AMT
           END-IF
           ADD YKPRC-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 2 GRID TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2540-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-2-GRID-RATE(1) TO YKPRC-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF YKPRC-AMOUNT-2-AMT NOT < WS-AMOUNT-2-GRID-LOW(WS-TX)
                   MOVE WS-AMOUNT-2-GRID-RATE(WS-TX) TO YKPRC-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKPRC-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKPRC-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2550-FLAG-CONTROL.
           IF YKPRC-CONTROL-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YKPRC-CONTROL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO YKPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO YKPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKPRC-REGION-CD
                   ADD YKPRC-CONTROL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKPRC-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKPRC-CONTROL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-CONTROL.
           IF YKPRC-CONTROL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKPRC-CONTROL-YTD * YKPRC-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2600-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE YKPRC-AMOUNT-1-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2610-DESCRIBE-REGION.
           EVALUATE YKPRC-REGION-CD
               WHEN 'C'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'V'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 YTD FEE ON THE CONTROL AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2620-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN YKPRC-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO YKPRC-AMOUNT-1-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKPRC-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YKPRC-AMOUNT-1-YTD ROUNDED =
                       YKPRC-CONTROL-AMT * YKPRC-MARGIN / 200
               WHEN YKPRC-REGION-CD = 'C'
                   MOVE 15.00 TO YKPRC-AMOUNT-1-YTD
               WHEN YKPRC-REGION-CD = 'H'
                   MOVE 10.00 TO YKPRC-AMOUNT-1-YTD
               WHEN OTHER
                   COMPUTE YKPRC-AMOUNT-1-YTD ROUNDED =
                       YKPRC-CONTROL-AMT * YKPRC-MARGIN / 100
           END-EVALUATE
           IF YKPRC-AMOUNT-1-YTD > 0 AND YKPRC-AMOUNT-1-YTD < 10.00
               MOVE 10.00 TO YKPRC-AMOUNT-1-YTD
           END-IF
           IF YKPRC-AMOUNT-1-YTD > 35.00
               MOVE 35.00 TO YKPRC-AMOUNT-1-YTD
           END-IF
           ADD YKPRC-AMOUNT-1-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2630-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKPRC-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKPRC-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2640-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN LAST STMT DATE IS PAST
      * FTB -> GAP WHEN LAST STMT DATE IS PAST
      * GAP -> PAP WHEN LAST STMT DATE IS PAST
      * PAP -> VGC WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2650-SET-SOURCE.
           EVALUATE YKPRC-SOURCE-CD
               WHEN 'BNX'
                   IF YKPRC-LAST-STMT-DATE > 0 AND YKPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YKPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YKPRC-LAST-STMT-DATE > 0 AND YKPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YKPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YKPRC-LAST-STMT-DATE > 0 AND YKPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YKPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YKPRC-LAST-STMT-DATE > 0 AND YKPRC-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YKPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2660-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YKPRC-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKPRC-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       YKPRC-MARGIN / 200
               WHEN YKPRC-SOURCE-CD = 'BNX'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN YKPRC-SOURCE-CD = 'FTB'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       YKPRC-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2670-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YKPRC-EXPIRY-DATE > 0 AND YKPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKPRC-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YKPRC-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YKPRC-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD YKPRC-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKPRC-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2680-LIMIT-CONTROL.
           IF YKPRC-CONTROL-AMT < 50.00
               MOVE 50.00 TO YKPRC-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKPRC-CONTROL-AMT > 1000.00
               MOVE 1000.00 TO YKPRC-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2690-FLAG-AMOUNT-1.
           IF YKPRC-AMOUNT-1-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YKPRC-AMOUNT-1-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2700-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKPRC-PIVOT-CD
                   ADD YKPRC-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKPRC-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKPRC-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2710-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2720-FLAG-CONTROL.
           IF YKPRC-CONTROL-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YKPRC-CONTROL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2730-FLAG-CONTROL.
           IF YKPRC-CONTROL-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YKPRC-CONTROL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO YKPRC-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO YKPRC-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO YKPRC-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'YKPRC EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
