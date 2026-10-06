      *================================================================*
      * PROGRAM  : CNCAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : COIN AND CURRENCY RULE - DATE RULES. CALLED BY THE  *
      *            COIN AND CURRENCY BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-06-18 EWS  ORIGINAL                                       *
      * 1987-02-18 SKN  FEE ROUNDING - HALF UP INC1681                 *
      * 1991-01-15 BHS  NEW CURRENCY REPORT COLUMNS REQ3629            *
      * 1991-07-01 CRP  FEE FEE SCHEDULE UPDATE INC7175                *
      * 1994-02-01 BHS  FEE CALCULATION CORRECTED PRB9509              *
      * 1994-06-28 EWS  VARIANCE STATUS CODES ADDED PRB9884            *
      * 1997-12-18 RJT  FEE ROUNDING - HALF UP CHG3650                 *
      * 1998-08-11 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-03-27 CRP  VARIANCE LIMIT REVISED REQ7956                 *
      * 2004-10-22 GAF  SHIPMENT CUTOFF MOVED CHG9503                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNCAL.
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
                                           VALUE 9.000000.
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
      * WS-COIN-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COIN-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000261000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000751000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000774000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000337500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000584500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015750000000782000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000176500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018750000000354000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018850000000235500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000111500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022350000000617000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022400000000834000'.
       01  WS-COIN-BAND-TBL REDEFINES WS-COIN-BAND-VALUES.
           05  WS-COIN-BAND-ROW OCCURS 12 TIMES.
               10  WS-COIN-BAND-LOW        PIC 9(09)V99.
               10  WS-COIN-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000574000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000898000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000557000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000365500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000507500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013150000000556500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015650000000397500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016150000000413000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018650000000013500'.
       01  WS-FEE-GRID-TBL REDEFINES WS-FEE-GRID-VALUES.
           05  WS-FEE-GRID-ROW OCCURS 9 TIMES.
               10  WS-FEE-GRID-LOW         PIC 9(09)V99.
               10  WS-FEE-GRID-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000091000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000850500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000123000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000236000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000753000'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 5 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CNCALP.

       PROCEDURE DIVISION USING CNCAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-ITEM
           PERFORM 2010-EDIT-DESC
           PERFORM 2020-ACCUM-SOURCE
           PERFORM 2030-CHECK-EXPIRY
           PERFORM 2040-SELECT-FEE-PCT
           PERFORM 2050-AGE-NEXT-REVIEW
           PERFORM 2060-SELECT-RATE
           PERFORM 2070-SET-CHANNEL
           PERFORM 2080-FLAG-COIN
           PERFORM 2090-FLAG-VARIANCE
           PERFORM 2100-DESCRIBE-CHANNEL
           PERFORM 2110-FLAG-SHIPMENT
           PERFORM 2120-SELECT-FEE-PCT
           PERFORM 2130-PRORATE-SHIPMENT
           PERFORM 2140-PRORATE-STRAP
           PERFORM 2150-ACCUM-CARRIER
           PERFORM 2160-SELECT-RATE
           PERFORM 2170-LIMIT-SHIPMENT
           PERFORM 2180-SELECT-FEE-PCT
           PERFORM 2190-COUNT-RETRY
           PERFORM 2200-LIMIT-STRAP
           PERFORM 2210-AGE-EXPIRY
           PERFORM 2220-PRORATE-SHIPMENT
           PERFORM 2230-COUNT-NOTICE
           PERFORM 2240-DESCRIBE-SOURCE
           PERFORM 2250-AGE-NEXT-REVIEW
           PERFORM 2260-EDIT-DESC
           PERFORM 2270-ACCRUE-VARIANCE
           PERFORM 2280-EDIT-NAME
           PERFORM 2290-PRORATE-STRAP
           PERFORM 2300-SPLIT-SHIPMENT
           PERFORM 2310-AGE-NEXT-REVIEW
           PERFORM 2320-EDIT-DESC
           PERFORM 2330-FLAG-ITEM
           PERFORM 2340-ACCUM-SOURCE
           PERFORM 2350-CHECK-EXPIRY
           PERFORM 2360-EDIT-DESC
           PERFORM 2370-PRORATE-VARIANCE
           PERFORM 2380-PRORATE-FEE
           PERFORM 2390-FLAG-SHIPMENT
           PERFORM 2400-PRORATE-SHIPMENT
           PERFORM 2410-COUNT-RETRY
           PERFORM 2420-DESCRIBE-SOURCE
           PERFORM 2430-LIMIT-ITEM
           PERFORM 2440-AGE-NEXT-REVIEW
           PERFORM 2450-EDIT-NAME
           PERFORM 2460-EDIT-DESC
           PERFORM 2470-AGE-MATURITY
           PERFORM 2480-EDIT-NAME
           PERFORM 2490-COUNT-RETRY
           PERFORM 2500-DESCRIBE-SOURCE
           PERFORM 2510-PRORATE-SHIPMENT
           PERFORM 2520-SET-SOURCE
           PERFORM 2530-SPLIT-COIN
           PERFORM 2540-AGE-NEXT-REVIEW
           PERFORM 2550-CHECK-OPEN
           PERFORM 2560-CHECK-MATURITY
           PERFORM 2570-FLAG-ITEM
           PERFORM 2580-PRORATE-COIN
           PERFORM 2590-DESCRIBE-CARRIER
           PERFORM 2600-ACCRUE-VARIANCE
           PERFORM 2610-EDIT-DESC
           PERFORM 2620-AGE-EXPIRY
           PERFORM 2630-FLAG-SHIPMENT
           PERFORM 2640-DESCRIBE-STATUS
           PERFORM 2650-SPLIT-STRAP
           PERFORM 2660-FLAG-FEE
           PERFORM 2670-EDIT-NAME
           PERFORM 2680-EDIT-DESC
           PERFORM 2690-ACCRUE-SHIPMENT
           PERFORM 2700-COUNT-RETRY
           PERFORM 2710-COUNT-RETRY
           PERFORM 2720-LIMIT-FEE
           PERFORM 2730-ACCUM-STATUS
           PERFORM 2740-LIMIT-ITEM
           PERFORM 2750-FLAG-ITEM
           PERFORM 2760-DESCRIBE-CARRIER
           PERFORM 2770-ASSESS-OVERRIDE
           PERFORM 2780-SPLIT-VARIANCE
           PERFORM 2790-DESCRIBE-SOURCE
           PERFORM 2800-ACCUM-CHANNEL
           PERFORM 2810-DESCRIBE-CARRIER
           PERFORM 2820-COUNT-RETRY
           PERFORM 2830-CHECK-OPEN
           PERFORM 2840-ASSESS-OVERRIDE
           PERFORM 2850-EDIT-DESC
           PERFORM 2860-LIMIT-VARIANCE
           PERFORM 2870-SELECT-FEE-PCT
           PERFORM 2880-CHECK-NEXT-REVIEW
           PERFORM 2890-EDIT-NAME
           PERFORM 2900-ACCUM-STATUS
           PERFORM 2910-PRORATE-VARIANCE
           PERFORM 2920-DESCRIBE-STATUS
           PERFORM 2930-LIMIT-STRAP
           PERFORM 2940-COUNT-NOTICE
           PERFORM 2950-EDIT-DESC
           PERFORM 2960-ACCRUE-FEE
           PERFORM 2970-DESCRIBE-CARRIER
           PERFORM 2980-FLAG-STRAP
           PERFORM 2990-COUNT-RETRY
           PERFORM 3000-ACCUM-STATUS
           PERFORM 3010-LIMIT-FEE
           PERFORM 3020-FLAG-ITEM
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CNCAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E965 DESC REQUIRED
      * E490 WORK AMT 2 NEGATIVE
      * E711 WORK AMT 2 OVER 10000
      * E147 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2010-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-DESC = SPACES
                   MOVE 'E965' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E490' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN CNCAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPMENT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2020-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNCAL-SOURCE-CD
                   ADD CNCAL-SHIPMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNCAL-SHIPMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2030-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CNCAL-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CNCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CNCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-NEXT-REVIEW-DATE > 0 AND CNCAL-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COIN BAND TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2060-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-BAND-RATE(1) TO CNCAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CNCAL-STRAP-YTD NOT < WS-COIN-BAND-LOW(WS-TX)
                   MOVE WS-COIN-BAND-RATE(WS-TX) TO CNCAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNCAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNCAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AW -> DW WHEN NEXT REVIEW DATE IS PAST
      * DW -> GN WHEN NEXT REVIEW DATE IS PAST
      * GN -> RS WHEN NEXT REVIEW DATE IS PAST
      * RS -> UZ WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2070-SET-CHANNEL.
           EVALUATE CNCAL-CHANNEL-CD
               WHEN 'AW'
                   IF CNCAL-NEXT-REVIEW-DATE > 0 AND
                       CNCAL-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DW' TO CNCAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DW'
                   IF CNCAL-NEXT-REVIEW-DATE > 0 AND
                       CNCAL-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GN' TO CNCAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GN'
                   IF CNCAL-NEXT-REVIEW-DATE > 0 AND
                       CNCAL-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RS' TO CNCAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF CNCAL-NEXT-REVIEW-DATE > 0 AND
                       CNCAL-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UZ' TO CNCAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COIN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2080-FLAG-COIN.
           IF CNCAL-COIN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CNCAL-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2090-FLAG-VARIANCE.
           IF CNCAL-VARIANCE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CNCAL-VARIANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2100-DESCRIBE-CHANNEL.
           EVALUATE CNCAL-CHANNEL-CD
               WHEN 'AW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2110-FLAG-SHIPMENT.
           IF CNCAL-SHIPMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CNCAL-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN BAND TIER FOR THE VARIANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2120-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-BAND-RATE(1) TO CNCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CNCAL-VARIANCE-AMT NOT < WS-COIN-BAND-LOW(WS-TX)
                   MOVE WS-COIN-BAND-RATE(WS-TX) TO CNCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       2130-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNCAL-SHIPMENT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNCAL-SHIPMENT-AMT
           END-IF
           ADD CNCAL-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STRAP YTD.
      *----------------------------------------------------------------*
       2140-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNCAL-STRAP-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CNCAL-STRAP-YTD
           END-IF
           ADD CNCAL-STRAP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2150-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNCAL-CARRIER-CD
                   ADD CNCAL-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNCAL-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNCAL-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2160-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO CNCAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF CNCAL-FEE-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO CNCAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNCAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNCAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2170-LIMIT-SHIPMENT.
           IF CNCAL-SHIPMENT-YTD < 50.00
               MOVE 50.00 TO CNCAL-SHIPMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNCAL-SHIPMENT-YTD > 5000.00
               MOVE 5000.00 TO CNCAL-SHIPMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE COIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2180-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO CNCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF CNCAL-COIN-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO CNCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2190-COUNT-RETRY.
           IF CNCAL-RETRY-CNT < 9999999
               ADD 1 TO CNCAL-RETRY-CNT
           END-IF
           IF CNCAL-RETRY-CNT NOT < 3
               MOVE 'Y' TO CNCAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STRAP YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2200-LIMIT-STRAP.
           IF CNCAL-STRAP-YTD < 0.00
               MOVE 0.00 TO CNCAL-STRAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNCAL-STRAP-YTD > 10000.00
               MOVE 10000.00 TO CNCAL-STRAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2210-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-EXPIRY-DATE > 0 AND CNCAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNCAL-SHIPMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CNCAL-SHIPMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CNCAL-SHIPMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CNCAL-SHIPMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNCAL-SHIPMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STRAP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT YTD.
      *----------------------------------------------------------------*
       2220-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNCAL-SHIPMENT-YTD ROUNDED =
                   CNCAL-STRAP-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNCAL-SHIPMENT-YTD
           END-IF
           ADD CNCAL-SHIPMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-NOTICE.
           IF CNCAL-NOTICE-CNT < 9999999
               ADD 1 TO CNCAL-NOTICE-CNT
           END-IF
           IF CNCAL-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CNCAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2240-DESCRIBE-SOURCE.
           EVALUATE CNCAL-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2250-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-NEXT-REVIEW-DATE > 0 AND CNCAL-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNCAL-STRAP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CNCAL-STRAP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CNCAL-STRAP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CNCAL-STRAP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNCAL-STRAP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E101 DESC REQUIRED
      * E187 COIN AMT NEGATIVE
      * E314 COIN AMT OVER 99999
      * E897 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2260-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-DESC = SPACES
                   MOVE 'E101' TO WS-REJECT-CODE
               WHEN CNCAL-COIN-AMT < 0
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN CNCAL-COIN-AMT > 99999.00
                   MOVE 'E314' TO WS-REJECT-CODE
               WHEN CNCAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E897' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-VARIANCE.
           IF CNCAL-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNCAL-VARIANCE-AMT * CNCAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO CNCAL-SHIPMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E857 NAME REQUIRED
      * E462 WORK AMT 2 NEGATIVE
      * E184 WORK AMT 2 OVER 10000
      * E439 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2280-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-NAME = SPACES
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E462' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E184' TO WS-REJECT-CODE
               WHEN CNCAL-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STRAP YTD.
      *----------------------------------------------------------------*
       2290-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNCAL-STRAP-YTD ROUNDED =
                   CNCAL-COIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNCAL-STRAP-YTD
           END-IF
           ADD CNCAL-STRAP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2300-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CNCAL-SHIPMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CNCAL-SHIPMENT-AMT - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2310-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-NEXT-REVIEW-DATE > 0 AND CNCAL-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNCAL-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CNCAL-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CNCAL-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CNCAL-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNCAL-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E240 DESC REQUIRED
      * E419 VARIANCE AMT NEGATIVE
      * E359 VARIANCE AMT OVER 5000
      * E659 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2320-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-DESC = SPACES
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN CNCAL-VARIANCE-AMT < 0
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN CNCAL-VARIANCE-AMT > 5000.00
                   MOVE 'E359' TO WS-REJECT-CODE
               WHEN CNCAL-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E659' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2330-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNCAL-SOURCE-CD
                   ADD CNCAL-COIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNCAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNCAL-COIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CNCAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E895 DESC REQUIRED
      * E596 SHIPMENT YTD NEGATIVE
      * E673 SHIPMENT YTD OVER 10000
      * E681 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2360-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-DESC = SPACES
                   MOVE 'E895' TO WS-REJECT-CODE
               WHEN CNCAL-SHIPMENT-YTD < 0
                   MOVE 'E596' TO WS-REJECT-CODE
               WHEN CNCAL-SHIPMENT-YTD > 10000.00
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN CNCAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VARIANCE AMT.
      *----------------------------------------------------------------*
       2370-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNCAL-VARIANCE-AMT ROUNDED =
                   CNCAL-SHIPMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNCAL-VARIANCE-AMT
           END-IF
           ADD CNCAL-VARIANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       2380-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNCAL-FEE-YTD ROUNDED =
                   CNCAL-SHIPMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNCAL-FEE-YTD
           END-IF
           ADD CNCAL-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2390-FLAG-SHIPMENT.
           IF CNCAL-SHIPMENT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CNCAL-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT YTD.
      *----------------------------------------------------------------*
       2400-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNCAL-SHIPMENT-YTD ROUNDED =
                   CNCAL-VARIANCE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNCAL-SHIPMENT-YTD
           END-IF
           ADD CNCAL-SHIPMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-RETRY.
           IF CNCAL-RETRY-CNT < 9999999
               ADD 1 TO CNCAL-RETRY-CNT
           END-IF
           IF CNCAL-RETRY-CNT NOT < 10
               MOVE 'Y' TO CNCAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2420-DESCRIBE-SOURCE.
           EVALUATE CNCAL-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2430-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2440-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-NEXT-REVIEW-DATE > 0 AND CNCAL-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNCAL-STRAP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CNCAL-STRAP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CNCAL-STRAP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CNCAL-STRAP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNCAL-STRAP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E181 NAME REQUIRED
      * E572 COIN AMT NEGATIVE
      * E663 COIN AMT OVER 10000
      * E214 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2450-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-NAME = SPACES
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN CNCAL-COIN-AMT < 0
                   MOVE 'E572' TO WS-REJECT-CODE
               WHEN CNCAL-COIN-AMT > 10000.00
                   MOVE 'E663' TO WS-REJECT-CODE
               WHEN CNCAL-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E214' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E999 DESC REQUIRED
      * E294 WORK AMT NEGATIVE
      * E676 WORK AMT OVER 5000
      * E502 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2460-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-DESC = SPACES
                   MOVE 'E999' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN CNCAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E502' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2470-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-MATURITY-DATE > 0 AND CNCAL-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E245 NAME REQUIRED
      * E129 VARIANCE AMT NEGATIVE
      * E748 VARIANCE AMT OVER 25000
      * E176 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2480-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-NAME = SPACES
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN CNCAL-VARIANCE-AMT < 0
                   MOVE 'E129' TO WS-REJECT-CODE
               WHEN CNCAL-VARIANCE-AMT > 25000.00
                   MOVE 'E748' TO WS-REJECT-CODE
               WHEN CNCAL-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2490-COUNT-RETRY.
           IF CNCAL-RETRY-CNT < 9999999
               ADD 1 TO CNCAL-RETRY-CNT
           END-IF
           IF CNCAL-RETRY-CNT NOT < 12
               MOVE 'Y' TO CNCAL-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2500-DESCRIBE-SOURCE.
           EVALUATE CNCAL-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       2510-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNCAL-SHIPMENT-AMT ROUNDED =
                   CNCAL-VARIANCE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNCAL-SHIPMENT-AMT
           END-IF
           ADD CNCAL-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CFR -> DTX WHEN OPEN DATE IS PAST
      * DTX -> EEA WHEN OPEN DATE IS PAST
      * EEA -> EUA WHEN OPEN DATE IS PAST
      * EUA -> TPB WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2520-SET-SOURCE.
           EVALUATE CNCAL-SOURCE-CD
               WHEN 'CFR'
                   IF CNCAL-OPEN-DATE > 0 AND CNCAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DTX' TO CNCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DTX'
                   IF CNCAL-OPEN-DATE > 0 AND CNCAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EEA' TO CNCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EEA'
                   IF CNCAL-OPEN-DATE > 0 AND CNCAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO CNCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EUA'
                   IF CNCAL-OPEN-DATE > 0 AND CNCAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TPB' TO CNCAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COIN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CNCAL-COIN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CNCAL-COIN-AMT - (WS-WORK-AMT * 2)
           COMPUTE CNCAL-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-NEXT-REVIEW-DATE > 0 AND CNCAL-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNCAL-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2550-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CNCAL-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CNCAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2570-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COIN AMT.
      *----------------------------------------------------------------*
       2580-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNCAL-COIN-AMT ROUNDED =
                   CNCAL-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNCAL-COIN-AMT
           END-IF
           ADD CNCAL-COIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2590-DESCRIBE-CARRIER.
           EVALUATE CNCAL-CARRIER-CD
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2600-ACCRUE-VARIANCE.
           IF CNCAL-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNCAL-VARIANCE-AMT * CNCAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO CNCAL-SHIPMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E045 DESC REQUIRED
      * E849 FEE YTD NEGATIVE
      * E313 FEE YTD OVER 1000000
      * E643 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-DESC = SPACES
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN CNCAL-FEE-YTD < 0
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN CNCAL-FEE-YTD > 1000000.00
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN CNCAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E643' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2620-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-EXPIRY-DATE > 0 AND CNCAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNCAL-SHIPMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CNCAL-SHIPMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CNCAL-SHIPMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CNCAL-SHIPMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNCAL-SHIPMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2630-FLAG-SHIPMENT.
           IF CNCAL-SHIPMENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CNCAL-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2640-DESCRIBE-STATUS.
           EVALUATE CNCAL-STATUS-CD
               WHEN 'ADH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FVV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GCB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UZP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VXB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WTJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XNG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STRAP YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CNCAL-STRAP-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CNCAL-STRAP-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2660-FLAG-FEE.
           IF CNCAL-FEE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CNCAL-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E975 NAME REQUIRED
      * E577 FEE YTD NEGATIVE
      * E100 FEE YTD OVER 1000000
      * E078 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2670-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-NAME = SPACES
                   MOVE 'E975' TO WS-REJECT-CODE
               WHEN CNCAL-FEE-YTD < 0
                   MOVE 'E577' TO WS-REJECT-CODE
               WHEN CNCAL-FEE-YTD > 1000000.00
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN CNCAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E078' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E624 DESC REQUIRED
      * E666 SHIPMENT AMT NEGATIVE
      * E332 SHIPMENT AMT OVER 1000000
      * E238 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2680-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-DESC = SPACES
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN CNCAL-SHIPMENT-AMT < 0
                   MOVE 'E666' TO WS-REJECT-CODE
               WHEN CNCAL-SHIPMENT-AMT > 1000000.00
                   MOVE 'E332' TO WS-REJECT-CODE
               WHEN CNCAL-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2690-ACCRUE-SHIPMENT.
           IF CNCAL-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNCAL-SHIPMENT-AMT * CNCAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CNCAL-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2700-COUNT-RETRY.
           IF CNCAL-RETRY-CNT < 9999999
               ADD 1 TO CNCAL-RETRY-CNT
           END-IF
           IF CNCAL-RETRY-CNT NOT < 10
               MOVE 'Y' TO CNCAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2710-COUNT-RETRY.
           IF CNCAL-RETRY-CNT < 9999999
               ADD 1 TO CNCAL-RETRY-CNT
           END-IF
           IF CNCAL-RETRY-CNT NOT < 10
               MOVE 'Y' TO CNCAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2720-LIMIT-FEE.
           IF CNCAL-FEE-YTD < 100.00
               MOVE 100.00 TO CNCAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNCAL-FEE-YTD > 10000.00
               MOVE 10000.00 TO CNCAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2730-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNCAL-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNCAL-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2740-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2750-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2760-DESCRIBE-CARRIER.
           EVALUATE CNCAL-CARRIER-CD
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COIN FEE ON THE SHIPMENT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2770-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CNCAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CNCAL-COIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNCAL-EXEMPT-FLAG = 'Y'
                   COMPUTE CNCAL-COIN-AMT ROUNDED = CNCAL-SHIPMENT-YTD *
                       CNCAL-RATE / 200
               WHEN CNCAL-STATUS-CD = 'ADH'
                   MOVE 5.00 TO CNCAL-COIN-AMT
               WHEN CNCAL-STATUS-CD = 'FVV'
                   MOVE 15.00 TO CNCAL-COIN-AMT
               WHEN OTHER
                   COMPUTE CNCAL-COIN-AMT ROUNDED = CNCAL-SHIPMENT-YTD *
                       CNCAL-RATE / 100
           END-EVALUATE
           IF CNCAL-COIN-AMT > 0 AND CNCAL-COIN-AMT < 5.00
               MOVE 5.00 TO CNCAL-COIN-AMT
           END-IF
           IF CNCAL-COIN-AMT > 50.00
               MOVE 50.00 TO CNCAL-COIN-AMT
           END-IF
           ADD CNCAL-COIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2780-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE CNCAL-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2790-DESCRIBE-SOURCE.
           EVALUATE CNCAL-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2800-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNCAL-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNCAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2810-DESCRIBE-CARRIER.
           EVALUATE CNCAL-CARRIER-CD
               WHEN 'J'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2820-COUNT-RETRY.
           IF CNCAL-RETRY-CNT < 9999999
               ADD 1 TO CNCAL-RETRY-CNT
           END-IF
           IF CNCAL-RETRY-CNT NOT < 3
               MOVE 'Y' TO CNCAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2830-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CNCAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE VARIANCE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2840-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CNCAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNCAL-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CNCAL-VARIANCE-AMT *
                       CNCAL-FEE-PCT / 200
               WHEN CNCAL-SOURCE-CD = 'CFR'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN CNCAL-SOURCE-CD = 'DTX'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CNCAL-VARIANCE-AMT *
                       CNCAL-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E676 DESC REQUIRED
      * E819 FEE YTD NEGATIVE
      * E899 FEE YTD OVER 99999
      * E277 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2850-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-DESC = SPACES
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN CNCAL-FEE-YTD < 0
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN CNCAL-FEE-YTD > 99999.00
                   MOVE 'E899' TO WS-REJECT-CODE
               WHEN CNCAL-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2860-LIMIT-VARIANCE.
           IF CNCAL-VARIANCE-AMT < 0.00
               MOVE 0.00 TO CNCAL-VARIANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNCAL-VARIANCE-AMT > 500.00
               MOVE 500.00 TO CNCAL-VARIANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN BAND TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2870-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-BAND-RATE(1) TO CNCAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CNCAL-STRAP-YTD NOT < WS-COIN-BAND-LOW(WS-TX)
                   MOVE WS-COIN-BAND-RATE(WS-TX) TO CNCAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNCAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNCAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2880-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNCAL-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNCAL-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CNCAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E438 NAME REQUIRED
      * E789 STRAP YTD NEGATIVE
      * E021 STRAP YTD OVER 25000
      * E156 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2890-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-NAME = SPACES
                   MOVE 'E438' TO WS-REJECT-CODE
               WHEN CNCAL-STRAP-YTD < 0
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN CNCAL-STRAP-YTD > 25000.00
                   MOVE 'E021' TO WS-REJECT-CODE
               WHEN CNCAL-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPMENT YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2900-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNCAL-STATUS-CD
                   ADD CNCAL-SHIPMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNCAL-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNCAL-SHIPMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VARIANCE AMT.
      *----------------------------------------------------------------*
       2910-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNCAL-VARIANCE-AMT ROUNDED =
                   CNCAL-COIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNCAL-VARIANCE-AMT
           END-IF
           ADD CNCAL-VARIANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2920-DESCRIBE-STATUS.
           EVALUATE CNCAL-STATUS-CD
               WHEN 'ADH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FVV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GCB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UZP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VXB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WTJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XNG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STRAP YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2930-LIMIT-STRAP.
           IF CNCAL-STRAP-YTD < 100.00
               MOVE 100.00 TO CNCAL-STRAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNCAL-STRAP-YTD > 50000.00
               MOVE 50000.00 TO CNCAL-STRAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2940-COUNT-NOTICE.
           IF CNCAL-NOTICE-CNT < 9999999
               ADD 1 TO CNCAL-NOTICE-CNT
           END-IF
           IF CNCAL-NOTICE-CNT NOT < 3
               MOVE 'Y' TO CNCAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E634 DESC REQUIRED
      * E884 STRAP YTD NEGATIVE
      * E440 STRAP YTD OVER 50000
      * E667 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2950-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNCAL-DESC = SPACES
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN CNCAL-STRAP-YTD < 0
                   MOVE 'E884' TO WS-REJECT-CODE
               WHEN CNCAL-STRAP-YTD > 50000.00
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN CNCAL-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E667' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2960-ACCRUE-FEE.
           IF CNCAL-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNCAL-FEE-YTD * CNCAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CNCAL-STRAP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2970-DESCRIBE-CARRIER.
           EVALUATE CNCAL-CARRIER-CD
               WHEN 'J'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STRAP YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2980-FLAG-STRAP.
           IF CNCAL-STRAP-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CNCAL-STRAP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2990-COUNT-RETRY.
           IF CNCAL-RETRY-CNT < 9999999
               ADD 1 TO CNCAL-RETRY-CNT
           END-IF
           IF CNCAL-RETRY-CNT NOT < 6
               MOVE 'Y' TO CNCAL-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STRAP YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNCAL-STATUS-CD
                   ADD CNCAL-STRAP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNCAL-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNCAL-STRAP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-FEE.
           IF CNCAL-FEE-YTD < 100.00
               MOVE 100.00 TO CNCAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNCAL-FEE-YTD > 50000.00
               MOVE 50000.00 TO CNCAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3020-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CNCAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CNCAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CNCAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CNCAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
