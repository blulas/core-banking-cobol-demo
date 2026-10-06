      *================================================================*
      * PROGRAM  : PBPRC                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PASSBOOK PRINTING RULE - FEES. CALLED BY THE        *
      *            PASSBOOK PRINTING BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-05-22 BHS  ORIGINAL                                       *
      * 1998-05-13 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2005-01-06 RJT  BALANCE CALCULATION CORRECTED PRB5392          *
      * 2005-10-09 TWH  LINE CALCULATION CORRECTED PRB8131             *
      * 2008-10-26 PQR  LINE TABLE EXPANDED PRB0581                    *
      * 2009-09-20 DPS  INTEREST FEE SCHEDULE UPDATE INC8474           *
      * 2012-03-20 JLM  LINE EXCEPTIONS TO OPS QUEUE CHG6120           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBPRC.
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
                                           VALUE 8.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 91.
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
      * WS-INTEREST-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000513000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000714000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000584000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000053000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000304500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025100000000706000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025150000000230500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025250000000279000'.
       01  WS-INTEREST-SCHED-TBL REDEFINES WS-INTEREST-SCHED-VALUES.
           05  WS-INTEREST-SCHED-ROW OCCURS 8 TIMES.
               10  WS-INTEREST-SCHED-LOW   PIC 9(09)V99.
               10  WS-INTEREST-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000615000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000457000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000700000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000154500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010800000000308500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020800000000528500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023300000000470000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023550000000499500'.
       01  WS-INTEREST-BAND-TBL REDEFINES WS-INTEREST-BAND-VALUES.
           05  WS-INTEREST-BAND-ROW OCCURS 8 TIMES.
               10  WS-INTEREST-BAND-LOW    PIC 9(09)V99.
               10  WS-INTEREST-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-LINE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LINE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000333000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000368000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000771000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000450000000199000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002950000000536500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003200000000297500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003700000000438000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006200000000326000'.
       01  WS-LINE-GRID-TBL REDEFINES WS-LINE-GRID-VALUES.
           05  WS-LINE-GRID-ROW OCCURS 8 TIMES.
               10  WS-LINE-GRID-LOW        PIC 9(09)V99.
               10  WS-LINE-GRID-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PBPRCP.

       PROCEDURE DIVISION USING PBPRC-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-NEXT-REVIEW
           PERFORM 2010-PRORATE-UNPOSTED
           PERFORM 2020-PRORATE-INTEREST
           PERFORM 2030-ACCRUE-LINE
           PERFORM 2040-ASSESS-NOTICE-SENT
           PERFORM 2050-CHECK-NEXT-REVIEW
           PERFORM 2060-EDIT-CUST-ID
           PERFORM 2070-AGE-EXPIRY
           PERFORM 2080-COUNT-ITEM
           PERFORM 2090-ACCRUE-MARGIN
           PERFORM 2100-ACCUM-CHANNEL
           PERFORM 2110-FLAG-CARRYFWD
           PERFORM 2120-LIMIT-ITEM
           PERFORM 2130-PRORATE-PRINTED
           PERFORM 2140-ACCUM-REGION
           PERFORM 2150-SPLIT-INTEREST
           PERFORM 2160-FLAG-ITEM
           PERFORM 2170-CHECK-EXPIRY
           PERFORM 2180-COUNT-ITEM
           PERFORM 2190-PRORATE-CARRYFWD
           PERFORM 2200-COUNT-PAYMENT
           PERFORM 2210-AGE-MATURITY
           PERFORM 2220-FLAG-ITEM
           PERFORM 2230-COUNT-PAYMENT
           PERFORM 2240-SET-CHANNEL
           PERFORM 2250-DESCRIBE-SOURCE
           PERFORM 2260-COUNT-PAYMENT
           PERFORM 2270-AGE-NEXT-REVIEW
           PERFORM 2280-FLAG-ITEM
           PERFORM 2290-CHECK-NEXT-REVIEW
           PERFORM 2300-AGE-EXPIRY
           PERFORM 2310-ACCUM-SOURCE
           PERFORM 2320-FLAG-ITEM
           PERFORM 2330-EDIT-CUST-ID
           PERFORM 2340-COUNT-ITEM
           PERFORM 2350-CHECK-EFFECTIVE
           PERFORM 2360-COUNT-ITEM
           PERFORM 2370-FLAG-ITEM
           PERFORM 2380-ASSESS-NOTICE-SENT
           PERFORM 2390-AGE-NEXT-REVIEW
           PERFORM 2400-ACCRUE-INTEREST
           PERFORM 2410-SPLIT-INTEREST
           PERFORM 2420-FLAG-ITEM
           PERFORM 2430-SPLIT-UNPOSTED
           PERFORM 2440-PRORATE-UNPOSTED
           PERFORM 2450-ACCRUE-UNPOSTED
           PERFORM 2460-SELECT-MARGIN
           PERFORM 2470-AGE-EFFECTIVE
           PERFORM 2480-SET-SOURCE
           PERFORM 2490-SET-SOURCE
           PERFORM 2500-PRORATE-UNPOSTED
           PERFORM 2510-PRORATE-INTEREST
           PERFORM 2520-PRORATE-PRINTED
           PERFORM 2530-CHECK-MATURITY
           PERFORM 2540-ACCUM-PRINTER
           PERFORM 2550-LIMIT-LINE
           PERFORM 2560-DESCRIBE-SOURCE
           PERFORM 2570-ACCUM-PRINTER
           PERFORM 2580-ACCUM-CHANNEL
           PERFORM 2590-PRORATE-INTEREST
           PERFORM 2600-SELECT-PENALTY-PCT
           PERFORM 2610-SPLIT-PRINTED
           PERFORM 2620-FLAG-INTEREST
           PERFORM 2630-FLAG-PRINTED
           PERFORM 2640-CHECK-NEXT-REVIEW
           PERFORM 2650-SET-REGION
           PERFORM 2660-SET-CHANNEL
           PERFORM 2670-FLAG-UNPOSTED
           PERFORM 2680-LIMIT-LINE
           PERFORM 2690-ACCUM-PRINTER
           PERFORM 2700-LIMIT-PRINTED
           PERFORM 2710-FLAG-INTEREST
           PERFORM 2720-LIMIT-INTEREST
           PERFORM 2730-FLAG-PRINTED
           PERFORM 2740-CHECK-EFFECTIVE
           PERFORM 2750-AGE-EXPIRY
           PERFORM 2760-FLAG-ITEM
           PERFORM 2770-PRORATE-UNPOSTED
           PERFORM 2780-AGE-EXPIRY
           PERFORM 2790-SET-REGION
           PERFORM 2800-ACCRUE-INTEREST
           PERFORM 2810-LIMIT-INTEREST
           PERFORM 2820-DESCRIBE-CHANNEL
           PERFORM 2830-ACCRUE-INTEREST
           PERFORM 2840-SELECT-PENALTY-PCT
           PERFORM 2850-SELECT-MARGIN
           PERFORM 2860-DESCRIBE-PRINTER
           PERFORM 2870-LIMIT-LINE
           PERFORM 2880-EDIT-CUST-ID
           PERFORM 2890-SPLIT-INTEREST
           PERFORM 2900-CHECK-EFFECTIVE
           PERFORM 2910-ASSESS-AUTO-RENEW
           PERFORM 2920-AGE-MATURITY
           PERFORM 2930-SELECT-MARGIN
           PERFORM 2940-DESCRIBE-SOURCE
           PERFORM 2950-CHECK-EFFECTIVE
           PERFORM 2960-COUNT-ITEM
           PERFORM 2970-AGE-EXPIRY
           PERFORM 2980-EDIT-CUST-ID
           PERFORM 2990-ACCUM-PRINTER
           PERFORM 3000-LIMIT-LINE
           PERFORM 3010-SPLIT-LINE
           PERFORM 3020-ACCRUE-PENALTY-PCT
           PERFORM 3030-LIMIT-ITEM
           PERFORM 3040-AGE-MATURITY
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PBPRC-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-NEXT-REVIEW-DATE > 0 AND PBPRC-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LINE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO UNPOSTED AMT.
      *----------------------------------------------------------------*
       2010-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBPRC-UNPOSTED-AMT ROUNDED =
                   PBPRC-LINE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBPRC-UNPOSTED-AMT
           END-IF
           ADD PBPRC-UNPOSTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2020-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PBPRC-INTEREST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2030-ACCRUE-LINE.
           IF PBPRC-LINE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBPRC-LINE-YTD * PBPRC-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBPRC-PRINTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE UNPOSTED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2040-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBPRC-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBPRC-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PBPRC-UNPOSTED-AMT *
                       PBPRC-MARGIN / 200
               WHEN PBPRC-SOURCE-CD = 'JX'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN PBPRC-SOURCE-CD = 'ZC'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PBPRC-UNPOSTED-AMT *
                       PBPRC-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2050-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PBPRC-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E066 CUST ID REQUIRED
      * E366 INTEREST AMT NEGATIVE
      * E118 INTEREST AMT OVER 50000
      * E975 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2060-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBPRC-CUST-ID = SPACES
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN PBPRC-INTEREST-AMT < 0
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN PBPRC-INTEREST-AMT > 50000.00
                   MOVE 'E118' TO WS-REJECT-CODE
               WHEN PBPRC-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E975' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-EXPIRY-DATE > 0 AND PBPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-ITEM.
           IF PBPRC-ITEM-CNT < 9999999
               ADD 1 TO PBPRC-ITEM-CNT
           END-IF
           IF PBPRC-ITEM-CNT NOT < 12
               MOVE 'Y' TO PBPRC-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2090-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PBPRC-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBPRC-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2100-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBPRC-CHANNEL-CD
                   ADD PBPRC-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBPRC-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBPRC-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2110-FLAG-CARRYFWD.
           IF PBPRC-CARRYFWD-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBPRC-CARRYFWD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2120-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINTED AMT.
      *----------------------------------------------------------------*
       2130-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBPRC-PRINTED-AMT ROUNDED =
                   PBPRC-CARRYFWD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBPRC-PRINTED-AMT
           END-IF
           ADD PBPRC-PRINTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2140-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBPRC-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBPRC-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2150-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBPRC-INTEREST-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PBPRC-INTEREST-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2160-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2170-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PBPRC-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2180-COUNT-ITEM.
           IF PBPRC-ITEM-CNT < 9999999
               ADD 1 TO PBPRC-ITEM-CNT
           END-IF
           IF PBPRC-ITEM-CNT NOT < 10
               MOVE 'Y' TO PBPRC-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2190-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBPRC-CARRYFWD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-PAYMENT.
           IF PBPRC-PAYMENT-CNT < 9999999
               ADD 1 TO PBPRC-PAYMENT-CNT
           END-IF
           IF PBPRC-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PBPRC-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2210-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBPRC-LINE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBPRC-LINE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBPRC-LINE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBPRC-LINE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBPRC-LINE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2220-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-PAYMENT.
           IF PBPRC-PAYMENT-CNT < 9999999
               ADD 1 TO PBPRC-PAYMENT-CNT
           END-IF
           IF PBPRC-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PBPRC-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> D WHEN MATURITY DATE IS PAST
      * D -> H WHEN MATURITY DATE IS PAST
      * H -> L WHEN MATURITY DATE IS PAST
      * L -> M WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2240-SET-CHANNEL.
           EVALUATE PBPRC-CHANNEL-CD
               WHEN 'B'
                   IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PBPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PBPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PBPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PBPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2250-DESCRIBE-SOURCE.
           EVALUATE PBPRC-SOURCE-CD
               WHEN 'JX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2260-COUNT-PAYMENT.
           IF PBPRC-PAYMENT-CNT < 9999999
               ADD 1 TO PBPRC-PAYMENT-CNT
           END-IF
           IF PBPRC-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PBPRC-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-NEXT-REVIEW-DATE > 0 AND PBPRC-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBPRC-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBPRC-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PBPRC-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBPRC-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBPRC-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2280-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PBPRC-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2300-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-EXPIRY-DATE > 0 AND PBPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBPRC-PRINTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBPRC-PRINTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBPRC-PRINTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBPRC-PRINTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBPRC-PRINTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2310-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBPRC-SOURCE-CD
                   ADD PBPRC-LINE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBPRC-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBPRC-LINE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2320-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E595 CUST ID REQUIRED
      * E211 WORK AMT 2 NEGATIVE
      * E206 WORK AMT 2 OVER 25000
      * E435 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2330-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBPRC-CUST-ID = SPACES
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E211' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E206' TO WS-REJECT-CODE
               WHEN PBPRC-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2340-COUNT-ITEM.
           IF PBPRC-ITEM-CNT < 9999999
               ADD 1 TO PBPRC-ITEM-CNT
           END-IF
           IF PBPRC-ITEM-CNT NOT < 10
               MOVE 'Y' TO PBPRC-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PBPRC-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-ITEM.
           IF PBPRC-ITEM-CNT < 9999999
               ADD 1 TO PBPRC-ITEM-CNT
           END-IF
           IF PBPRC-ITEM-CNT NOT < 3
               MOVE 'Y' TO PBPRC-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2370-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED FEE ON THE LINE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2380-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBPRC-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PBPRC-UNPOSTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBPRC-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PBPRC-UNPOSTED-AMT ROUNDED = PBPRC-LINE-YTD *
                       PBPRC-MARGIN / 200
               WHEN PBPRC-SOURCE-CD = 'JX'
                   MOVE 15.00 TO PBPRC-UNPOSTED-AMT
               WHEN PBPRC-SOURCE-CD = 'ZC'
                   MOVE 10.00 TO PBPRC-UNPOSTED-AMT
               WHEN OTHER
                   COMPUTE PBPRC-UNPOSTED-AMT ROUNDED = PBPRC-LINE-YTD *
                       PBPRC-MARGIN / 100
           END-EVALUATE
           IF PBPRC-UNPOSTED-AMT > 0 AND PBPRC-UNPOSTED-AMT < 1.00
               MOVE 1.00 TO PBPRC-UNPOSTED-AMT
           END-IF
           IF PBPRC-UNPOSTED-AMT > 75.00
               MOVE 75.00 TO PBPRC-UNPOSTED-AMT
           END-IF
           ADD PBPRC-UNPOSTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-NEXT-REVIEW-DATE > 0 AND PBPRC-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBPRC-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBPRC-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBPRC-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PBPRC-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBPRC-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2400-ACCRUE-INTEREST.
           IF PBPRC-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBPRC-INTEREST-YTD * PBPRC-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2410-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBPRC-INTEREST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBPRC-INTEREST-AMT - (WS-WORK-AMT *
               3)
           COMPUTE PBPRC-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2420-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT UNPOSTED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2430-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = PBPRC-UNPOSTED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBPRC-UNPOSTED-AMT - (WS-WORK-AMT *
               4)
           COMPUTE PBPRC-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO UNPOSTED AMT.
      *----------------------------------------------------------------*
       2440-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBPRC-UNPOSTED-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBPRC-UNPOSTED-AMT
           END-IF
           ADD PBPRC-UNPOSTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2450-ACCRUE-UNPOSTED.
           IF PBPRC-UNPOSTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBPRC-UNPOSTED-AMT * PBPRC-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PBPRC-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO PBPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO PBPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2470-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-EFFECTIVE-DATE > 0 AND PBPRC-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * JX -> ZC WHEN MATURITY DATE IS PAST
      * ZC -> ZY WHEN MATURITY DATE IS PAST
      * ZY -> JX WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2480-SET-SOURCE.
           EVALUATE PBPRC-SOURCE-CD
               WHEN 'JX'
                   IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZC' TO PBPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZC'
                   IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZY' TO PBPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZY'
                   IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JX' TO PBPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * JX -> ZC WHEN NEXT REVIEW DATE IS PAST
      * ZC -> ZY WHEN NEXT REVIEW DATE IS PAST
      * ZY -> JX WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-SOURCE.
           EVALUATE PBPRC-SOURCE-CD
               WHEN 'JX'
                   IF PBPRC-NEXT-REVIEW-DATE > 0 AND
                       PBPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZC' TO PBPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZC'
                   IF PBPRC-NEXT-REVIEW-DATE > 0 AND
                       PBPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZY' TO PBPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZY'
                   IF PBPRC-NEXT-REVIEW-DATE > 0 AND
                       PBPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JX' TO PBPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO UNPOSTED AMT.
      *----------------------------------------------------------------*
       2500-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBPRC-UNPOSTED-AMT ROUNDED =
                   PBPRC-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBPRC-UNPOSTED-AMT
           END-IF
           ADD PBPRC-UNPOSTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE LINE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       2510-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBPRC-INTEREST-AMT ROUNDED =
                   PBPRC-LINE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBPRC-INTEREST-AMT
           END-IF
           ADD PBPRC-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PRINTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2520-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PBPRC-PRINTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2530-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PBPRC-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBPRC-PRINTER-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBPRC-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * LINE YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2550-LIMIT-LINE.
           IF PBPRC-LINE-YTD < 100.00
               MOVE 100.00 TO PBPRC-LINE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBPRC-LINE-YTD > 5000.00
               MOVE 5000.00 TO PBPRC-LINE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2560-DESCRIBE-SOURCE.
           EVALUATE PBPRC-SOURCE-CD
               WHEN 'JX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE YTD BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2570-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBPRC-PRINTER-CD
                   ADD PBPRC-LINE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBPRC-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBPRC-LINE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBPRC-CHANNEL-CD
                   ADD PBPRC-LINE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBPRC-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBPRC-LINE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       2590-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBPRC-INTEREST-AMT ROUNDED =
                   PBPRC-INTEREST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBPRC-INTEREST-AMT
           END-IF
           ADD PBPRC-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2600-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-BAND-RATE(1) TO PBPRC-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-INTEREST-BAND-LOW(WS-TX)
                   MOVE WS-INTEREST-BAND-RATE(WS-TX) TO
                       PBPRC-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBPRC-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBPRC-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINTED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2610-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBPRC-PRINTED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBPRC-PRINTED-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBPRC-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2620-FLAG-INTEREST.
           IF PBPRC-INTEREST-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBPRC-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2630-FLAG-PRINTED.
           IF PBPRC-PRINTED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBPRC-PRINTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2640-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PBPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN MATURITY DATE IS PAST
      * GL -> PE WHEN MATURITY DATE IS PAST
      * PE -> RJ WHEN MATURITY DATE IS PAST
      * RJ -> VB WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2650-SET-REGION.
           EVALUATE PBPRC-REGION-CD
               WHEN 'ER'
                   IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> D WHEN EFFECTIVE DATE IS PAST
      * D -> H WHEN EFFECTIVE DATE IS PAST
      * H -> L WHEN EFFECTIVE DATE IS PAST
      * L -> M WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2660-SET-CHANNEL.
           EVALUATE PBPRC-CHANNEL-CD
               WHEN 'B'
                   IF PBPRC-EFFECTIVE-DATE > 0 AND PBPRC-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'D' TO PBPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PBPRC-EFFECTIVE-DATE > 0 AND PBPRC-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO PBPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PBPRC-EFFECTIVE-DATE > 0 AND PBPRC-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO PBPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF PBPRC-EFFECTIVE-DATE > 0 AND PBPRC-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO PBPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * UNPOSTED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2670-FLAG-UNPOSTED.
           IF PBPRC-UNPOSTED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBPRC-UNPOSTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * LINE YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2680-LIMIT-LINE.
           IF PBPRC-LINE-YTD < 100.00
               MOVE 100.00 TO PBPRC-LINE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBPRC-LINE-YTD > 1000.00
               MOVE 1000.00 TO PBPRC-LINE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2690-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBPRC-PRINTER-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBPRC-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2700-LIMIT-PRINTED.
           IF PBPRC-PRINTED-AMT < 100.00
               MOVE 100.00 TO PBPRC-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBPRC-PRINTED-AMT > 1000.00
               MOVE 1000.00 TO PBPRC-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2710-FLAG-INTEREST.
           IF PBPRC-INTEREST-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBPRC-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2720-LIMIT-INTEREST.
           IF PBPRC-INTEREST-AMT < 10.00
               MOVE 10.00 TO PBPRC-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBPRC-INTEREST-AMT > 5000.00
               MOVE 5000.00 TO PBPRC-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2730-FLAG-PRINTED.
           IF PBPRC-PRINTED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBPRC-PRINTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2740-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PBPRC-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2750-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-EXPIRY-DATE > 0 AND PBPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2760-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO UNPOSTED AMT.
      *----------------------------------------------------------------*
       2770-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBPRC-UNPOSTED-AMT ROUNDED =
                   PBPRC-INTEREST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBPRC-UNPOSTED-AMT
           END-IF
           ADD PBPRC-UNPOSTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2780-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-EXPIRY-DATE > 0 AND PBPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN EXPIRY DATE IS PAST
      * GL -> PE WHEN EXPIRY DATE IS PAST
      * PE -> RJ WHEN EXPIRY DATE IS PAST
      * RJ -> VB WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2790-SET-REGION.
           EVALUATE PBPRC-REGION-CD
               WHEN 'ER'
                   IF PBPRC-EXPIRY-DATE > 0 AND PBPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBPRC-EXPIRY-DATE > 0 AND PBPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBPRC-EXPIRY-DATE > 0 AND PBPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBPRC-EXPIRY-DATE > 0 AND PBPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBPRC-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2800-ACCRUE-INTEREST.
           IF PBPRC-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBPRC-INTEREST-AMT * PBPRC-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PBPRC-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2810-LIMIT-INTEREST.
           IF PBPRC-INTEREST-AMT < 0.00
               MOVE 0.00 TO PBPRC-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBPRC-INTEREST-AMT > 5000.00
               MOVE 5000.00 TO PBPRC-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2820-DESCRIBE-CHANNEL.
           EVALUATE PBPRC-CHANNEL-CD
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2830-ACCRUE-INTEREST.
           IF PBPRC-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBPRC-INTEREST-YTD * PBPRC-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBPRC-PRINTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2840-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO PBPRC-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO
                       PBPRC-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBPRC-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBPRC-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LINE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2850-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-LINE-GRID-RATE(1) TO PBPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-LINE-GRID-LOW(WS-TX)
                   MOVE WS-LINE-GRID-RATE(WS-TX) TO PBPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2860-DESCRIBE-PRINTER.
           EVALUATE PBPRC-PRINTER-CD
               WHEN 'CB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LINE YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2870-LIMIT-LINE.
           IF PBPRC-LINE-YTD < 25.00
               MOVE 25.00 TO PBPRC-LINE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBPRC-LINE-YTD > 5000.00
               MOVE 5000.00 TO PBPRC-LINE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E405 CUST ID REQUIRED
      * E034 INTEREST AMT NEGATIVE
      * E580 INTEREST AMT OVER 250000
      * E516 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2880-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBPRC-CUST-ID = SPACES
                   MOVE 'E405' TO WS-REJECT-CODE
               WHEN PBPRC-INTEREST-AMT < 0
                   MOVE 'E034' TO WS-REJECT-CODE
               WHEN PBPRC-INTEREST-AMT > 250000.00
                   MOVE 'E580' TO WS-REJECT-CODE
               WHEN PBPRC-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2890-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBPRC-INTEREST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PBPRC-INTEREST-YTD - (WS-WORK-AMT *
               4)
           COMPUTE PBPRC-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2900-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PBPRC-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LINE YTD FEE ON THE CARRYFWD YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2910-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBPRC-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBPRC-LINE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBPRC-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PBPRC-LINE-YTD ROUNDED = PBPRC-CARRYFWD-YTD *
                       PBPRC-PENALTY-PCT / 200
               WHEN PBPRC-PRINTER-CD = 'CB'
                   MOVE 20.00 TO PBPRC-LINE-YTD
               WHEN PBPRC-PRINTER-CD = 'EP'
                   MOVE 3.00 TO PBPRC-LINE-YTD
               WHEN OTHER
                   COMPUTE PBPRC-LINE-YTD ROUNDED = PBPRC-CARRYFWD-YTD *
                       PBPRC-PENALTY-PCT / 100
           END-EVALUATE
           IF PBPRC-LINE-YTD > 0 AND PBPRC-LINE-YTD < 1.00
               MOVE 1.00 TO PBPRC-LINE-YTD
           END-IF
           IF PBPRC-LINE-YTD > 250.00
               MOVE 250.00 TO PBPRC-LINE-YTD
           END-IF
           ADD PBPRC-LINE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2920-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBPRC-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBPRC-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBPRC-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBPRC-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBPRC-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE CARRYFWD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2930-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO PBPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBPRC-CARRYFWD-YTD NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO PBPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2940-DESCRIBE-SOURCE.
           EVALUATE PBPRC-SOURCE-CD
               WHEN 'JX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2950-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PBPRC-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2960-COUNT-ITEM.
           IF PBPRC-ITEM-CNT < 9999999
               ADD 1 TO PBPRC-ITEM-CNT
           END-IF
           IF PBPRC-ITEM-CNT NOT < 3
               MOVE 'Y' TO PBPRC-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2970-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-EXPIRY-DATE > 0 AND PBPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBPRC-UNPOSTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E202 CUST ID REQUIRED
      * E812 PRINTED AMT NEGATIVE
      * E724 PRINTED AMT OVER 250000
      * E623 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2980-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBPRC-CUST-ID = SPACES
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN PBPRC-PRINTED-AMT < 0
                   MOVE 'E812' TO WS-REJECT-CODE
               WHEN PBPRC-PRINTED-AMT > 250000.00
                   MOVE 'E724' TO WS-REJECT-CODE
               WHEN PBPRC-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE YTD BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2990-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBPRC-PRINTER-CD
                   ADD PBPRC-LINE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBPRC-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBPRC-LINE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * LINE YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3000-LIMIT-LINE.
           IF PBPRC-LINE-YTD < 25.00
               MOVE 25.00 TO PBPRC-LINE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBPRC-LINE-YTD > 2500.00
               MOVE 2500.00 TO PBPRC-LINE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LINE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3010-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBPRC-LINE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PBPRC-LINE-YTD - (WS-WORK-AMT * 12)
           COMPUTE PBPRC-CARRYFWD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PBPRC-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBPRC-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3030-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBPRC-MATURITY-DATE > 0 AND PBPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBPRC-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBPRC-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBPRC-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBPRC-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBPRC-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PBPRC-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PBPRC-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PBPRC-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PBPRC EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
