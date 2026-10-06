      *================================================================*
      * PROGRAM  : SDLIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : SAFE DEPOSIT BOXES RULE - PRICING. CALLED BY THE    *
      *            SAFE DEPOSIT BOXES BATCH AND ONLINE PROGRAMS.       *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-07-12 MKL  ORIGINAL                                       *
      * 1993-03-02 DPS  DRILL FEE SCHEDULE UPDATE PRB8191              *
      * 1997-04-06 SKN  NEW LATE-FEE REPORT COLUMNS REQ1256            *
      * 1999-07-27 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-04-04 JLM  DEPOSIT LIMIT REVISED INC1443                  *
      * 2000-08-25 EWS  DRILL ROUNDING - HALF UP PRB6515               *
      * 2000-12-13 CRP  DRILL CUTOFF MOVED CHG6429                     *
      * 2001-06-02 GAF  NEW DEPOSIT REPORT COLUMNS CHG3311             *
      * 2004-01-09 EWS  AUDIT FINDING - KEY-FEE CONTROL TOTALS PRB9761 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDLIM.
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
                                           VALUE 21.000000.
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
      * WS-DRILL-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DRILL-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000204500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000891500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000205500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000079500'.
       01  WS-DRILL-SCHED-TBL REDEFINES WS-DRILL-SCHED-VALUES.
           05  WS-DRILL-SCHED-ROW OCCURS 4 TIMES.
               10  WS-DRILL-SCHED-LOW      PIC 9(09)V99.
               10  WS-DRILL-SCHED-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-KEY-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-KEY-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000786500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000171000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000390000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000394000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000698000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014100000000220000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014350000000555500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014850000000185500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000525000'.
       01  WS-KEY-FEE-GRID-TBL REDEFINES WS-KEY-FEE-GRID-VALUES.
           05  WS-KEY-FEE-GRID-ROW OCCURS 9 TIMES.
               10  WS-KEY-FEE-GRID-LOW     PIC 9(09)V99.
               10  WS-KEY-FEE-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEPOSIT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEPOSIT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000494500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000171000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000540500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000372000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000748500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000302000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017250000000458500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022250000000323000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024750000000748000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025750000000041000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026750000000684500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029250000000383000'.
       01  WS-DEPOSIT-TIER-TBL REDEFINES WS-DEPOSIT-TIER-VALUES.
           05  WS-DEPOSIT-TIER-ROW OCCURS 12 TIMES.
               10  WS-DEPOSIT-TIER-LOW     PIC 9(09)V99.
               10  WS-DEPOSIT-TIER-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SDLIMP.

       PROCEDURE DIVISION USING SDLIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-LATE-FEE
           PERFORM 2010-DESCRIBE-REASON
           PERFORM 2020-SELECT-DISCOUNT
           PERFORM 2030-ASSESS-OVERRIDE
           PERFORM 2040-COUNT-NOTICE
           PERFORM 2050-PRORATE-RENT
           PERFORM 2060-DESCRIBE-CYCLE
           PERFORM 2070-CHECK-LAST-ACT
           PERFORM 2080-ASSESS-EXEMPT
           PERFORM 2090-PRORATE-LATE-FEE
           PERFORM 2100-SELECT-FEE-PCT
           PERFORM 2110-SPLIT-LATE-FEE
           PERFORM 2120-AGE-NEXT-DUE
           PERFORM 2130-PRORATE-LATE-FEE
           PERFORM 2140-AGE-LAST-ACT
           PERFORM 2150-DESCRIBE-CYCLE
           PERFORM 2160-SPLIT-LATE-FEE
           PERFORM 2170-COUNT-RETRY
           PERFORM 2180-ACCRUE-DEPOSIT
           PERFORM 2190-FLAG-DEPOSIT
           PERFORM 2200-SPLIT-DEPOSIT
           PERFORM 2210-PRORATE-LATE-FEE
           PERFORM 2220-PRORATE-RENT
           PERFORM 2230-EDIT-CONTACT
           PERFORM 2240-ACCUM-CYCLE
           PERFORM 2250-SET-REASON
           PERFORM 2260-SELECT-DISCOUNT
           PERFORM 2270-FLAG-RENT
           PERFORM 2280-LIMIT-ITEM
           PERFORM 2290-CHECK-NEXT-DUE
           PERFORM 2300-AGE-OPEN
           PERFORM 2310-PRORATE-DEPOSIT
           PERFORM 2320-SELECT-FEE-PCT
           PERFORM 2330-CHECK-LAST-ACT
           PERFORM 2340-ACCUM-CYCLE
           PERFORM 2350-ASSESS-REVIEW
           PERFORM 2360-ASSESS-EXEMPT
           PERFORM 2370-FLAG-DEPOSIT
           PERFORM 2380-ACCRUE-DEPOSIT
           PERFORM 2390-LIMIT-ITEM
           PERFORM 2400-FLAG-LATE-FEE
           PERFORM 2410-ACCUM-CYCLE
           PERFORM 2420-COUNT-NOTICE
           PERFORM 2430-PRORATE-DEPOSIT
           PERFORM 2440-SELECT-FEE-PCT
           PERFORM 2450-SPLIT-DRILL
           PERFORM 2460-ACCUM-REASON
           PERFORM 2470-FLAG-DEPOSIT
           PERFORM 2480-COUNT-NOTICE
           PERFORM 2490-ACCRUE-RENT
           PERFORM 2500-EDIT-ADDR
           PERFORM 2510-SELECT-FEE-PCT
           PERFORM 2520-DESCRIBE-REASON
           PERFORM 2530-DESCRIBE-CYCLE
           PERFORM 2540-COUNT-NOTICE
           PERFORM 2550-SET-BOX-SIZE
           PERFORM 2560-EDIT-ADDR
           PERFORM 2570-SPLIT-DEPOSIT
           PERFORM 2580-SET-BOX-SIZE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SDLIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDLIM-LATE-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SDLIM-LATE-FEE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2010-DESCRIBE-REASON.
           EVALUATE SDLIM-REASON-CD
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE RENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2020-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO SDLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDLIM-RENT-YTD NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO SDLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE YTD FEE ON THE LATE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2030-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDLIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SDLIM-LATE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDLIM-EXEMPT-FLAG = 'Y'
                   COMPUTE SDLIM-LATE-FEE-YTD ROUNDED =
                       SDLIM-LATE-FEE-AMT * SDLIM-DISCOUNT / 200
               WHEN SDLIM-REASON-CD = 'J'
                   MOVE 5.00 TO SDLIM-LATE-FEE-YTD
               WHEN SDLIM-REASON-CD = 'K'
                   MOVE 7.00 TO SDLIM-LATE-FEE-YTD
               WHEN OTHER
                   COMPUTE SDLIM-LATE-FEE-YTD ROUNDED =
                       SDLIM-LATE-FEE-AMT * SDLIM-DISCOUNT / 100
           END-EVALUATE
           IF SDLIM-LATE-FEE-YTD > 0 AND SDLIM-LATE-FEE-YTD < 1.00
               MOVE 1.00 TO SDLIM-LATE-FEE-YTD
           END-IF
           IF SDLIM-LATE-FEE-YTD > 150.00
               MOVE 150.00 TO SDLIM-LATE-FEE-YTD
           END-IF
           ADD SDLIM-LATE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-NOTICE.
           IF SDLIM-NOTICE-CNT < 9999999
               ADD 1 TO SDLIM-NOTICE-CNT
           END-IF
           IF SDLIM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO SDLIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RENT YTD.
      *----------------------------------------------------------------*
       2050-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDLIM-RENT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDLIM-RENT-YTD
           END-IF
           ADD SDLIM-RENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2060-DESCRIBE-CYCLE.
           EVALUATE SDLIM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2070-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDLIM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDLIM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SDLIM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2080-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN SDLIM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO SDLIM-LATE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDLIM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDLIM-LATE-FEE-YTD ROUNDED = WS-WORK-AMT-2 *
                       SDLIM-FEE-PCT / 200
               WHEN SDLIM-CYCLE-CD = 'AP'
                   MOVE 7.00 TO SDLIM-LATE-FEE-YTD
               WHEN SDLIM-CYCLE-CD = 'DA'
                   MOVE 10.00 TO SDLIM-LATE-FEE-YTD
               WHEN OTHER
                   COMPUTE SDLIM-LATE-FEE-YTD ROUNDED = WS-WORK-AMT-2 *
                       SDLIM-FEE-PCT / 100
           END-EVALUATE
           IF SDLIM-LATE-FEE-YTD > 0 AND SDLIM-LATE-FEE-YTD < 5.00
               MOVE 5.00 TO SDLIM-LATE-FEE-YTD
           END-IF
           IF SDLIM-LATE-FEE-YTD > 35.00
               MOVE 35.00 TO SDLIM-LATE-FEE-YTD
           END-IF
           ADD SDLIM-LATE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE RENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LATE FEE AMT.
      *----------------------------------------------------------------*
       2090-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDLIM-LATE-FEE-AMT ROUNDED =
                   SDLIM-RENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDLIM-LATE-FEE-AMT
           END-IF
           ADD SDLIM-LATE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2100-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-GRID-RATE(1) TO SDLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-KEY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-KEY-FEE-GRID-RATE(WS-TX) TO SDLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2110-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDLIM-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2120-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDLIM-NEXT-DUE-DATE > 0 AND SDLIM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDLIM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDLIM-LATE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDLIM-LATE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDLIM-LATE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDLIM-LATE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDLIM-LATE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2130-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDLIM-LATE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDLIM-LAST-ACT-DATE > 0 AND SDLIM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDLIM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDLIM-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDLIM-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SDLIM-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDLIM-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDLIM-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2150-DESCRIBE-CYCLE.
           EVALUATE SDLIM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDLIM-LATE-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SDLIM-LATE-FEE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE SDLIM-RENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2170-COUNT-RETRY.
           IF SDLIM-RETRY-CNT < 9999999
               ADD 1 TO SDLIM-RETRY-CNT
           END-IF
           IF SDLIM-RETRY-CNT NOT < 6
               MOVE 'Y' TO SDLIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2180-ACCRUE-DEPOSIT.
           IF SDLIM-DEPOSIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDLIM-DEPOSIT-AMT * SDLIM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDLIM-LATE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2190-FLAG-DEPOSIT.
           IF SDLIM-DEPOSIT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDLIM-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2200-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDLIM-DEPOSIT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SDLIM-DEPOSIT-YTD - (WS-WORK-AMT * 4)
           COMPUTE SDLIM-LATE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2210-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDLIM-LATE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RENT YTD.
      *----------------------------------------------------------------*
       2220-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDLIM-RENT-YTD ROUNDED =
                   SDLIM-DEPOSIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDLIM-RENT-YTD
           END-IF
           ADD SDLIM-RENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E444 CONTACT REQUIRED
      * E255 LATE FEE YTD NEGATIVE
      * E786 LATE FEE YTD OVER 250000
      * E183 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDLIM-CONTACT = SPACES
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN SDLIM-LATE-FEE-YTD < 0
                   MOVE 'E255' TO WS-REJECT-CODE
               WHEN SDLIM-LATE-FEE-YTD > 250000.00
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN SDLIM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E183' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2240-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDLIM-CYCLE-CD
                   ADD SDLIM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDLIM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDLIM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN LAST ACT DATE IS PAST
      * K -> L WHEN LAST ACT DATE IS PAST
      * L -> M WHEN LAST ACT DATE IS PAST
      * M -> S WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2250-SET-REASON.
           EVALUATE SDLIM-REASON-CD
               WHEN 'J'
                   IF SDLIM-LAST-ACT-DATE > 0 AND SDLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO SDLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF SDLIM-LAST-ACT-DATE > 0 AND SDLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SDLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SDLIM-LAST-ACT-DATE > 0 AND SDLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO SDLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF SDLIM-LAST-ACT-DATE > 0 AND SDLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO SDLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DRILL SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2260-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DRILL-SCHED-RATE(1) TO SDLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-DRILL-SCHED-LOW(WS-TX)
                   MOVE WS-DRILL-SCHED-RATE(WS-TX) TO SDLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RENT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2270-FLAG-RENT.
           IF SDLIM-RENT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDLIM-RENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2280-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDLIM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDLIM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO SDLIM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2300-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDLIM-OPEN-DATE > 0 AND SDLIM-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDLIM-LATE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDLIM-LATE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDLIM-LATE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDLIM-LATE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDLIM-LATE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DRILL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       2310-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDLIM-DEPOSIT-YTD ROUNDED =
                   SDLIM-DRILL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDLIM-DEPOSIT-YTD
           END-IF
           ADD SDLIM-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE GRID TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2320-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-GRID-RATE(1) TO SDLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SDLIM-DEPOSIT-YTD NOT < WS-KEY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-KEY-FEE-GRID-RATE(WS-TX) TO SDLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2330-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDLIM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDLIM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SDLIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDLIM-CYCLE-CD
                   ADD SDLIM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDLIM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDLIM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT YTD FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2350-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDLIM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDLIM-RENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDLIM-EXEMPT-FLAG = 'Y'
                   COMPUTE SDLIM-RENT-YTD ROUNDED = SDLIM-DEPOSIT-AMT *
                       SDLIM-FEE-PCT / 200
               WHEN SDLIM-REASON-CD = 'J'
                   MOVE 15.00 TO SDLIM-RENT-YTD
               WHEN SDLIM-REASON-CD = 'K'
                   MOVE 7.00 TO SDLIM-RENT-YTD
               WHEN OTHER
                   COMPUTE SDLIM-RENT-YTD ROUNDED = SDLIM-DEPOSIT-AMT *
                       SDLIM-FEE-PCT / 100
           END-EVALUATE
           IF SDLIM-RENT-YTD > 0 AND SDLIM-RENT-YTD < 5.00
               MOVE 5.00 TO SDLIM-RENT-YTD
           END-IF
           IF SDLIM-RENT-YTD > 75.00
               MOVE 75.00 TO SDLIM-RENT-YTD
           END-IF
           ADD SDLIM-RENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE RENT YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2360-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN SDLIM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO SDLIM-RENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDLIM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDLIM-RENT-YTD ROUNDED = WS-WORK-AMT *
                       SDLIM-DISCOUNT / 200
               WHEN SDLIM-REASON-CD = 'J'
                   MOVE 12.00 TO SDLIM-RENT-YTD
               WHEN SDLIM-REASON-CD = 'K'
                   MOVE 7.00 TO SDLIM-RENT-YTD
               WHEN OTHER
                   COMPUTE SDLIM-RENT-YTD ROUNDED = WS-WORK-AMT *
                       SDLIM-DISCOUNT / 100
           END-EVALUATE
           IF SDLIM-RENT-YTD > 0 AND SDLIM-RENT-YTD < 2.00
               MOVE 2.00 TO SDLIM-RENT-YTD
           END-IF
           IF SDLIM-RENT-YTD > 35.00
               MOVE 35.00 TO SDLIM-RENT-YTD
           END-IF
           ADD SDLIM-RENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2370-FLAG-DEPOSIT.
           IF SDLIM-DEPOSIT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDLIM-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-DEPOSIT.
           IF SDLIM-DEPOSIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDLIM-DEPOSIT-AMT * SDLIM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2390-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2400-FLAG-LATE-FEE.
           IF SDLIM-LATE-FEE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDLIM-LATE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2410-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDLIM-CYCLE-CD
                   ADD SDLIM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDLIM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDLIM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2420-COUNT-NOTICE.
           IF SDLIM-NOTICE-CNT < 9999999
               ADD 1 TO SDLIM-NOTICE-CNT
           END-IF
           IF SDLIM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO SDLIM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       2430-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDLIM-DEPOSIT-YTD ROUNDED =
                   SDLIM-DEPOSIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDLIM-DEPOSIT-YTD
           END-IF
           ADD SDLIM-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE GRID TIER FOR THE RENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-GRID-RATE(1) TO SDLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SDLIM-RENT-YTD NOT < WS-KEY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-KEY-FEE-GRID-RATE(WS-TX) TO SDLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRILL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2450-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDLIM-DRILL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SDLIM-DRILL-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2460-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDLIM-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDLIM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2470-FLAG-DEPOSIT.
           IF SDLIM-DEPOSIT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDLIM-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2480-COUNT-NOTICE.
           IF SDLIM-NOTICE-CNT < 9999999
               ADD 1 TO SDLIM-NOTICE-CNT
           END-IF
           IF SDLIM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO SDLIM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2490-ACCRUE-RENT.
           IF SDLIM-RENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDLIM-RENT-YTD * SDLIM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDLIM-DRILL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E304 ADDR REQUIRED
      * E189 RENT YTD NEGATIVE
      * E673 RENT YTD OVER 50000
      * E658 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2500-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDLIM-ADDR = SPACES
                   MOVE 'E304' TO WS-REJECT-CODE
               WHEN SDLIM-RENT-YTD < 0
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN SDLIM-RENT-YTD > 50000.00
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN SDLIM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E658' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2510-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO SDLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDLIM-DEPOSIT-AMT NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO SDLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2520-DESCRIBE-REASON.
           EVALUATE SDLIM-REASON-CD
               WHEN 'J'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2530-DESCRIBE-CYCLE.
           EVALUATE SDLIM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-NOTICE.
           IF SDLIM-NOTICE-CNT < 9999999
               ADD 1 TO SDLIM-NOTICE-CNT
           END-IF
           IF SDLIM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO SDLIM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN LAST ACT DATE IS PAST
      * MG -> VG WHEN LAST ACT DATE IS PAST
      * VG -> VN WHEN LAST ACT DATE IS PAST
      * VN -> XJ WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2550-SET-BOX-SIZE.
           EVALUATE SDLIM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDLIM-LAST-ACT-DATE > 0 AND SDLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDLIM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDLIM-LAST-ACT-DATE > 0 AND SDLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDLIM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDLIM-LAST-ACT-DATE > 0 AND SDLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDLIM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDLIM-LAST-ACT-DATE > 0 AND SDLIM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDLIM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E796 ADDR REQUIRED
      * E934 WORK AMT 2 NEGATIVE
      * E960 WORK AMT 2 OVER 250000
      * E473 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2560-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDLIM-ADDR = SPACES
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E934' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN SDLIM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E473' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2570-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDLIM-DEPOSIT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDLIM-DEPOSIT-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDLIM-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN NEXT DUE DATE IS PAST
      * MG -> VG WHEN NEXT DUE DATE IS PAST
      * VG -> VN WHEN NEXT DUE DATE IS PAST
      * VN -> XJ WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2580-SET-BOX-SIZE.
           EVALUATE SDLIM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDLIM-NEXT-DUE-DATE > 0 AND SDLIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDLIM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDLIM-NEXT-DUE-DATE > 0 AND SDLIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDLIM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDLIM-NEXT-DUE-DATE > 0 AND SDLIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDLIM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDLIM-NEXT-DUE-DATE > 0 AND SDLIM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDLIM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SDLIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SDLIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SDLIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SDLIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
