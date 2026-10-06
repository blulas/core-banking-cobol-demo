      *================================================================*
      * PROGRAM  : CLRISK                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : COLLECTIONS QUEUE RULE - DATE RULES. CALLED BY THE  *
      *            COLLECTIONS QUEUE BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-10-15 LMB  ORIGINAL                                       *
      * 1995-01-12 GAF  WRITEOFF ROUNDING - HALF UP CHG2953            *
      * 1995-03-14 JLM  FEE CUTOFF MOVED CHG7947                       *
      * 1996-02-05 GAF  PAST-DUE LIMIT REVISED REQ8644                 *
      * 2000-03-02 RJT  WRITEOFF RERUN PROTECTION CHG7655              *
      * 2004-07-07 DPS  NEW COLLECTED REPORT COLUMNS REQ9506           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLRISK.
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
                                           VALUE 8.000000.
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
      * WS-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000044500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000609500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000799000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000485000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000650500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017600000000785500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000451000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022850000000322500'.
       01  WS-FEE-GRID-TBL REDEFINES WS-FEE-GRID-VALUES.
           05  WS-FEE-GRID-ROW OCCURS 8 TIMES.
               10  WS-FEE-GRID-LOW         PIC 9(09)V99.
               10  WS-FEE-GRID-RATE        PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CLRISKP.

       PROCEDURE DIVISION USING CLRISK-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-LIMIT-ITEM
           PERFORM 2010-SELECT-MARGIN
           PERFORM 2020-LIMIT-PAST-DUE
           PERFORM 2030-DESCRIBE-CYCLE
           PERFORM 2040-DESCRIBE-STATUS
           PERFORM 2050-AGE-NOTICE
           PERFORM 2060-AGE-EFFECTIVE
           PERFORM 2070-PRORATE-PAST-DUE
           PERFORM 2080-COUNT-RETRY
           PERFORM 2090-PRORATE-PAST-DUE
           PERFORM 2100-ASSESS-NOTICE-SENT
           PERFORM 2110-EDIT-CONTACT
           PERFORM 2120-AGE-OPEN
           PERFORM 2130-PRORATE-FEE
           PERFORM 2140-SPLIT-PAST-DUE
           PERFORM 2150-ASSESS-NOTICE-SENT
           PERFORM 2160-PRORATE-COLLECTED
           PERFORM 2170-AGE-OPEN
           PERFORM 2180-CHECK-NOTICE
           PERFORM 2190-FLAG-PROMISED
           PERFORM 2200-SET-REGION
           PERFORM 2210-LIMIT-ITEM
           PERFORM 2220-COUNT-ITEM
           PERFORM 2230-ACCRUE-WRITEOFF
           PERFORM 2240-COUNT-RETRY
           PERFORM 2250-AGE-OPEN
           PERFORM 2260-CHECK-OPEN
           PERFORM 2270-FLAG-FEE
           PERFORM 2280-FLAG-PAST-DUE
           PERFORM 2290-AGE-NOTICE
           PERFORM 2300-SELECT-FEE-PCT
           PERFORM 2310-SELECT-FEE-PCT
           PERFORM 2320-PRORATE-PAST-DUE
           PERFORM 2330-CHECK-EFFECTIVE
           PERFORM 2340-ACCUM-CYCLE
           PERFORM 2350-FLAG-ITEM
           PERFORM 2360-COUNT-RETRY
           PERFORM 2370-ACCUM-REGION
           PERFORM 2380-SPLIT-COLLECTED
           PERFORM 2390-LIMIT-PROMISED
           PERFORM 2400-SELECT-FEE-PCT
           PERFORM 2410-COUNT-RETRY
           PERFORM 2420-ACCRUE-FEE
           PERFORM 2430-ACCRUE-COLLECTED
           PERFORM 2440-SELECT-FEE-PCT
           PERFORM 2450-ASSESS-NOTICE-SENT
           PERFORM 2460-LIMIT-FEE
           PERFORM 2470-SPLIT-WRITEOFF
           PERFORM 2480-SELECT-FEE-PCT
           PERFORM 2490-PRORATE-PAST-DUE
           PERFORM 2500-COUNT-ITEM
           PERFORM 2510-ASSESS-REVIEW
           PERFORM 2520-ACCRUE-PROMISED
           PERFORM 2530-COUNT-ITEM
           PERFORM 2540-SELECT-MARGIN
           PERFORM 2550-SELECT-MARGIN
           PERFORM 2560-AGE-NOTICE
           PERFORM 2570-DESCRIBE-REGION
           PERFORM 2580-DESCRIBE-ACTION
           PERFORM 2590-COUNT-ITEM
           PERFORM 2600-COUNT-ITEM
           PERFORM 2610-PRORATE-PAST-DUE
           PERFORM 2620-ACCRUE-COLLECTED
           PERFORM 2630-CHECK-OPEN
           PERFORM 2640-CHECK-NOTICE
           PERFORM 2650-AGE-EFFECTIVE
           PERFORM 2660-LIMIT-FEE
           PERFORM 2670-ACCRUE-PAST-DUE
           PERFORM 2680-SPLIT-WRITEOFF
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CLRISK-RETURN-CODE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2000-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2010-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CLRISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CLRISK-COLLECTED-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CLRISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLRISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLRISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2020-LIMIT-PAST-DUE.
           IF CLRISK-PAST-DUE-YTD < 100.00
               MOVE 100.00 TO CLRISK-PAST-DUE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLRISK-PAST-DUE-YTD > 5000.00
               MOVE 5000.00 TO CLRISK-PAST-DUE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2030-DESCRIBE-CYCLE.
           EVALUATE CLRISK-CYCLE-CD
               WHEN 'BE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2040-DESCRIBE-STATUS.
           EVALUATE CLRISK-STATUS-CD
               WHEN 'PS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-NOTICE-DATE > 0 AND CLRISK-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLRISK-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CLRISK-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CLRISK-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CLRISK-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLRISK-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-EFFECTIVE-DATE > 0 AND CLRISK-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLRISK-PROMISED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CLRISK-PROMISED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CLRISK-PROMISED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CLRISK-PROMISED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLRISK-PROMISED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       2070-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLRISK-PAST-DUE-AMT ROUNDED =
                   CLRISK-PAST-DUE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLRISK-PAST-DUE-AMT
           END-IF
           ADD CLRISK-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-RETRY.
           IF CLRISK-RETRY-CNT < 9999999
               ADD 1 TO CLRISK-RETRY-CNT
           END-IF
           IF CLRISK-RETRY-CNT NOT < 10
               MOVE 'Y' TO CLRISK-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       2090-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLRISK-PAST-DUE-AMT ROUNDED =
                   CLRISK-PAST-DUE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CLRISK-PAST-DUE-AMT
           END-IF
           ADD CLRISK-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FEE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2100-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CLRISK-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLRISK-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CLRISK-FEE-YTD *
                       CLRISK-FEE-PCT / 200
               WHEN CLRISK-REGION-CD = 'AT'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN CLRISK-REGION-CD = 'AU'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CLRISK-FEE-YTD *
                       CLRISK-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E831 CONTACT REQUIRED
      * E136 PAST DUE AMT NEGATIVE
      * E855 PAST DUE AMT OVER 5000
      * E936 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2110-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLRISK-CONTACT = SPACES
                   MOVE 'E831' TO WS-REJECT-CODE
               WHEN CLRISK-PAST-DUE-AMT < 0
                   MOVE 'E136' TO WS-REJECT-CODE
               WHEN CLRISK-PAST-DUE-AMT > 5000.00
                   MOVE 'E855' TO WS-REJECT-CODE
               WHEN CLRISK-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2120-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-OPEN-DATE > 0 AND CLRISK-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLRISK-PROMISED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CLRISK-PROMISED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CLRISK-PROMISED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CLRISK-PROMISED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLRISK-PROMISED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       2130-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLRISK-FEE-YTD ROUNDED =
                   CLRISK-PAST-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CLRISK-FEE-YTD
           END-IF
           ADD CLRISK-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2140-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = CLRISK-PAST-DUE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CLRISK-PAST-DUE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FEE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2150-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CLRISK-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLRISK-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CLRISK-FEE-YTD *
                       CLRISK-FEE-PCT / 200
               WHEN CLRISK-ACTION-CD = 'KJ'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN CLRISK-ACTION-CD = 'TM'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CLRISK-FEE-YTD *
                       CLRISK-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2160-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CLRISK-COLLECTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2170-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-OPEN-DATE > 0 AND CLRISK-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-OPEN-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CLRISK-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2190-FLAG-PROMISED.
           IF CLRISK-PROMISED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CLRISK-PROMISED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN LAST MAINT DATE IS PAST
      * AU -> XR WHEN LAST MAINT DATE IS PAST
      * XR -> XU WHEN LAST MAINT DATE IS PAST
      * XU -> AT WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2200-SET-REGION.
           EVALUATE CLRISK-REGION-CD
               WHEN 'AT'
                   IF CLRISK-LAST-MAINT-DATE > 0 AND
                       CLRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'AU' TO CLRISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AU'
                   IF CLRISK-LAST-MAINT-DATE > 0 AND
                       CLRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XR' TO CLRISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XR'
                   IF CLRISK-LAST-MAINT-DATE > 0 AND
                       CLRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XU' TO CLRISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XU'
                   IF CLRISK-LAST-MAINT-DATE > 0 AND
                       CLRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'AT' TO CLRISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2210-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2220-COUNT-ITEM.
           IF CLRISK-ITEM-CNT < 9999999
               ADD 1 TO CLRISK-ITEM-CNT
           END-IF
           IF CLRISK-ITEM-CNT NOT < 12
               MOVE 'Y' TO CLRISK-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2230-ACCRUE-WRITEOFF.
           IF CLRISK-WRITEOFF-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLRISK-WRITEOFF-YTD * CLRISK-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CLRISK-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-RETRY.
           IF CLRISK-RETRY-CNT < 9999999
               ADD 1 TO CLRISK-RETRY-CNT
           END-IF
           IF CLRISK-RETRY-CNT NOT < 12
               MOVE 'Y' TO CLRISK-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2250-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-OPEN-DATE > 0 AND CLRISK-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-OPEN-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2260-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CLRISK-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2270-FLAG-FEE.
           IF CLRISK-FEE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CLRISK-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2280-FLAG-PAST-DUE.
           IF CLRISK-PAST-DUE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CLRISK-PAST-DUE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-NOTICE-DATE > 0 AND CLRISK-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLRISK-PAST-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CLRISK-PAST-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CLRISK-PAST-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CLRISK-PAST-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLRISK-PAST-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE PROMISED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2300-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CLRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CLRISK-PROMISED-YTD NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CLRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE PAST DUE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2310-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CLRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CLRISK-PAST-DUE-YTD NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CLRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAST DUE YTD.
      *----------------------------------------------------------------*
       2320-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLRISK-PAST-DUE-YTD ROUNDED =
                   CLRISK-PROMISED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLRISK-PAST-DUE-YTD
           END-IF
           ADD CLRISK-PAST-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2330-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CLRISK-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLRISK-CYCLE-CD
                   ADD CLRISK-WRITEOFF-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLRISK-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLRISK-WRITEOFF-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2350-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-RETRY.
           IF CLRISK-RETRY-CNT < 9999999
               ADD 1 TO CLRISK-RETRY-CNT
           END-IF
           IF CLRISK-RETRY-CNT NOT < 6
               MOVE 'Y' TO CLRISK-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2370-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLRISK-REGION-CD
                   ADD CLRISK-WRITEOFF-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLRISK-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLRISK-WRITEOFF-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2380-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = CLRISK-COLLECTED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CLRISK-COLLECTED-AMT - (WS-WORK-AMT *
               2)
           COMPUTE CLRISK-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2390-LIMIT-PROMISED.
           IF CLRISK-PROMISED-YTD < 10.00
               MOVE 10.00 TO CLRISK-PROMISED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLRISK-PROMISED-YTD > 5000.00
               MOVE 5000.00 TO CLRISK-PROMISED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2400-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CLRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CLRISK-PAST-DUE-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CLRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-RETRY.
           IF CLRISK-RETRY-CNT < 9999999
               ADD 1 TO CLRISK-RETRY-CNT
           END-IF
           IF CLRISK-RETRY-CNT NOT < 3
               MOVE 'Y' TO CLRISK-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2420-ACCRUE-FEE.
           IF CLRISK-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLRISK-FEE-YTD * CLRISK-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CLRISK-PROMISED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2430-ACCRUE-COLLECTED.
           IF CLRISK-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLRISK-COLLECTED-AMT * CLRISK-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CLRISK-WRITEOFF-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CLRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CLRISK-FEE-YTD NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CLRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED YTD FEE ON THE WRITEOFF YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2450-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CLRISK-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CLRISK-PROMISED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLRISK-EXEMPT-FLAG = 'Y'
                   COMPUTE CLRISK-PROMISED-YTD ROUNDED =
                       CLRISK-WRITEOFF-YTD * CLRISK-MARGIN / 200
               WHEN CLRISK-ACTION-CD = 'KJ'
                   MOVE 20.00 TO CLRISK-PROMISED-YTD
               WHEN CLRISK-ACTION-CD = 'TM'
                   MOVE 15.00 TO CLRISK-PROMISED-YTD
               WHEN OTHER
                   COMPUTE CLRISK-PROMISED-YTD ROUNDED =
                       CLRISK-WRITEOFF-YTD * CLRISK-MARGIN / 100
           END-EVALUATE
           IF CLRISK-PROMISED-YTD > 0 AND CLRISK-PROMISED-YTD < 10.00
               MOVE 10.00 TO CLRISK-PROMISED-YTD
           END-IF
           IF CLRISK-PROMISED-YTD > 25.00
               MOVE 25.00 TO CLRISK-PROMISED-YTD
           END-IF
           ADD CLRISK-PROMISED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2460-LIMIT-FEE.
           IF CLRISK-FEE-YTD < 50.00
               MOVE 50.00 TO CLRISK-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLRISK-FEE-YTD > 500.00
               MOVE 500.00 TO CLRISK-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2470-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE CLRISK-WRITEOFF-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2480-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CLRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CLRISK-PAST-DUE-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CLRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAST DUE YTD.
      *----------------------------------------------------------------*
       2490-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLRISK-PAST-DUE-YTD ROUNDED =
                   CLRISK-PROMISED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLRISK-PAST-DUE-YTD
           END-IF
           ADD CLRISK-PAST-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2500-COUNT-ITEM.
           IF CLRISK-ITEM-CNT < 9999999
               ADD 1 TO CLRISK-ITEM-CNT
           END-IF
           IF CLRISK-ITEM-CNT NOT < 12
               MOVE 'Y' TO CLRISK-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE FEE ON THE COLLECTED AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2510-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CLRISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CLRISK-PAST-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLRISK-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CLRISK-PAST-DUE-AMT ROUNDED =
                       CLRISK-COLLECTED-AMT * CLRISK-MARGIN / 200
               WHEN CLRISK-STATUS-CD = 'PS'
                   MOVE 20.00 TO CLRISK-PAST-DUE-AMT
               WHEN CLRISK-STATUS-CD = 'UR'
                   MOVE 15.00 TO CLRISK-PAST-DUE-AMT
               WHEN OTHER
                   COMPUTE CLRISK-PAST-DUE-AMT ROUNDED =
                       CLRISK-COLLECTED-AMT * CLRISK-MARGIN / 100
           END-EVALUATE
           IF CLRISK-PAST-DUE-AMT > 0 AND CLRISK-PAST-DUE-AMT < 2.00
               MOVE 2.00 TO CLRISK-PAST-DUE-AMT
           END-IF
           IF CLRISK-PAST-DUE-AMT > 50.00
               MOVE 50.00 TO CLRISK-PAST-DUE-AMT
           END-IF
           ADD CLRISK-PAST-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PROMISED YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-PROMISED.
           IF CLRISK-PROMISED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLRISK-PROMISED-YTD * CLRISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2530-COUNT-ITEM.
           IF CLRISK-ITEM-CNT < 9999999
               ADD 1 TO CLRISK-ITEM-CNT
           END-IF
           IF CLRISK-ITEM-CNT NOT < 3
               MOVE 'Y' TO CLRISK-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2540-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CLRISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CLRISK-FEE-YTD NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CLRISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLRISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLRISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE PAST DUE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2550-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CLRISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CLRISK-PAST-DUE-YTD NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CLRISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLRISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLRISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2560-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-NOTICE-DATE > 0 AND CLRISK-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLRISK-COLLECTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CLRISK-COLLECTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CLRISK-COLLECTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CLRISK-COLLECTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLRISK-COLLECTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2570-DESCRIBE-REGION.
           EVALUATE CLRISK-REGION-CD
               WHEN 'AT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'AU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2580-DESCRIBE-ACTION.
           EVALUATE CLRISK-ACTION-CD
               WHEN 'KJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-ITEM.
           IF CLRISK-ITEM-CNT < 9999999
               ADD 1 TO CLRISK-ITEM-CNT
           END-IF
           IF CLRISK-ITEM-CNT NOT < 10
               MOVE 'Y' TO CLRISK-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2600-COUNT-ITEM.
           IF CLRISK-ITEM-CNT < 9999999
               ADD 1 TO CLRISK-ITEM-CNT
           END-IF
           IF CLRISK-ITEM-CNT NOT < 3
               MOVE 'Y' TO CLRISK-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2610-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLRISK-PAST-DUE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2620-ACCRUE-COLLECTED.
           IF CLRISK-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLRISK-COLLECTED-AMT * CLRISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO CLRISK-WRITEOFF-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CLRISK-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2640-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CLRISK-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2650-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CLRISK-EFFECTIVE-DATE > 0 AND CLRISK-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLRISK-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLRISK-PAST-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CLRISK-PAST-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CLRISK-PAST-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CLRISK-PAST-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLRISK-PAST-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2660-LIMIT-FEE.
           IF CLRISK-FEE-YTD < 25.00
               MOVE 25.00 TO CLRISK-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLRISK-FEE-YTD > 50000.00
               MOVE 50000.00 TO CLRISK-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2670-ACCRUE-PAST-DUE.
           IF CLRISK-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLRISK-PAST-DUE-AMT * CLRISK-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CLRISK-PAST-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2680-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = CLRISK-WRITEOFF-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CLRISK-WRITEOFF-YTD - (WS-WORK-AMT *
               3)
           COMPUTE CLRISK-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CLRISK-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CLRISK-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CLRISK-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CLRISK EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
