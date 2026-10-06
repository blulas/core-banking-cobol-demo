      *================================================================*
      * PROGRAM  : CLTIER                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : COLLECTIONS QUEUE RULE - FEES. CALLED BY THE        *
      *            COLLECTIONS QUEUE BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-09-04 JLM  ORIGINAL                                       *
      * 1996-04-22 JLM  WRITEOFF ROUNDING - HALF UP INC4998            *
      * 1996-05-28 RJT  WRITEOFF EXCEPTIONS TO OPS QUEUE CHG9112       *
      * 1997-02-23 JLM  FEE TABLE EXPANDED INC7427                     *
      * 1999-12-01 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-12-13 JLM  NEW PAST-DUE REPORT COLUMNS PRB8207            *
      * 2001-12-17 PQR  PAST-DUE CUTOFF MOVED CHG1407                  *
      * 2004-04-05 RJT  PROMISED FEE SCHEDULE UPDATE INC6971           *
      * 2004-09-03 RJT  PERFORMANCE - FEE READ AHEAD REQ8978           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLTIER.
       AUTHOR. M K LOWE.

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
                                           VALUE 22.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 31.
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
      * WS-COLLECTE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COLLECTE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000761500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000711000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000093000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000544000'.
       01  WS-COLLECTE-TIER-TBL REDEFINES WS-COLLECTE-TIER-VALUES.
           05  WS-COLLECTE-TIER-ROW OCCURS 4 TIMES.
               10  WS-COLLECTE-TIER-LOW    PIC 9(09)V99.
               10  WS-COLLECTE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COLLECTE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COLLECTE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000822000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000057500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000216000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000476000'.
       01  WS-COLLECTE-SCHED-TBL REDEFINES WS-COLLECTE-SCHED-VALUES.
           05  WS-COLLECTE-SCHED-ROW OCCURS 4 TIMES.
               10  WS-COLLECTE-SCHED-LOW   PIC 9(09)V99.
               10  WS-COLLECTE-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CLTIERP.

       PROCEDURE DIVISION USING CLTIER-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-NEXT-DUE
           PERFORM 2010-AGE-NEXT-DUE
           PERFORM 2020-EDIT-CUST-ID
           PERFORM 2030-COUNT-RETRY
           PERFORM 2040-SELECT-MARGIN
           PERFORM 2050-SET-REASON
           PERFORM 2060-SELECT-MARGIN
           PERFORM 2070-PRORATE-PROMISED
           PERFORM 2080-SPLIT-COLLECTED
           PERFORM 2090-FLAG-ITEM
           PERFORM 2100-EDIT-CUST-ID
           PERFORM 2110-ASSESS-OVERRIDE
           PERFORM 2120-SPLIT-ITEM
           PERFORM 2130-PRORATE-PROMISED
           PERFORM 2140-CHECK-OPEN
           PERFORM 2150-SPLIT-COLLECTED
           PERFORM 2160-AGE-NEXT-DUE
           PERFORM 2170-ACCRUE-PAST-DUE
           PERFORM 2180-PRORATE-PAST-DUE
           PERFORM 2190-ASSESS-OVERRIDE
           PERFORM 2200-ACCRUE-COLLECTED
           PERFORM 2210-SET-REASON
           PERFORM 2220-EDIT-CONTACT
           PERFORM 2230-EDIT-CONTACT
           PERFORM 2240-SET-STATUS
           PERFORM 2250-EDIT-CONTACT
           PERFORM 2260-SET-STATUS
           PERFORM 2270-ACCUM-STATUS
           PERFORM 2280-AGE-OPEN
           PERFORM 2290-FLAG-PROMISED
           PERFORM 2300-SET-STATUS
           PERFORM 2310-ASSESS-SUSPEND
           PERFORM 2320-PRORATE-COLLECTED
           PERFORM 2330-DESCRIBE-ACTION
           PERFORM 2340-ACCUM-REASON
           PERFORM 2350-PRORATE-PROMISED
           PERFORM 2360-EDIT-CUST-ID
           PERFORM 2370-FLAG-PAST-DUE
           PERFORM 2380-DESCRIBE-CYCLE
           PERFORM 2390-CHECK-OPEN
           PERFORM 2400-SET-STATUS
           PERFORM 2410-ACCUM-REASON
           PERFORM 2420-COUNT-CYCLE
           PERFORM 2430-DESCRIBE-STATUS
           PERFORM 2440-ACCUM-ACTION
           PERFORM 2450-LIMIT-ITEM
           PERFORM 2460-SELECT-FEE-PCT
           PERFORM 2470-AGE-CLOSE
           PERFORM 2480-SET-STATUS
           PERFORM 2490-ACCUM-CYCLE
           PERFORM 2500-ACCUM-CYCLE
           PERFORM 2510-ASSESS-SUSPEND
           PERFORM 2520-SELECT-MARGIN
           PERFORM 2530-AGE-LAST-PAY
           PERFORM 2540-EDIT-CUST-ID
           PERFORM 2550-SPLIT-PROMISED
           PERFORM 2560-EDIT-CONTACT
           PERFORM 2570-FLAG-ITEM
           PERFORM 2580-ASSESS-EXEMPT
           PERFORM 2590-SELECT-FEE-PCT
           PERFORM 2600-SELECT-MARGIN
           PERFORM 2610-ASSESS-OVERRIDE
           PERFORM 2620-ASSESS-OVERRIDE
           PERFORM 2630-ACCRUE-PAST-DUE
           PERFORM 2640-DESCRIBE-ACTION
           PERFORM 2650-CHECK-CLOSE
           PERFORM 2660-LIMIT-PROMISED
           PERFORM 2670-COUNT-RETRY
           PERFORM 2680-AGE-NEXT-DUE
           PERFORM 2690-FLAG-ITEM
           PERFORM 2700-SPLIT-COLLECTED
           PERFORM 2710-SELECT-MARGIN
           PERFORM 2720-ACCRUE-PAST-DUE
           PERFORM 2730-ACCUM-CYCLE
           PERFORM 2740-ACCRUE-MARGIN
           PERFORM 2750-COUNT-RETRY
           PERFORM 2760-ACCUM-ACTION
           PERFORM 2770-SELECT-FEE-PCT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CLTIER-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CLTIER-NEXT-DUE-DATE > 0 AND CLTIER-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLTIER-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CLTIER-NEXT-DUE-DATE > 0 AND CLTIER-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLTIER-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E760 CUST ID REQUIRED
      * E822 WORK AMT NEGATIVE
      * E653 WORK AMT OVER 250000
      * E696 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2020-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLTIER-CUST-ID = SPACES
                   MOVE 'E760' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E653' TO WS-REJECT-CODE
               WHEN CLTIER-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E696' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2030-COUNT-RETRY.
           IF CLTIER-RETRY-CNT < 9999999
               ADD 1 TO CLTIER-RETRY-CNT
           END-IF
           IF CLTIER-RETRY-CNT NOT < 12
               MOVE 'Y' TO CLTIER-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-SCHED-RATE(1) TO CLTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-COLLECTE-SCHED-LOW(WS-TX)
                   MOVE WS-COLLECTE-SCHED-RATE(WS-TX) TO CLTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN CLOSE DATE IS PAST
      * BJ -> GG WHEN CLOSE DATE IS PAST
      * GG -> ZR WHEN CLOSE DATE IS PAST
      * ZR -> AT WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2050-SET-REASON.
           EVALUATE CLTIER-REASON-CD
               WHEN 'AT'
                   IF CLTIER-CLOSE-DATE > 0 AND CLTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO CLTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF CLTIER-CLOSE-DATE > 0 AND CLTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO CLTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF CLTIER-CLOSE-DATE > 0 AND CLTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZR' TO CLTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF CLTIER-CLOSE-DATE > 0 AND CLTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO CLTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE TIER TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2060-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-TIER-RATE(1) TO CLTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CLTIER-COLLECTED-AMT NOT < WS-COLLECTE-TIER-LOW(WS-TX)
                   MOVE WS-COLLECTE-TIER-RATE(WS-TX) TO CLTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PROMISED AMT.
      *----------------------------------------------------------------*
       2070-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLTIER-PROMISED-AMT ROUNDED =
                   CLTIER-PROMISED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLTIER-PROMISED-AMT
           END-IF
           ADD CLTIER-PROMISED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2080-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = CLTIER-COLLECTED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CLTIER-COLLECTED-YTD - (WS-WORK-AMT *
               4)
           COMPUTE CLTIER-PROMISED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2090-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E634 CUST ID REQUIRED
      * E152 PROMISED AMT NEGATIVE
      * E205 PROMISED AMT OVER 1000000
      * E673 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLTIER-CUST-ID = SPACES
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN CLTIER-PROMISED-AMT < 0
                   MOVE 'E152' TO WS-REJECT-CODE
               WHEN CLTIER-PROMISED-AMT > 1000000.00
                   MOVE 'E205' TO WS-REJECT-CODE
               WHEN CLTIER-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2110-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CLTIER-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLTIER-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       CLTIER-MARGIN / 200
               WHEN CLTIER-STATUS-CD = 'PS'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN CLTIER-STATUS-CD = 'UR'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       CLTIER-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2120-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PROMISED YTD.
      *----------------------------------------------------------------*
       2130-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLTIER-PROMISED-YTD ROUNDED =
                   CLTIER-COLLECTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CLTIER-PROMISED-YTD
           END-IF
           ADD CLTIER-PROMISED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CLTIER-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CLTIER-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2150-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE CLTIER-COLLECTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2160-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CLTIER-NEXT-DUE-DATE > 0 AND CLTIER-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLTIER-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLTIER-PROMISED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CLTIER-PROMISED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CLTIER-PROMISED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CLTIER-PROMISED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLTIER-PROMISED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2170-ACCRUE-PAST-DUE.
           IF CLTIER-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLTIER-PAST-DUE-AMT * CLTIER-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAST DUE YTD.
      *----------------------------------------------------------------*
       2180-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLTIER-PAST-DUE-YTD ROUNDED =
                   CLTIER-PAST-DUE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLTIER-PAST-DUE-YTD
           END-IF
           ADD CLTIER-PAST-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED YTD FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2190-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CLTIER-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CLTIER-COLLECTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLTIER-EXEMPT-FLAG = 'Y'
                   COMPUTE CLTIER-COLLECTED-YTD ROUNDED = WS-WORK-AMT *
                       CLTIER-FEE-PCT / 200
               WHEN CLTIER-REASON-CD = 'AT'
                   MOVE 5.00 TO CLTIER-COLLECTED-YTD
               WHEN CLTIER-REASON-CD = 'BJ'
                   MOVE 20.00 TO CLTIER-COLLECTED-YTD
               WHEN OTHER
                   COMPUTE CLTIER-COLLECTED-YTD ROUNDED = WS-WORK-AMT *
                       CLTIER-FEE-PCT / 100
           END-EVALUATE
           IF CLTIER-COLLECTED-YTD > 0 AND CLTIER-COLLECTED-YTD < 1.00
               MOVE 1.00 TO CLTIER-COLLECTED-YTD
           END-IF
           IF CLTIER-COLLECTED-YTD > 100.00
               MOVE 100.00 TO CLTIER-COLLECTED-YTD
           END-IF
           ADD CLTIER-COLLECTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2200-ACCRUE-COLLECTED.
           IF CLTIER-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLTIER-COLLECTED-AMT * CLTIER-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN LAST PAY DATE IS PAST
      * BJ -> GG WHEN LAST PAY DATE IS PAST
      * GG -> ZR WHEN LAST PAY DATE IS PAST
      * ZR -> AT WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-REASON.
           EVALUATE CLTIER-REASON-CD
               WHEN 'AT'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BJ' TO CLTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GG' TO CLTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ZR' TO CLTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AT' TO CLTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E665 CONTACT REQUIRED
      * E810 COLLECTED YTD NEGATIVE
      * E229 COLLECTED YTD OVER 1000000
      * E122 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2220-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLTIER-CONTACT = SPACES
                   MOVE 'E665' TO WS-REJECT-CODE
               WHEN CLTIER-COLLECTED-YTD < 0
                   MOVE 'E810' TO WS-REJECT-CODE
               WHEN CLTIER-COLLECTED-YTD > 1000000.00
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN CLTIER-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E122' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E176 CONTACT REQUIRED
      * E673 PAST DUE YTD NEGATIVE
      * E406 PAST DUE YTD OVER 250000
      * E395 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLTIER-CONTACT = SPACES
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN CLTIER-PAST-DUE-YTD < 0
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN CLTIER-PAST-DUE-YTD > 250000.00
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN CLTIER-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN LAST PAY DATE IS PAST
      * UR -> XM WHEN LAST PAY DATE IS PAST
      * XM -> YA WHEN LAST PAY DATE IS PAST
      * YA -> PS WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2240-SET-STATUS.
           EVALUATE CLTIER-STATUS-CD
               WHEN 'PS'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UR' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XM' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YA' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PS' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E095 CONTACT REQUIRED
      * E817 COLLECTED AMT NEGATIVE
      * E811 COLLECTED AMT OVER 250000
      * E450 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2250-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLTIER-CONTACT = SPACES
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN CLTIER-COLLECTED-AMT < 0
                   MOVE 'E817' TO WS-REJECT-CODE
               WHEN CLTIER-COLLECTED-AMT > 250000.00
                   MOVE 'E811' TO WS-REJECT-CODE
               WHEN CLTIER-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN LAST PAY DATE IS PAST
      * UR -> XM WHEN LAST PAY DATE IS PAST
      * XM -> YA WHEN LAST PAY DATE IS PAST
      * YA -> PS WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2260-SET-STATUS.
           EVALUATE CLTIER-STATUS-CD
               WHEN 'PS'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UR' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XM' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YA' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PS' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2270-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLTIER-STATUS-CD
                   ADD CLTIER-PAST-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLTIER-PAST-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2280-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CLTIER-OPEN-DATE > 0 AND CLTIER-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLTIER-PROMISED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CLTIER-PROMISED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CLTIER-PROMISED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CLTIER-PROMISED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLTIER-PROMISED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PROMISED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2290-FLAG-PROMISED.
           IF CLTIER-PROMISED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CLTIER-PROMISED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN OPEN DATE IS PAST
      * UR -> XM WHEN OPEN DATE IS PAST
      * XM -> YA WHEN OPEN DATE IS PAST
      * YA -> PS WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2300-SET-STATUS.
           EVALUATE CLTIER-STATUS-CD
               WHEN 'PS'
                   IF CLTIER-OPEN-DATE > 0 AND CLTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CLTIER-OPEN-DATE > 0 AND CLTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF CLTIER-OPEN-DATE > 0 AND CLTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF CLTIER-OPEN-DATE > 0 AND CLTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE COLLECTED YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2310-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CLTIER-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLTIER-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CLTIER-COLLECTED-YTD *
                       CLTIER-MARGIN / 200
               WHEN CLTIER-REASON-CD = 'AT'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN CLTIER-REASON-CD = 'BJ'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CLTIER-COLLECTED-YTD *
                       CLTIER-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COLLECTED AMT.
      *----------------------------------------------------------------*
       2320-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLTIER-COLLECTED-AMT ROUNDED =
                   CLTIER-COLLECTED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLTIER-COLLECTED-AMT
           END-IF
           ADD CLTIER-COLLECTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2330-DESCRIBE-ACTION.
           EVALUATE CLTIER-ACTION-CD
               WHEN 'KJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLTIER-REASON-CD
                   ADD CLTIER-PROMISED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLTIER-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLTIER-PROMISED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PROMISED AMT.
      *----------------------------------------------------------------*
       2350-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLTIER-PROMISED-AMT ROUNDED =
                   CLTIER-PROMISED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLTIER-PROMISED-AMT
           END-IF
           ADD CLTIER-PROMISED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E055 CUST ID REQUIRED
      * E719 PROMISED YTD NEGATIVE
      * E979 PROMISED YTD OVER 1000000
      * E037 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2360-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLTIER-CUST-ID = SPACES
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN CLTIER-PROMISED-YTD < 0
                   MOVE 'E719' TO WS-REJECT-CODE
               WHEN CLTIER-PROMISED-YTD > 1000000.00
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN CLTIER-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2370-FLAG-PAST-DUE.
           IF CLTIER-PAST-DUE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CLTIER-PAST-DUE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-CYCLE.
           EVALUATE CLTIER-CYCLE-CD
               WHEN 'BE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2390-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CLTIER-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CLTIER-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN LAST PAY DATE IS PAST
      * UR -> XM WHEN LAST PAY DATE IS PAST
      * XM -> YA WHEN LAST PAY DATE IS PAST
      * YA -> PS WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2400-SET-STATUS.
           EVALUATE CLTIER-STATUS-CD
               WHEN 'PS'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UR' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XM' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YA' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PS' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2410-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLTIER-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLTIER-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2420-COUNT-CYCLE.
           IF CLTIER-CYCLE-CNT < 9999999
               ADD 1 TO CLTIER-CYCLE-CNT
           END-IF
           IF CLTIER-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CLTIER-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2430-DESCRIBE-STATUS.
           EVALUATE CLTIER-STATUS-CD
               WHEN 'PS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED AMT BY ACTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2440-ACCUM-ACTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLTIER-ACTION-CD
                   ADD CLTIER-COLLECTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLTIER-ACTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLTIER-COLLECTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2450-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-SCHED-RATE(1) TO CLTIER-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-COLLECTE-SCHED-LOW(WS-TX)
                   MOVE WS-COLLECTE-SCHED-RATE(WS-TX) TO CLTIER-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLTIER-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLTIER-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2470-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CLTIER-CLOSE-DATE > 0 AND CLTIER-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN LAST PAY DATE IS PAST
      * UR -> XM WHEN LAST PAY DATE IS PAST
      * XM -> YA WHEN LAST PAY DATE IS PAST
      * YA -> PS WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2480-SET-STATUS.
           EVALUATE CLTIER-STATUS-CD
               WHEN 'PS'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UR' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XM' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YA' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PS' TO CLTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLTIER-CYCLE-CD
                   ADD CLTIER-PROMISED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLTIER-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLTIER-PROMISED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLTIER-CYCLE-CD
                   ADD CLTIER-COLLECTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLTIER-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLTIER-COLLECTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED FEE ON THE PAST DUE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2510-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CLTIER-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO CLTIER-PROMISED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLTIER-EXEMPT-FLAG = 'Y'
                   COMPUTE CLTIER-PROMISED-AMT ROUNDED =
                       CLTIER-PAST-DUE-YTD * CLTIER-MARGIN / 200
               WHEN CLTIER-STATUS-CD = 'PS'
                   MOVE 5.00 TO CLTIER-PROMISED-AMT
               WHEN CLTIER-STATUS-CD = 'UR'
                   MOVE 5.00 TO CLTIER-PROMISED-AMT
               WHEN OTHER
                   COMPUTE CLTIER-PROMISED-AMT ROUNDED =
                       CLTIER-PAST-DUE-YTD * CLTIER-MARGIN / 100
           END-EVALUATE
           IF CLTIER-PROMISED-AMT > 0 AND CLTIER-PROMISED-AMT < 1.00
               MOVE 1.00 TO CLTIER-PROMISED-AMT
           END-IF
           IF CLTIER-PROMISED-AMT > 35.00
               MOVE 35.00 TO CLTIER-PROMISED-AMT
           END-IF
           ADD CLTIER-PROMISED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2520-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-TIER-RATE(1) TO CLTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-COLLECTE-TIER-LOW(WS-TX)
                   MOVE WS-COLLECTE-TIER-RATE(WS-TX) TO CLTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2530-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CLTIER-LAST-PAY-DATE > 0 AND CLTIER-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLTIER-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLTIER-COLLECTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E668 CUST ID REQUIRED
      * E326 PAST DUE AMT NEGATIVE
      * E797 PAST DUE AMT OVER 250000
      * E323 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2540-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLTIER-CUST-ID = SPACES
                   MOVE 'E668' TO WS-REJECT-CODE
               WHEN CLTIER-PAST-DUE-AMT < 0
                   MOVE 'E326' TO WS-REJECT-CODE
               WHEN CLTIER-PAST-DUE-AMT > 250000.00
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN CLTIER-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-PROMISED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE CLTIER-PROMISED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E413 CONTACT REQUIRED
      * E998 COLLECTED YTD NEGATIVE
      * E082 COLLECTED YTD OVER 10000
      * E037 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2560-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLTIER-CONTACT = SPACES
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN CLTIER-COLLECTED-YTD < 0
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN CLTIER-COLLECTED-YTD > 10000.00
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN CLTIER-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2570-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED YTD FEE ON THE COLLECTED AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2580-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CLTIER-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CLTIER-COLLECTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLTIER-OVERRIDE-FLAG = 'Y'
                   COMPUTE CLTIER-COLLECTED-YTD ROUNDED =
                       CLTIER-COLLECTED-AMT * CLTIER-MARGIN / 200
               WHEN CLTIER-STATUS-CD = 'PS'
                   MOVE 3.00 TO CLTIER-COLLECTED-YTD
               WHEN CLTIER-STATUS-CD = 'UR'
                   MOVE 12.00 TO CLTIER-COLLECTED-YTD
               WHEN OTHER
                   COMPUTE CLTIER-COLLECTED-YTD ROUNDED =
                       CLTIER-COLLECTED-AMT * CLTIER-MARGIN / 100
           END-EVALUATE
           IF CLTIER-COLLECTED-YTD > 0 AND CLTIER-COLLECTED-YTD < 1.00
               MOVE 1.00 TO CLTIER-COLLECTED-YTD
           END-IF
           IF CLTIER-COLLECTED-YTD > 100.00
               MOVE 100.00 TO CLTIER-COLLECTED-YTD
           END-IF
           ADD CLTIER-COLLECTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2590-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-TIER-RATE(1) TO CLTIER-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-COLLECTE-TIER-LOW(WS-TX)
                   MOVE WS-COLLECTE-TIER-RATE(WS-TX) TO CLTIER-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLTIER-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLTIER-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE SCHED TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2600-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-SCHED-RATE(1) TO CLTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CLTIER-PAST-DUE-AMT NOT < WS-COLLECTE-SCHED-LOW(WS-TX)
                   MOVE WS-COLLECTE-SCHED-RATE(WS-TX) TO CLTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PROMISED YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2610-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CLTIER-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLTIER-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CLTIER-PROMISED-YTD *
                       CLTIER-FEE-PCT / 200
               WHEN CLTIER-CYCLE-CD = 'BE'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN CLTIER-CYCLE-CD = 'GV'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CLTIER-PROMISED-YTD *
                       CLTIER-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED YTD FEE ON THE PAST DUE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2620-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CLTIER-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CLTIER-PROMISED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLTIER-EXEMPT-FLAG = 'Y'
                   COMPUTE CLTIER-PROMISED-YTD ROUNDED =
                       CLTIER-PAST-DUE-YTD * CLTIER-MARGIN / 200
               WHEN CLTIER-STATUS-CD = 'PS'
                   MOVE 10.00 TO CLTIER-PROMISED-YTD
               WHEN CLTIER-STATUS-CD = 'UR'
                   MOVE 7.00 TO CLTIER-PROMISED-YTD
               WHEN OTHER
                   COMPUTE CLTIER-PROMISED-YTD ROUNDED =
                       CLTIER-PAST-DUE-YTD * CLTIER-MARGIN / 100
           END-EVALUATE
           IF CLTIER-PROMISED-YTD > 0 AND CLTIER-PROMISED-YTD < 5.00
               MOVE 5.00 TO CLTIER-PROMISED-YTD
           END-IF
           IF CLTIER-PROMISED-YTD > 35.00
               MOVE 35.00 TO CLTIER-PROMISED-YTD
           END-IF
           ADD CLTIER-PROMISED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2630-ACCRUE-PAST-DUE.
           IF CLTIER-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLTIER-PAST-DUE-AMT * CLTIER-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CLTIER-PAST-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2640-DESCRIBE-ACTION.
           EVALUATE CLTIER-ACTION-CD
               WHEN 'KJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2650-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CLTIER-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CLTIER-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2660-LIMIT-PROMISED.
           IF CLTIER-PROMISED-AMT < 10.00
               MOVE 10.00 TO CLTIER-PROMISED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLTIER-PROMISED-AMT > 2500.00
               MOVE 2500.00 TO CLTIER-PROMISED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2670-COUNT-RETRY.
           IF CLTIER-RETRY-CNT < 9999999
               ADD 1 TO CLTIER-RETRY-CNT
           END-IF
           IF CLTIER-RETRY-CNT NOT < 5
               MOVE 'Y' TO CLTIER-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2680-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CLTIER-NEXT-DUE-DATE > 0 AND CLTIER-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLTIER-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2690-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2700-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE CLTIER-COLLECTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE SCHED TIER FOR THE COLLECTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2710-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-SCHED-RATE(1) TO CLTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CLTIER-COLLECTED-YTD NOT <
                   WS-COLLECTE-SCHED-LOW(WS-TX)
                   MOVE WS-COLLECTE-SCHED-RATE(WS-TX) TO CLTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2720-ACCRUE-PAST-DUE.
           IF CLTIER-PAST-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLTIER-PAST-DUE-YTD * CLTIER-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CLTIER-PROMISED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2730-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLTIER-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLTIER-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2740-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CLTIER-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CLTIER-PROMISED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2750-COUNT-RETRY.
           IF CLTIER-RETRY-CNT < 9999999
               ADD 1 TO CLTIER-RETRY-CNT
           END-IF
           IF CLTIER-RETRY-CNT NOT < 12
               MOVE 'Y' TO CLTIER-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED AMT BY ACTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2760-ACCUM-ACTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLTIER-ACTION-CD
                   ADD CLTIER-PROMISED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLTIER-ACTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLTIER-PROMISED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2770-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-TIER-RATE(1) TO CLTIER-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-COLLECTE-TIER-LOW(WS-TX)
                   MOVE WS-COLLECTE-TIER-RATE(WS-TX) TO CLTIER-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLTIER-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLTIER-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CLTIER-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CLTIER-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CLTIER-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CLTIER EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
