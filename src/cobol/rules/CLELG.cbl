      *================================================================*
      * PROGRAM  : CLELG                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : COLLECTIONS QUEUE RULE - FEES. CALLED BY THE        *
      *            COLLECTIONS QUEUE BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-06-25 DPS  ORIGINAL                                       *
      * 1996-06-02 MKL  PAST-DUE EXCEPTIONS TO OPS QUEUE CHG9404       *
      * 1997-01-28 GAF  PERFORMANCE - WRITEOFF READ AHEAD INC5729      *
      * 1998-09-27 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-08-06 LMB  PAST-DUE RERUN PROTECTION REQ3464              *
      * 1999-03-21 JLM  WRITEOFF STATUS CODES ADDED PRB8820            *
      * 1999-03-24 MKL  WRITEOFF TABLE EXPANDED CHG1102                *
      * 2000-10-16 MKL  PERFORMANCE - COLLECTED READ AHEAD REQ2310     *
      * 2001-03-06 PQR  PROMISED STATUS CODES ADDED INC0100            *
      * 2002-03-06 LMB  FEE LIMIT REVISED REQ6342                      *
      * 2003-09-03 CRP  PAST-DUE LIMIT REVISED REQ2721                 *
      * 2003-07-06 MKL  COLLECTED CALCULATION CORRECTED PRB5436        *
      * 2004-12-23 CRP  PROMISED LIMIT REVISED PRB0374                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLELG.
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
                                           VALUE 17.000000.
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
      * WS-PAST-DUE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAST-DUE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000431500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000806000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000793000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000834000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000288500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022100000000632000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023100000000548500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023600000000378500'.
       01  WS-PAST-DUE-GRID-TBL REDEFINES WS-PAST-DUE-GRID-VALUES.
           05  WS-PAST-DUE-GRID-ROW OCCURS 8 TIMES.
               10  WS-PAST-DUE-GRID-LOW    PIC 9(09)V99.
               10  WS-PAST-DUE-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PROMISED-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PROMISED-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000286500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000346000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000155500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004500000000423000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004600000000496000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000409000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000809500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000726500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000219000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000301000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000280000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000086000'.
       01  WS-PROMISED-SCHED-TBL REDEFINES WS-PROMISED-SCHED-VALUES.
           05  WS-PROMISED-SCHED-ROW OCCURS 12 TIMES.
               10  WS-PROMISED-SCHED-LOW   PIC 9(09)V99.
               10  WS-PROMISED-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WRITEOFF-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WRITEOFF-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000240000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000288000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000399500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000525000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002900000000366500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002950000000081500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000168000'.
       01  WS-WRITEOFF-SCHED-TBL REDEFINES WS-WRITEOFF-SCHED-VALUES.
           05  WS-WRITEOFF-SCHED-ROW OCCURS 7 TIMES.
               10  WS-WRITEOFF-SCHED-LOW   PIC 9(09)V99.
               10  WS-WRITEOFF-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CLELGP.

       PROCEDURE DIVISION USING CLELG-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-EFFECTIVE
           PERFORM 2010-ASSESS-EXEMPT
           PERFORM 2020-SELECT-MARGIN
           PERFORM 2030-CHECK-NEXT-DUE
           PERFORM 2040-EDIT-DESC
           PERFORM 2050-LIMIT-WRITEOFF
           PERFORM 2060-ACCRUE-WRITEOFF
           PERFORM 2070-SELECT-FEE-PCT
           PERFORM 2080-AGE-LAST-MAINT
           PERFORM 2090-SET-QUEUE
           PERFORM 2100-SPLIT-ITEM
           PERFORM 2110-SET-REASON
           PERFORM 2120-LIMIT-WRITEOFF
           PERFORM 2130-LIMIT-ITEM
           PERFORM 2140-LIMIT-ITEM
           PERFORM 2150-ACCUM-CYCLE
           PERFORM 2160-CHECK-EXPIRY
           PERFORM 2170-SET-QUEUE
           PERFORM 2180-LIMIT-ITEM
           PERFORM 2190-SET-STATUS
           PERFORM 2200-PRORATE-WRITEOFF
           PERFORM 2210-EDIT-ADDR
           PERFORM 2220-PRORATE-PAST-DUE
           PERFORM 2230-SET-STATUS
           PERFORM 2240-LIMIT-COLLECTED
           PERFORM 2250-SET-STATUS
           PERFORM 2260-PRORATE-FEE
           PERFORM 2270-ASSESS-NOTICE-SENT
           PERFORM 2280-COUNT-NOTICE
           PERFORM 2290-LIMIT-FEE
           PERFORM 2300-LIMIT-COLLECTED
           PERFORM 2310-EDIT-ADDR
           PERFORM 2320-CHECK-LAST-MAINT
           PERFORM 2330-DESCRIBE-QUEUE
           PERFORM 2340-SPLIT-FEE
           PERFORM 2350-ACCUM-QUEUE
           PERFORM 2360-ASSESS-OVERRIDE
           PERFORM 2370-PRORATE-WRITEOFF
           PERFORM 2380-AGE-EFFECTIVE
           PERFORM 2390-AGE-NEXT-DUE
           PERFORM 2400-CHECK-EFFECTIVE
           PERFORM 2410-ACCUM-QUEUE
           PERFORM 2420-SELECT-MARGIN
           PERFORM 2430-CHECK-LAST-MAINT
           PERFORM 2440-CHECK-NEXT-DUE
           PERFORM 2450-LIMIT-WRITEOFF
           PERFORM 2460-LIMIT-WRITEOFF
           PERFORM 2470-ACCRUE-FEE-PCT
           PERFORM 2480-ACCRUE-FEE-PCT
           PERFORM 2490-CHECK-NEXT-DUE
           PERFORM 2500-CHECK-NEXT-DUE
           PERFORM 2510-PRORATE-FEE
           PERFORM 2520-ACCRUE-COLLECTED
           PERFORM 2530-SPLIT-WRITEOFF
           PERFORM 2540-ASSESS-NOTICE-SENT
           PERFORM 2550-LIMIT-FEE
           PERFORM 2560-PRORATE-WRITEOFF
           PERFORM 2570-LIMIT-WRITEOFF
           PERFORM 2580-SET-REASON
           PERFORM 2590-DESCRIBE-QUEUE
           PERFORM 2600-PRORATE-WRITEOFF
           PERFORM 2610-SET-STATUS
           PERFORM 2620-ASSESS-NOTICE-SENT
           PERFORM 2630-EDIT-DESC
           PERFORM 2640-EDIT-DESC
           PERFORM 2650-CHECK-EXPIRY
           PERFORM 2660-SET-STATUS
           PERFORM 2670-FLAG-FEE
           PERFORM 2680-PRORATE-PAST-DUE
           PERFORM 2690-ACCRUE-FEE
           PERFORM 2700-FLAG-ITEM
           PERFORM 2710-COUNT-NOTICE
           PERFORM 2720-FLAG-ITEM
           PERFORM 2730-ASSESS-OVERRIDE
           PERFORM 2740-LIMIT-ITEM
           PERFORM 2750-SPLIT-PAST-DUE
           PERFORM 2760-SET-REASON
           PERFORM 2770-SPLIT-WRITEOFF
           PERFORM 2780-ACCUM-QUEUE
           PERFORM 2790-LIMIT-ITEM
           PERFORM 2800-ASSESS-EXEMPT
           PERFORM 2810-CHECK-NEXT-DUE
           PERFORM 2820-ACCRUE-WRITEOFF
           PERFORM 2830-DESCRIBE-STATUS
           PERFORM 2840-ASSESS-NOTICE-SENT
           PERFORM 2850-EDIT-DESC
           PERFORM 2860-FLAG-PAST-DUE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CLELG-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLELG-WRITEOFF-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CLELG-WRITEOFF-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CLELG-WRITEOFF-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CLELG-WRITEOFF-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLELG-WRITEOFF-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2010-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CLELG-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CLELG-PAST-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE CLELG-PAST-DUE-AMT ROUNDED = WS-WORK-AMT-2 *
                       CLELG-FEE-PCT / 200
               WHEN CLELG-REASON-CD = 'AT'
                   MOVE 10.00 TO CLELG-PAST-DUE-AMT
               WHEN CLELG-REASON-CD = 'BJ'
                   MOVE 10.00 TO CLELG-PAST-DUE-AMT
               WHEN OTHER
                   COMPUTE CLELG-PAST-DUE-AMT ROUNDED = WS-WORK-AMT-2 *
                       CLELG-FEE-PCT / 100
           END-EVALUATE
           IF CLELG-PAST-DUE-AMT > 0 AND CLELG-PAST-DUE-AMT < 1.00
               MOVE 1.00 TO CLELG-PAST-DUE-AMT
           END-IF
           IF CLELG-PAST-DUE-AMT > 100.00
               MOVE 100.00 TO CLELG-PAST-DUE-AMT
           END-IF
           ADD CLELG-PAST-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2020-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-SCHED-RATE(1) TO CLELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-WRITEOFF-SCHED-LOW(WS-TX)
                   MOVE WS-WRITEOFF-SCHED-RATE(WS-TX) TO CLELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2030-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CLELG-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CLELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E549 DESC REQUIRED
      * E901 WORK AMT NEGATIVE
      * E950 WORK AMT OVER 250000
      * E697 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2040-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLELG-DESC = SPACES
                   MOVE 'E549' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E901' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN CLELG-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E697' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2050-LIMIT-WRITEOFF.
           IF CLELG-WRITEOFF-YTD < 50.00
               MOVE 50.00 TO CLELG-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLELG-WRITEOFF-YTD > 2500.00
               MOVE 2500.00 TO CLELG-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2060-ACCRUE-WRITEOFF.
           IF CLELG-WRITEOFF-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLELG-WRITEOFF-YTD * CLELG-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CLELG-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2070-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-SCHED-RATE(1) TO CLELG-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-WRITEOFF-SCHED-LOW(WS-TX)
                   MOVE WS-WRITEOFF-SCHED-RATE(WS-TX) TO CLELG-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLELG-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLELG-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2080-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF CLELG-LAST-MAINT-DATE > 0 AND CLELG-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLELG-WRITEOFF-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CLELG-WRITEOFF-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CLELG-WRITEOFF-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CLELG-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLELG-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN EFFECTIVE DATE IS PAST
      * ER -> HA WHEN EFFECTIVE DATE IS PAST
      * HA -> HW WHEN EFFECTIVE DATE IS PAST
      * HW -> ME WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2090-SET-QUEUE.
           EVALUATE CLELG-QUEUE-CD
               WHEN 'DL'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ER' TO CLELG-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HA' TO CLELG-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HW' TO CLELG-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ME' TO CLELG-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2100-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN EFFECTIVE DATE IS PAST
      * BJ -> GG WHEN EFFECTIVE DATE IS PAST
      * GG -> ZR WHEN EFFECTIVE DATE IS PAST
      * ZR -> AT WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2110-SET-REASON.
           EVALUATE CLELG-REASON-CD
               WHEN 'AT'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BJ' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GG' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ZR' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AT' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2120-LIMIT-WRITEOFF.
           IF CLELG-WRITEOFF-YTD < 25.00
               MOVE 25.00 TO CLELG-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLELG-WRITEOFF-YTD > 500.00
               MOVE 500.00 TO CLELG-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2140-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2150-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLELG-CYCLE-CD
                   ADD CLELG-PAST-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLELG-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLELG-PAST-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2160-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CLELG-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CLELG-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN EFFECTIVE DATE IS PAST
      * ER -> HA WHEN EFFECTIVE DATE IS PAST
      * HA -> HW WHEN EFFECTIVE DATE IS PAST
      * HW -> ME WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2170-SET-QUEUE.
           EVALUATE CLELG-QUEUE-CD
               WHEN 'DL'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ER' TO CLELG-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HA' TO CLELG-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HW' TO CLELG-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ME' TO CLELG-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2180-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN EFFECTIVE DATE IS PAST
      * UR -> XM WHEN EFFECTIVE DATE IS PAST
      * XM -> YA WHEN EFFECTIVE DATE IS PAST
      * YA -> PS WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2190-SET-STATUS.
           EVALUATE CLELG-STATUS-CD
               WHEN 'PS'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UR' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XM' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YA' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PS' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2200-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLELG-WRITEOFF-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E257 ADDR REQUIRED
      * E722 WORK AMT 2 NEGATIVE
      * E826 WORK AMT 2 OVER 10000
      * E676 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2210-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLELG-ADDR = SPACES
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E722' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN CLELG-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2220-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLELG-PAST-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN EXPIRY DATE IS PAST
      * UR -> XM WHEN EXPIRY DATE IS PAST
      * XM -> YA WHEN EXPIRY DATE IS PAST
      * YA -> PS WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2230-SET-STATUS.
           EVALUATE CLELG-STATUS-CD
               WHEN 'PS'
                   IF CLELG-EXPIRY-DATE > 0 AND CLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CLELG-EXPIRY-DATE > 0 AND CLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF CLELG-EXPIRY-DATE > 0 AND CLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF CLELG-EXPIRY-DATE > 0 AND CLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2240-LIMIT-COLLECTED.
           IF CLELG-COLLECTED-AMT < 0.00
               MOVE 0.00 TO CLELG-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLELG-COLLECTED-AMT > 500.00
               MOVE 500.00 TO CLELG-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN NEXT DUE DATE IS PAST
      * UR -> XM WHEN NEXT DUE DATE IS PAST
      * XM -> YA WHEN NEXT DUE DATE IS PAST
      * YA -> PS WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2250-SET-STATUS.
           EVALUATE CLELG-STATUS-CD
               WHEN 'PS'
                   IF CLELG-NEXT-DUE-DATE > 0 AND CLELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CLELG-NEXT-DUE-DATE > 0 AND CLELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF CLELG-NEXT-DUE-DATE > 0 AND CLELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF CLELG-NEXT-DUE-DATE > 0 AND CLELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2260-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLELG-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PAST DUE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2270-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CLELG-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLELG-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CLELG-PAST-DUE-AMT *
                       CLELG-FEE-PCT / 200
               WHEN CLELG-CYCLE-CD = 'BE'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN CLELG-CYCLE-CD = 'GV'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CLELG-PAST-DUE-AMT *
                       CLELG-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-NOTICE.
           IF CLELG-NOTICE-CNT < 9999999
               ADD 1 TO CLELG-NOTICE-CNT
           END-IF
           IF CLELG-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CLELG-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2290-LIMIT-FEE.
           IF CLELG-FEE-AMT < 10.00
               MOVE 10.00 TO CLELG-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLELG-FEE-AMT > 1000.00
               MOVE 1000.00 TO CLELG-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2300-LIMIT-COLLECTED.
           IF CLELG-COLLECTED-AMT < 25.00
               MOVE 25.00 TO CLELG-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLELG-COLLECTED-AMT > 50000.00
               MOVE 50000.00 TO CLELG-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E702 ADDR REQUIRED
      * E425 WORK AMT NEGATIVE
      * E968 WORK AMT OVER 5000
      * E497 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2310-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLELG-ADDR = SPACES
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E425' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E968' TO WS-REJECT-CODE
               WHEN CLELG-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E497' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2320-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF CLELG-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CLELG-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2330-DESCRIBE-QUEUE.
           EVALUATE CLELG-QUEUE-CD
               WHEN 'DL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ER'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ME'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2340-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = CLELG-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CLELG-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE CLELG-WRITEOFF-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2350-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLELG-QUEUE-CD
                   ADD CLELG-WRITEOFF-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLELG-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLELG-WRITEOFF-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2360-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CLELG-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CLELG-COLLECTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLELG-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CLELG-COLLECTED-AMT ROUNDED = WS-WORK-AMT-2 *
                       CLELG-MARGIN / 200
               WHEN CLELG-REASON-CD = 'AT'
                   MOVE 7.00 TO CLELG-COLLECTED-AMT
               WHEN CLELG-REASON-CD = 'BJ'
                   MOVE 7.00 TO CLELG-COLLECTED-AMT
               WHEN OTHER
                   COMPUTE CLELG-COLLECTED-AMT ROUNDED = WS-WORK-AMT-2 *
                       CLELG-MARGIN / 100
           END-EVALUATE
           IF CLELG-COLLECTED-AMT > 0 AND CLELG-COLLECTED-AMT < 1.00
               MOVE 1.00 TO CLELG-COLLECTED-AMT
           END-IF
           IF CLELG-COLLECTED-AMT > 50.00
               MOVE 50.00 TO CLELG-COLLECTED-AMT
           END-IF
           ADD CLELG-COLLECTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WRITEOFF YTD.
      *----------------------------------------------------------------*
       2370-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLELG-WRITEOFF-YTD ROUNDED =
                   CLELG-COLLECTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLELG-WRITEOFF-YTD
           END-IF
           ADD CLELG-WRITEOFF-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2380-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLELG-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CLELG-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CLELG-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CLELG-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLELG-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CLELG-NEXT-DUE-DATE > 0 AND CLELG-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CLELG-WRITEOFF-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CLELG-WRITEOFF-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CLELG-WRITEOFF-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CLELG-WRITEOFF-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CLELG-WRITEOFF-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2400-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CLELG-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CLELG-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2410-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLELG-QUEUE-CD
                   ADD CLELG-WRITEOFF-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLELG-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLELG-WRITEOFF-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PROMISED SCHED TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PROMISED-SCHED-RATE(1) TO CLELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CLELG-FEE-YTD NOT < WS-PROMISED-SCHED-LOW(WS-TX)
                   MOVE WS-PROMISED-SCHED-RATE(WS-TX) TO CLELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2430-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF CLELG-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CLELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CLELG-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CLELG-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2450-LIMIT-WRITEOFF.
           IF CLELG-WRITEOFF-AMT < 10.00
               MOVE 10.00 TO CLELG-WRITEOFF-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLELG-WRITEOFF-AMT > 5000.00
               MOVE 5000.00 TO CLELG-WRITEOFF-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2460-LIMIT-WRITEOFF.
           IF CLELG-WRITEOFF-YTD < 50.00
               MOVE 50.00 TO CLELG-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLELG-WRITEOFF-YTD > 2500.00
               MOVE 2500.00 TO CLELG-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2470-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CLELG-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CLELG-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2480-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CLELG-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CLELG-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CLELG-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CLELG-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2500-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CLELG-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CLELG-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       2510-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLELG-FEE-AMT ROUNDED =
                   CLELG-WRITEOFF-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CLELG-FEE-AMT
           END-IF
           ADD CLELG-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-COLLECTED.
           IF CLELG-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLELG-COLLECTED-AMT * CLELG-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CLELG-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = CLELG-WRITEOFF-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CLELG-WRITEOFF-AMT - (WS-WORK-AMT *
               3)
           COMPUTE CLELG-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2540-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CLELG-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CLELG-WRITEOFF-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLELG-EXEMPT-FLAG = 'Y'
                   COMPUTE CLELG-WRITEOFF-AMT ROUNDED = WS-WORK-AMT-2 *
                       CLELG-FEE-PCT / 200
               WHEN CLELG-STATUS-CD = 'PS'
                   MOVE 10.00 TO CLELG-WRITEOFF-AMT
               WHEN CLELG-STATUS-CD = 'UR'
                   MOVE 10.00 TO CLELG-WRITEOFF-AMT
               WHEN OTHER
                   COMPUTE CLELG-WRITEOFF-AMT ROUNDED = WS-WORK-AMT-2 *
                       CLELG-FEE-PCT / 100
           END-EVALUATE
           IF CLELG-WRITEOFF-AMT > 0 AND CLELG-WRITEOFF-AMT < 10.00
               MOVE 10.00 TO CLELG-WRITEOFF-AMT
           END-IF
           IF CLELG-WRITEOFF-AMT > 50.00
               MOVE 50.00 TO CLELG-WRITEOFF-AMT
           END-IF
           ADD CLELG-WRITEOFF-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2550-LIMIT-FEE.
           IF CLELG-FEE-AMT < 100.00
               MOVE 100.00 TO CLELG-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLELG-FEE-AMT > 2500.00
               MOVE 2500.00 TO CLELG-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WRITEOFF AMT.
      *----------------------------------------------------------------*
       2560-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLELG-WRITEOFF-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLELG-WRITEOFF-AMT
           END-IF
           ADD CLELG-WRITEOFF-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2570-LIMIT-WRITEOFF.
           IF CLELG-WRITEOFF-YTD < 25.00
               MOVE 25.00 TO CLELG-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLELG-WRITEOFF-YTD > 10000.00
               MOVE 10000.00 TO CLELG-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN EXPIRY DATE IS PAST
      * BJ -> GG WHEN EXPIRY DATE IS PAST
      * GG -> ZR WHEN EXPIRY DATE IS PAST
      * ZR -> AT WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2580-SET-REASON.
           EVALUATE CLELG-REASON-CD
               WHEN 'AT'
                   IF CLELG-EXPIRY-DATE > 0 AND CLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF CLELG-EXPIRY-DATE > 0 AND CLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF CLELG-EXPIRY-DATE > 0 AND CLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZR' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF CLELG-EXPIRY-DATE > 0 AND CLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2590-DESCRIBE-QUEUE.
           EVALUATE CLELG-QUEUE-CD
               WHEN 'DL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ER'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ME'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2600-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLELG-WRITEOFF-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN NEXT DUE DATE IS PAST
      * UR -> XM WHEN NEXT DUE DATE IS PAST
      * XM -> YA WHEN NEXT DUE DATE IS PAST
      * YA -> PS WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2610-SET-STATUS.
           EVALUATE CLELG-STATUS-CD
               WHEN 'PS'
                   IF CLELG-NEXT-DUE-DATE > 0 AND CLELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CLELG-NEXT-DUE-DATE > 0 AND CLELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF CLELG-NEXT-DUE-DATE > 0 AND CLELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF CLELG-NEXT-DUE-DATE > 0 AND CLELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF YTD FEE ON THE COLLECTED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2620-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CLELG-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CLELG-WRITEOFF-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLELG-EXEMPT-FLAG = 'Y'
                   COMPUTE CLELG-WRITEOFF-YTD ROUNDED =
                       CLELG-COLLECTED-AMT * CLELG-MARGIN / 200
               WHEN CLELG-REASON-CD = 'AT'
                   MOVE 5.00 TO CLELG-WRITEOFF-YTD
               WHEN CLELG-REASON-CD = 'BJ'
                   MOVE 20.00 TO CLELG-WRITEOFF-YTD
               WHEN OTHER
                   COMPUTE CLELG-WRITEOFF-YTD ROUNDED =
                       CLELG-COLLECTED-AMT * CLELG-MARGIN / 100
           END-EVALUATE
           IF CLELG-WRITEOFF-YTD > 0 AND CLELG-WRITEOFF-YTD < 5.00
               MOVE 5.00 TO CLELG-WRITEOFF-YTD
           END-IF
           IF CLELG-WRITEOFF-YTD > 250.00
               MOVE 250.00 TO CLELG-WRITEOFF-YTD
           END-IF
           ADD CLELG-WRITEOFF-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E918 DESC REQUIRED
      * E284 WORK AMT NEGATIVE
      * E406 WORK AMT OVER 5000
      * E606 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2630-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLELG-DESC = SPACES
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN CLELG-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E635 DESC REQUIRED
      * E982 COLLECTED AMT NEGATIVE
      * E904 COLLECTED AMT OVER 50000
      * E340 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2640-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLELG-DESC = SPACES
                   MOVE 'E635' TO WS-REJECT-CODE
               WHEN CLELG-COLLECTED-AMT < 0
                   MOVE 'E982' TO WS-REJECT-CODE
               WHEN CLELG-COLLECTED-AMT > 50000.00
                   MOVE 'E904' TO WS-REJECT-CODE
               WHEN CLELG-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2650-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CLELG-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CLELG-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN LAST MAINT DATE IS PAST
      * UR -> XM WHEN LAST MAINT DATE IS PAST
      * XM -> YA WHEN LAST MAINT DATE IS PAST
      * YA -> PS WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2660-SET-STATUS.
           EVALUATE CLELG-STATUS-CD
               WHEN 'PS'
                   IF CLELG-LAST-MAINT-DATE > 0 AND
                       CLELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UR' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CLELG-LAST-MAINT-DATE > 0 AND
                       CLELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XM' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF CLELG-LAST-MAINT-DATE > 0 AND
                       CLELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'YA' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF CLELG-LAST-MAINT-DATE > 0 AND
                       CLELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'PS' TO CLELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2670-FLAG-FEE.
           IF CLELG-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CLELG-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       2680-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLELG-PAST-DUE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLELG-PAST-DUE-AMT
           END-IF
           ADD CLELG-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2690-ACCRUE-FEE.
           IF CLELG-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLELG-FEE-AMT * CLELG-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CLELG-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2700-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2710-COUNT-NOTICE.
           IF CLELG-NOTICE-CNT < 9999999
               ADD 1 TO CLELG-NOTICE-CNT
           END-IF
           IF CLELG-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CLELG-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2720-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2730-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CLELG-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLELG-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CLELG-WRITEOFF-AMT *
                       CLELG-MARGIN / 200
               WHEN CLELG-CYCLE-CD = 'BE'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN CLELG-CYCLE-CD = 'GV'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CLELG-WRITEOFF-AMT *
                       CLELG-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2740-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2750-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE CLELG-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN EFFECTIVE DATE IS PAST
      * BJ -> GG WHEN EFFECTIVE DATE IS PAST
      * GG -> ZR WHEN EFFECTIVE DATE IS PAST
      * ZR -> AT WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2760-SET-REASON.
           EVALUATE CLELG-REASON-CD
               WHEN 'AT'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BJ' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GG' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ZR' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF CLELG-EFFECTIVE-DATE > 0 AND CLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AT' TO CLELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2770-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE CLELG-WRITEOFF-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2780-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLELG-QUEUE-CD
                   ADD CLELG-WRITEOFF-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLELG-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLELG-WRITEOFF-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2790-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2800-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CLELG-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CLELG-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE CLELG-FEE-YTD ROUNDED = WS-WORK-AMT *
                       CLELG-MARGIN / 200
               WHEN CLELG-STATUS-CD = 'PS'
                   MOVE 3.00 TO CLELG-FEE-YTD
               WHEN CLELG-STATUS-CD = 'UR'
                   MOVE 5.00 TO CLELG-FEE-YTD
               WHEN OTHER
                   COMPUTE CLELG-FEE-YTD ROUNDED = WS-WORK-AMT *
                       CLELG-MARGIN / 100
           END-EVALUATE
           IF CLELG-FEE-YTD > 0 AND CLELG-FEE-YTD < 10.00
               MOVE 10.00 TO CLELG-FEE-YTD
           END-IF
           IF CLELG-FEE-YTD > 75.00
               MOVE 75.00 TO CLELG-FEE-YTD
           END-IF
           ADD CLELG-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2810-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CLELG-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CLELG-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2820-ACCRUE-WRITEOFF.
           IF CLELG-WRITEOFF-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLELG-WRITEOFF-AMT * CLELG-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO CLELG-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2830-DESCRIBE-STATUS.
           EVALUATE CLELG-STATUS-CD
               WHEN 'PS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF YTD FEE ON THE PAST DUE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2840-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CLELG-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CLELG-WRITEOFF-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLELG-EXEMPT-FLAG = 'Y'
                   COMPUTE CLELG-WRITEOFF-YTD ROUNDED =
                       CLELG-PAST-DUE-AMT * CLELG-FEE-PCT / 200
               WHEN CLELG-CYCLE-CD = 'BE'
                   MOVE 10.00 TO CLELG-WRITEOFF-YTD
               WHEN CLELG-CYCLE-CD = 'GV'
                   MOVE 20.00 TO CLELG-WRITEOFF-YTD
               WHEN OTHER
                   COMPUTE CLELG-WRITEOFF-YTD ROUNDED =
                       CLELG-PAST-DUE-AMT * CLELG-FEE-PCT / 100
           END-EVALUATE
           IF CLELG-WRITEOFF-YTD > 0 AND CLELG-WRITEOFF-YTD < 5.00
               MOVE 5.00 TO CLELG-WRITEOFF-YTD
           END-IF
           IF CLELG-WRITEOFF-YTD > 100.00
               MOVE 100.00 TO CLELG-WRITEOFF-YTD
           END-IF
           ADD CLELG-WRITEOFF-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E936 DESC REQUIRED
      * E039 WORK AMT 2 NEGATIVE
      * E827 WORK AMT 2 OVER 10000
      * E085 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2850-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLELG-DESC = SPACES
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E039' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN CLELG-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E085' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2860-FLAG-PAST-DUE.
           IF CLELG-PAST-DUE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CLELG-PAST-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CLELG-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CLELG-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CLELG-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CLELG EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
