      *================================================================*
      * PROGRAM  : PPFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : POSITIVE PAY RULE - FEES. CALLED BY THE POSITIVE PAY*
      *            BATCH AND ONLINE PROGRAMS.                          *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-14 JLM  ORIGINAL                                       *
      * 1991-03-15 LMB  PAID STATUS CODES ADDED INC8352                *
      * 1991-09-16 BHS  PAID CUTOFF MOVED PRB8277                      *
      * 1992-07-14 RJT  ISSUED STATUS CODES ADDED PRB1934              *
      * 1994-10-16 PQR  NEW ISSUED REPORT COLUMNS REQ0184              *
      * 1995-08-13 SKN  PAID NOTICE TEXT PER LEGAL INC4500             *
      * 1995-12-23 BHS  EXCEPT FEE SCHEDULE UPDATE PRB1449             *
      * 1999-05-14 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-06-10 LMB  PERFORMANCE - VOID READ AHEAD PRB1997          *
      * 2002-12-28 RJT  PAID LIMIT REVISED PRB1940                     *
      * 2003-07-05 EWS  EXCEPT ROUNDING - HALF UP REQ2509              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPFEE.
       AUTHOR. J L MARSH.

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
                                           VALUE 19.000000.
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
      * WS-VOID-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000733000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000892000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000151500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000178500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000636500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015150000000009000'.
       01  WS-VOID-SCHED-TBL REDEFINES WS-VOID-SCHED-VALUES.
           05  WS-VOID-SCHED-ROW OCCURS 6 TIMES.
               10  WS-VOID-SCHED-LOW       PIC 9(09)V99.
               10  WS-VOID-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOID-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000665000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000111000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000409500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000638500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001400000000496500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001900000000867000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011900000000884000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011950000000020500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014450000000392000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014700000000369000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014800000000822000'.
       01  WS-VOID-GRID-TBL REDEFINES WS-VOID-GRID-VALUES.
           05  WS-VOID-GRID-ROW OCCURS 11 TIMES.
               10  WS-VOID-GRID-LOW        PIC 9(09)V99.
               10  WS-VOID-GRID-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PPFEEP.

       PROCEDURE DIVISION USING PPFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-EDIT-REF-NO
           PERFORM 2010-COUNT-NOTICE
           PERFORM 2020-ACCUM-REASON
           PERFORM 2030-COUNT-PAYMENT
           PERFORM 2040-EDIT-REF-NO
           PERFORM 2050-ACCRUE-PAID
           PERFORM 2060-SET-MATCH-RULE
           PERFORM 2070-AGE-OPEN
           PERFORM 2080-COUNT-PAYMENT
           PERFORM 2090-AGE-LAST-PAY
           PERFORM 2100-ASSESS-NOTICE-SENT
           PERFORM 2110-ACCRUE-STALE
           PERFORM 2120-PRORATE-PAID
           PERFORM 2130-FLAG-ITEM
           PERFORM 2140-EDIT-REF-NO
           PERFORM 2150-AGE-OPEN
           PERFORM 2160-LIMIT-ITEM
           PERFORM 2170-LIMIT-ITEM
           PERFORM 2180-CHECK-NEXT-REVIEW
           PERFORM 2190-AGE-NEXT-REVIEW
           PERFORM 2200-PRORATE-VOID
           PERFORM 2210-ACCRUE-DISCOUNT
           PERFORM 2220-ACCRUE-PAID
           PERFORM 2230-ACCRUE-MARGIN
           PERFORM 2240-FLAG-EXCEPT
           PERFORM 2250-ACCUM-STATUS
           PERFORM 2260-ACCUM-MATCH-RULE
           PERFORM 2270-LIMIT-ITEM
           PERFORM 2280-LIMIT-STALE
           PERFORM 2290-SET-MATCH-RULE
           PERFORM 2300-COUNT-NOTICE
           PERFORM 2310-SET-REASON
           PERFORM 2320-SET-REGION
           PERFORM 2330-LIMIT-ITEM
           PERFORM 2340-SPLIT-PAID
           PERFORM 2350-AGE-OPEN
           PERFORM 2360-COUNT-PAYMENT
           PERFORM 2370-FLAG-PAID
           PERFORM 2380-FLAG-ITEM
           PERFORM 2390-FLAG-ITEM
           PERFORM 2400-DESCRIBE-REASON
           PERFORM 2410-DESCRIBE-STATUS
           PERFORM 2420-COUNT-NOTICE
           PERFORM 2430-EDIT-CONTACT
           PERFORM 2440-SELECT-MARGIN
           PERFORM 2450-DESCRIBE-MATCH-RULE
           PERFORM 2460-SELECT-DISCOUNT
           PERFORM 2470-SET-REGION
           PERFORM 2480-COUNT-PAYMENT
           PERFORM 2490-FLAG-VOID
           PERFORM 2500-ACCUM-STATUS
           PERFORM 2510-PRORATE-ITEM
           PERFORM 2520-COUNT-PAYMENT
           PERFORM 2530-COUNT-NOTICE
           PERFORM 2540-ASSESS-OVERRIDE
           PERFORM 2550-EDIT-CONTACT
           PERFORM 2560-EDIT-CONTACT
           PERFORM 2570-SET-REASON
           PERFORM 2580-AGE-NEXT-REVIEW
           PERFORM 2590-AGE-OPEN
           PERFORM 2600-AGE-OPEN
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PPFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E073 REF NO REQUIRED
      * E306 VOID YTD NEGATIVE
      * E457 VOID YTD OVER 10000
      * E096 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2000-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPFEE-REF-NO = SPACES
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN PPFEE-VOID-YTD < 0
                   MOVE 'E306' TO WS-REJECT-CODE
               WHEN PPFEE-VOID-YTD > 10000.00
                   MOVE 'E457' TO WS-REJECT-CODE
               WHEN PPFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-NOTICE.
           IF PPFEE-NOTICE-CNT < 9999999
               ADD 1 TO PPFEE-NOTICE-CNT
           END-IF
           IF PPFEE-NOTICE-CNT NOT < 3
               MOVE 'Y' TO PPFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2020-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPFEE-REASON-CD
                   ADD PPFEE-EXCEPT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPFEE-EXCEPT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2030-COUNT-PAYMENT.
           IF PPFEE-PAYMENT-CNT < 9999999
               ADD 1 TO PPFEE-PAYMENT-CNT
           END-IF
           IF PPFEE-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PPFEE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E208 REF NO REQUIRED
      * E826 VOID AMT NEGATIVE
      * E566 VOID AMT OVER 25000
      * E773 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2040-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPFEE-REF-NO = SPACES
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN PPFEE-VOID-AMT < 0
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN PPFEE-VOID-AMT > 25000.00
                   MOVE 'E566' TO WS-REJECT-CODE
               WHEN PPFEE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2050-ACCRUE-PAID.
           IF PPFEE-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPFEE-PAID-YTD * PPFEE-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MATCH RULE CD:
      * A -> B WHEN CLOSE DATE IS PAST
      * B -> F WHEN CLOSE DATE IS PAST
      * F -> M WHEN CLOSE DATE IS PAST
      * M -> S WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2060-SET-MATCH-RULE.
           EVALUATE PPFEE-MATCH-RULE-CD
               WHEN 'A'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPFEE-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO PPFEE-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPFEE-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO PPFEE-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPFEE-OPEN-DATE > 0 AND PPFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPFEE-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPFEE-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PPFEE-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PPFEE-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPFEE-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-PAYMENT.
           IF PPFEE-PAYMENT-CNT < 9999999
               ADD 1 TO PPFEE-PAYMENT-CNT
           END-IF
           IF PPFEE-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PPFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2090-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPFEE-LAST-PAY-DATE > 0 AND PPFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE PAID AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2100-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPFEE-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE PPFEE-VOID-AMT ROUNDED = PPFEE-PAID-AMT *
                       PPFEE-DISCOUNT / 200
               WHEN PPFEE-REASON-CD = 'B'
                   MOVE 12.00 TO PPFEE-VOID-AMT
               WHEN PPFEE-REASON-CD = 'M'
                   MOVE 5.00 TO PPFEE-VOID-AMT
               WHEN OTHER
                   COMPUTE PPFEE-VOID-AMT ROUNDED = PPFEE-PAID-AMT *
                       PPFEE-DISCOUNT / 100
           END-EVALUATE
           IF PPFEE-VOID-AMT > 0 AND PPFEE-VOID-AMT < 1.00
               MOVE 1.00 TO PPFEE-VOID-AMT
           END-IF
           IF PPFEE-VOID-AMT > 150.00
               MOVE 150.00 TO PPFEE-VOID-AMT
           END-IF
           ADD PPFEE-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2110-ACCRUE-STALE.
           IF PPFEE-STALE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPFEE-STALE-YTD * PPFEE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO PPFEE-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       2120-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPFEE-PAID-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPFEE-PAID-YTD
           END-IF
           ADD PPFEE-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2130-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E506 REF NO REQUIRED
      * E058 VOID AMT NEGATIVE
      * E363 VOID AMT OVER 25000
      * E801 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2140-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPFEE-REF-NO = SPACES
                   MOVE 'E506' TO WS-REJECT-CODE
               WHEN PPFEE-VOID-AMT < 0
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN PPFEE-VOID-AMT > 25000.00
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN PPFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPFEE-OPEN-DATE > 0 AND PPFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2160-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2170-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PPFEE-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2190-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPFEE-NEXT-REVIEW-DATE > 0 AND PPFEE-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPFEE-EXCEPT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPFEE-EXCEPT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPFEE-EXCEPT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPFEE-EXCEPT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPFEE-EXCEPT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       2200-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPFEE-VOID-AMT ROUNDED =
                   PPFEE-PAID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPFEE-VOID-AMT
           END-IF
           ADD PPFEE-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2210-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PPFEE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO PPFEE-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2220-ACCRUE-PAID.
           IF PPFEE-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPFEE-PAID-AMT * PPFEE-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2230-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PPFEE-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPFEE-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2240-FLAG-EXCEPT.
           IF PPFEE-EXCEPT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPFEE-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPFEE-STATUS-CD
                   ADD PPFEE-PAID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPFEE-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPFEE-PAID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY MATCH RULE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2260-ACCUM-MATCH-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPFEE-MATCH-RULE-CD
                   ADD PPFEE-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPFEE-MATCH-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPFEE-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2270-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2280-LIMIT-STALE.
           IF PPFEE-STALE-YTD < 50.00
               MOVE 50.00 TO PPFEE-STALE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPFEE-STALE-YTD > 2500.00
               MOVE 2500.00 TO PPFEE-STALE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MATCH RULE CD:
      * A -> B WHEN CLOSE DATE IS PAST
      * B -> F WHEN CLOSE DATE IS PAST
      * F -> M WHEN CLOSE DATE IS PAST
      * M -> S WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2290-SET-MATCH-RULE.
           EVALUATE PPFEE-MATCH-RULE-CD
               WHEN 'A'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPFEE-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO PPFEE-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPFEE-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO PPFEE-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2300-COUNT-NOTICE.
           IF PPFEE-NOTICE-CNT < 9999999
               ADD 1 TO PPFEE-NOTICE-CNT
           END-IF
           IF PPFEE-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PPFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN OPEN DATE IS PAST
      * M -> N WHEN OPEN DATE IS PAST
      * N -> U WHEN OPEN DATE IS PAST
      * U -> V WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-REASON.
           EVALUATE PPFEE-REASON-CD
               WHEN 'B'
                   IF PPFEE-OPEN-DATE > 0 AND PPFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPFEE-OPEN-DATE > 0 AND PPFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPFEE-OPEN-DATE > 0 AND PPFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPFEE-OPEN-DATE > 0 AND PPFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN LAST PAY DATE IS PAST
      * B -> D WHEN LAST PAY DATE IS PAST
      * D -> H WHEN LAST PAY DATE IS PAST
      * H -> N WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2320-SET-REGION.
           EVALUATE PPFEE-REGION-CD
               WHEN 'A'
                   IF PPFEE-LAST-PAY-DATE > 0 AND PPFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPFEE-LAST-PAY-DATE > 0 AND PPFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PPFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PPFEE-LAST-PAY-DATE > 0 AND PPFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PPFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PPFEE-LAST-PAY-DATE > 0 AND PPFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2330-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2340-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPFEE-PAID-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PPFEE-PAID-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2350-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPFEE-OPEN-DATE > 0 AND PPFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPFEE-VOID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPFEE-VOID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPFEE-VOID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPFEE-VOID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPFEE-VOID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-PAYMENT.
           IF PPFEE-PAYMENT-CNT < 9999999
               ADD 1 TO PPFEE-PAYMENT-CNT
           END-IF
           IF PPFEE-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PPFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2370-FLAG-PAID.
           IF PPFEE-PAID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPFEE-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2380-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2390-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2400-DESCRIBE-REASON.
           EVALUATE PPFEE-REASON-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'V'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2410-DESCRIBE-STATUS.
           EVALUATE PPFEE-STATUS-CD
               WHEN 'DR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2420-COUNT-NOTICE.
           IF PPFEE-NOTICE-CNT < 9999999
               ADD 1 TO PPFEE-NOTICE-CNT
           END-IF
           IF PPFEE-NOTICE-CNT NOT < 3
               MOVE 'Y' TO PPFEE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E833 CONTACT REQUIRED
      * E463 VOID YTD NEGATIVE
      * E248 VOID YTD OVER 5000
      * E785 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2430-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPFEE-CONTACT = SPACES
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN PPFEE-VOID-YTD < 0
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN PPFEE-VOID-YTD > 5000.00
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN PPFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E785' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-GRID-RATE(1) TO PPFEE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-VOID-GRID-LOW(WS-TX)
                   MOVE WS-VOID-GRID-RATE(WS-TX) TO PPFEE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPFEE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPFEE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2450-DESCRIBE-MATCH-RULE.
           EVALUATE PPFEE-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PPFEE-PAID-YTD NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN CLOSE DATE IS PAST
      * B -> D WHEN CLOSE DATE IS PAST
      * D -> H WHEN CLOSE DATE IS PAST
      * H -> N WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-REGION.
           EVALUATE PPFEE-REGION-CD
               WHEN 'A'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PPFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PPFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2480-COUNT-PAYMENT.
           IF PPFEE-PAYMENT-CNT < 9999999
               ADD 1 TO PPFEE-PAYMENT-CNT
           END-IF
           IF PPFEE-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PPFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2490-FLAG-VOID.
           IF PPFEE-VOID-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPFEE-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPFEE-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPFEE-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2510-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2520-COUNT-PAYMENT.
           IF PPFEE-PAYMENT-CNT < 9999999
               ADD 1 TO PPFEE-PAYMENT-CNT
           END-IF
           IF PPFEE-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PPFEE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2530-COUNT-NOTICE.
           IF PPFEE-NOTICE-CNT < 9999999
               ADD 1 TO PPFEE-NOTICE-CNT
           END-IF
           IF PPFEE-NOTICE-CNT NOT < 5
               MOVE 'Y' TO PPFEE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2540-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPFEE-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPFEE-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PPFEE-PAID-AMT ROUNDED = WS-WORK-AMT *
                       PPFEE-DISCOUNT / 200
               WHEN PPFEE-MATCH-RULE-CD = 'A'
                   MOVE 20.00 TO PPFEE-PAID-AMT
               WHEN PPFEE-MATCH-RULE-CD = 'B'
                   MOVE 3.00 TO PPFEE-PAID-AMT
               WHEN OTHER
                   COMPUTE PPFEE-PAID-AMT ROUNDED = WS-WORK-AMT *
                       PPFEE-DISCOUNT / 100
           END-EVALUATE
           IF PPFEE-PAID-AMT > 0 AND PPFEE-PAID-AMT < 1.00
               MOVE 1.00 TO PPFEE-PAID-AMT
           END-IF
           IF PPFEE-PAID-AMT > 250.00
               MOVE 250.00 TO PPFEE-PAID-AMT
           END-IF
           ADD PPFEE-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E143 CONTACT REQUIRED
      * E786 VOID AMT NEGATIVE
      * E114 VOID AMT OVER 99999
      * E832 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2550-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPFEE-CONTACT = SPACES
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN PPFEE-VOID-AMT < 0
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN PPFEE-VOID-AMT > 99999.00
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN PPFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E832' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E675 CONTACT REQUIRED
      * E171 WORK AMT 2 NEGATIVE
      * E019 WORK AMT 2 OVER 25000
      * E547 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2560-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPFEE-CONTACT = SPACES
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E171' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN PPFEE-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E547' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN CLOSE DATE IS PAST
      * M -> N WHEN CLOSE DATE IS PAST
      * N -> U WHEN CLOSE DATE IS PAST
      * U -> V WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2570-SET-REASON.
           EVALUATE PPFEE-REASON-CD
               WHEN 'B'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPFEE-CLOSE-DATE > 0 AND PPFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2580-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPFEE-NEXT-REVIEW-DATE > 0 AND PPFEE-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPFEE-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPFEE-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPFEE-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PPFEE-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPFEE-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2590-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPFEE-OPEN-DATE > 0 AND PPFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPFEE-PAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPFEE-PAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPFEE-PAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPFEE-PAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPFEE-PAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2600-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPFEE-OPEN-DATE > 0 AND PPFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPFEE-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPFEE-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPFEE-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPFEE-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPFEE-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PPFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PPFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PPFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PPFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
