      *================================================================*
      * PROGRAM  : SDVAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : SAFE DEPOSIT BOXES RULE - TIERING. CALLED BY THE    *
      *            SAFE DEPOSIT BOXES BATCH AND ONLINE PROGRAMS.       *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-09-13 BHS  ORIGINAL                                       *
      * 1996-11-05 SKN  LATE-FEE EXCEPTIONS TO OPS QUEUE PRB7303       *
      * 1996-11-25 MKL  PREPAID STATUS CODES ADDED INC3064             *
      * 1997-04-11 DPS  DEPOSIT NOTICE TEXT PER LEGAL CHG0602          *
      * 1999-08-24 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-12-16 BHS  PREPAID TABLE EXPANDED PRB5694                 *
      * 2000-02-08 PQR  LATE-FEE FEE SCHEDULE UPDATE CHG8426           *
      * 2001-11-18 CRP  RENT FEE SCHEDULE UPDATE CHG5865               *
      * 2006-04-17 EWS  LATE-FEE EXCEPTIONS TO OPS QUEUE PRB6243       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDVAL.
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
                                           VALUE 12.000000.
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
      * WS-RENT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RENT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000644500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000131000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000320500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000472000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015300000000888000'.
       01  WS-RENT-TIER-TBL REDEFINES WS-RENT-TIER-VALUES.
           05  WS-RENT-TIER-ROW OCCURS 5 TIMES.
               10  WS-RENT-TIER-LOW        PIC 9(09)V99.
               10  WS-RENT-TIER-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SDVALP.

       PROCEDURE DIVISION USING SDVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PRORATE-DEPOSIT
           PERFORM 2010-ACCRUE-DISCOUNT
           PERFORM 2020-PRORATE-DRILL
           PERFORM 2030-FLAG-KEY-FEE
           PERFORM 2040-AGE-LAST-MAINT
           PERFORM 2050-CHECK-OPEN
           PERFORM 2060-DESCRIBE-REASON
           PERFORM 2070-AGE-LAST-MAINT
           PERFORM 2080-SPLIT-KEY-FEE
           PERFORM 2090-COUNT-RETRY
           PERFORM 2100-ACCRUE-PENALTY-PCT
           PERFORM 2110-COUNT-NOTICE
           PERFORM 2120-SPLIT-DEPOSIT
           PERFORM 2130-ASSESS-REVIEW
           PERFORM 2140-SET-BRANCH
           PERFORM 2150-FLAG-DEPOSIT
           PERFORM 2160-ACCUM-CYCLE
           PERFORM 2170-PRORATE-DRILL
           PERFORM 2180-SET-BRANCH
           PERFORM 2190-CHECK-OPEN
           PERFORM 2200-COUNT-NOTICE
           PERFORM 2210-AGE-LAST-STMT
           PERFORM 2220-ASSESS-AUTO-RENEW
           PERFORM 2230-SPLIT-RENT
           PERFORM 2240-COUNT-RETRY
           PERFORM 2250-ACCUM-CYCLE
           PERFORM 2260-ASSESS-AUTO-RENEW
           PERFORM 2270-COUNT-NOTICE
           PERFORM 2280-PRORATE-DRILL
           PERFORM 2290-COUNT-RETRY
           PERFORM 2300-COUNT-RETRY
           PERFORM 2310-DESCRIBE-BOX-SIZE
           PERFORM 2320-PRORATE-KEY-FEE
           PERFORM 2330-COUNT-RETRY
           PERFORM 2340-SET-REASON
           PERFORM 2350-ACCUM-CYCLE
           PERFORM 2360-ASSESS-OVERRIDE
           PERFORM 2370-AGE-OPEN
           PERFORM 2380-AGE-LAST-STMT
           PERFORM 2390-AGE-LAST-MAINT
           PERFORM 2400-EDIT-NAME
           PERFORM 2410-CHECK-NEXT-DUE
           PERFORM 2420-SPLIT-DRILL
           PERFORM 2430-SET-CYCLE
           PERFORM 2440-ACCUM-CYCLE
           PERFORM 2450-LIMIT-DRILL
           PERFORM 2460-DESCRIBE-CYCLE
           PERFORM 2470-ACCUM-BRANCH
           PERFORM 2480-COUNT-NOTICE
           PERFORM 2490-SELECT-DISCOUNT
           PERFORM 2500-SET-BOX-SIZE
           PERFORM 2510-ACCUM-BRANCH
           PERFORM 2520-FLAG-ITEM
           PERFORM 2530-CHECK-LAST-MAINT
           PERFORM 2540-ACCRUE-PENALTY-PCT
           PERFORM 2550-ACCRUE-PENALTY-PCT
           PERFORM 2560-CHECK-LAST-STMT
           PERFORM 2570-LIMIT-DRILL
           PERFORM 2580-SPLIT-DEPOSIT
           PERFORM 2590-AGE-OPEN
           PERFORM 2600-SPLIT-DRILL
           PERFORM 2610-ASSESS-OVERRIDE
           PERFORM 2620-CHECK-LAST-MAINT
           PERFORM 2630-ACCRUE-DRILL
           PERFORM 2640-ACCRUE-KEY-FEE
           PERFORM 2650-PRORATE-DEPOSIT
           PERFORM 2660-COUNT-RETRY
           PERFORM 2670-CHECK-OPEN
           PERFORM 2680-ACCRUE-PENALTY-PCT
           PERFORM 2690-ACCRUE-DRILL
           PERFORM 2700-SET-REASON
           PERFORM 2710-ACCRUE-RENT
           PERFORM 2720-DESCRIBE-CYCLE
           PERFORM 2730-SPLIT-RENT
           PERFORM 2740-EDIT-CONTACT
           PERFORM 2750-SPLIT-RENT
           PERFORM 2760-SPLIT-RENT
           PERFORM 2770-SELECT-DISCOUNT
           PERFORM 2780-PRORATE-DRILL
           PERFORM 2790-ASSESS-OVERRIDE
           PERFORM 2800-SET-REASON
           PERFORM 2810-PRORATE-DRILL
           PERFORM 2820-FLAG-DEPOSIT
           PERFORM 2830-CHECK-LAST-MAINT
           PERFORM 2840-COUNT-NOTICE
           PERFORM 2850-ACCRUE-DRILL
           PERFORM 2860-SET-BRANCH
           PERFORM 2870-CHECK-NEXT-DUE
           PERFORM 2880-PRORATE-DEPOSIT
           PERFORM 2890-PRORATE-DRILL
           PERFORM 2900-ACCRUE-KEY-FEE
           PERFORM 2910-SET-BRANCH
           PERFORM 2920-ACCUM-CYCLE
           PERFORM 2930-COUNT-NOTICE
           PERFORM 2940-ACCRUE-RENT
           PERFORM 2950-SELECT-PENALTY-PCT
           PERFORM 2960-FLAG-DRILL
           PERFORM 2970-EDIT-NAME
           PERFORM 2980-AGE-LAST-MAINT
           PERFORM 2990-ACCRUE-DRILL
           PERFORM 3000-PRORATE-DEPOSIT
           PERFORM 3010-DESCRIBE-BRANCH
           PERFORM 3020-EDIT-CONTACT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SDVAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2000-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDVAL-DEPOSIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2010-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDVAL-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDVAL-RENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRILL YTD.
      *----------------------------------------------------------------*
       2020-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDVAL-DRILL-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDVAL-DRILL-YTD
           END-IF
           ADD SDVAL-DRILL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * KEY FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2030-FLAG-KEY-FEE.
           IF SDVAL-KEY-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDVAL-KEY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2040-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-LAST-MAINT-DATE > 0 AND SDVAL-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDVAL-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDVAL-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDVAL-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDVAL-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDVAL-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2050-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO SDVAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2060-DESCRIBE-REASON.
           EVALUATE SDVAL-REASON-CD
               WHEN 'J'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-LAST-MAINT-DATE > 0 AND SDVAL-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2080-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDVAL-KEY-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SDVAL-KEY-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE SDVAL-RENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2090-COUNT-RETRY.
           IF SDVAL-RETRY-CNT < 9999999
               ADD 1 TO SDVAL-RETRY-CNT
           END-IF
           IF SDVAL-RETRY-CNT NOT < 6
               MOVE 'Y' TO SDVAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2100-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDVAL-KEY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2110-COUNT-NOTICE.
           IF SDVAL-NOTICE-CNT < 9999999
               ADD 1 TO SDVAL-NOTICE-CNT
           END-IF
           IF SDVAL-NOTICE-CNT NOT < 10
               MOVE 'Y' TO SDVAL-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2120-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDVAL-DEPOSIT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDVAL-DEPOSIT-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE YTD FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2130-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDVAL-KEY-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDVAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDVAL-KEY-FEE-YTD ROUNDED = SDVAL-DEPOSIT-AMT
                       * SDVAL-DISCOUNT / 200
               WHEN SDVAL-CYCLE-CD = 'AP'
                   MOVE 15.00 TO SDVAL-KEY-FEE-YTD
               WHEN SDVAL-CYCLE-CD = 'DA'
                   MOVE 15.00 TO SDVAL-KEY-FEE-YTD
               WHEN OTHER
                   COMPUTE SDVAL-KEY-FEE-YTD ROUNDED = SDVAL-DEPOSIT-AMT
                       * SDVAL-DISCOUNT / 100
           END-EVALUATE
           IF SDVAL-KEY-FEE-YTD > 0 AND SDVAL-KEY-FEE-YTD < 1.00
               MOVE 1.00 TO SDVAL-KEY-FEE-YTD
           END-IF
           IF SDVAL-KEY-FEE-YTD > 250.00
               MOVE 250.00 TO SDVAL-KEY-FEE-YTD
           END-IF
           ADD SDVAL-KEY-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN LAST STMT DATE IS PAST
      * FJW -> YFB WHEN LAST STMT DATE IS PAST
      * YFB -> EXS WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2140-SET-BRANCH.
           EVALUATE SDVAL-BRANCH-CD
               WHEN 'EXS'
                   IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2150-FLAG-DEPOSIT.
           IF SDVAL-DEPOSIT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDVAL-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2160-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDVAL-CYCLE-CD
                   ADD SDVAL-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDVAL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDVAL-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRILL AMT.
      *----------------------------------------------------------------*
       2170-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDVAL-DRILL-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDVAL-DRILL-AMT
           END-IF
           ADD SDVAL-DRILL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN NEXT DUE DATE IS PAST
      * FJW -> YFB WHEN NEXT DUE DATE IS PAST
      * YFB -> EXS WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2180-SET-BRANCH.
           EVALUATE SDVAL-BRANCH-CD
               WHEN 'EXS'
                   IF SDVAL-NEXT-DUE-DATE > 0 AND SDVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDVAL-NEXT-DUE-DATE > 0 AND SDVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDVAL-NEXT-DUE-DATE > 0 AND SDVAL-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2190-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SDVAL-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-NOTICE.
           IF SDVAL-NOTICE-CNT < 9999999
               ADD 1 TO SDVAL-NOTICE-CNT
           END-IF
           IF SDVAL-NOTICE-CNT NOT < 6
               MOVE 'Y' TO SDVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2210-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDVAL-DRILL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDVAL-DRILL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDVAL-DRILL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDVAL-DRILL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDVAL-DRILL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2220-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDVAL-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDVAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SDVAL-DEPOSIT-AMT *
                       SDVAL-PENALTY-PCT / 200
               WHEN SDVAL-BOX-SIZE-CD = 'CU'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN SDVAL-BOX-SIZE-CD = 'MG'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SDVAL-DEPOSIT-AMT *
                       SDVAL-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT RENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2230-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDVAL-RENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDVAL-RENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-RETRY.
           IF SDVAL-RETRY-CNT < 9999999
               ADD 1 TO SDVAL-RETRY-CNT
           END-IF
           IF SDVAL-RETRY-CNT NOT < 6
               MOVE 'Y' TO SDVAL-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDVAL-CYCLE-CD
                   ADD SDVAL-RENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDVAL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDVAL-RENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2260-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDVAL-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDVAL-RENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDVAL-REVIEW-FLAG = 'Y'
                   COMPUTE SDVAL-RENT-AMT ROUNDED = WS-WORK-AMT *
                       SDVAL-DISCOUNT / 200
               WHEN SDVAL-REASON-CD = 'J'
                   MOVE 5.00 TO SDVAL-RENT-AMT
               WHEN SDVAL-REASON-CD = 'K'
                   MOVE 7.00 TO SDVAL-RENT-AMT
               WHEN OTHER
                   COMPUTE SDVAL-RENT-AMT ROUNDED = WS-WORK-AMT *
                       SDVAL-DISCOUNT / 100
           END-EVALUATE
           IF SDVAL-RENT-AMT > 0 AND SDVAL-RENT-AMT < 1.00
               MOVE 1.00 TO SDVAL-RENT-AMT
           END-IF
           IF SDVAL-RENT-AMT > 35.00
               MOVE 35.00 TO SDVAL-RENT-AMT
           END-IF
           ADD SDVAL-RENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2270-COUNT-NOTICE.
           IF SDVAL-NOTICE-CNT < 9999999
               ADD 1 TO SDVAL-NOTICE-CNT
           END-IF
           IF SDVAL-NOTICE-CNT NOT < 6
               MOVE 'Y' TO SDVAL-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DRILL YTD.
      *----------------------------------------------------------------*
       2280-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDVAL-DRILL-YTD ROUNDED =
                   SDVAL-RENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDVAL-DRILL-YTD
           END-IF
           ADD SDVAL-DRILL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2290-COUNT-RETRY.
           IF SDVAL-RETRY-CNT < 9999999
               ADD 1 TO SDVAL-RETRY-CNT
           END-IF
           IF SDVAL-RETRY-CNT NOT < 12
               MOVE 'Y' TO SDVAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2300-COUNT-RETRY.
           IF SDVAL-RETRY-CNT < 9999999
               ADD 1 TO SDVAL-RETRY-CNT
           END-IF
           IF SDVAL-RETRY-CNT NOT < 5
               MOVE 'Y' TO SDVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2310-DESCRIBE-BOX-SIZE.
           EVALUATE SDVAL-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO KEY FEE YTD.
      *----------------------------------------------------------------*
       2320-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDVAL-KEY-FEE-YTD ROUNDED =
                   SDVAL-DEPOSIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDVAL-KEY-FEE-YTD
           END-IF
           ADD SDVAL-KEY-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2330-COUNT-RETRY.
           IF SDVAL-RETRY-CNT < 9999999
               ADD 1 TO SDVAL-RETRY-CNT
           END-IF
           IF SDVAL-RETRY-CNT NOT < 3
               MOVE 'Y' TO SDVAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN OPEN DATE IS PAST
      * K -> L WHEN OPEN DATE IS PAST
      * L -> M WHEN OPEN DATE IS PAST
      * M -> S WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2340-SET-REASON.
           EVALUATE SDVAL-REASON-CD
               WHEN 'J'
                   IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2350-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDVAL-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDVAL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2360-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SDVAL-RENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDVAL-REVIEW-FLAG = 'Y'
                   COMPUTE SDVAL-RENT-YTD ROUNDED = WS-WORK-AMT-2 *
                       SDVAL-DISCOUNT / 200
               WHEN SDVAL-BOX-SIZE-CD = 'CU'
                   MOVE 3.00 TO SDVAL-RENT-YTD
               WHEN SDVAL-BOX-SIZE-CD = 'MG'
                   MOVE 15.00 TO SDVAL-RENT-YTD
               WHEN OTHER
                   COMPUTE SDVAL-RENT-YTD ROUNDED = WS-WORK-AMT-2 *
                       SDVAL-DISCOUNT / 100
           END-EVALUATE
           IF SDVAL-RENT-YTD > 0 AND SDVAL-RENT-YTD < 5.00
               MOVE 5.00 TO SDVAL-RENT-YTD
           END-IF
           IF SDVAL-RENT-YTD > 35.00
               MOVE 35.00 TO SDVAL-RENT-YTD
           END-IF
           ADD SDVAL-RENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2370-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2380-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-LAST-MAINT-DATE > 0 AND SDVAL-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDVAL-RENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E734 NAME REQUIRED
      * E639 WORK AMT NEGATIVE
      * E977 WORK AMT OVER 1000000
      * E001 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2400-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDVAL-NAME = SPACES
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E639' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E977' TO WS-REJECT-CODE
               WHEN SDVAL-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E001' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2410-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SDVAL-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE SDVAL-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * AP -> DA WHEN LAST MAINT DATE IS PAST
      * DA -> HV WHEN LAST MAINT DATE IS PAST
      * HV -> SE WHEN LAST MAINT DATE IS PAST
      * SE -> AP WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2430-SET-CYCLE.
           EVALUATE SDVAL-CYCLE-CD
               WHEN 'AP'
                   IF SDVAL-LAST-MAINT-DATE > 0 AND
                       SDVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'DA' TO SDVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DA'
                   IF SDVAL-LAST-MAINT-DATE > 0 AND
                       SDVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HV' TO SDVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HV'
                   IF SDVAL-LAST-MAINT-DATE > 0 AND
                       SDVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'SE' TO SDVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SE'
                   IF SDVAL-LAST-MAINT-DATE > 0 AND
                       SDVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'AP' TO SDVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2440-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDVAL-CYCLE-CD
                   ADD SDVAL-RENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDVAL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDVAL-RENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2450-LIMIT-DRILL.
           IF SDVAL-DRILL-AMT < 25.00
               MOVE 25.00 TO SDVAL-DRILL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDVAL-DRILL-AMT > 500.00
               MOVE 500.00 TO SDVAL-DRILL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2460-DESCRIBE-CYCLE.
           EVALUATE SDVAL-CYCLE-CD
               WHEN 'AP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2470-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDVAL-BRANCH-CD
                   ADD SDVAL-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDVAL-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDVAL-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2480-COUNT-NOTICE.
           IF SDVAL-NOTICE-CNT < 9999999
               ADD 1 TO SDVAL-NOTICE-CNT
           END-IF
           IF SDVAL-NOTICE-CNT NOT < 3
               MOVE 'Y' TO SDVAL-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RENT TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2490-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-TIER-RATE(1) TO SDVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-RENT-TIER-LOW(WS-TX)
                   MOVE WS-RENT-TIER-RATE(WS-TX) TO SDVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN LAST STMT DATE IS PAST
      * MG -> VG WHEN LAST STMT DATE IS PAST
      * VG -> VN WHEN LAST STMT DATE IS PAST
      * VN -> XJ WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2500-SET-BOX-SIZE.
           EVALUATE SDVAL-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDVAL-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDVAL-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDVAL-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDVAL-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDVAL-BRANCH-CD
                   ADD SDVAL-KEY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDVAL-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDVAL-KEY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2520-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2530-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SDVAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2540-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDVAL-RENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2550-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDVAL-DRILL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SDVAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRILL YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2570-LIMIT-DRILL.
           IF SDVAL-DRILL-YTD < 10.00
               MOVE 10.00 TO SDVAL-DRILL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDVAL-DRILL-YTD > 1000.00
               MOVE 1000.00 TO SDVAL-DRILL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2580-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDVAL-DEPOSIT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDVAL-DEPOSIT-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDVAL-RENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2590-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDVAL-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SDVAL-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SDVAL-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDVAL-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDVAL-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2600-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE SDVAL-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT FEE ON THE DRILL YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2610-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SDVAL-RENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDVAL-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SDVAL-RENT-AMT ROUNDED = SDVAL-DRILL-YTD *
                       SDVAL-DISCOUNT / 200
               WHEN SDVAL-BOX-SIZE-CD = 'CU'
                   MOVE 20.00 TO SDVAL-RENT-AMT
               WHEN SDVAL-BOX-SIZE-CD = 'MG'
                   MOVE 15.00 TO SDVAL-RENT-AMT
               WHEN OTHER
                   COMPUTE SDVAL-RENT-AMT ROUNDED = SDVAL-DRILL-YTD *
                       SDVAL-DISCOUNT / 100
           END-EVALUATE
           IF SDVAL-RENT-AMT > 0 AND SDVAL-RENT-AMT < 1.00
               MOVE 1.00 TO SDVAL-RENT-AMT
           END-IF
           IF SDVAL-RENT-AMT > 25.00
               MOVE 25.00 TO SDVAL-RENT-AMT
           END-IF
           ADD SDVAL-RENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2620-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SDVAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2630-ACCRUE-DRILL.
           IF SDVAL-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDVAL-DRILL-AMT * SDVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDVAL-KEY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2640-ACCRUE-KEY-FEE.
           IF SDVAL-KEY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDVAL-KEY-FEE-YTD * SDVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       2650-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDVAL-DEPOSIT-AMT ROUNDED =
                   SDVAL-DRILL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDVAL-DEPOSIT-AMT
           END-IF
           ADD SDVAL-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2660-COUNT-RETRY.
           IF SDVAL-RETRY-CNT < 9999999
               ADD 1 TO SDVAL-RETRY-CNT
           END-IF
           IF SDVAL-RETRY-CNT NOT < 12
               MOVE 'Y' TO SDVAL-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2670-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SDVAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2680-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SDVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDVAL-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2690-ACCRUE-DRILL.
           IF SDVAL-DRILL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDVAL-DRILL-YTD * SDVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDVAL-RENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN OPEN DATE IS PAST
      * K -> L WHEN OPEN DATE IS PAST
      * L -> M WHEN OPEN DATE IS PAST
      * M -> S WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2700-SET-REASON.
           EVALUATE SDVAL-REASON-CD
               WHEN 'J'
                   IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2710-ACCRUE-RENT.
           IF SDVAL-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDVAL-RENT-AMT * SDVAL-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDVAL-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2720-DESCRIBE-CYCLE.
           EVALUATE SDVAL-CYCLE-CD
               WHEN 'AP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RENT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2730-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDVAL-RENT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SDVAL-RENT-YTD - (WS-WORK-AMT * 12)
           COMPUTE SDVAL-RENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E105 CONTACT REQUIRED
      * E526 WORK AMT NEGATIVE
      * E373 WORK AMT OVER 10000
      * E709 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2740-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDVAL-CONTACT = SPACES
                   MOVE 'E105' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E373' TO WS-REJECT-CODE
               WHEN SDVAL-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E709' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2750-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDVAL-RENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SDVAL-RENT-AMT - (WS-WORK-AMT * 12)
           COMPUTE SDVAL-DRILL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2760-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDVAL-RENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RENT TIER TIER FOR THE DRILL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2770-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-TIER-RATE(1) TO SDVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SDVAL-DRILL-YTD NOT < WS-RENT-TIER-LOW(WS-TX)
                   MOVE WS-RENT-TIER-RATE(WS-TX) TO SDVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRILL YTD.
      *----------------------------------------------------------------*
       2780-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDVAL-DRILL-YTD ROUNDED =
                   SDVAL-DRILL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDVAL-DRILL-YTD
           END-IF
           ADD SDVAL-DRILL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE RENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2790-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDVAL-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SDVAL-RENT-AMT *
                       SDVAL-DISCOUNT / 200
               WHEN SDVAL-BRANCH-CD = 'EXS'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN SDVAL-BRANCH-CD = 'FJW'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SDVAL-RENT-AMT *
                       SDVAL-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN LAST MAINT DATE IS PAST
      * K -> L WHEN LAST MAINT DATE IS PAST
      * L -> M WHEN LAST MAINT DATE IS PAST
      * M -> S WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2800-SET-REASON.
           EVALUATE SDVAL-REASON-CD
               WHEN 'J'
                   IF SDVAL-LAST-MAINT-DATE > 0 AND
                       SDVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'K' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF SDVAL-LAST-MAINT-DATE > 0 AND
                       SDVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SDVAL-LAST-MAINT-DATE > 0 AND
                       SDVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF SDVAL-LAST-MAINT-DATE > 0 AND
                       SDVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO SDVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DRILL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2810-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SDVAL-DRILL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2820-FLAG-DEPOSIT.
           IF SDVAL-DEPOSIT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDVAL-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2830-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SDVAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2840-COUNT-NOTICE.
           IF SDVAL-NOTICE-CNT < 9999999
               ADD 1 TO SDVAL-NOTICE-CNT
           END-IF
           IF SDVAL-NOTICE-CNT NOT < 3
               MOVE 'Y' TO SDVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2850-ACCRUE-DRILL.
           IF SDVAL-DRILL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDVAL-DRILL-YTD * SDVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN LAST STMT DATE IS PAST
      * FJW -> YFB WHEN LAST STMT DATE IS PAST
      * YFB -> EXS WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2860-SET-BRANCH.
           EVALUATE SDVAL-BRANCH-CD
               WHEN 'EXS'
                   IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDVAL-LAST-STMT-DATE > 0 AND SDVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2870-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO SDVAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       2880-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDVAL-DEPOSIT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDVAL-DEPOSIT-AMT
           END-IF
           ADD SDVAL-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE RENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DRILL YTD.
      *----------------------------------------------------------------*
       2890-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDVAL-DRILL-YTD ROUNDED =
                   SDVAL-RENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDVAL-DRILL-YTD
           END-IF
           ADD SDVAL-DRILL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2900-ACCRUE-KEY-FEE.
           IF SDVAL-KEY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDVAL-KEY-FEE-YTD * SDVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDVAL-DRILL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN OPEN DATE IS PAST
      * FJW -> YFB WHEN OPEN DATE IS PAST
      * YFB -> EXS WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2910-SET-BRANCH.
           EVALUATE SDVAL-BRANCH-CD
               WHEN 'EXS'
                   IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDVAL-OPEN-DATE > 0 AND SDVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2920-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDVAL-CYCLE-CD
                   ADD SDVAL-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDVAL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDVAL-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2930-COUNT-NOTICE.
           IF SDVAL-NOTICE-CNT < 9999999
               ADD 1 TO SDVAL-NOTICE-CNT
           END-IF
           IF SDVAL-NOTICE-CNT NOT < 5
               MOVE 'Y' TO SDVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2940-ACCRUE-RENT.
           IF SDVAL-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDVAL-RENT-AMT * SDVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RENT TIER TIER FOR THE DRILL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2950-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-TIER-RATE(1) TO SDVAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SDVAL-DRILL-AMT NOT < WS-RENT-TIER-LOW(WS-TX)
                   MOVE WS-RENT-TIER-RATE(WS-TX) TO SDVAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDVAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDVAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2960-FLAG-DRILL.
           IF SDVAL-DRILL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDVAL-DRILL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E734 NAME REQUIRED
      * E885 WORK AMT NEGATIVE
      * E588 WORK AMT OVER 50000
      * E800 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2970-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDVAL-NAME = SPACES
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E588' TO WS-REJECT-CODE
               WHEN SDVAL-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2980-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDVAL-LAST-MAINT-DATE > 0 AND SDVAL-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDVAL-RENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDVAL-RENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDVAL-RENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDVAL-RENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDVAL-RENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2990-ACCRUE-DRILL.
           IF SDVAL-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDVAL-DRILL-AMT * SDVAL-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDVAL-RENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDVAL-DEPOSIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-BRANCH.
           EVALUATE SDVAL-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E208 CONTACT REQUIRED
      * E739 RENT YTD NEGATIVE
      * E956 RENT YTD OVER 25000
      * E973 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDVAL-CONTACT = SPACES
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN SDVAL-RENT-YTD < 0
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN SDVAL-RENT-YTD > 25000.00
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN SDVAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E973' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SDVAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SDVAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SDVAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SDVAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
